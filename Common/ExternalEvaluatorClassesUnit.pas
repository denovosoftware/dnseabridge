unit ExternalEvaluatorClassesUnit;

{***************************************************************************************************
This De Novo Software External Application Bridge accepts data via TCP/IP and
converts it into a format required for the opaR library.

Copyright (C) 2016 De Novo Software

This program is free software: you can redistribute it and/or modify it under the terms of 
the GNU General Public License as published by the Free Software Foundation, either 
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  
If not, see <http://www.gnu.org/licenses/>.

***************************************************************************************************}



interface

uses
   System.Classes,

   System.SysUtils,
   System.Generics.Collections
   ;

const
  EV_STATUS_OK = 0;
  EV_STATUS_ERROR = 1;

  CURRENT_DNS_EA_BRIDGE_VERSION = 3;

  DNSEABRIDGE_ERROR_LOG_APPDATA_FOLDER = 'De Novo Software\External Application Bridge\';
  DNSEABRIDGE_ERROR_LOG_FILE_NAME = 'DNSEABridgeErrors.txt';

  DNSEABRIDGE_ERROR_LOG = DNSEABRIDGE_ERROR_LOG_APPDATA_FOLDER + DNSEABRIDGE_ERROR_LOG_FILE_NAME;
  DNSEABRIDGE_NO_UI_CMD_PARAM = '-NoUI';

type
  TGenericDataType = (
    gdt_IsSingle,
    gdt_IsClassification,
    gdt_NoData);

  TAbstractExternalEvaluatorResultData = class(TComponent)
  strict private
    FParamType: TGenericDataType;
    FParamName: string;
  public
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
    procedure AfterConstruction; override;
  published
    property ParamType: TGenericDataType read FParamType write FParamType;
    property ParamName: string read FParamName write FParamName;
  end;

  TExternalEvaluatorResultData<T> = class(TAbstractExternalEvaluatorResultData)
  const
    STREAM_VERSION: integer = 1;
  strict private
    FData: TArray<T>;
    procedure SetData(const aData: TArray<T>);
  public
    property Data: TArray<T> read FData write SetData;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

  TClusterAssignmentResultData = class(TExternalEvaluatorResultData<integer>)
  strict private
    FClusterNames: TStringList;
    FNumOfClusters: Integer;
  public
    constructor Create(aowner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  published
    property ClusterNames: TStringList read FClusterNames write FClusterNames;
    property NumOfClusters: integer read FNumOfClusters write FNumOfClusters;
  end;

  TNewParameterResultData = class(TExternalEvaluatorResultData<double>)
  public
    procedure AfterConstruction; override;
  end;

  TAutoGatingExternalEvaluatorResultData = class(TAbstractExternalEvaluatorResultData)
  const
    GATING_ML_STREAM_VERSION: integer = 1;
  strict private
    FGatingMl: String;
  public
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure AfterConstruction; override;
    property GatingMl: String read FGatingMl write FGatingMl;
  end;

  TExternalEvaluatorResultDataList = class(
      TObjectList<TAbstractExternalEvaluatorResultData>)
  const
    STREAM_VERSION: integer = 1;
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
  end;

  TExternalEvaluatorResult = class(TComponent)
  const
    STREAM_VERSION: integer = 1;
    RESULT_DATA_LIST_STREAM_VERSION: integer = 1;
  strict private
    FResultDataList: TExternalEvaluatorResultDataList;
    FStatus: Integer;
    FErrorMessage: string;
 strict protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadResultDataList(aStream: TStream);
    procedure WriteResultDataList(aStream: TStream);
  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    function GetResult(aIndex: Integer): TAbstractExternalEvaluatorResultData;
    procedure AddResultData(aResultData: TAbstractExternalEvaluatorResultData);
    property ResultDataList: TExternalEvaluatorResultDataList read FResultDataList write FResultDataList;
  published
    property Status: Integer read FStatus write FStatus;
    property Errormessage: string read FErrorMessage write FErrormessage;
  end;

  TArrayMatrix<T> = array of TArray<T>;
  TDoubleMatrix = TArrayMatrix<Double>;

  TExternalEvaluatorInput = class(TComponent)
  const
    STREAM_VERSION: integer = 1;
    INPUT_MTX_STREAM_VERSION: integer = 1;
  strict private
    FScriptFileName: string;
    FInputMatrix: TDoubleMatrix;
    FParameterNames: TStringList;
    FEventsAsRows: boolean;
    procedure ReadInputMatrix(fromStream: TStream);
    procedure WriteInputMatrix(toStream: TStream);
    procedure SetParameterNames(const Value: TStringList);
  strict protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SizeMatrix(aRow: Integer; aCol: Integer);
    // The colums for this matrix contain the data parameters. The matrix row data are the
    // cells/data points.
    property InputMatrix: TDoubleMatrix read FInputMatrix write FInputMatrix;
  published
    // The script file name is the path and name of the script that will be executeded by the  evaluator.
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    property ParameterNames: TStringList read FParameterNames write SetParameterNames;
    property EventsAsRows: boolean read FEventsAsRows write FEventsAsRows;
  end;


  INeedConnectionCompleteNotifications = interface
    ['{036359C5-5762-4D3F-8A7D-924EBDAEAF7C}']
    procedure ConnectionCompleted;
  end;

  IBasicEvaluator = Interface
    ['{DFABC3D7-9934-4A85-BCC1-B6546A26B177}']
    function IdentifierName: string;
    procedure InitializeConnections;
    procedure AddConnectionCompleteNotifier(const aNeedsConnectionNotification:
        INeedConnectionCompleteNotifications);
    procedure RemoveConnectionCompleteNotifier(const aNeedsConnectionNotification:
        INeedConnectionCompleteNotifications);
  end;

implementation

procedure TExternalEvaluatorResultData<T>.SetData(const aData: TArray<T>);
begin
  FData := copy(aData)
end;

procedure TExternalEvaluatorResultData<T>.SaveToStream(aStream: TStream);
var
  cntr: Integer;
  curResult: T;
  numElements: Integer;
begin
  aStream.Write(STREAM_VERSION, sizeof(STREAM_VERSION));

  numElements:= Length(Data);
  aStream.Write(numElements, sizeof(numElements));

  for cntr := 0 to numElements - 1 do
  begin
    curResult := Data[cntr];
    aStream.Write(curResult, sizeof(curResult));
  end
end;

procedure TExternalEvaluatorResultData<T>.LoadFromStream(aStream: TStream);
var
  cntr: Integer;
  curResult: T;
  numElements: Integer;
  ver: integer;
begin
  aStream.Read(ver, sizeof(ver));

  if ver <> STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number streaming ' +
        'TExternalEvaluatorResultData.%sExpected: %d Found: %d',
        [sLineBReak, STREAM_VERSION, ver]);
  end;

  aStream.read(numElements,sizeOf(numElements));
  SetLength(FData, numElements);

  for cntr :=0 to numElements - 1 do
  begin
    aStream.Read(curResult,sizeof(curResult));
    FData[cntr] := curResult;
  end
end;

procedure TExternalEvaluatorResultDataList.SaveToStream(aStream: TStream);
var
  cntr: integer;
  numElements: integer;
begin
  aStream.Write(STREAM_VERSION, sizeof(STREAM_VERSION));
  numElements := Count;
  aStream.Write(numElements, sizeof(numElements));
  for cntr := 0 to numElements - 1 do
  begin
    aStream.WriteComponent(Items[cntr]);
    Items[cntr].SaveToStream(aStream);
  end
end;

procedure TExternalEvaluatorResultDataList.LoadFromStream(aStream: TStream);
var
  ver: integer;
  CurResult: TAbstractExternalEvaluatorResultData;
  cntr: integer;
  numElements: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect ersion number streaming ' +
        'TExternalEvaluatorResultDataList.%sExpected: %d Found: %d',
        [sLineBReak, STREAM_VERSION, ver]);
  end;

  aStream.read(numElements,sizeOf(numElements));
  for cntr := 0 to numElements - 1 do
  begin
    curResult:=aStream.ReadComponent(nil) as TAbstractExternalEvaluatorResultData;
    curResult.LoadFromStream(aStream);
    Add(curResult)
  end;
end;

procedure TExternalEvaluatorResult.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ResultDataList', ReadResultDataList, WriteResultDataList, True);
end;

procedure TExternalEvaluatorResult.ReadResultDataList(aStream: TStream);
var
  ver: integer;
begin
  aStream.Read(ver, sizeof(integer));

  if ver <> RESULT_DATA_LIST_STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number loading ' +
        'TExternalEvaluatorResult.ResultDataList.%sExpected: %d. Found: %d.',
        [sLineBreak, RESULT_DATA_LIST_STREAM_VERSION, ver]);
  end;

  FResultDataList.LoadFromStream(aStream);
end;

constructor TExternalEvaluatorResult.Create(aowner: TComponent);
begin
  inherited;
  FResultDataList := TExternalEvaluatorResultDataList.Create;
end;

destructor TExternalEvaluatorResult.Destroy;
begin
  FResultDataList.Free;
  inherited;
end;

procedure TExternalEvaluatorResult.WriteResultDataList(aStream: TStream);
begin
  aStream.Write(RESULT_DATA_LIST_STREAM_VERSION, SizeOf(RESULT_DATA_LIST_STREAM_VERSION));
  FResultDataList.SaveToStream(aStream);
end;

function TExternalEvaluatorResult.GetResult(aIndex: Integer): TAbstractExternalEvaluatorResultData;
begin
  result := FResultDataList[aIndex];
end;

procedure TExternalEvaluatorResult.AddResultData(aResultData: TAbstractExternalEvaluatorResultData);
begin
  FResultDataList.add(aResultData);
end;

procedure TExternalEvaluatorResult.AfterConstruction;
begin
  inherited;
  FStatus := EV_STATUS_ERROR;
  FErrorMessage := '';
end;

procedure TExternalEvaluatorResult.SaveToStream(aStream: TStream);
begin
  aStream.Write(STREAM_VERSION, SizeOf(STREAM_VERSION));
  aStream.WriteComponent(self);
end;

procedure TExternalEvaluatorResult.LoadFromStream(aStream: TStream);
var
  ver: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number loading ' +
        'TExternalEvaluatorResult.%sExpected: %d. Found: %d.',
        [sLineBreak, STREAM_VERSION, ver]);
  end;

  aStream.ReadComponent(self);
end;

constructor TClusterAssignmentResultData.Create(aowner: TComponent);
begin
  inherited;
  FClusterNames := TStringList.Create;
end;

destructor TClusterAssignmentResultData.Destroy;
begin
  FClusterNames.Free;
  inherited;
end;

procedure TClusterAssignmentResultData.AfterConstruction;
begin
  inherited;
  ParamType := gdt_IsClassification;
end;


procedure TNewParameterResultData.AfterConstruction;
begin
  inherited
  ParamType := gdt_IsSingle;
end;

procedure TAutoGatingExternalEvaluatorResultData.SaveToStream(aStream: TStream);
var
  len: Cardinal;
begin
  inherited;
  len := Length(FGatingMl);

  aStream.Write(GATING_ML_STREAM_VERSION, SizeOf(GATING_ML_STREAM_VERSION));
  aStream.Write(len, SizeOf(len));
  aStream.Write(PChar(FGatingMl)^, len * SizeOf(Char));
end;

procedure TAutoGatingExternalEvaluatorResultData.LoadFromStream(aStream: TStream);
var
  len: Cardinal;
  streamedCharsBuffer: array of Char;
  bufferAsPChar: PChar;
  ver: integer;
begin
  inherited;
  aStream.Read(ver, SizeOf(ver));
  if ver <> GATING_ML_STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number loading ' +
        'TAutoGatingExternalEvaluatorResultData.%sExpected: %d. Found: %d.',
        [sLineBreak, GATING_ML_STREAM_VERSION, ver]);
  end;

  // Read the current stream into a array of char. This array will larger than
  // the FGatingMl to account for null termination.
  // Explicitly set the last char to #0 so that we have a valid null terminated PChar
  aStream.Read(len, SizeOf(len));
  SetLength(streamedCharsBuffer, len + 1);
  streamedCharsBuffer[len] := #0;

  bufferAsPChar := @(streamedCharsBuffer[0]);
  aStream.Read(bufferAsPChar^, len * SizeOf(Char));
  FGatingMl := bufferAsPChar;
end;

procedure TAutoGatingExternalEvaluatorResultData.AfterConstruction;
begin
  inherited;
  ParamType := gdt_NoData;
end;

constructor TExternalEvaluatorInput.Create(aowner: TComponent);
begin
  inherited;
  FParameterNames := TStringList.Create;
end;

destructor TExternalEvaluatorInput.Destroy;
begin
  FParameterNames.Free;
  inherited;
end;

procedure TExternalEvaluatorInput.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('InputMatrix', ReadInputMatrix, WriteInputMatrix, True);
end;

procedure TExternalEvaluatorInput.ReadInputMatrix(fromStream: TStream);
var
  cntrRow: integer;
  cntrColumn: Integer;
  numColElements: integer;
  numRowElements: integer;
  curResult : single;
  ver: integer;
begin
  fromStream.read(ver, sizeof(ver));

  if ver <> INPUT_MTX_STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number loading ' +
        'TExternalEvaluatorInput.InputMatrix.%sExpected: %d. Found: %d.',
        [sLineBreak, STREAM_VERSION, ver]);
  end;

  fromStream.Read(numColElements, SizeOf(numColElements));
  fromStream.Read(numRowElements, SizeOf(numRowElements));
  SetLength(FInputMatrix, numRowElements, numColElements);

  for cntrRow :=0 to numRowElements - 1 do
  begin
    for cntrColumn := 0 to numColElements -1 do
    begin
      fromStream.Read(curResult, sizeof(curResult));
      InputMatrix[cntrRow][cntrColumn] := curResult;
    end;
  end;
end;

procedure TExternalEvaluatorInput.writeInputMatrix(toStream: TStream);
var
  cntrRow: integer;
  cntrColumn: Integer;
  numColElements: integer;
  numRowElements: integer;
  curResult : single;
begin
  toStream.Write(INPUT_MTX_STREAM_VERSION, sizeof(INPUT_MTX_STREAM_VERSION));

  numRowElements:= Length(InputMatrix);

  if numRowElements > 0 then
    numColElements:= Length(InputMatrix[0])
  else
   numColElements := 0;

  toStream.Write(numColElements, SizeOf(numColElements));
  toStream.Write(numRowElements, SizeOf(numRowElements));

  for cntrRow :=0 to numRowElements - 1 do
  begin
    for cntrColumn :=0 to numColElements - 1 do
    begin
      curResult := InputMatrix[cntrRow][cntrColumn];
      toStream.Write(curResult, sizeof(curResult));
    end;
  end;
end;

procedure TExternalEvaluatorInput.SaveToStream(aStream: TStream);
begin
  aStream.Write(STREAM_VERSION, sizeof(STREAM_VERSION));
  aStream.WriteComponent(self);
end;

procedure TExternalEvaluatorInput.LoadFromStream(aStream: TStream);
var
  ver: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> STREAM_VERSION then
  begin
    raise Exception.CreateFmt('Incorrect version number found when streaming ' +
        'TExternalEvaluatorInput.%sExpected: %d Found: %d',
        [sLineBReak, STREAM_VERSION, ver]);
  end;

  aStream.ReadComponent(self);
end;

procedure TExternalEvaluatorInput.SizeMatrix(aRow: Integer; aCol: Integer);
begin
  SetLength(FInputMatrix, aRow, aCol);
end;

procedure TExternalEvaluatorInput.SetParameterNames(const Value: TStringList);
begin
  FParameterNames.Assign(Value);
end;

procedure TAbstractExternalEvaluatorResultData.AfterConstruction;
begin
  inherited;
  FParamType := TGenericDataType.gdt_NoData;
  FParamName := '';
end;

initialization
  RegisterClasses([TExternalEvaluatorInput, TExternalEvaluatorResult,
                    TClusterAssignmentResultData, TNewParameterResultData,
                    TAutoGatingExternalEvaluatorResultData]);

end.

