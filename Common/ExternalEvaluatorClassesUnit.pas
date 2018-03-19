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
   Classes,
   contnrs,
   System.SysUtils,
   System.Generics.Collections
   ;

const
  EV_STATUS_OK = 0;
  EV_STATUS_ERROR = 1;

  CURRENT_DNS_EA_BRIDGE_VERSION = 2;

  DNSEABRIDGE_ERROR_LOG_APPDATA_FOLDER = 'De Novo Software\External Application Bridge\';
  DNSEABRIDGE_ERROR_LOG_FILE_NAME = 'DNSEABridgeErrors.txt';

  DNSEABRIDGE_ERROR_LOG = DNSEABRIDGE_ERROR_LOG_APPDATA_FOLDER + DNSEABRIDGE_ERROR_LOG_FILE_NAME;

type
  TGenericDataType = (
    gdt_IsSingle,
    gdt_IsClassification);

  TAbstractExternalEvaluatorResultData = class(TComponent)
  private
    FParamType: TGenericDataType;
    FParamName: string;
  public
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
  published
    property ParamType: TGenericDataType read FParamType write FParamType;
    property ParamName: string read FParamName write FParamName;
  end;

  TExternalEvaluatorResultData<T> = class(TAbstractExternalEvaluatorResultData)
  private
    FData: TArray<T>;
  protected
    procedure SetData(const aData: TArray<T>);
  public
    property Data: TArray<T> read FData write SetData;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

  TClusterAssignmentResultData = class(TExternalEvaluatorResultData<integer>)
  private
    FClusterNames: TStringList;
    FNumOfClusters: Integer;
  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
  published
    property ClusterNames:TStringList read FClusterNames write FClusterNames;
    property NumOfClusters:integer read FNumOfClusters write FNumOfClusters;
  end;

  TNewParameterResultData = class(TExternalEvaluatorResultData<double>)
  private
  public
    constructor Create(aowner: TComponent); override;
  end;

  TExternalEvaluatorResultDataList = class(TObjectList<TAbstractExternalEvaluatorResultData>)
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
  end;

  TExternalEvaluatorResult = class(TComponent)
  private
    FResultDataList: TExternalEvaluatorResultDataList;
    FStatus: Integer;
    FErrorMessage: string;
 protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure readResultDataList(aStream: TStream);
    procedure writeResultDataList(aStream: TStream);
  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    function GetResult(aIndex: Integer): TAbstractExternalEvaluatorResultData;
    procedure AddResultData(aResultData: TAbstractExternalEvaluatorResultData);
    property ResultDataList: TExternalEvaluatorResultDataList read FResultDataList write FResultDataList;
  published
    property Status: Integer read FStatus write FStatus;
    property Errormessage: string read FErrorMessage write FErrormessage;
  end;

  TDynMatrix<T> = array of TArray<T>;

  TExternalEvaluatorinput = class(TComponent)
  private
    FScriptFileName: string;
    FInputMatrix: TDynMatrix<double>;
    FParameterNames: TStringList;
    FEventsAsRows: boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure readInputMatrix(fromStream: TStream);
    procedure writeInputMatrix(toStream: TStream);
    procedure SetParameterNames(const Value: TStringList);
  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SizeMatrix(aRow: Integer; aCol: Integer);
    // The colums for this matrix contain the data parameters. The matrix row data are the
    // cells/data points.
    property InputMatrix: TDynMatrix<double> read FInputMatrix write FInputMatrix;
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
  ver: integer;
begin
  ver := 1;
  aStream.Write(ver, sizeof(ver));

  numElements:= Length(Data);
  aStream.Write(numElements, sizeof(numElements));

  for cntr :=0 to numElements - 1 do
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
  aStream.Read(ver,sizeof(ver));

  if ver <> 1 then
  begin
    raise Exception.Create('TExternalEvaluatorResultData incorrect version number');
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
  ver: integer;
  cntr: integer;
  numElements: integer;
begin
  ver := 1;
  aStream.Write(ver, sizeof(ver));
  numElements:= count;
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

  if ver <> 1 then
  begin
    raise Exception.Create('TExternalEvaluatorResultDataList incorrect version number');
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

procedure TExternalEvaluatorResult.readResultDataList(aStream: TStream);
var
  ver: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> 1 then
  begin
    raise Exception.Create('TExternalEvaluatorResultList incorrect version number');
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

procedure TExternalEvaluatorResult.writeResultDataList(aStream: TStream);
var
ver : integer;
begin
  ver := 1;
  aStream.Write(ver,sizeof(ver));

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

procedure TExternalEvaluatorResult.SaveToStream(aStream: TStream);
var
  ver: integer;
begin
  ver := 1;
  aStream.Write(ver, sizeof(ver));
  aStream.WriteComponent(self);
end;

procedure TExternalEvaluatorResult.LoadFromStream(aStream: TStream);
var
  ver: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> 1 then
  begin
    raise Exception.Create('TExternalEvaluatorResult incorrect version number');
  end;

  aStream.ReadComponent(self);
end;

constructor TClusterAssignmentResultData.Create(aowner: TComponent);
begin
  inherited
  ParamType := gdt_IsClassification;
  FClusterNames := TStringList.Create;
end;

destructor TClusterAssignmentResultData.Destroy;
begin
  FClusterNames.Free;
  inherited;
end;

constructor TNewParameterResultData.Create(aowner: TComponent);
begin
  inherited
  ParamType := gdt_IsSingle;
end;

constructor TExternalEvaluatorinput.Create(aowner: TComponent);
begin
  inherited;
  FParameterNames := TStringList.Create;
end;

destructor TExternalEvaluatorinput.Destroy;
begin
  FParameterNames.Free;
  inherited;
end;

procedure TExternalEvaluatorinput.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('InputMatrix', readInputMatrix, writeInputMatrix, True);
end;

procedure TExternalEvaluatorinput.readInputMatrix(fromStream: TStream);
var
  cntrRow: integer;
  cntrColumn: Integer;
  numColElements: integer;
  numRowElements: integer;
  curResult : single;
  ver: integer;
begin
  fromStream.read(ver, sizeof(ver));
  fromStream.read(numColElements,sizeOf(numColElements));
  fromStream.read(numRowElements,sizeOf(numRowElements));
  SetLength(FInputmatrix, numRowElements, numColElements);

  for cntrRow :=0 to numRowElements - 1 do
    for cntrColumn := 0 to numColElements -1 do
    begin
      fromStream.Read(curResult,sizeof(curResult));
      InputMatrix[cntrRow][cntrColumn] := curResult;
    end
end;

procedure TExternalEvaluatorinput.writeInputMatrix (toStream: TStream);
var
  cntrRow: integer;
  cntrColumn: Integer;
  numColElements: integer;
  numRowElements: integer;
  curResult : single;
  ver: integer;
begin
  ver:=1;
  toStream.Write(ver,sizeof(ver));

  numRowElements:= Length(InputMatrix);

  if numRowElements > 0 then
    numColElements:= Length(InputMatrix[0])
  else
   numColElements := 0;

  toStream.Write(numColElements, sizeof(numColElements));
  toStream.Write(numRowElements, sizeof(numRowElements));

  for cntrRow :=0 to numRowElements - 1 do
    for cntrColumn :=0 to numColElements - 1 do
    begin
      curResult := InputMatrix[cntrRow][cntrColumn];
      toStream.Write(curResult, sizeof(curResult));
  end
end;

procedure TExternalEvaluatorinput.SaveToStream(aStream: TStream);
var
  ver: integer;
begin
  ver := 1;
  aStream.Write(ver,sizeof(ver));
  aStream.WriteComponent(self)
end;

procedure TExternalEvaluatorinput.LoadFromStream(aStream: TStream);
var
ver: integer;
begin
  aStream.Read(ver,sizeof(ver));

  if ver <> 1 then
  begin
    raise Exception.Create('TExternalEvaluatorinput incorrect version number');
  end;

  aStream.ReadComponent(self)
end;

procedure TExternalEvaluatorinput.SizeMatrix(aRow: Integer; aCol: Integer);
begin
  SetLength(FInputMatrix, aRow, aCol);
end;

procedure TExternalEvaluatorinput.SetParameterNames(const Value: TStringList);
begin
  FParameterNames.Assign(Value);
end;

initialization
  registerClasses([TExternalEvaluatorinput, TExternalEvaluatorResult,
                    TClusterAssignmentResultData, TNewParameterResultData]);

end.

