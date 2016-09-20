unit DNSEABridgeVCLDataModuleUnit;

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
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  System.Actions,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Tether.Manager,
  System.Tether.AppProfile,

  {Indy Sockets}
  IPPeerClient,
  IPPeerServer,

  {opaR}
  opaR.Engine,
  opaR.Interfaces,

  {De Novo Software External Application Bridge}
  ExternalEvaluatorClassesUnit,
  AboutDNSEABridgeFormUnit;

type
  TDNSEABridgeVCLDataModule = class(TDataModule)
    DenovoRemoteOpaRTetheringManager: TTetheringManager;
    TetheringAppProfile: TTetheringAppProfile;
    DNSEABridgeActionList: TActionList;
    acRunCluster: TAction;
    acNewParam: TAction;
    acCloseDNSEABridge: TAction;
    TrayIconDNSEABridge: TTrayIcon;
    popupMenuDNSEABridge: TPopupMenu;
    mniAbout: TMenuItem;
    mniQuit: TMenuItem;
    procedure acCloseDNSEABridgeExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure InputDataResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure acRunClusterExecute(Sender: TObject);
    procedure acNewParamExecute(Sender: TObject);
    procedure ExitDNSEABridgeWithWarning(Sender: TObject);
    procedure ShowAboutForm(Sender: TObject);
  strict private
    FEngine: IREngine;
    FInputData: TExternalEvaluatorinput;
    procedure CloseDNSEABridge;
  strict protected
    function GetS4Results(const aInput: TExternalEvaluatorInput; aResultData:
      TExternalEvaluatorResult): IS4Object;
    function ValidateS4Results(const aNumNewParams: integer; aNames:
        TArray<string>; const aNumElements: integer): boolean;
    procedure SetErrorMessage(const aErrorMessage: string; aResult:
        TExternalEvaluatorResult);
    procedure SetResultResource(aResult: TExternalEvaluatorResult);
  end;

var
  DNSEABridgeVCLDataModule: TDNSEABridgeVCLDataModule;

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  Windows;

var
  DNSEABridgeMutexHandle: THandle;


const
  WARNING_CLOSE_SERVER = 'Exiting the De Novo Software External Application '+
    'Bridge can cause failure in FCS Express data transformations '+
    'that interact with the application bridge. ' +
    sLineBreak + 'Do you wish to close the De Novo Software External Application Bridge?';
  INPUT_NAME = 'RInputData';
  RESULT_NAME = 'RResult';
  SERVER_VERSION_NAME = 'ServerVersion';

procedure TDNSEABridgeVCLDataModule.acCloseDNSEABridgeExecute(Sender: TObject);
begin
  CloseDNSEABridge;
end;

procedure TDNSEABridgeVCLDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FInputData);
end;

procedure TDNSEABridgeVCLDataModule.DataModuleCreate(Sender: TObject);
begin
  try
    TREngine.SetEnvironmentVariables;
    FEngine := TREngine.GetInstance;
    FInputData := TExternalEvaluatorInput.Create(nil);
    TetheringAppProfile.Resources.FindByName(SERVER_VERSION_NAME).Value := 1;
  except
    on E: exception do
      // Something went wrong with the service. Make the version invalid so we don't even try to connect to it.
      TetheringAppProfile.Resources.FindByName(SERVER_VERSION_NAME).Value := 0;
  end;
end;

procedure TDNSEABridgeVCLDataModule.InputDataResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
begin
  // Check for input data
  if (AResource.Name = INPUT_NAME) then
  begin
   AResource.Value.AsStream.Position := 0;
   FInputData.LoadFromStream(AResource.Value.AsStream);
  end;
end;

procedure TDNSEABridgeVCLDataModule.acRunClusterExecute(Sender: TObject);
var
 clusteringResult: TExternalEvaluatorResult;
 s4Results: IS4Object;
 clusterNames: Tarray<string>;
 numberOfClusters: integer;
 clusterAssignments: Tarray<integer>;
 newClusterResultData: TClusterAssignmentResultData;
begin
  clusteringResult := TExternalEvaluatorResult.Create(nil);

  try
  begin
    FInputData.LoadFromStream(TetheringAppProfile.GetRemoteResourceValue(
      DenovoRemoteOpaRTetheringManager.RemoteProfiles.Items[0],INPUT_NAME).Value.AsStream);
    s4Results := GetS4Results(FInputData, clusteringResult);

    if (s4Results <> nil) then
    begin
      clusterAssignments := s4Results['clusterAssignments'].AsInteger.ToArray;
      numberofClusters := s4Results['numberOfClusters'].AsInteger.First;
      clusterNames := s4Results['clusterNames'].AsCharacter.ToArray;

      if ValidateS4Results(numberOfClusters, clusterNames, Length(clusterAssignments)) then
      begin
        newClusterResultData:= TClusterAssignmentResultData.Create(nil);
        newClusterResultData.ParamName:= IntToStr(numberOfClusters) + ' ClusterAssignments';
        newClusterResultData.Data:= clusterAssignments;
        newClusterResultData.ClusterNames.AddStrings(clusterNames);
        newClusterResultData.NumOfClusters:=numberofClusters;
        clusteringResult.AddResultData(newClusterResultData);
        clusteringResult.Status := EV_STATUS_OK;
      end
      else
        SetErrorMessage('Number of clusters did not match results.', clusteringResult);
    end
    else
     SetErrorMessage('Cluster result data is not valid.', clusteringResult);
  end
  except
  on e: Exception do
    SetErrorMessage('An exception occurred while processing cluster asignment.',
      clusteringResult);
  end;

  SetResultResource(clusteringResult);
  FreeAndNil(clusteringResult);
end;

procedure TDNSEABridgeVCLDataModule.acNewParamExecute(Sender: TObject);
var
  newParamResult: TExternalEvaluatorResult;
  cntr: Integer;
  s4Results: IS4Object;
  numberOfNewParams: integer;
  newParamNames: Tarray<string>;
  newColumns: TDynMatrix<double>;
  newParamResultData: TnewParameterResultData;
begin
  newParamResult := TExternalEvaluatorResult.Create(nil);
  try
  begin
    FInputData.LoadFromStream(TetheringAppProfile.GetRemoteResourceValue(
      DenovoRemoteOpaRTetheringManager.RemoteProfiles.Items[0],INPUT_NAME).Value.AsStream);
    s4Results := GetS4Results(FInputData, newParamResult);

    if (s4Results <> nil) then
    begin
      newColumns:= s4Results['newParamData'].AsNumericMatrix.GetArrayFast;
      numberOfNewParams:= s4Results['numberOfNewParams'].AsInteger.First;
      newParamNames:= s4Results['newParamNames'].AsCharacter.ToArray;

      if ValidateS4Results(numberOfNewParams, newParamNames, length(newColumns))then
      begin
        for cntr := 0 to numberOfNewParams -1 do
        begin
          newParamResultData:= TNewParameterResultData.Create(nil);
          newParamResultData.ParamName:= newParamNames[cntr];
          newParamResultData.Data:= newColumns[cntr];
          newParamResult.AddResultData(newParamResultData);
          newParamResult.Status := EV_STATUS_OK;
        end
      end
      else
        SetErrorMessage ('Number of new parameters did not match results.', newParamResult);
    end
    else
      SetErrorMessage('Parameter result data invalid.', newParamResult);
  end;

  except
    on e: Exception do
      SetErrorMessage('An exception occurred while processing new parameter ' +
                        'transformation.', newParamResult);
  end;

  SetResultResource(newParamResult);
  FreeAndNil(newParamResult);
end;

procedure TDNSEABridgeVCLDataModule.CloseDNSEABridge;
begin
  TrayIconDNSEABridge.Visible := False;
  Application.ProcessMessages;
  TrayIconDNSEABridge.Free;
  TetheringAppProfile.Free;
  DenovoRemoteOpaRTetheringManager.Free;
  Application.MainForm.Close;
end;

procedure TDNSEABridgeVCLDataModule.SetResultResource(aResult: TExternalEvaluatorResult);
var
  outStream: TMemoryStream;
begin
  outStream:= TMemoryStream.Create;
  outStream.Position := 0;
  aResult.SaveToStream(outStream);
  outStream.Position := 0;
  TetheringAppProfile.Resources.FindByName(RESULT_NAME).Value := outStream;
  outStream.Free;
end;

function TDNSEABridgeVCLDataModule.GetS4Results(const aInput: TExternalEvaluatorinput;  aResultData: TExternalEvaluatorResult): IS4Object;
var
  fileName : String;
  fn1: IRFunction;
  expr: ISymbolicExpression;
  newMat: INumericMatrix;
  paramCntr: integer;
  setParamNamesDef: TStringBuilder;
  ParamNameOrientation: String;
  matSymbolName: String;
begin
  if Assigned(FEngine) then
  begin
  // catch any exceptions coming from R
  try
    newMat := FEngine.CreateNumericMatrix(aInput.InputMatrix);

    // Choose to set the col/row names based on the matrix orientation.
    if (aInput.EventsAsRows) then
      ParamNameOrientation := 'colnames'
    else
      ParamNameOrientation := 'rownames';

    // Set the variable inside R so we can run a script on it.
    matSymbolName := 'mat';
    FEngine.SetSymbol(matSymbolName, newMat as ISymbolicExpression);

    // Make R script to add row or column names;
    setParamNamesDef := TStringBuilder.Create;
    setParamNamesDef.Append(ParamNameOrientation);
    setParamNamesDef.Append('(');
    setParamNamesDef.Append(matSymbolname);
    setParamNamesDef.Append(') <- c(');

    // Add in names with quoted text.
    for paramCntr := 0 to aInput.ParameterNames.Count - 1 do
    begin
      setParamNamesDef.Append(QuotedStr(aInput.ParameterNames[paramCntr]));
      setParamNamesDef.Append(', ')
    end;

    // Remove the trailing comma.
    setParamNamesDef.Length := setParamNamesDef.Length - 2;

    // Finish up the text
    setParamNamesDef.Append(')');

    // Set the row/column names
    FEngine.Evaluate(setParamNamesDef.ToString);

    setParamNamesDef.Free;

    // Get the matrix from R since we are going to pass it into our execute function.
    newMat := FEngine.GetSymbol(matSymbolName).AsNumericMatrix;

    // R expects forward slashes not back slashes.
    fileName := StringReplace(aInput.ScriptFileName, '\', '/', [rfReplaceAll]);
    FEngine.Evaluate('source(' + QuotedStr(fileName) + ')');
    fn1 := FEngine.Evaluate('Execute').AsFunction;
    expr := fn1.Invoke(newMat as ISymbolicExpression);
    result := expr.AsS4;

  except
    on e: Exception do
    begin
      SetErrorMessage ('Error running R Script: ', aResultData);
      result := nil;
    end;
    end;
  end
  else
  begin
    SetErrorMessage ('R is not Installed: ', aResultData);
    result := nil;
  end;
end;

procedure TDNSEABridgeVCLDataModule.ShowAboutForm(Sender: TObject);
begin
  // Create the form if it has not been created yet
  if not Assigned(AboutDNSEABridgeForm) then
    AboutDNSEABridgeForm := TAboutDNSEABridgeForm.Create(Application);

  // Show the about form.
  //  If the form was closed, this will reopen the form.
  //  If the form is already showing, this will bring the form to the front
  AboutDNSEABridgeForm.Show;
end;

procedure TDNSEABridgeVCLDataModule.ExitDNSEABridgeWithWarning(Sender: TObject);
begin
  if MessageDlg(WARNING_CLOSE_SERVER, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    CloseDNSEABridge;
end;

function TDNSEABridgeVCLDataModule.ValidateS4Results(const aNumNewParams: integer; aNames:
  TArray<string>; const aNumElements: integer): boolean;
begin
  result := (aNumNewParams = length(aNames));
end;

procedure TDNSEABridgeVCLDataModule.SetErrorMessage(const aErrorMessage: string; aResult:
  TExternalEvaluatorResult);
begin
  aResult.Status := EV_STATUS_ERROR;
  aResult.Errormessage := aErrorMessage;
end;


initialization

  DNSEABridgeMutexHandle := CreateMutex(nil, false, 'DNSEABridgeMutex');


finalization

  CloseHandle(DNSEABridgeMutexHandle);


end.
