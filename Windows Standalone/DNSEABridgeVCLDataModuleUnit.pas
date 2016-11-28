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
    FDenovoRemoteOpaRTetheringManager: TTetheringManager;
    FTetheringAppProfile: TTetheringAppProfile;
    procedure CloseDNSEABridge;
    procedure SetupREngine;
    procedure SetupTetheringResourcesAndActions;
  strict protected
    function GetS4Results(const aInput: TExternalEvaluatorInput; aResultData:
        TExternalEvaluatorResult): IS4Object;
    function ValidateS4Results(const aNumNewParams: integer; aNames:
        TArray<string>; const aNumElements: integer): boolean;
    procedure AppendErrorMessage(const aErrorMessage: string; aResult:
        TExternalEvaluatorResult);
    procedure SetResultResource(aResult: TExternalEvaluatorResult);
    property DenovoRemoteOpaRTetheringManager: TTetheringManager read
      FDenovoRemoteOpaRTetheringManager;
    property TetheringAppProfile: TTetheringAppProfile read
      FTetheringAppProfile;
  end;

var
  DNSEABridgeVCLDataModule: TDNSEABridgeVCLDataModule;

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  Windows, opaR.EngineExtension, System.Math, opaR.NativeUtility;

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


///
///  TCloseThread gives us access to the main thread.
///  Application.MainForm.Close will only work from the main thread
///
///  If the DNS EA Bridge is closed with the action acCloseDNSEABridge, it must
///  be called asynchronously. If the remote tether does not call this action
///  asynch, then the remote action call will cause a crash as the bridge may
///  not properly signal the end of the action.
///
type
  TCloseThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TCloseThread.Execute;
begin
  Synchronize(Application.MainForm.Close);
end;

procedure TDNSEABridgeVCLDataModule.acCloseDNSEABridgeExecute(Sender: TObject);
begin
  CloseDNSEABridge;
end;

procedure TDNSEABridgeVCLDataModule.DataModuleDestroy(Sender: TObject);
begin
  // Actions that are added to the tethering profile will notify the profile when
  // they are freed. This will cause an access violation if we do not remove this
  // notification before freeing the tethering profile
  acCloseDNSEABridge.RemoveFreeNotification(FTetheringAppProfile);
  acRunCluster.RemoveFreeNotification(FTetheringAppProfile);
  acNewParam.RemoveFreeNotification(FTetheringAppProfile);

  FreeAndNil(FInputData);
  try
    FreeAndNil(FDenovoRemoteOpaRTetheringManager);
  except
    on Exception do
    // Ignore any and all errors
    ;
  end;
  try
    FreeAndNil(FTetheringAppProfile);
  except
    on Exception do
    // Ignore any and all errors
    ;
  end;
end;

procedure TDNSEABridgeVCLDataModule.DataModuleCreate(Sender: TObject);
begin
  FDenovoRemoteOpaRTetheringManager:= TTetheringManager.Create(nil);
  FDenovoRemoteOpaRTetheringManager.Password := '2667F497E81344268D5BCBF062A6E3C0';
  FDenovoRemoteOpaRTetheringManager.Text := 'DenovoRemoteOpaRTetheringManager';
  FDenovoRemoteOpaRTetheringManager.Name := 'DenovoRemoteOpaRTetheringManager';
  FDenovoRemoteOpaRTetheringManager.AllowedAdapters := 'Network';
  FDenovoRemoteOpaRTetheringManager.Enabled := True;

  FTetheringAppProfile:= TTetheringAppProfile.Create(nil);
  FTetheringAppProfile.Manager := FDenovoRemoteOpaRTetheringManager;
  FTetheringAppProfile.Group := 'DenovoOpaREvaluator';
  FTetheringAppProfile.Name := 'TetheringAppProfile';
  FTetheringAppProfile.Text := 'TetheringAppProfile';
  FTetheringAppProfile.Visible := True;
  FTetheringAppProfile.Enabled := True;

  SetupTetheringResourcesAndActions;

  FInputData := TExternalEvaluatorInput.Create(nil);

  SetupREngine;
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
        AppendErrorMessage(Format('Number of clusters did not match results: '+
          #13#10'Number of Clusters: %d, Number of Names: %d', [numberOfClusters,
          length(clusterNames)]), clusteringResult);
    end
    else
      AppendErrorMessage('Cluster result data is not valid.', clusteringResult);

  except
    on e: Exception do
      AppendErrorMessage('An exception occurred while processing cluster asignment: '
        + e.Message, clusteringResult);
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
        AppendErrorMessage(Format('Number of new parameters did not match results: '+
          #13#10'Number of New Parameters: %d, Number of Names: %d', [numberOfNewParams,
          length(newParamNames)]), newParamResult);
    end
    else
      AppendErrorMessage('Parameter result data invalid.', newParamResult);

  except
    on e: Exception do
      AppendErrorMessage('An exception occurred while processing new parameter ' +
                        'transformation: '+ e.Message, newParamResult);
  end;

  SetResultResource(newParamResult);
  FreeAndNil(newParamResult);
end;

procedure TDNSEABridgeVCLDataModule.CloseDNSEABridge;
var
  closeThread: TCloseThread;
begin
  TrayIconDNSEABridge.Visible := False;
  closeThread := TCloseThread.Create;
  closeThread.FreeOnTerminate := True;
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

function TDNSEABridgeVCLDataModule.GetS4Results(const aInput:
    TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult): IS4Object;
var
  fileName : String;
  fn1: IRFunction;
  expr: ISymbolicExpression;
  newMat: INumericMatrix;
  paramCntr: integer;
  setParamNamesDef: TStringBuilder;
  ParamNameOrientation: String;
  matSymbolName: String;
  savedExceptionMask: TArithmeticExceptionMask;
begin
  if not Assigned(FEngine) then
  begin
    AppendErrorMessage ('R is not Installed.', aResultData);
    result := nil;
    exit;
  end;

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

    try
      // Save the current exception mask
      // Add exceptions to the mask for floating point operations. The R.dll does
      // not handle these exceptions gracefully, and we want the behavior in the
      // script to replicate the behavior seen when running R from the R GUI
      savedExceptionMask := System.Math.GetExceptionMask;
      System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                                      exOverflow, exUnderflow, exPrecision]);

      // Execute the R script file with the data supplied from the
      // TExternalEvaluatorInput
      expr := fn1.Invoke(newMat as ISymbolicExpression);
      result := expr.AsS4;
    finally
      // We now want to reset the exception mask to the state it was before we
      // ran the R script. Also, clear and raise any exceptions that may have
      // been thrown during the execution of the R script.
      System.Math.SetExceptionMask(savedExceptionMask);
      ClearExceptions(True, [exInvalidOp, exDenormalized, exZeroDivide,
                                      exOverflow, exUnderflow, exPrecision]);
    end;

  except
    on e: Exception do
    begin
      AppendErrorMessage ('Error running R Script:'#13#10 + e.Message, aResultData);
      result := nil;
    end;
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

procedure TDNSEABridgeVCLDataModule.AppendErrorMessage(const aErrorMessage:
    string; aResult: TExternalEvaluatorResult);
begin
  aResult.Status := EV_STATUS_ERROR;
  if aResult.ErrorMessage = '' then
    aResult.ErrorMessage := aErrorMessage
  else
    aResult.ErrorMessage := aResult.ErrorMessage + #13#10 + aErrorMessage;
end;

procedure TDNSEABridgeVCLDataModule.SetupREngine;
begin
  try
    TREngine.SetEnvironmentVariables;
    FEngine := TREngine.GetInstance;
    FTetheringAppProfile.Resources.FindByName(SERVER_VERSION_NAME).Value := 1;
  except
    on E: exception do
      // Something went wrong with the service.
      // Make the version invalid so we don't even try to connect to it.
      FTetheringAppProfile.Resources.FindByName(SERVER_VERSION_NAME).Value := 0;
  end;
end;

procedure TDNSEABridgeVCLDataModule.SetupTetheringResourcesAndActions;
var
  tetheringResource: TLocalResource;
  tetheringAction: TLocalAction;
begin
  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := 'RInputData';
  tetheringResource.ResType := TRemoteResourceType.Stream;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;
  tetheringResource.OnResourceReceived := InputDataResourceReceived;

  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := 'RResult';
  tetheringResource.IsPublic := True;
  tetheringResource.ResType := TRemoteResourceType.Stream;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;

  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := 'ServerVersion';
  tetheringResource.ResType := TRemoteResourceType.Data;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;
  tetheringResource.OnResourceReceived := InputDataResourceReceived;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := 'acRunCluster';
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acRunCluster;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := 'acNewParam';
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acNewParam;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := 'acCloseDNSEABridge';
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acCloseDNSEABridge;
  tetheringAction.NotifyUpdates := False;
end;


initialization

  DNSEABridgeMutexHandle := CreateMutex(nil, false, 'DNSEABridgeMutex');


finalization

  CloseHandle(DNSEABridgeMutexHandle);


end.
