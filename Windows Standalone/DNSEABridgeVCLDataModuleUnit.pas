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
  System.Actions,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Tether.Manager,
  System.Tether.AppProfile,
  System.Tether.NetworkAdapter,
  Vcl.ActnList,
  {Indy Sockets}
  IPPeerClient,
  IPPeerServer,

  {De Novo Software External Application Bridge}

  {$IFDEF WINE}
  DNSEABridgeWineSpecificActivationUnit,
  {$ELSE}
  DNSEABridgeWindowsTrayIconUnit,
  {$ENDIF}

  ExternalEvaluatorClassesUnit,
  DNSEABridgeRScriptRunnerUnit
  ;

type
  TDNSEABridgeVCLDataModule = class(TDataModule)
    DNSEABridgeActionList: TActionList;
    acRunCluster: TAction;
    acNewParam: TAction;
    acCloseDNSEABridge: TAction;
    acGetExceptionsText: TAction;
    acAutoGating: TAction;
    procedure acAutoGatingExecute(Sender: TObject);
    procedure acCloseDNSEABridgeExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure InputDataResourceReceived(const Sender: TObject; const AResource:
        TRemoteResource);
    procedure acRunClusterExecute(Sender: TObject);
    procedure acNewParamExecute(Sender: TObject);
    procedure acGetExceptionsTextExecute(Sender: TObject);
    procedure DoOnException(Sender: TObject; aException: Exception);
  strict private
    FRScriptRunner: TDNSEABridgeRScriptRunner;
    FInputData: TExternalEvaluatorInput;
    FDenovoRemoteOpaRTetheringManager: TTetheringManager;
    FErrorStringList: TStringList;
    FTetheringAppProfile: TTetheringAppProfile;
    FShowUI: boolean;
    FREngineHasBeenSetup: boolean;
{$IFDEF WINE}
    FWineSpecificAction: TDNSEABridgeWineSpecificActivation;
{$ELSE}
    FDNSEABridgeTrayIconForm: TDNSEABridgeWindowsTrayIconForm;
{$ENDIF}
    procedure PrepareToCalculateInputUsingR;
    procedure CloseDNSEABridge(Sender: TObject);
    procedure HandleBeforeClosingBridge;
    procedure SetupApplicationEvents;
    procedure SetupTetheringResourcesAndActions;
{$IFNDEF WINE}
    procedure GetVersionOfRInstalled(out aRVersionString: string);
{$ENDIF}
    procedure TetheringManagerErrorHandler(const Sender, Data: TObject; AError:
        TTetheringError);
    procedure SetupREngineIfNeeded;
  strict protected
    procedure AppendErrorMessage(const aErrorMessage: string; aResult:
        TExternalEvaluatorResult; const aException: Exception = nil);
    procedure SetResultResource(aResult: TExternalEvaluatorResult);
    property DenovoRemoteOpaRTetheringManager: TTetheringManager read
      FDenovoRemoteOpaRTetheringManager;
    property TetheringAppProfile: TTetheringAppProfile read
      FTetheringAppProfile;
  public
    property ErrorStringList: TStringList read FErrorStringList;
  end;

var
  DNSEABridgeVCLDataModule: TDNSEABridgeVCLDataModule;

procedure LogExceptionInFile(aException: Exception);

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  System.Math,
  WinApi.Windows,
  WinApi.ShlObj,
  WinApi.KnownFolders,
  Winapi.ActiveX,
  Vcl.Forms
  ;

procedure LogExceptionInFile(aException: Exception);

  procedure ShowErrorMsg(const aErrorMsg: string);
  var
    flags: Integer;
    errMsg: string;
  begin
    flags := MB_OK or MB_ICONERROR;

    if (aErrorMsg <> '') and (aErrorMsg[Length(aErrorMsg)] > '.') then
      errMsg := aErrorMsg + '.'
    else
      errMsg := aErrorMsg;

    MessageBox(0, PChar(errMsg), 'De Novo Software External Application Bridge', flags);
  end;

  procedure WriteErrorToFile(bException: Exception; const bFileName: string);
  var
    fileStrings: TStringList;
  begin
    if bFileName = '' then
    begin
      ShowErrorMsg(bException.ClassName + ': ' + bException.Message);
      exit;
    end;

    fileStrings := TStringList.Create;
    if FileExists(bFileName) then
      fileStrings.LoadFromFile(bFileName);

    fileStrings.Add(Format('%s - %s: %s',[
              FormatDateTime('mm/dd/yyyy hh:nn:ss AM/PM', Now),
              bException.ClassName,
              bException.Message]));

    // Try to write the file to the log file. If there is a problem with this
    // write step, show the error as a popup
    try
      fileStrings.SaveToFile(bFileName);
    except
      on Exception do
      begin
        ShowErrorMsg(bException.ClassName + ': ' + bException.Message);
      end;
    end;
    FreeAndNil(fileStrings);
  end;

  function DefaultFileName: string;
  var
    folder: string;
    PAppDataFolder: PChar;
  begin
    PAppDataFolder := nil;
    result := '';

    if SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DEFAULT, THandle(0),
                                    PAppDataFolder) = S_OK then
    begin
      folder := IncludeTrailingPathDelimiter(string(PAppDataFolder));
      folder := folder + DNSEABRIDGE_ERROR_LOG_APPDATA_FOLDER;

      if not DirectoryExists(folder) then
        ForceDirectories(folder);

      result := folder + DNSEABRIDGE_ERROR_LOG_FILE_NAME;
    end;

    if PAppDataFolder <> nil then
      CoTaskMemFree(PAppDataFolder);
  end;

begin
  WriteErrorToFile(aException, DefaultFileName);
end;

var
  DNSEABridgeMutexHandle: THandle;


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
  TBeforeShutdownEvent = procedure of object;

  TCloseThread = class(TThread)
  strict private
    FOnBeforeShutdown: TBeforeShutdownEvent;
    procedure DoBeforeShutdown;
    procedure ExecuteInternal;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property OnBeforeShutdown: TBeforeShutdownEvent read FOnBeforeShutdown write
        FOnBeforeShutdown;
  end;

constructor TCloseThread.Create;
begin
  inherited Create(True);
end;

procedure TCloseThread.DoBeforeShutdown;
begin
  if Assigned(FOnBeforeShutdown) then
    FOnBeforeShutdown;
end;

procedure TCloseThread.Execute;
begin
  Synchronize(ExecuteInternal);
end;

procedure TCloseThread.ExecuteInternal;
begin
  DoBeforeShutdown;
  Application.MainForm.Close;
end;

procedure TDNSEABridgeVCLDataModule.acAutoGatingExecute(Sender: TObject);
var
  autogatingResult: TExternalEvaluatorResult;
begin
  autogatingResult := TExternalEvaluatorResult.Create(nil);
  try
    PrepareToCalculateInputUsingR;

    FRScriptRunner.PerformAutogating(FInputData, autogatingResult);
  except
    on e: Exception do
      AppendErrorMessage('An exception occurred while processing autogating: ' + e.Message,
            autogatingResult, e);
  end;

  SetResultResource(autogatingResult);
  FreeAndNil(autogatingResult);
end;

procedure TDNSEABridgeVCLDataModule.acCloseDNSEABridgeExecute(Sender: TObject);
begin
  CloseDNSEABridge(Sender);
end;

procedure TDNSEABridgeVCLDataModule.DataModuleDestroy(Sender: TObject);
begin
  // Just in-case we get here without properly closing the tray icon,
  // we want to try again to hide the icon
  HandleBeforeClosingBridge;
  // Actions that are added to the tethering profile will notify the profile when
  // they are freed. This will cause an access violation if we do not remove this
  // notification before freeing the tethering profile
  acCloseDNSEABridge.RemoveFreeNotification(FTetheringAppProfile);
  acRunCluster.RemoveFreeNotification(FTetheringAppProfile);
  acNewParam.RemoveFreeNotification(FTetheringAppProfile);
  acAutoGating.RemoveFreeNotification(FTetheringAppProfile);
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
  FErrorStringList.Free;
end;

procedure TDNSEABridgeVCLDataModule.DataModuleCreate(Sender: TObject);
var
  paramCntr: Integer;
begin
  FREngineHasBeenSetup := False;
  FErrorStringList := TStringList.Create;
  try
    // Check if the calling process wants to enablee the UI.
    FShowUI := True;
    for paramCntr := 1 to ParamCount do
      if SameText(Trim(ParamStr(paramCntr)), DNSEABRIDGE_NO_UI_CMD_PARAM) then
        FShowUI := False;

  {$IFDEF WINE}
    FWineSpecificAction := TDNSEABridgeWineSpecificActivation.Create;
    if FShowUI then
      FWineSpecificAction.MinimizeDNSEABridgeOnStartup;
  {$ELSE}
    FDNSEABridgeTrayIconForm := TDNSEABridgeWindowsTrayIconForm.Create(nil);
    FDNSEABridgeTrayIconForm.OnNeedsToQuitApplication := CloseDNSEABridge;
    FDNSEABridgeTrayIconForm.OnNeedsVersionOfR := GetVersionOfRInstalled;
  {$ENDIF}

    FInputData := TExternalEvaluatorInput.Create(nil);

    FDenovoRemoteOpaRTetheringManager:= TTetheringManager.Create(nil);
    FDenovoRemoteOpaRTetheringManager.Password := TETHERING_MANAGER_PASSWORD;
    FDenovoRemoteOpaRTetheringManager.Text := TETHERING_REMOTE_MANAGER_NAME;
    FDenovoRemoteOpaRTetheringManager.Name := TETHERING_REMOTE_MANAGER_NAME;
    FDenovoRemoteOpaRTetheringManager.AllowedAdapters := 'Network';
    FDenovoRemoteOpaRTetheringManager.Enabled := True;
    FDenovoRemoteOpaRTetheringManager.OnError := TetheringManagerErrorHandler;

    FTetheringAppProfile:= TTetheringAppProfile.Create(nil);
    FTetheringAppProfile.Manager := FDenovoRemoteOpaRTetheringManager;
    FTetheringAppProfile.Group := DNSEABRIDGE_OPA_R_GROUP_NAME;
    FTetheringAppProfile.Name := TETHERING_PROFILE_NAME;
    FTetheringAppProfile.Text := TETHERING_PROFILE_NAME;
    FTetheringAppProfile.Visible := True;
    FTetheringAppProfile.Enabled := True;

    // Now that we have created the tethering objects, we should now create
    // the remote events on the tethering profile. This way, if something goes
    // wrong while setting up script runner, the remote action to return the errpr
    // messages and to close the DNSEABridge will be hooked up.
    SetupTetheringResourcesAndActions;

    // Finally, create the script runner.
    FRScriptRunner := TDNSEABridgeRScriptRunner.Create(self);
    FRScriptRunner.OnNeedsToAppendErrorMessage := AppendErrorMessage;

  except
    // If we have any exceptions when setting up the EA bridge, then pass the
    // exception to the default exception handler. This will populate the default
    // error log and set the server version to zero
    on E: Exception do
      DoOnException(self, E);
  end;

  // After creating the tethering objects, setup the events of the TApplication form
  // For windows, this will setup the application with the DoOnException event.
  // For WINE, this will also setup OnRestore and OnActivate events
  SetupApplicationEvents;
end;

procedure TDNSEABridgeVCLDataModule.InputDataResourceReceived(const Sender:
    TObject; const AResource: TRemoteResource);
begin
  // Check for input data
  if AResource.Name = DNSEABRIDGE_INPUT_NAME then
  begin
    AResource.Value.AsStream.Position := 0;
    FInputData.LoadFromStream(AResource.Value.AsStream);
  end;
end;

procedure TDNSEABridgeVCLDataModule.acRunClusterExecute(Sender: TObject);
var
  clusteringResult: TExternalEvaluatorResult;
begin
  clusteringResult := TExternalEvaluatorResult.Create(nil);

  try
    PrepareToCalculateInputUsingR;

    FRScriptRunner.PerformClustering(FInputData, clusteringResult);
  except
    on e: Exception do
      AppendErrorMessage('An exception occurred while processing cluster asignment: '
        + e.Message, clusteringResult, e);
  end;

  SetResultResource(clusteringResult);
  FreeAndNil(clusteringResult);
end;

procedure TDNSEABridgeVCLDataModule.acNewParamExecute(Sender: TObject);
var
  newParamResult: TExternalEvaluatorResult;
begin
  newParamResult := TExternalEvaluatorResult.Create(nil);
  try
    PrepareToCalculateInputUsingR;

    FRScriptRunner.PerformNewParam(FInputData, newParamResult);
  except
    on e: Exception do
      AppendErrorMessage('An exception occurred while processing new parameter ' +
                        'transformation: '+ e.Message, newParamResult, e);
  end;

  SetResultResource(newParamResult);
  FreeAndNil(newParamResult);
end;

procedure TDNSEABridgeVCLDataModule.acGetExceptionsTextExecute(Sender: TObject);
var
  exceptionsResult: TExternalEvaluatorResult;
  errorCntr: integer;
begin
  exceptionsResult := TExternalEvaluatorResult.Create(nil);
  for errorCntr := 0 to ErrorStringList.Count - 1 do
    AppendErrorMessage(ErrorStringList[errorCntr], exceptionsResult);

  SetResultResource(exceptionsResult);
  exceptionsResult.Free;
end;

procedure TDNSEABridgeVCLDataModule.CloseDNSEABridge(Sender: TObject);
var
  closeThread: TCloseThread;
begin
  closeThread := TCloseThread.Create;
  closeThread.OnBeforeShutdown := HandleBeforeClosingBridge;
  closeThread.FreeOnTerminate := True;
  closeThread.Start;
end;

procedure TDNSEABridgeVCLDataModule.SetResultResource(aResult: TExternalEvaluatorResult);
var
  outStream: TMemoryStream;
begin
  outStream:= TMemoryStream.Create;
  outStream.Position := 0;
  aResult.SaveToStream(outStream);
  outStream.Position := 0;
  TetheringAppProfile.Resources.FindByName(DNSEABRIDGE_REMOTE_RESULT_NAME).Value := outStream;
  outStream.Free;
end;

procedure TDNSEABridgeVCLDataModule.AppendErrorMessage(const aErrorMessage:
    string; aResult: TExternalEvaluatorResult; const aException: Exception =
    nil);
begin
  aResult.Status := EV_STATUS_ERROR;
  if aResult.ErrorMessage = '' then
    aResult.ErrorMessage := aErrorMessage
  else
    aResult.ErrorMessage := aResult.ErrorMessage + CRLF + aErrorMessage;
end;

procedure TDNSEABridgeVCLDataModule.DoOnException(Sender: TObject; aException:
    Exception);
var
  versionResource: TLocalResource;
begin
  ErrorStringList.Add(Format('%s: %s',[aException.ClassName, aException.Message]));

  versionResource := nil;
  if Assigned(FTetheringAppProfile) then
  begin
      versionResource := FTetheringAppProfile.Resources.FindByName(
                          DNS_EA_BRIDGE_VERSION_VERSION_NAME);
  end;

  if Assigned(versionResource) then
    versionResource.Value := 0
  else
    LogExceptionInFile(aException);
end;

{$IFNDEF WINE}
procedure TDNSEABridgeVCLDataModule.GetVersionOfRInstalled(out aRVersionString:
    string);
begin
  SetupREngineIfNeeded;
  aRVersionString := FRScriptRunner.CurrentVersionOfRdll;
end;
{$ENDIF}

procedure TDNSEABridgeVCLDataModule.HandleBeforeClosingBridge;
begin
  Application.OnRestore := nil;
  Application.OnActivate := nil;
{$IFNDEF WINE}
  FDNSEABridgeTrayIconForm.PrepareToShutdownApplication;
{$ENDIF}
end;

procedure TDNSEABridgeVCLDataModule.PrepareToCalculateInputUsingR;
var
  profileInfo: TTetheringProfileInfo;
  inputStream: TStream;
begin
  SetupREngineIfNeeded;

  profileInfo := DenovoRemoteOpaRTetheringManager.RemoteProfiles.Items[0];
  inputStream := TetheringAppProfile.GetRemoteResourceValue(
                    profileInfo, DNSEABRIDGE_INPUT_NAME).Value.AsStream;

  inputStream.Position := 0;
  FInputData.LoadFromStream(inputStream);
end;

procedure TDNSEABridgeVCLDataModule.SetupApplicationEvents;
begin
  Application.OnException := DoOnException;
  {$IFDEF WINE}
  Application.OnRestore := FWineSpecificAction.HandleApplicationRestore;
  Application.OnActivate := FWineSpecificAction.HandleApplicationActivate;
  {$ENDIF}
end;

procedure TDNSEABridgeVCLDataModule.SetupREngineIfNeeded;
begin
  TThread.Synchronize(
    TThread.CurrentThread,
    procedure
    begin
      if not FREngineHasBeenSetup then
      begin
        FREngineHasBeenSetup := True;
        try
          FRScriptRunner.SetupREngine;
        except
          on E: Exception do
            DoOnException(self, E);
        end;
      end;
    end);
end;

procedure TDNSEABridgeVCLDataModule.SetupTetheringResourcesAndActions;
var
  tetheringResource: TLocalResource;
  tetheringAction: TLocalAction;
begin
  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := DNSEABRIDGE_INPUT_NAME;
  tetheringResource.ResType := TRemoteResourceType.Stream;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;
  tetheringResource.OnResourceReceived := InputDataResourceReceived;

  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := DNSEABRIDGE_REMOTE_RESULT_NAME;
  tetheringResource.IsPublic := True;
  tetheringResource.ResType := TRemoteResourceType.Stream;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;

  tetheringResource:= FTetheringAppProfile.Resources.Add;
  tetheringResource.Name := DNS_EA_BRIDGE_VERSION_VERSION_NAME;
  tetheringResource.ResType := TRemoteResourceType.Data;
  tetheringResource.Kind := TTetheringRemoteKind.Shared;
  tetheringResource.OnResourceReceived := InputDataResourceReceived;
  tetheringResource.Value :=
                        ExternalEvaluatorClassesUnit.CURRENT_DNS_EA_BRIDGE_VERSION;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := DNSEABRIDGE_CLUSTER_ACTION_NAME;
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acRunCluster;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := DNSEABRIDGE_NEW_PARAM_ACTION_NAME;
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acNewParam;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := DNSEABRIDGE_CLOSE_ACTION_NAME;
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acCloseDNSEABridge;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := DNSEABRIDGE_GET_EXCEPTION_ACTION_NAME;
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acGetExceptionsText;
  tetheringAction.NotifyUpdates := False;

  tetheringAction:= FTetheringAppProfile.Actions.Add;
  tetheringAction.Name := DNSEABRIDGE_AUTOGATING_ACTION_NAME;
  tetheringAction.Kind := TTetheringRemoteKind.Shared;
  tetheringAction.Action := acAutoGating;
  tetheringAction.NotifyUpdates := False;
end;

procedure TDNSEABridgeVCLDataModule.TetheringManagerErrorHandler(const Sender,
    Data: TObject; AError: TTetheringError);
var
  dataAsAdapter: TTetheringAdapter;
  errorMsg: string;
begin
  if (aError = TTetheringError.InitAdapterError) and
      (Data is TTetheringAdapter) then
  begin
    dataAsAdapter := Data as TTetheringAdapter;

    errorMsg := Format(
        'Unable to StartListening on adapter: %s', [dataAsAdapter.AdapterType]);
    ErrorStringList.Add(errorMsg);
  end;
end;


initialization
  DNSEABridgeMutexHandle := CreateMutex(nil, false, 'DNSEABridgeMutex');

finalization
  CloseHandle(DNSEABridgeMutexHandle);


end.
