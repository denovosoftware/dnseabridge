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
  Vcl.ActnList,

  {De Novo Software External Application Bridge}

  {$IFDEF WINE}
  DNSEABridgeWineSpecificActivationUnit,
  {$ELSE}
  DNSEABridgeWindowsTrayIconUnit,
  {$ENDIF}

  ExternalEvaluatorClassesUnit,
  DNSEABridgeEvaluatorThreads,
  DNSEABridgeRScriptRunnerUnit
  ;

type
  TDNSEABridgeVCLDataModule = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DoOnException(Sender: TObject; aException: Exception);
  strict private
    FRScriptRunner: TDNSEABridgeRScriptRunner;
    FEvaluatorReceiveThread: TEvaluatorReceiveThread;
    FShowUI: boolean;
    FREngineHasBeenSetup: boolean;
{$IFDEF WINE}
    FWineSpecificAction: TDNSEABridgeWineSpecificActivation;
{$ELSE}
    FDNSEABridgeTrayIconForm: TDNSEABridgeWindowsTrayIconForm;
{$ENDIF}

    procedure PerformAutoGatingHandler(const aEvaluatorInput:
        TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
    procedure PerformRunClusterHandler(const aEvaluatorInput:
        TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
    procedure PerformNewParamHandler(const aEvaluatorInput:
        TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
    procedure CloseDNSEABridgeHandler(Sender: TObject);

    procedure CloseDNSEABridge(Sender: TObject);
    procedure HandleBeforeClosingBridge;
    procedure SetupApplicationEvents;
{$IFNDEF WINE}
    procedure GetVersionOfRInstalled(out aRVersionString: string);
{$ENDIF}
    procedure SetupREngineIfNeeded;
  strict protected
    procedure AppendErrorMessage(const aErrorMessage: string; aResult:
        TExternalEvaluatorResult; const aException: Exception = nil);
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

procedure TDNSEABridgeVCLDataModule.PerformAutoGatingHandler(const
    aEvaluatorInput: TExternalEvaluatorInput; aResultData:
    TExternalEvaluatorResult);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      try
        SetupREngineIfNeeded;

        FRScriptRunner.PerformAutogating(aEvaluatorInput, aResultData);
      except
        on e: Exception do
          AppendErrorMessage('An exception occurred while processing autogating: ' + e.Message,
                aResultData, e);
      end;
    end);
end;

procedure TDNSEABridgeVCLDataModule.CloseDNSEABridgeHandler(Sender: TObject);
begin
  CloseDNSEABridge(Sender);
end;

procedure TDNSEABridgeVCLDataModule.DataModuleDestroy(Sender: TObject);
begin
  // Just in-case we get here without properly closing the tray icon,
  // we want to try again to hide the icon
  HandleBeforeClosingBridge;
  FEvaluatorReceiveThread.KillThread;
  FEvaluatorReceiveThread.WaitFor;
  FEvaluatorReceiveThread.Free;
end;

procedure TDNSEABridgeVCLDataModule.DataModuleCreate(Sender: TObject);
var
  callingCopyDataHandle: Int64;
  paramCntr: Integer;
begin
  FREngineHasBeenSetup := False;

  // Check if the calling process wants to enablee the UI.
  FShowUI := True;
  callingCopyDataHandle := -1;
  for paramCntr := 1 to ParamCount do
  begin
    if SameText(Trim(ParamStr(paramCntr)), DNSEABRIDGE_NO_UI_CMD_PARAM) then
      FShowUI := False
    else if callingCopyDataHandle = -1 then
      TryStrToInt64(Trim(ParamStr(paramCntr)), callingCopyDataHandle)
  end;

  FEvaluatorReceiveThread := TEvaluatorReceiveThread.Create;
  FEvaluatorReceiveThread.OnPerformAutoGating := PerformAutoGatingHandler;
  FEvaluatorReceiveThread.OnPerformCluster := PerformRunClusterHandler;
  FEvaluatorReceiveThread.OnPerformNewParam := PerformNewParamHandler;
  FEvaluatorReceiveThread.OnShutdown := CloseDNSEABridgeHandler;
  FEvaluatorReceiveThread.Initialize(callingCopyDataHandle);

  try

  {$IFDEF WINE}
    FWineSpecificAction := TDNSEABridgeWineSpecificActivation.Create;
    if FShowUI then
      FWineSpecificAction.MinimizeDNSEABridgeOnStartup;
  {$ELSE}
    FDNSEABridgeTrayIconForm := TDNSEABridgeWindowsTrayIconForm.Create(nil);
    FDNSEABridgeTrayIconForm.OnNeedsToQuitApplication := CloseDNSEABridge;
    FDNSEABridgeTrayIconForm.OnNeedsVersionOfR := GetVersionOfRInstalled;
  {$ENDIF}

    // Create the script runner.
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

procedure TDNSEABridgeVCLDataModule.PerformRunClusterHandler(const
    aEvaluatorInput: TExternalEvaluatorInput; aResultData:
    TExternalEvaluatorResult);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      try
        SetupREngineIfNeeded;

        FRScriptRunner.PerformClustering(aEvaluatorInput, aResultData);
      except
        on e: Exception do
          AppendErrorMessage('An exception occurred while processing cluster asignment: ' + e.Message,
                aResultData, e);
      end;
    end);
end;

procedure TDNSEABridgeVCLDataModule.PerformNewParamHandler(const
    aEvaluatorInput: TExternalEvaluatorInput; aResultData:
    TExternalEvaluatorResult);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      try
        SetupREngineIfNeeded;

        FRScriptRunner.PerformNewParam(aEvaluatorInput, aResultData);
      except
        on e: Exception do
          AppendErrorMessage('An exception occurred while processing new parameter ' +
                        'transformation: '+ e.Message, aResultData, e);
      end;
    end);
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
begin
  FEvaluatorReceiveThread.AddError(Format('%s: %s',[aException.ClassName, aException.Message]), False);
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

end.
