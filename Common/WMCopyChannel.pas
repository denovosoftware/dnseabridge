unit WMCopyChannel;

{***************************************************************************************************
This De Novo Software External Application Bridge accepts data via WM_COPY messages and
converts it into a format required for the opaR library.

Copyright (C) 2023-2023 De Novo Software

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.SyncObjs, System.Types, System.DateUtils;

type
  /// <summary>
  ///   The connection status
  /// </summary>
  TCopyChannelConnectionStatus = (
    cccs_Connecting,
    cccs_Connected,
    cccs_ConnectionClosed);

  /// <summary>
  ///   The message type
  /// </summary>
  TCopyChannelMessageType = (emt_SetReturnHandle, emt_GetErrors, emt_Error,
      emt_Exiting, emt_RequestShutdown, emt_CancelRequest, emt_Heartbeat,
      emt_Data);

  /// <summary>
  ///   A wrapper record for our communcations data
  /// </summary>
  TCopyChannelCommuncationsData = record
    MessageType: TCopyChannelMessageType;
    RequestData: TByteDynArray;

    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToStream(const aStream: TStream);
    function CreateMemoryStreamForRequestData: TMemoryStream;
    procedure SetRequestData(const aStream: TMemoryStream);
  end;

  /// <summary>
  ///   The base copy channel thread
  /// </summary>
  TCopyChannelCommunicationThread = class(TThread)
  strict private
    FInternalWindowHandle: HWND;
    FConnectionStatus: TCopyChannelConnectionStatus;
    FOnConnected: TNotifyEvent;
    FTargetWindowHandle: UInt64;

    procedure AllocateHWnd; virtual;
    procedure DeallocateHWnd; virtual;
    function GetInternalWindowHandle: HWND;
  private
    procedure DecodeEvaluatorRequest(var Msg: TWMCopyData);
  strict protected
    procedure CheckTargetWindowHandle;
    procedure ProcessEvaluatorRequest(const aMessageType: TCopyChannelMessageType;
        const aRequestData: TByteDynArray);
    procedure ReceiveRequest(const aCommuncationsData:
        TCopyChannelCommuncationsData); virtual; abstract;
    procedure SetConnection(const aTargetWindowHandle: UInt64; const
        aConnectionStatus: TCopyChannelConnectionStatus);
  protected
    procedure SendBackgroundRequest(const aMessageType: TCopyChannelMessageType;
        const aReturnData: TByteDynArray = nil);
  public
    procedure Execute; override;
    procedure KillThread;

    property ConnectionStatus: TCopyChannelConnectionStatus read
        FConnectionStatus;
    property InternalWindowHandle: HWND read GetInternalWindowHandle;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  end;

  /// <summary>
  ///   The copy channel heartbeat thread keeps the connection alive while
  ///   the request is being processed by the receiver.
  /// </summary>
  TCopyChannelHeartbeatThread = class(TThread)
  strict private
    FCopyChannelThread: TCopyChannelCommunicationThread;
  public
    procedure Execute; override;

    procedure Initialize(const aCopyChannelThread: TCopyChannelCommunicationThread);
  end;

  TCopyChannelHeartbeatThreadCreateProc = reference to  function: TCopyChannelHeartbeatThread;

  /// <summary>
  ///   The copy channel thread responsible for receiving data
  /// </summary>
  TCopyChannelReceiveThread = class(TCopyChannelCommunicationThread)
  strict private
    FErrorList: TStringList;
    FErrorListLock: TSpinLock;

    procedure SetReturnHandleOnTarget;
  strict protected
    procedure ReceiveRequest(const aCommuncationsData:
        TCopyChannelCommuncationsData); override;

    procedure ReceiveRequestInternal(const aCommuncationsData:
        TCopyChannelCommuncationsData; const aHeartbeatThreadCreate:
        TCopyChannelHeartbeatThreadCreateProc); virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute; override;

    procedure AddError(const aErrorMessage: string; const aNotifyCaller: Boolean);
    procedure Initialize(const aTargetWindowHandle: UInt64);
  end;

  /// <summary>
  ///   Anonymous method to abort the send request
  /// </summary>
  TCopyChannelAbortRequest = reference to function: Boolean;

  /// <summary>
  ///   The copy channel thread responsible for sending data
  /// </summary>
  TCopyChannelSendThread = class(TCopyChannelCommunicationThread)
  strict private
    FAllowIncomingRequests: Boolean;
    FErrorReceived: Boolean;
    FLastHeartbeat: TDateTime;
    FReceivedData: TByteDynArray;
    FReceiveDataLock: TSpinLock;
    FRequestInProgress: Integer;
    FResponseReceived: Boolean;
  strict protected
    function ProcessEvaluatorRequestWithResponse(const aRequestData: TByteDynArray;
        var aResponseData: TByteDynArray; const aNoResponseTimeout,
        aMaximumRequestTime: Int64; const aMessageType: TCopyChannelMessageType =
        emt_Data; const aAbortRequest: TCopyChannelAbortRequest = nil): Boolean;
        overload;
    function ProcessEvaluatorRequestWithResponse(const aRequestData, aResponseData:
        TMemoryStream; const aNoResponseTimeout, aMaximumRequestTime: Int64; const
        aMessageType: TCopyChannelMessageType = emt_Data; const aAbortRequest:
        TCopyChannelAbortRequest = nil): Boolean; overload;

    procedure ReceiveRequest(const aCommuncationsData:
        TCopyChannelCommuncationsData); override;
  public
    procedure AfterConstruction; override;
    function GetErrorList(const aTimeout: Int64): TStringDynArray;
    procedure ShutdownReceiver;

    property LastHeartbeat: TDateTime read FLastHeartbeat;
  end;

implementation

function TCopyChannelCommuncationsData.CreateMemoryStreamForRequestData:
    TMemoryStream;
begin
  if Length(Self.RequestData) > 0 then
  begin
    Result := TMemoryStream.Create;
    Result.Write(Self.RequestData[0], Length(Self.RequestData));
    Result.Position := 0;
  end
  else
    raise Exception.Create('No data contained in the request data.');
end;

procedure TCopyChannelCommuncationsData.LoadFromStream(const aStream: TStream);
const
  IDENTIFIER: ShortString = 'DNSCOPYDATA';
  VERSION: Integer = 1;
var
  dataLength: Integer;
  readIdentifier: ShortString;
  readVersion: Integer;
begin
  SetLength(readIdentifier, Length(IDENTIFIER));
  aStream.Read(readIdentifier[1], Length(IDENTIFIER));
  if readIdentifier <> IDENTIFIER then
    raise Exception.Create('Invalid Message Received.');
  aStream.Read(readVersion, SizeOf(readVersion));
  if readVersion <> VERSION then
    raise Exception.Create('Invalid Message Version Received.');
  aStream.Read(Self.MessageType, SizeOf(Self.MessageType));
  aStream.Read(dataLength, SizeOf(dataLength));
  if dataLength > 0 then
  begin
    SetLength(Self.RequestData, dataLength);
    aStream.Read(Self.RequestData[0], dataLength);
  end
  else
    Self.RequestData := nil;
end;

procedure TCopyChannelCommuncationsData.SaveToStream(const aStream: TStream);
const
  IDENTIFIER: ShortString = 'DNSCOPYDATA';
  VERSION: Integer = 1;
var
  dataLength: Integer;
begin
  aStream.Write(IDENTIFIER[1], Length(IDENTIFIER));
  aStream.Write(VERSION, SizeOf(VERSION));
  aStream.Write(Self.MessageType, SizeOf(Self.MessageType));
  dataLength := Length(Self.RequestData);
  aStream.Write(dataLength, SizeOf(dataLength));
  if dataLength > 0 then
    aStream.Write(Self.RequestData[0], dataLength);
end;

procedure TCopyChannelCommuncationsData.SetRequestData(const aStream: TMemoryStream);
begin
  SetLength(Self.RequestData, aStream.Size);
  Move(aStream.Memory^, Self.RequestData[0], Length(Self.RequestData));
end;

/// <summary>
///   Windows Handler
/// </summary>
function InternalWinProc(aWindowHandle: HWND; aMessage: UINT; aWParam: WPARAM;
    aLParam: LPARAM): LRESULT; stdcall;
var
  processingThread: TObject;
  copyDataMessage: TMessage;
begin
  // Get the pointer associated with the window handle. This will be nil while
  // we are still initializing the thread so we have to cater for that scenario.
{$IFDEF WIN64}
  processingThread := TObject(GetWindowLongPtr(aWindowHandle, 0));
{$ELSE}
  processingThread := TObject(GetWindowLong(aWindowHandle, 0));
{$ENDIF}
  // Are we dealing with a WM_COPYDATA message for our thread?
  if (processingThread is TCopyChannelCommunicationThread) and (aMessage = WM_COPYDATA) then
  begin
    copyDataMessage.Msg := aMessage;
    copyDataMessage.wParam := aWParam;
    copyDataMessage.lParam := aLParam;
    TCopyChannelCommunicationThread(processingThread).DecodeEvaluatorRequest(TWMCopyData(copyDataMessage));
    Result := copyDataMessage.Result;
  end
  else
    Result := DefWindowProc(aWindowHandle, aMessage, aWParam, aLParam)
end;

procedure ThreadSleep(const aDelay: Integer);
var
  handle: THandle;
begin
  handle := CreateSemaphore(nil, 0, 1, '');
  WaitForSingleObject(handle, aDelay);
  CloseHandle(handle);
end;

procedure TCopyChannelCommunicationThread.AllocateHWnd;
const
  WorkerThreadWindowClassName = 'DNSCCopyDataThreadWindowClass';
var
  registerClass: TWndClass;
begin
  // Nothing to do if hidden window is already created
  if FInternalWindowHandle <> 0 then
    Exit;

  // We may allow people to stop and start the bridge on demand so we first
  // check if the window class is registered before registering it again.
  if not Winapi.Windows.GetClassInfo(HInstance, WorkerThreadWindowClassName, registerClass) then
  begin
    registerClass.hInstance := HInstance;
    registerClass.lpszClassName := WorkerThreadWindowClassName;
    registerClass.style := 0;
    registerClass.lpfnWndProc := @InternalWinProc;
    registerClass.cbClsExtra := 0;
    registerClass.cbWndExtra := SizeOf(Pointer);
    registerClass.hIcon := 0;
    registerClass.hCursor := 0;
    registerClass.hbrBackground := 0;
    registerClass.lpszMenuName := nil;
    if Winapi.Windows.RegisterClass(registerClass) = 0 then
      raise Exception.Create('Error registering class for DNSEA Bridge: ' + SysErrorMessage(GetLastError));
  end;

  // Create the window for the messages
  FInternalWindowHandle := CreateWindowEx(WS_EX_TOOLWINDOW,
      registerClass.lpszClassName, '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);

  if FInternalWindowHandle = 0 then
    raise Exception.Create('Error creating hidden window for DNSEA Bridge: ' + SysErrorMessage(GetLastError));

  // Set the pointer associated with this handle to point back to our thread class
{$IFDEF WIN64}
  SetWindowLongPtr(FInternalWindowHandle, 0, INT_PTR(Self));
{$ELSE}
  SetWindowLong(FInternalWindowHandle, 0, Longint(Self));
{$ENDIF}
end;

procedure TCopyChannelCommunicationThread.CheckTargetWindowHandle;
begin
  // Check that the target handle is set
  if FTargetWindowHandle = 0 then
  begin
    if ConnectionStatus = cccs_ConnectionClosed then
      raise Exception.Create('Target Process has been closed.')
    else if ConnectionStatus = cccs_Connecting then
      raise Exception.Create('Connection has not yet been established.')
    else
      raise Exception.Create('Unknown error establishing connection.');
  end;
end;

procedure TCopyChannelCommunicationThread.DeallocateHWnd;
begin
  if FInternalWindowHandle = 0 then
    Exit;
{$IFDEF WIN64}
  SetWindowLongPtr(FInternalWindowHandle, 0, 0);
{$ELSE}
  SetWindowLong(FInternalWindowHandle, 0, 0);
{$ENDIF}
  DestroyWindow(FInternalWindowHandle);
  FInternalWindowHandle := 0;
end;

procedure TCopyChannelCommunicationThread.Execute;
var
  messageToProcess: TMsg;
begin
  AllocateHWnd;
  try
    // Process the messages here.
    while (not Terminated) and GetMessage(messageToProcess, 0, 0, 0) do
    begin
      TranslateMessage(messageToProcess);
      DispatchMessage(messageToProcess)
    end;
  finally
    DeallocateHWnd;
  end;
end;

procedure TCopyChannelCommunicationThread.DecodeEvaluatorRequest(var Msg: TWMCopyData);
var
  memStream: TMemoryStream;
  communcationsData: TCopyChannelCommuncationsData;
begin
  memStream := TMemoryStream.Create;
  memStream.Write(Msg.CopyDataStruct.lpData^, Msg.CopyDataStruct.cbData);
  memStream.Position := 0;
  // Decode the packet
  try
    communcationsData.LoadFromStream(memStream);
    // Launch thread with the data here
    TThread.CreateAnonymousThread(
        procedure
        begin
          ReceiveRequest(communcationsData);
        end).Start;
  except
    on E: Exception do
    begin
      // Save the exception
      Sleep(0);
    end;
  end;
  memStream.Free;
end;

function TCopyChannelCommunicationThread.GetInternalWindowHandle: HWND;
const
  TIMEOUT = 3000; // Wait 3 seconds and then raise an error because we didn't start
var
  startTick: Cardinal;
begin
  if FInternalWindowHandle = 0 then
  begin
    // Wait for the handle to be allocated (happens at start of thread)
    startTick := GetTickCount;
    while (FInternalWindowHandle = 0) and ((GetTickCount - startTick) < TIMEOUT) do
    begin
      ThreadSleep(50);
    end;
    if FInternalWindowHandle = 0 then
      raise Exception.Create('Unable to get internal handle for channel.');
  end;

  Result := FInternalWindowHandle;
end;

procedure TCopyChannelCommunicationThread.KillThread;
begin
  // Tell the thread to stop
  Terminate;
  // Tell the message loop to exit
  PostMessage(InternalWindowHandle, WM_QUIT, 0, 0);
end;

procedure TCopyChannelCommunicationThread.ProcessEvaluatorRequest(const
    aMessageType: TCopyChannelMessageType; const aRequestData: TByteDynArray);
var
  memStream: TMemoryStream;
  request: TCopyChannelCommuncationsData;
  copyDataStruct: TCopyDataStruct;
  truncatedHandle: HWND;
begin
  CheckTargetWindowHandle;
  // Create a stream to send the data
  memStream := TMemoryStream.Create;
  try
    // Save the request
    request.MessageType := aMessageType;
    request.RequestData := aRequestData;
    request.SaveToStream(memStream);
    // Send it
    copyDataStruct.dwData := 0; // see notes further below
    copyDataStruct.cbData := memStream.Size;
    copyDataStruct.lpData := memStream.Memory;
    {$IFDEF WIN64}
    truncatedHandle := FTargetWindowHandle;
    {$ELSE}
    // It's safe to truncate the handle in Win32 according to:
    //   https://msdn.microsoft.com/en-us/library/windows/desktop/aa384203%28v=vs.85%29.aspx
    // because handles only use the lower 32 bits.
    truncatedHandle := FTargetWindowHandle;
    {$ENDIF}
    SendMessage(truncatedHandle, WM_COPYDATA, 0, LPARAM(@copyDataStruct));
  finally
    memStream.Free;
  end;
end;

procedure TCopyChannelCommunicationThread.SendBackgroundRequest(const
    aMessageType: TCopyChannelMessageType; const aReturnData: TByteDynArray =
    nil);
begin
  TThread.CreateAnonymousThread(
      procedure
      begin
        try
          ProcessEvaluatorRequest(aMessageType, aReturnData);
        except
          // Ignore any errors
        end;
      end).Start;
end;

procedure TCopyChannelCommunicationThread.SetConnection(const
    aTargetWindowHandle: UInt64; const aConnectionStatus:
    TCopyChannelConnectionStatus);
begin
  FTargetWindowHandle := aTargetWindowHandle;
  FConnectionStatus := aConnectionStatus;
  if (aConnectionStatus = cccs_Connected) and Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TCopyChannelReceiveThread.AddError(const aErrorMessage: string; const
    aNotifyCaller: Boolean);
begin
  FErrorListLock.Enter;
  try
    FErrorList.Add(aErrorMessage);
  finally
    FErrorListLock.Exit;
  end;
  if aNotifyCaller then
    SendBackgroundRequest(emt_Error, nil);
end;

procedure TCopyChannelReceiveThread.AfterConstruction;
begin
  inherited;
  FErrorList := TStringList.Create;
  FErrorListLock := TSpinLock.Create(True);
end;

procedure TCopyChannelReceiveThread.BeforeDestruction;
begin
  FErrorList.Free;
  inherited;
end;

procedure TCopyChannelReceiveThread.Execute;
begin
  inherited;

  if ConnectionStatus = cccs_Connected then
    SendBackgroundRequest(emt_Exiting);
end;

procedure TCopyChannelReceiveThread.ReceiveRequest(const aCommuncationsData:
    TCopyChannelCommuncationsData);
var
  returnData: TByteDynArray;
begin
  inherited;

  if (aCommuncationsData.MessageType = emt_GetErrors) then
  begin
    FErrorListLock.Enter;
    try
      returnData := TEncoding.UTF8.GetBytes(FErrorList.Text);
      FErrorList.Clear;
    finally
      FErrorListLock.Exit;
    end;
    SendBackgroundRequest(emt_Data, returnData);
  end
  else
  begin
    // We use a heartnbeat thread so the caller knows that we are still
    // working on the request. If we crash or hang, the caller will know about
    // it and can return an error to the user instead of hanging forever. We
    // don't manage the lifecycle of the heartbeat thread here. We leave that
    // for the implementer of ReceiveRequestInternal because that code may
    // want to create the heartbeat thread in a different thread so it has
    // to manage the lifecycle.
    ReceiveRequestInternal(aCommuncationsData,
      function : TCopyChannelHeartbeatThread
      begin
        Result := TCopyChannelHeartbeatThread.Create(True);
        Result.Initialize(Self);
        Result.Start;
      end);
  end;
end;

procedure TCopyChannelReceiveThread.Initialize(const aTargetWindowHandle:
    UInt64);
begin
  // Set the handle and mark that we are connected
  SetConnection(aTargetWindowHandle, cccs_Connected);
  // Send a message to the server to tell it about us
  SetReturnHandleOnTarget;
end;

procedure TCopyChannelReceiveThread.SetReturnHandleOnTarget;
var
  requestArray: TByteDynArray;
  ourWindowHandle: UInt64;
begin
  ourWindowHandle := InternalWindowHandle;
  SetLength(requestArray, SizeOf(ourWindowHandle));
  Move(ourWindowHandle, requestArray[0], SizeOf(ourWindowHandle));
  ProcessEvaluatorRequest(emt_SetReturnHandle, requestArray);
end;

procedure TCopyChannelSendThread.AfterConstruction;
begin
  inherited;
  FReceiveDataLock := TSpinLock.Create(True);
end;

function TCopyChannelSendThread.GetErrorList(const aTimeout: Int64):
    TStringDynArray;
var
  errorList: TByteDynArray;
begin
  ProcessEvaluatorRequestWithResponse(nil, errorList, aTimeout, aTimeout, emt_GetErrors);
  Result := TEncoding.UTF8.GetString(errorList).Split([#10#13]);
end;

function TCopyChannelSendThread.ProcessEvaluatorRequestWithResponse(const
    aRequestData: TByteDynArray; var aResponseData: TByteDynArray; const
    aNoResponseTimeout, aMaximumRequestTime: Int64; const aMessageType:
    TCopyChannelMessageType = emt_Data; const aAbortRequest:
    TCopyChannelAbortRequest = nil): Boolean;
var
  abortRequest: Boolean;
  errorSubmittingRequest: string;
  heartbeatFailed: Boolean;
  requestSubmissionFinished: Boolean;
  startTick: Cardinal;
begin
  CheckTargetWindowHandle;
  // Try replace the 0 with a 1 in FRequestInProgress
  if TInterlocked.CompareExchange(FRequestInProgress, 1, 0) <> 0 then
    raise Exception.Create('A request is already in progress.');
  try
    startTick := GetTickCount;
    requestSubmissionFinished := False;
    FResponseReceived := False;
    FErrorReceived := False;
    abortRequest := False;
    // Clear any data we have received before
    FReceiveDataLock.Enter;
    try
      FReceivedData := nil;
    finally
      FReceiveDataLock.Exit;
    end;
    FAllowIncomingRequests := True;
    try
      errorSubmittingRequest := '';
      TThread.CreateAnonymousThread(
          procedure
          begin
            try
              ProcessEvaluatorRequest(aMessageType, aRequestData);
            except
              on E: Exception do
              begin
                errorSubmittingRequest := E.Message;
              end;
            end;
            requestSubmissionFinished := True;
          end).Start;
      // Wait for the request to finish
      while (not abortRequest) and (not requestSubmissionFinished) and
            ((GetTickCount - startTick) < aNoResponseTimeout) do
      begin
        ThreadSleep(50);
        if Assigned(aAbortRequest) then
          abortRequest := aAbortRequest;
      end;

      if not abortRequest then
      begin
        // Did we manage to send the request?
        if not requestSubmissionFinished then
        begin
          SendBackgroundRequest(emt_CancelRequest);
          raise Exception.Create('Timed out waiting for request to send!');
        end;
        if errorSubmittingRequest <> '' then
          raise Exception.Create('Error submitting request: ' + errorSubmittingRequest);

        // Initialize the heartbeat
        TInterlocked.Exchange(Double(FLastHeartbeat), Now);
      end;

      // Now wait for a response
      heartbeatFailed := False;
      while (not abortRequest) and
            (not FResponseReceived) and
            (not heartbeatFailed) and
            ((aMaximumRequestTime = -1) or
             ((GetTickCount - startTick) < aMaximumRequestTime)) do
      begin
        ThreadSleep(50);
        if Assigned(aAbortRequest) then
          abortRequest := aAbortRequest;
        heartbeatFailed := (SecondSpan(LastHeartbeat, Now) >= (aNoResponseTimeout div 1000));
      end;
      if FResponseReceived then
      begin
        Result := not FErrorReceived;
        if Result then
        begin
          // Set the result data
          FReceiveDataLock.Enter;
          try
            aResponseData := FReceivedData;
            FReceivedData := nil;
          finally
            FReceiveDataLock.Exit;
          end;
        end;
      end
      else
      begin
        Result := False;
        SendBackgroundRequest(emt_CancelRequest);
        if heartbeatFailed then
          raise Exception.Create('Lost connection to receiver!');
        if not abortRequest then
          raise Exception.Create('Timed out waiting for response!');
      end;
    finally
      FAllowIncomingRequests := False;
    end;
  finally
    // We don't need a compareexchange here because we are the only one that
    // would have been able to set this to 1;
    FRequestInProgress := 0;
  end;
end;

function TCopyChannelSendThread.ProcessEvaluatorRequestWithResponse(const
    aRequestData, aResponseData: TMemoryStream; const aNoResponseTimeout,
    aMaximumRequestTime: Int64; const aMessageType: TCopyChannelMessageType =
    emt_Data; const aAbortRequest: TCopyChannelAbortRequest = nil): Boolean;
var
  requestByteData: TByteDynArray;
  responseByteData: TByteDynArray;
begin
  SetLength(requestByteData, aRequestData.Size);
  Move(aRequestData.Memory^, requestByteData[0], Length(requestByteData));
  Result := ProcessEvaluatorRequestWithResponse(requestByteData,
      responseByteData, aNoResponseTimeout, aMaximumRequestTime,
      aMessageType, aAbortRequest);
  if Length(responseByteData) > 0 then
  begin
    aResponseData.Write(responseByteData[0], Length(responseByteData));
    aResponseData.Position := 0;
  end;
end;

procedure TCopyChannelSendThread.ReceiveRequest(const aCommuncationsData:
    TCopyChannelCommuncationsData);
var
  receivedWindowHandle: UInt64;
begin
  inherited;

  // We only check for the messages that we expect as a sender
  if (aCommuncationsData.MessageType = emt_SetReturnHandle) then
  begin
    if Length(aCommuncationsData.RequestData) = SizeOf(receivedWindowHandle) then
    begin
      Move(aCommuncationsData.RequestData[0], receivedWindowHandle, SizeOf(receivedWindowHandle));
      // Set the handle and mark that we are connected
      SetConnection(receivedWindowHandle, cccs_Connected);
    end;
  end
  else if (aCommuncationsData.MessageType = emt_Heartbeat) then
  begin
    TInterlocked.Exchange(Double(FLastHeartbeat), Now);
  end
  else if (aCommuncationsData.MessageType = emt_Exiting) then
  begin
    // Clear the handle and mark that we are no longer connected
    SetConnection(0, cccs_ConnectionClosed);
  end
  else if (aCommuncationsData.MessageType = emt_Data) and FAllowIncomingRequests then
  begin
    // We have a response so save it to the received data
    FReceiveDataLock.Enter;
    try
      FReceivedData := aCommuncationsData.RequestData;
    finally
      FReceiveDataLock.Exit;
    end;
    FResponseReceived := True;
  end
  else if (aCommuncationsData.MessageType = emt_Error) and FAllowIncomingRequests then
  begin
    // We have an error response
    FErrorReceived := True;
    FResponseReceived := True;
  end;
end;

procedure TCopyChannelSendThread.ShutdownReceiver;
begin
  SendBackgroundRequest(emt_RequestShutdown, nil);
end;

procedure TCopyChannelHeartbeatThread.Execute;
const
  NOTIFY_INTERVAL = 400; // Notify the sender every 0.4 seconds
  SLEEP_TIME = 50; // Sleep for 50ms
var
  notifyCount: Integer;
begin
  inherited;
  notifyCount := NOTIFY_INTERVAL div SLEEP_TIME;
  while not Terminated do
  begin
    ThreadSleep(SLEEP_TIME);
    Dec(notifyCount);
    if notifyCount <= 0 then
    begin
      notifyCount := NOTIFY_INTERVAL div SLEEP_TIME;
      FCopyChannelThread.SendBackgroundRequest(emt_Heartbeat, nil);
    end;
  end;
end;

procedure TCopyChannelHeartbeatThread.Initialize(const aCopyChannelThread:
    TCopyChannelCommunicationThread);
begin
  FCopyChannelThread := aCopyChannelThread;
end;

end.
