unit DNSEABridgeEvaluatorThreads;

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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Rtti,
  ExternalEvaluatorClassesUnit,
  WMCopyChannel;

type
  TOnEvaluatorRequest = procedure(const aEvaluatorInput: TExternalEvaluatorInput;
      aResultData: TExternalEvaluatorResult) of object;

  TEvaluatorReceiveThread = class(TCopyChannelReceiveThread)
  strict private
    FOnPerformCluster: TOnEvaluatorRequest;
    FOnPerformNewParam: TOnEvaluatorRequest;
    FOnPerformAutoGating: TOnEvaluatorRequest;
    FOnShutdown: TNotifyEvent;

    procedure PerformEvaluatorAction(const aCommuncationsData:
        TCopyChannelCommuncationsData);
  strict protected
    procedure ReceiveRequestInternal(const aCommuncationsData:
        TCopyChannelCommuncationsData; const aHeartbeatThreadCreate:
        TCopyChannelHeartbeatThreadCreateProc); override;
  public
    property OnPerformCluster: TOnEvaluatorRequest read FOnPerformCluster write
        FOnPerformCluster;
    property OnPerformNewParam: TOnEvaluatorRequest read FOnPerformNewParam write
        FOnPerformNewParam;
    property OnPerformAutoGating: TOnEvaluatorRequest read FOnPerformAutoGating write
        FOnPerformAutoGating;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  end;

implementation

procedure TEvaluatorReceiveThread.PerformEvaluatorAction(const
    aCommuncationsData: TCopyChannelCommuncationsData);
var
  dataStream: TMemoryStream;
  evaluatorInput: TExternalEvaluatorinput;
  evaluatorResult: TExternalEvaluatorResult;
  resultByteData: TByteDynArray;
begin
  try
    dataStream := aCommuncationsData.CreateMemoryStreamForRequestData;
    evaluatorInput := TExternalEvaluatorinput.Create(nil);
    evaluatorResult := TExternalEvaluatorResult.Create(nil);
    try
      evaluatorInput.LoadFromStream(dataStream);
      case evaluatorInput.EvaluatorAction of
        eea_RunCluster:
        begin
          if Assigned(FOnPerformCluster) then
            FOnPerformCluster(evaluatorInput, evaluatorResult);
        end;
        eea_NewParam:
        begin
          if Assigned(FOnPerformNewParam) then
            FOnPerformNewParam(evaluatorInput, evaluatorResult);
        end;
        eea_AutoGating:
        begin
          if Assigned(FOnPerformAutoGating) then
            FOnPerformAutoGating(evaluatorInput, evaluatorResult);
        end;
      else
        raise Exception.Create('Unsupported Evaluator Action Type: ' + TRttiEnumerationType.GetName<TExternalEvaluatorActions>(evaluatorInput.EvaluatorAction));
      end;

      // Send back the result from the request
      dataStream.Clear;
      evaluatorResult.SaveToStream(dataStream);
      SetLength(resultByteData, dataStream.Size);
      Move(dataStream.Memory^, resultByteData[0], Length(resultByteData));
      ProcessEvaluatorRequest(emt_Data, resultByteData);
    finally
      evaluatorResult.Free;
      evaluatorInput.Free;
      dataStream.Free;
    end;
  except
    on E: Exception do
    begin
      AddError(E.Message, True);
    end;
  end;
end;

procedure TEvaluatorReceiveThread.ReceiveRequestInternal(const
    aCommuncationsData: TCopyChannelCommuncationsData; const
    aHeartbeatThreadCreate: TCopyChannelHeartbeatThreadCreateProc);
begin
  inherited;

  if aCommuncationsData.MessageType = emt_Data then
  begin
    TThread.CreateAnonymousThread(
      procedure
      var
        heartbeatThread: TCopyChannelHeartbeatThread;
      begin
        heartbeatThread := aHeartbeatThreadCreate;
        try
          PerformEvaluatorAction(aCommuncationsData);
        finally
          heartbeatThread.Terminate;
          heartbeatThread.WaitFor;
          heartbeatThread.Free;
        end;
      end
    ).Start;
  end
  else if (aCommuncationsData.MessageType = emt_RequestShutdown) and Assigned(FOnShutdown) then
    FOnShutdown(Self);
end;

end.
