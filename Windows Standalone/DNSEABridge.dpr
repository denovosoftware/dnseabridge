program DNSEABridge;

{$R *.res}

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


{--------------------------------------------------------------
  The De Novo Software External Application Bridge will capture
  errors that are thrown during execution. A message box will
  be displayed for the error.
  For debugging purposes a log file may be passed as a command
  line option. In this case, no message box will be displayed
  and the error messaage will be written to the provided file.
  If the folder for the log file is not found, then the log file
  will be written into the folder that the executable is found.
 --------------------------------------------------------------}

uses
  Vcl.Forms,
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  DNSEABridgeVCLDataModuleUnit in 'DNSEABridgeVCLDataModuleUnit.pas' {DNSEABridgeVCLDataModule: TDataModule},
  ExternalEvaluatorClassesUnit in '..\Common\ExternalEvaluatorClassesUnit.pas',
  DNSEABridgeHiddenForm in 'DNSEABridgeHiddenForm.pas' {Form1},
  AboutDNSEABridgeFormUnit in 'AboutDNSEABridgeFormUnit.pas' {AboutDNSEABridgeForm},
  DNSEABridgeRScriptRunnerUnit in '..\Common\DNSEABridgeRScriptRunnerUnit.pas';

begin
  try
    Application.Initialize;
    Application.CreateForm(TDNSEABridgeVCLDataModule, DNSEABridgeVCLDataModule);
    Application.OnException := DNSEABridgeVCLDataModule.DoOnException;
    Application.CreateForm(TForm1, Form1);
    Application.ShowMainForm := False;
    Application.Run;
  except
    on E: Exception do
      DNSEABridgeVCLDataModuleUnit.LogExceptionInFile(E);
  end;
end.
