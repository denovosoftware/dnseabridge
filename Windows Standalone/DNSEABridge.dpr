program DNSEABridge;

{$R *.res}

uses
  Vcl.Forms,
  DNSEABridgeVCLDataModuleUnit in 'DNSEABridgeVCLDataModuleUnit.pas' {DNSEABridgeVCLDataModule: TDataModule},
  ExternalEvaluatorClassesUnit in '..\Common\ExternalEvaluatorClassesUnit.pas',
  DNSEABridgeHiddenForm in 'DNSEABridgeHiddenForm.pas' {Form1},
  AboutDNSEABridgeFormUnit in 'AboutDNSEABridgeFormUnit.pas' {AboutDNSEABridgeForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDNSEABridgeVCLDataModule, DNSEABridgeVCLDataModule);
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm := False;
  Application.Run;
end.
