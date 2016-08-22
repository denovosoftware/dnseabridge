program OpaRStandAloneApp;

uses
  Vcl.Forms,
  OpaRVCLDataModuleUnit in 'OpaRVCLDataModuleUnit.pas' {OpaRVCLDataModule: TDataModule},
  ExternalEvaluatorClassesUnit in '..\Common\ExternalEvaluatorClassesUnit.pas',
  OpaRStandAlone in 'OpaRStandAlone.pas' {Form1},
  AboutDNSEABridgeFormUnit in 'AboutDNSEABridgeFormUnit.pas' {AboutDNSEABridgeForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOpaRVCLDataModule, OpaRVCLDataModule);
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm := False;
  Application.Run;
end.
