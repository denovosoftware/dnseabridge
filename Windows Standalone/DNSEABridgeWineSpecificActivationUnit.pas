unit DNSEABridgeWineSpecificActivationUnit;

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
  System.Classes,
  VCL.Forms,
  AboutDNSEABridgeFormUnit;

type
  TDNSEABridgeWineSpecificActivation = class
  strict private
    FHasBeenRestoredAlready: boolean;
  public
    procedure MinimizeDNSEABridgeOnStartup;
    procedure HandleApplicationRestore(Sender: TObject);
    procedure HandleApplicationActivate(Sender: TObject);
  end;


implementation


procedure ShowAboutFormAndCreateIfNeeded;
begin
  if not Assigned(AboutDNSEABridgeForm) then
    AboutDNSEABridgeForm := TAboutDNSEABridgeForm.Create(Application);

  AboutDNSEABridgeForm.Visible := True;
  AboutDNSEABridgeForm.Show;
end;

type
  TCreateHiddenAboutFormThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

procedure TCreateHiddenAboutFormThread.Execute;
begin
  inherited;
  Synchronize(
    procedure
    begin
      ShowAboutFormAndCreateIfNeeded;

      Application.Minimize;
      AboutDNSEABridgeForm.Close;
    end);
end;

constructor TCreateHiddenAboutFormThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TDNSEABridgeWineSpecificActivation.HandleApplicationActivate(Sender:
    TObject);
begin
  if FHasBeenRestoredAlready then
    ShowAboutFormAndCreateIfNeeded;
end;

procedure TDNSEABridgeWineSpecificActivation.HandleApplicationRestore(Sender:
    TObject);
begin
  ShowAboutFormAndCreateIfNeeded;
  FHasBeenRestoredAlready := True;
end;

procedure TDNSEABridgeWineSpecificActivation.MinimizeDNSEABridgeOnStartup;
var
  createAboutThread: TCreateHiddenAboutFormThread;
begin
  createAboutThread := TCreateHiddenAboutFormThread.Create;
  createAboutThread.Start;
  FHasBeenRestoredAlready := False;
end;

end.
