unit DNSEABridgeWindowsTrayIconUnit;

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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  AboutDNSEABridgeFormUnit;

type
  TNeedsRVersionEvent = procedure(out aRServerVersion: string) of object;

  TDNSEABridgeWindowsTrayIconForm = class(TForm)
    popupMenuTrayIcon: TPopupMenu;
    DNSEABridgeWindowsTrayIcon: TTrayIcon;
    ImageList1: TImageList;
    mniAbout: TMenuItem;
    mniQuit: TMenuItem;
    mniRDllVersion: TMenuItem;
    procedure DNSEABridgeWindowsTrayIconDblClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniQuitClick(Sender: TObject);
    procedure popupMenuTrayIconPopup(Sender: TObject);
  strict private
    FOnNeedsToQuitApplication: TNotifyEvent;
    FOnNeedsVersionOfR: TNeedsRVersionEvent;
    function GetRVersion: string;
    procedure DoNeedsToQuitApplication(Sender: TObject);
    procedure ShowAboutForm;
    procedure QuitDNSEABridgeWithWarning(Sender: TObject);
  public
    procedure PrepareToShutdownApplication;
    property OnNeedsVersionOfR: TNeedsRVersionEvent read FOnNeedsVersionOfR write
        FOnNeedsVersionOfR;
    property OnNeedsToQuitApplication: TNotifyEvent read FOnNeedsToQuitApplication
        write FOnNeedsToQuitApplication;
  end;

var
  DNSEABridgeWindowsTrayIconForm: TDNSEABridgeWindowsTrayIconForm;

implementation


{$R *.dfm}
const
  WARNING_CLOSE_SERVER = 'Exiting the De Novo Software External Application '+
    'Bridge can cause failure in FCS Express data transformations '+
    'that interact with the application bridge. ' +
    sLineBreak + 'Do you wish to close the De Novo Software External Application Bridge?';

procedure TDNSEABridgeWindowsTrayIconForm.DNSEABridgeWindowsTrayIconDblClick(
    Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TDNSEABridgeWindowsTrayIconForm.DoNeedsToQuitApplication(Sender:
    TObject);
begin
  if Assigned(FOnNeedsToQuitApplication) then
    FOnNeedsToQuitApplication(Sender);
end;

procedure TDNSEABridgeWindowsTrayIconForm.mniAboutClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TDNSEABridgeWindowsTrayIconForm.mniQuitClick(Sender: TObject);
begin
  QuitDNSEABridgeWithWarning(Sender);
end;

type
  TSetRDllCaptionThread = class(TThread)
  strict private
    FMenuItem: TMenuItem;
    FRVersionString: string;
  strict protected
    procedure Execute; override;
  public
    constructor Create(const aRMenuItem: TMenuItem; const aRVersionString: string);
  end;

constructor TSetRDllCaptionThread.Create(const aRMenuItem: TMenuItem; const
    aRVersionString: string);
begin
  inherited Create(True);
  FMenuItem := aRMenuItem;
  FRVersionString := aRVersionString;
  FreeOnTerminate := True;
end;

procedure TSetRDllCaptionThread.Execute;
begin
  inherited;
  Synchronize(
    procedure
    begin
      if FRVersionString <> '' then
        FMenuItem.Caption := FRVersionString
      else
        FMenuItem.Caption := 'R was not found';
    end
    );
end;

function TDNSEABridgeWindowsTrayIconForm.GetRVersion: string;
begin
  Result := '';
  if Assigned(OnNeedsVersionOfR) then
    OnNeedsVersionOfR(result);
end;

procedure TDNSEABridgeWindowsTrayIconForm.popupMenuTrayIconPopup(Sender:
    TObject);
var
  setCaptionThread: TSetRDllCaptionThread;
  rVersion: string;
begin
  rVersion := GetRVersion;
  setCaptionThread := TSetRDllCaptionThread.Create(mniRDllVersion, rVersion);
  setCaptionThread.Start;
end;

procedure TDNSEABridgeWindowsTrayIconForm.PrepareToShutdownApplication;
begin
  TThread.Synchronize(
    TThread.CurrentThread,
    procedure
    begin
      DNSEABridgeWindowsTrayIcon.Visible := False;
    end);
end;

procedure TDNSEABridgeWindowsTrayIconForm.QuitDNSEABridgeWithWarning(Sender:
    TObject);
begin
  if MessageDlg(WARNING_CLOSE_SERVER, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    DoNeedsToQuitApplication(Sender);
end;

procedure TDNSEABridgeWindowsTrayIconForm.ShowAboutForm;
begin
  // Create the form if it has not been created yet
  if not Assigned(AboutDNSEABridgeForm) then
    AboutDNSEABridgeForm := TAboutDNSEABridgeForm.Create(Application);

  // Show the about form.
  //  If the form was closed, this will reopen the form.
  //  If the form is already showing, this will bring the form to the front
  AboutDNSEABridgeForm.Show;
end;


end.
