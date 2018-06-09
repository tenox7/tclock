unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, Registry;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorDialogFg: TColorDialog;
    ColorDialogBg: TColorDialog;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    About: TMenuItem;
    IsOnTop: TMenuItem;
    FontPick: TMenuItem;
    Background: TMenuItem;
    AMPM: TMenuItem;
    Alpha: TMenuItem;
    a40: TMenuItem;
    a20: TMenuItem;
    a60: TMenuItem;
    a80: TMenuItem;
    a0: TMenuItem;
    Foreground: TMenuItem;
    RunOnStart: TMenuItem;
    Seconds: TMenuItem;
    TitleBar: TMenuItem;
    Quit: TMenuItem;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure a0Click(Sender: TObject);
    procedure a20Click(Sender: TObject);
    procedure a40Click(Sender: TObject);
    procedure a60Click(Sender: TObject);
    procedure a80Click(Sender: TObject);
    procedure AMPMClick(Sender: TObject);
    procedure BackgroundClick(Sender: TObject);
    procedure FontPickClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IsOnTopClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure ForegroundClick(Sender: TObject);
    procedure RunOnStartClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure SecondsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TitleBarClick(Sender: TObject);
    procedure WindowToFont();
    procedure TimeFormat();
    procedure SaveConfig();
    procedure LoadConfig();
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  FMouseDownPt: TPoint;
  TimeStr: string;
  TimeFmt: string;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SaveConfig();
var
  Cfg: TRegistry;
  RunKey: TRegistry;
begin

  Cfg := TRegistry.Create(KEY_READ Or KEY_WRITE);
  Cfg.RootKey := HKEY_CURRENT_USER;
  if Cfg.OpenKey('Software\\Tenox\\TClock\\',True) then
  begin
    Cfg.WriteBool('TitleBar', TitleBar.Checked);
    Cfg.WriteBool('IsOnTop', IsOnTop.Checked);
    Cfg.WriteBool('Seconds', Seconds.Checked);
    Cfg.WriteBool('AMPM', AMPM.Checked);
    Cfg.WriteString('Background', ColorToString(Form1.Color));
    Cfg.WriteString('FontName', Label1.Font.Name);
    Cfg.WriteString('FontColor', ColorToString(Label1.Font.Color));
    Cfg.WriteInteger('FontSize', Label1.Font.Size);
    Cfg.WriteInteger('FontStyle', Integer(Label1.Font.Style));
    Cfg.WriteInteger('Xpos', Form1.Left);
    Cfg.WriteInteger('Ypos', Form1.Top);
    Cfg.WriteInteger('Alpha', Form1.AlphaBlendValue);
  end;
  Cfg.Free;

  RunKey := TRegistry.Create(KEY_READ Or KEY_WRITE);
  RunKey.RootKey := HKEY_CURRENT_USER;
  if RunKey.OpenKey('Software\\Microsoft\\Windows\\CurrentVersion\\Run\\',True) then
      if RunKey.ValueExists('TClock') then
        RunOnStart.Checked := True;

  if RunOnStart.Checked then
  begin
    RunKey.WriteString('TClock', '"' + Application.ExeName + '"');
  end
  else
  begin
    RunKey.DeleteValue('TClock');
  end;
  RunKey.Free;
end;

procedure TForm1.LoadConfig();
var
  Cfg: TRegistry;
  RunKey: TRegistry;
begin
  Cfg := TRegistry.Create(KEY_READ Or KEY_WRITE);
  Cfg.RootKey := HKEY_CURRENT_USER;
  if Cfg.OpenKey('Software\\Tenox\\TClock\\',True) then
  begin
    if Cfg.ValueExists('TitleBar') then
    begin
      TitleBar.Checked := Cfg.ReadBool('TitleBar');
      if TitleBar.Checked then
        Form1.BorderStyle := bsToolWindow
      else
        Form1.BorderStyle := bsNone;
    end;

    if Cfg.ValueExists('IsOnTop') then
    begin
      IsOnTop.Checked := Cfg.ReadBool('IsOnTop');
      if IsOnTop.Checked then
        Form1.FormStyle := fsSystemStayOnTop
      else
        Form1.FormStyle := fsNormal;
    end;

    if Cfg.ValueExists('Seconds') then
      Seconds.Checked := Cfg.ReadBool('Seconds');

    if Cfg.ValueExists('AMPM') then
      AMPM.Checked := Cfg.ReadBool('AMPM');

    if Cfg.ValueExists('Background') then
      Form1.Color := StringToColor(Cfg.ReadString('Background'));

    if Cfg.ValueExists('FontName') then
      Label1.Font.Name := Cfg.ReadString('FontName');

    if Cfg.ValueExists('FontColor') then
      Label1.Font.Color := StringToColor(Cfg.ReadString('FontColor'));

    if Cfg.ValueExists('FontSize') then
      Label1.Font.Size := Cfg.ReadInteger('FontSize');

    if Cfg.ValueExists('FontStyle') then
      Label1.Font.Style := TFontStyles(Cfg.ReadInteger('FontStyle'));

    if Cfg.ValueExists('Xpos') Then
      Form1.Left := Cfg.ReadInteger('Xpos');

    if Cfg.ValueExists('Ypos') Then
      Form1.Top := Cfg.ReadInteger('Ypos');

    if Cfg.ValueExists('Alpha') Then
    begin
      Form1.AlphaBlendValue := Cfg.ReadInteger('Alpha');
      if Form1.AlphaBlendValue > 0 then Form1.AlphaBlend := True
      else Form1.AlphaBlend := False;
      if Form1.AlphaBlend = True then a0.Checked := False;
    end;

    Cfg.Free;

    RunKey := TRegistry.Create(KEY_READ Or KEY_WRITE);
    RunKey.RootKey := HKEY_CURRENT_USER;
    if RunKey.OpenKey('Software\\Microsoft\\Windows\\CurrentVersion\\Run\\',True) then
       if RunKey.ValueExists('TClock') then
          RunOnStart.Checked := True;

    RunKey.Free;
  end;
end;


procedure TForm1.WindowToFont();
begin
  Form1.Width := Label1.Canvas.TextWidth(TimeStr) + 20;
  Form1.Height := Label1.Canvas.TextHeight(TimeStr) + 10;
end;

procedure TForm1.TimeFormat();
begin
  if Seconds.Checked and AMPM.Checked then
  begin
    TimeStr := '00:00:00 XX';
    TimeFmt := 'hh:mm:ss AM/PM';
  end
  else if Seconds.Checked and not AMPM.Checked then
  begin
    TimeStr := '00:00:00';
    TimeFmt := 'hh:mm:ss';
  end
  else if not Seconds.Checked and AMPM.Checked then
  begin
    TimeStr := '00:00 XX';
    TimeFmt := 'hh:mm AM/PM';
  end
  else if not Seconds.Checked and not AMPM.Checked then
  begin
    TimeStr := '00:00';
    TimeFmt := 'hh:mm';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadConfig;
  TimeFormat;
  WindowToFont;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveConfig;
end;

procedure TForm1.FontPickClick(Sender: TObject);

begin
  FontDialog1.Font := Label1.Font;
  if FontDialog1.Execute then
  begin
    Label1.Font := FontDialog1.Font;
    WindowToFont;
  end;
  SaveConfig;
end;

procedure TForm1.ForegroundClick(Sender: TObject);
begin
  if ColorDialogFg.Execute then
    Label1.Font.Color := ColorDialogFg.Color;
  SaveConfig;
end;

procedure TForm1.BackgroundClick(Sender: TObject);
begin
  if ColorDialogBg.Execute then
    Form1.Color := ColorDialogBg.Color;
  SaveConfig;
end;

procedure TForm1.AMPMClick(Sender: TObject);
begin
  if AMPM.Checked = True then
    AMPM.Checked := False
  else
    AMPM.Checked := True;

  TimeFormat;
  WindowToFont;
  SaveConfig;
end;

procedure TForm1.a0Click(Sender: TObject);
begin
  Form1.AlphaBlend := False;
  Form1.AlphaBlendValue := 255;
  a0.Checked := True;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TForm1.a20Click(Sender: TObject);
begin
  Form1.AlphaBlend := True;
  Form1.AlphaBlendValue := 200;
  a0.Checked := False;
  a20.Checked := True;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TForm1.a40Click(Sender: TObject);
begin
  Form1.AlphaBlend := True;
  Form1.AlphaBlendValue := 150;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := True;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TForm1.a60Click(Sender: TObject);
begin
  Form1.AlphaBlend := True;
  Form1.AlphaBlendValue := 100;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := True;
  a80.Checked := False;
  SaveConfig;
end;

procedure TForm1.a80Click(Sender: TObject);
begin
  Form1.AlphaBlend := True;
  Form1.AlphaBlendValue := 50;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := True;
  SaveConfig;
end;

procedure TForm1.SecondsClick(Sender: TObject);
begin
  if Seconds.Checked = True then
    Seconds.Checked := False
  else
    Seconds.Checked := True;

  TimeFormat;
  WindowToFont;
  SaveConfig;
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  ShowMessage('Tenox Desktop Clock v2.0' + sLineBreak +
    'Copyright (c) 2017-2018 by Antoni Sawicki' + sLineBreak +
    'https://github.com/tenox7/tclock/');
end;


procedure TForm1.RunOnStartClick(Sender: TObject);
begin
  if RunOnStart.Checked then
  begin
    RunOnStart.Checked := False;
  end
  else
  begin
    RunOnStart.Checked := True;
  end;
end;

procedure TForm1.QuitClick(Sender: TObject);
begin
  SaveConfig;
  Application.Terminate();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := FormatDateTime(TimeFmt, Now);
end;


procedure TForm1.TitleBarClick(Sender: TObject);
begin
  if Form1.BorderStyle = bsNone then
    begin
      Form1.BorderStyle := bsToolWindow;
      TitleBar.Checked := True;
    end
  else
    begin
      Form1.BorderStyle := bsNone;
      TitleBar.Checked := False;
    end;
    SaveConfig;
end;

procedure TForm1.IsOnTopClick(Sender: TObject);
begin
  if Form1.FormStyle = fsSystemStayOnTop then
  begin
    Form1.FormStyle := fsNormal;
    IsOnTop.Checked := False;
  end
  else
  begin
    Form1.FormStyle := fsSystemStayOnTop;
    IsOnTop.Checked := True;
  end;
  SaveConfig;
end;


procedure TForm1.Label1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseDownPt := Point(X, Y);
end;

procedure TForm1.Label1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if (ssLeft in Shift) then
  begin
    Left := Left + (X - FMouseDownPt.X);
    Top := Top + (Y - FMouseDownPt.Y);
  end;
end;


end.
