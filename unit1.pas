unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, Registry;

type

  { TClockForm }

  TClockForm = class(TForm)
    ColorDialogFg: TColorDialog;
    ColorDialogBg: TColorDialog;
    FontDialog1: TFontDialog;
    ClockLabel: TLabel;
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
    ClockMenu: TPopupMenu;
    ClockTimer: TTimer;
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
    procedure ClockTimerTick(Sender: TObject);
    procedure ClockLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ClockLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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
  ClockForm: TClockForm;
  FMouseDownPt: TPoint;
  TimeStr: string;
  TimeFmt: string;

implementation

{$R *.lfm}

{ TClockForm }

procedure TClockForm.SaveConfig();
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
    Cfg.WriteString('Background', ColorToString(ClockForm.Color));
    Cfg.WriteString('FontName', ClockLabel.Font.Name);
    Cfg.WriteString('FontColor', ColorToString(ClockLabel.Font.Color));
    Cfg.WriteInteger('FontSize', ClockLabel.Font.Size);
    Cfg.WriteInteger('FontStyle', Integer(ClockLabel.Font.Style));
    Cfg.WriteInteger('Xpos', ClockForm.Left);
    Cfg.WriteInteger('Ypos', ClockForm.Top);
    Cfg.WriteInteger('Alpha', ClockForm.AlphaBlendValue);
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

procedure TClockForm.LoadConfig();
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
        ClockForm.BorderStyle := bsToolWindow
      else
        ClockForm.BorderStyle := bsNone;
    end;

    if Cfg.ValueExists('IsOnTop') then
    begin
      IsOnTop.Checked := Cfg.ReadBool('IsOnTop');
      if IsOnTop.Checked then
        ClockForm.FormStyle := fsSystemStayOnTop
      else
        ClockForm.FormStyle := fsNormal;
    end;

    if Cfg.ValueExists('Seconds') then
      Seconds.Checked := Cfg.ReadBool('Seconds');

    if Cfg.ValueExists('AMPM') then
      AMPM.Checked := Cfg.ReadBool('AMPM');

    if Cfg.ValueExists('Background') then
      ClockForm.Color := StringToColor(Cfg.ReadString('Background'));

    if Cfg.ValueExists('FontName') then
      ClockLabel.Font.Name := Cfg.ReadString('FontName');

    if Cfg.ValueExists('FontColor') then
      ClockLabel.Font.Color := StringToColor(Cfg.ReadString('FontColor'));

    if Cfg.ValueExists('FontSize') then
      ClockLabel.Font.Size := Cfg.ReadInteger('FontSize');

    if Cfg.ValueExists('FontStyle') then
      ClockLabel.Font.Style := TFontStyles(Cfg.ReadInteger('FontStyle'));

    if Cfg.ValueExists('Xpos') Then
      ClockForm.Left := Cfg.ReadInteger('Xpos');

    if Cfg.ValueExists('Ypos') Then
      ClockForm.Top := Cfg.ReadInteger('Ypos');

    if Cfg.ValueExists('Alpha') Then
    begin
      ClockForm.AlphaBlendValue := Cfg.ReadInteger('Alpha');
      if ClockForm.AlphaBlendValue > 0 then ClockForm.AlphaBlend := True
      else ClockForm.AlphaBlend := False;
      if ClockForm.AlphaBlend = True then a0.Checked := False;
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


procedure TClockForm.WindowToFont();
begin
  ClockForm.Width := ClockLabel.Canvas.TextWidth(TimeStr) + 20;
  ClockForm.Height := ClockLabel.Canvas.TextHeight(TimeStr) + 10;
end;

procedure TClockForm.TimeFormat();
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

procedure TClockForm.FormCreate(Sender: TObject);
begin
  LoadConfig;
  TimeFormat;
  WindowToFont;
end;

procedure TClockForm.FormDestroy(Sender: TObject);
begin
  SaveConfig;
end;

procedure TClockForm.FontPickClick(Sender: TObject);
begin
  FontDialog1.Font := ClockLabel.Font;
  if FontDialog1.Execute then
  begin
    ClockLabel.Font := FontDialog1.Font;
    WindowToFont;
  end;
  SaveConfig;
end;

procedure TClockForm.ForegroundClick(Sender: TObject);
begin
  if ColorDialogFg.Execute then
    ClockLabel.Font.Color := ColorDialogFg.Color;
  SaveConfig;
end;

procedure TClockForm.BackgroundClick(Sender: TObject);
begin
  if ColorDialogBg.Execute then
    ClockForm.Color := ColorDialogBg.Color;
  SaveConfig;
end;

procedure TClockForm.AMPMClick(Sender: TObject);
begin
  if AMPM.Checked = True then
    AMPM.Checked := False
  else
    AMPM.Checked := True;

  TimeFormat;
  WindowToFont;
  SaveConfig;
end;

procedure TClockForm.a0Click(Sender: TObject);
begin
  ClockForm.AlphaBlend := False;
  ClockForm.AlphaBlendValue := 255;
  a0.Checked := True;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TClockForm.a20Click(Sender: TObject);
begin
  ClockForm.AlphaBlend := True;
  ClockForm.AlphaBlendValue := 200;
  a0.Checked := False;
  a20.Checked := True;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TClockForm.a40Click(Sender: TObject);
begin
  ClockForm.AlphaBlend := True;
  ClockForm.AlphaBlendValue := 150;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := True;
  a60.Checked := False;
  a80.Checked := False;
  SaveConfig;
end;

procedure TClockForm.a60Click(Sender: TObject);
begin
  ClockForm.AlphaBlend := True;
  ClockForm.AlphaBlendValue := 100;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := True;
  a80.Checked := False;
  SaveConfig;
end;

procedure TClockForm.a80Click(Sender: TObject);
begin
  ClockForm.AlphaBlend := True;
  ClockForm.AlphaBlendValue := 50;
  a0.Checked := False;
  a20.Checked := False;
  a40.Checked := False;
  a60.Checked := False;
  a80.Checked := True;
  SaveConfig;
end;

procedure TClockForm.SecondsClick(Sender: TObject);
begin
  if Seconds.Checked = True then
    Seconds.Checked := False
  else
    Seconds.Checked := True;

  TimeFormat;
  WindowToFont;
  SaveConfig;
end;

procedure TClockForm.AboutClick(Sender: TObject);
begin
  ShowMessage('Tenox Desktop Clock v2.0' + sLineBreak +
    'Copyright (c) 2017-2018 by Antoni Sawicki' + sLineBreak +
    'https://github.com/tenox7/tclock/');
end;


procedure TClockForm.RunOnStartClick(Sender: TObject);
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

procedure TClockForm.QuitClick(Sender: TObject);
begin
  SaveConfig;
  Application.Terminate();
end;

procedure TClockForm.ClockTimerTick(Sender: TObject);
begin
  ClockLabel.Caption := FormatDateTime(TimeFmt, Now);
end;


procedure TClockForm.TitleBarClick(Sender: TObject);
begin
  if ClockForm.BorderStyle = bsNone then
    begin
      ClockForm.BorderStyle := bsToolWindow;
      TitleBar.Checked := True;
    end
  else
    begin
      ClockForm.BorderStyle := bsNone;
      TitleBar.Checked := False;
    end;
    SaveConfig;
end;

procedure TClockForm.IsOnTopClick(Sender: TObject);
begin
  if ClockForm.FormStyle = fsSystemStayOnTop then
  begin
    ClockForm.FormStyle := fsNormal;
    IsOnTop.Checked := False;
  end
  else
  begin
    ClockForm.FormStyle := fsSystemStayOnTop;
    IsOnTop.Checked := True;
  end;
  SaveConfig;
end;


procedure TClockForm.ClockLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseDownPt := Point(X, Y);
end;

procedure TClockForm.ClockLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if (ssLeft in Shift) then
  begin
    Left := Left + (X - FMouseDownPt.X);
    Top := Top + (Y - FMouseDownPt.Y);
  end;
end;


end.
