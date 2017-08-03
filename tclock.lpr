program tclock;
//{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  //TrayIcon1.Show;
  //Form1.ShowInTaskBar := stNever;
  //Application.MainFormOnTaskBar := True; https://bugs.freepascal.org/view.php?id=17294
  Application.Run;
end.

