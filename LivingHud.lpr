program LivingHud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, livinghome, utils, logviewer
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(THomeForm, HomeForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.Run;
  halt(HudSysExitCode);
end.

