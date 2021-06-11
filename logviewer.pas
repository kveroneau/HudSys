unit logviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils,
  homecu, pisound;

type

  THUDCtlEvent = procedure(data: String) of Object;

  { TLogForm }

  TLogForm = class(TForm)
    CloseBtn: TButton;
    Label1: TLabel;
    CULog: TListBox;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FHomeCU: THomeCUTCP;
    FOnHUDCtl: THUDCtlEvent;
    function TimeStamp: string;
    procedure OnSay(message: string);
    procedure OnLog(message: string);
    procedure OnCmd(cmd: string);
    procedure OnCtl(data: string);
  public
    procedure AddLog(message: string);
    property OnHUDCtl: THUDCtlEvent read FOnHUDCtl write FOnHUDCtl;
  end;

var
  LogForm: TLogForm;

implementation

{$R *.lfm}

{ TLogForm }

procedure TLogForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TLogForm.FormCreate(Sender: TObject);
begin
  AddLog('HUD Started.');
  FHomeCU:=THomeCUTCP.Create;
  FHomeCU.OnSay:=@OnSay;
  FHomeCU.OnLog:=@OnLog;
  FHomeCU.OnCmd:=@OnCmd;
  FHomeCU.OnCtl:=@OnCtl;
  FHomeCU.Start;
end;

procedure TLogForm.FormDestroy(Sender: TObject);
begin
  if not FHomeCU.Finished then
  begin
    FHomeCU.OnSay:=Nil;
    FHomeCU.Stop;
  end;
  Sleep(500);
  FHomeCU.Free;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin
  {$IFDEF CPUARM}
  Top:=0;
  Left:=0;
  WindowState:=wsFullScreen;
  {$ENDIF}
  AddLog('Log Viewer Opened.');
end;

function TLogForm.TimeStamp: string;
begin
  Result:=FormatDateTime('m-dd-yyyy H:M', Now);
end;

procedure TLogForm.OnSay(message: string);
begin
  SoundSystem.Say(message);
  AddLog(message);
end;

procedure TLogForm.OnLog(message: string);
begin
  AddLog(message);
end;

procedure TLogForm.OnCmd(cmd: string);
begin
  AddLog(cmd);
end;

procedure TLogForm.OnCtl(data: string);
begin
  if Assigned(FOnHUDCtl) then
    FOnHUDCtl(data);
end;

procedure TLogForm.AddLog(message: string);
begin
  CULog.Items.Add('['+TimeStamp+'] '+message);
end;

end.

