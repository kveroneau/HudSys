unit livinghome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, homecu, fpjson, piservice, dateutils, pisound, utils,
  logviewer;

type

  { THomeForm }

  THomeForm = class(TForm)
    BedroomBtn: TButton;
    GamingBtn: TButton;
    MultiRoom: TCheckBox;
    RelaxBtn: TButton;
    EntranceBtn: TButton;
    ReloadBtn: TButton;
    StudyBtn: TButton;
    AllOffBtn: TButton;
    AlertText: TLabel;
    AlarmIcon: TImage;
    BrightBtn: TButton;
    LogBtn: TButton;
    CalendarDate: TLabel;
    SleepMode: TCheckBox;
    IsPaused: TCheckBox;
    ExitButton: TButton;
    Clock: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Conditions: TLabel;
    FeelsLike: TLabel;
    Timer: TTimer;
    Brightness: TTrackBar;
    procedure AllOffBtnClick(Sender: TObject);
    procedure BedroomBtnClick(Sender: TObject);
    procedure EntranceBtnClick(Sender: TObject);
    procedure GamingBtnClick(Sender: TObject);
    procedure MultiRoomClick(Sender: TObject);
    procedure RelaxBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure StudyBtnClick(Sender: TObject);
    procedure BrightBtnClick(Sender: TObject);
    procedure BrightnessChange(Sender: TObject);
    procedure ClockClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IsPausedClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    cu: THomeCU;
    FScreenOn: Boolean;
    FUpdating: Boolean;
    FAlertCount: Integer;
    procedure UpdateClock;
    procedure UpdateWeather;
    function FormatDigit(d: Word): String;
    procedure UpdateUI;
    procedure HUDCtl(data: string);
    procedure UpdateAlert(message: string);
  public

  end;

var
  HomeForm: THomeForm;

implementation

{$R *.lfm}

{ THomeForm }

procedure THomeForm.TimerTimer(Sender: TObject);
begin
  if not Assigned(LogForm.OnHUDCtl) then
    LogForm.OnHUDCtl:=@HUDCtl;
  if not Assigned(LogForm.OnAlert) then
    LogForm.OnAlert:=@UpdateAlert;
  UpdateClock;
  UpdateWeather;
  cu.UpdateConfig;
  UpdateUI;
end;

procedure THomeForm.FormCreate(Sender: TObject);
begin
  {$IFDEF CPUARM}
  Top:=0;
  Left:=0;
  WindowState:=wsFullScreen;
  {$ENDIF}
  UpdateClock;
  cu:=THomeCU.Create;
  UpdateWeather;
  UpdateUI;
  Brightness.Position:=PiGetBrightness;
  FScreenOn:=True;
  SoundSystem.Start;
  SoundSystem.Say('Interface started.');
  FAlertCount:=0;
end;

procedure THomeForm.FormDestroy(Sender: TObject);
begin
  SoundSystem.Stop;
  Sleep(1000); { Wait for thread to term. }
  cu.Free;
end;

procedure THomeForm.IsPausedClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    cu.Pause:=IsPaused.Checked;
    LogForm.AddLog('Paused Clicked.');
  end;
end;

procedure THomeForm.LogBtnClick(Sender: TObject);
begin
  LogForm.ShowModal;
end;

procedure THomeForm.ExitButtonClick(Sender: TObject);
begin
  HudSysExitCode:=2;
  Close;
end;

procedure THomeForm.StudyBtnClick(Sender: TObject);
begin
  cu.Study;
  LogForm.AddLog('Study clicked.');
end;

procedure THomeForm.BrightBtnClick(Sender: TObject);
begin
  cu.AllBright;
  LogForm.AddLog('All Bright clicked.');
end;

procedure THomeForm.BrightnessChange(Sender: TObject);
begin
  PiBrightness(Brightness.Position);
end;

procedure THomeForm.ClockClick(Sender: TObject);
begin
  if FScreenOn then
  begin
    PiDisplayOff;
    FScreenOn:=False;
    LogForm.AddLog('Display manually turned off.');
  end
  else
  begin
    PiDisplayOn;
    FScreenOn:=True;
    LogForm.AddLog('Display manually turned on.');
  end;
end;

procedure THomeForm.AllOffBtnClick(Sender: TObject);
begin
  FUpdating:=True;
  cu.AllOff;
  LogForm.AddLog('All Off clicked.');
  FUpdating:=False;
end;

procedure THomeForm.BedroomBtnClick(Sender: TObject);
begin
  cu.Bedroom;
  LogForm.AddLog('Bedroom clicked.');
end;

procedure THomeForm.EntranceBtnClick(Sender: TObject);
begin
  cu.Entrance;
  LogForm.AddLog('Enterance clicked.');
end;

procedure THomeForm.GamingBtnClick(Sender: TObject);
begin
  cu.Theme('AarronGaming');
  LogForm.AddLog('Gaming Theme clicked.');
end;

procedure THomeForm.MultiRoomClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  cu.Rooms:=MultiRoom.Checked;
  LogForm.AddLog('Multi Room clicked.');
end;

procedure THomeForm.RelaxBtnClick(Sender: TObject);
begin
  cu.Relax;
  LogForm.AddLog('Relax clicked.');
end;

procedure THomeForm.ReloadBtnClick(Sender: TObject);
begin
  HudSysExitCode:=0;
  Close;
end;

procedure THomeForm.UpdateClock;
var
  tm: TSystemTime;
begin
  GetLocalTime(tm);
  Clock.Caption:=FormatDigit(tm.Hour)+':'+FormatDigit(tm.Minute);
  CalendarDate.Caption:=FormatDateTime('dddd mmmm d, yyyy', Today);
end;

procedure THomeForm.UpdateWeather;
begin
  Conditions.Caption:=cu.Weather.Strings['conditions'];
  if cu.Weather.Types['feels_like'] = jtString then
    FeelsLike.Caption:=cu.Weather.Strings['feels_like']+'C'
  else
    FeelsLike.Caption:='Unavailable';
end;

function THomeForm.FormatDigit(d: Word): String;
begin
  if d > 9 then
    Result:=IntToStr(d)
  else
    Result:='0'+IntToStr(d);
end;

procedure THomeForm.UpdateUI;
begin
  FUpdating:=True;
  SleepMode.Checked:=cu.Sleeping;
  IsPaused.Checked:=cu.Pause;
  MultiRoom.Checked:=cu.Rooms;
  if FAlertCount > 0 then
    Dec(FAlertCount);
  if FAlertCount = 0 then
    AlertText.Visible:=False;
  FUpdating:=False;
end;

procedure THomeForm.HUDCtl(data: string);
begin
  if FUpdating then
    Exit;
  if data = 'ON' then
  begin
    PiDisplayOn;
    FScreenOn:=True;
  end
  else if data = 'OFF' then
  begin
    PiDisplayOff;
    FScreenOn:=False;
  end;
end;

procedure THomeForm.UpdateAlert(message: string);
begin
  AlertText.Caption:=message;
  AlertText.Visible:=True;
  FAlertCount:=5;
end;

end.

