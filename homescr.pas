unit homescr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, homecu, fpjson, piservice, dateutils, pisound, kevtel, utils,
  logviewer;

type

  { THomeForm }

  THomeForm = class(TForm)
    BedroomBtn: TButton;
    AllOffBtn: TButton;
    AlertText: TLabel;
    AlarmIcon: TImage;
    BrightBtn: TButton;
    LogBtn: TButton;
    SpecialBtn: TButton;
    IPSetBtn: TButton;
    CalendarDate: TLabel;
    SleepMode: TCheckBox;
    IsPaused: TCheckBox;
    Wakeme: TCheckBox;
    SetAlarm: TButton;
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
    procedure BrightBtnClick(Sender: TObject);
    procedure BrightnessChange(Sender: TObject);
    procedure ClockClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IPSetBtnClick(Sender: TObject);
    procedure IsPausedClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure SetAlarmClick(Sender: TObject);
    procedure SleepModeClick(Sender: TObject);
    procedure SpecialBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure WakemeClick(Sender: TObject);
  private
    cu: THomeCU;
    FScreenOn: Boolean;
    FAlarm: TDateTime;
    FUpdating: Boolean;
    procedure UpdateClock;
    procedure UpdateWeather;
    function FormatDigit(d: Word): String;
    procedure PlayAlarm;
    procedure UpdateUI;
    procedure HideUI;
    procedure HUDCtl(data: string);
  public

  end;

var
  HomeForm: THomeForm;

implementation

{$R *.lfm}

const
  ALARM_SOUND = 'sounds/alarm.ogg';

{ THomeForm }

procedure THomeForm.TimerTimer(Sender: TObject);
begin
  if not Assigned(LogForm.OnHUDCtl) then
    LogForm.OnHUDCtl:=@HUDCtl;
  UpdateClock;
  UpdateWeather;
  cu.UpdateConfig;
  UpdateUI;
end;

procedure THomeForm.WakemeClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    cu.Wakeme:=Wakeme.Checked;
    LogForm.AddLog('Wake Me clicked.');
  end;
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
  if GetHostName = 'livingroom' then
    HideUI;
  Brightness.Position:=PiGetBrightness;
  FScreenOn:=True;
  FAlarm:=0;
  SoundSystem.Start;
  SoundSystem.Say('Interface started.');
end;

procedure THomeForm.FormDestroy(Sender: TObject);
begin
  SoundSystem.Stop;
  Sleep(1000); { Wait for thread to term. }
  cu.Free;
end;

procedure THomeForm.IPSetBtnClick(Sender: TObject);
begin
  cu.Firewall('ipset');
  LogForm.AddLog('IPSet Clicked.');
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

procedure THomeForm.SetAlarmClick(Sender: TObject);
var
  msg: string;
begin
  if SetAlarm.Caption = 'Alarm Set' then
  begin
    AlertText.Visible:=False;
    FAlarm:=0;
    SetAlarm.Caption:='Set Alarm';
    AlarmIcon.Visible:=False;
    LogForm.AddLog('Alarm unset.');
    Exit;
  end;
  {$IFDEF DEBUG}
  {FAlarm:=IncMinute(Now, 1);}
  FAlarm:=EncodeTime(13,35,0,0);
  {$ELSE}
  {FAlarm:=IncMinute(Now, 30);
  FAlarm:=IncHour(FAlarm, 8);}
  FAlarm:=EncodeTime(7,0,0,0);
  {$ENDIF}
  msg:='Alarm has been set to '+IntToStr(HourOf(FAlarm))+':';
  msg:=msg+IntToStr(MinuteOf(FAlarm));
  LogForm.AddLog(msg);
  SoundSystem.Say(msg);
  AlertText.Caption:='Alarm set to '+IntToStr(HourOf(FAlarm))+':'+IntToStr(MinuteOf(FAlarm));
  AlertText.Visible:=True;
  SetAlarm.Caption:='Alarm Set';
  AlarmIcon.Visible:=True;
end;

procedure THomeForm.SleepModeClick(Sender: TObject);
var
  ivr: TIVR;
begin
  if not FUpdating then
  try
    ivr:=TIVR.Create;
    ivr.Sleeping:=SleepMode.Checked;
    LogForm.AddLog('Sleep mode clicked.');
  finally
    ivr.Free;
  end;
end;

procedure THomeForm.SpecialBtnClick(Sender: TObject);
begin
  if SpecialBtn.Caption = 'Special' then
    SpecialBtn.Caption:='Confirm Spcl'
  else
  begin
    cu.Firewall('flush');
    LogForm.AddLog('Special clicked.');
  end;
end;

procedure THomeForm.ExitButtonClick(Sender: TObject);
begin
  HudSysExitCode:=2;
  Close;
end;

procedure THomeForm.BedroomBtnClick(Sender: TObject);
begin
  cu.Bedroom;
  LogForm.AddLog('Bedroom clicked.');
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
  if SoundSystem.SoundPlaying then
  begin
    SoundSystem.StopSound;
    FAlarm:=IncMinute(Now, 15);
    AlertText.Caption:='Alarm snoozed for 15 minutes.';
    AlertText.Visible:=True;
    SetAlarm.Caption:='Alarm Set';
    LogForm.AddLog('Alarm snoozed for 15 minutes.');
    Exit;
  end;
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
  cu.AllOff;
  LogForm.AddLog('All Off clicked.');
end;

procedure THomeForm.UpdateClock;
var
  tm: TSystemTime;
begin
  GetLocalTime(tm);
  Clock.Caption:=FormatDigit(tm.Hour)+':'+FormatDigit(tm.Minute);
  CalendarDate.Caption:=FormatDateTime('dddd mmmm d, yyyy', Today);
  if FAlarm > 0 then
    if (tm.Hour = HourOf(FAlarm)) and (tm.Minute = MinuteOf(FAlarm)) then
      PlayAlarm;
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

procedure THomeForm.PlayAlarm;
begin
  AlertText.Visible:=False;
  FAlarm:=0;
  SetAlarm.Caption:='Set Alarm';
  if not cu.Sleeping then
    Exit;
  SoundSystem.Play(ALARM_SOUND);
  LogForm.AddLog('Alarm triggered.');
  {$IFDEF CPUARM}
  PiDisplayOn;
  FScreenOn:=True;
  {$ENDIF}
end;

procedure THomeForm.UpdateUI;
begin
  FUpdating:=True;
  SleepMode.Checked:=cu.Sleeping;
  IsPaused.Checked:=cu.Pause;
  Wakeme.Checked:=cu.Wakeme;
  if FAlarm > 0 then
    AlarmIcon.Visible:=True
  else
    AlarmIcon.Visible:=False;
  AlertText.Visible:=False;
  FUpdating:=False;
end;

procedure THomeForm.HideUI;
begin
  SleepMode.Visible:=False;
  IsPaused.Visible:=False;
  Wakeme.Visible:=False;
  SpecialBtn.Visible:=False;
  IPSetBtn.Visible:=False;
  SetAlarm.Visible:=False;
  AllOffBtn.Visible:=False;
  BedroomBtn.Visible:=False;
end;

procedure THomeForm.HUDCtl(data: string);
begin
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

end.

