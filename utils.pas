unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  HudSysExitCode: integer = 0;

function GetHostName: string;

implementation

function GetHostName: string;
var
  f: Text;
begin
  Assign(f, '/etc/hostname');
  Reset(f);
  ReadLn(f, Result);
  Close(f);
end;

end.

