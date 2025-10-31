unit logoutput;

{$mode objfpc}{$H+}

interface

uses
  Platform,
  SysUtils;

procedure Log(str : string);

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);

implementation

procedure Log(str : string);
begin
  LoggingOutput(str);
end;

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);
begin
  //if (atag <> 'USB') and (atag <> 'Device') then
  if (atag = 'WIFIdevice') or (atag = 'MMC') or (atag = 'Network') then
    LoggingOutput('('+atag+')' + ' ' + acontent);
end;

initialization

end.

