unit overrides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, platform, logoutput, globalconfig;

implementation

procedure loggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);
begin
  log(atag + ' ' + acontent);
end;


initialization
  MMC_AUTOSTART := False;
  BCM2710_REGISTER_SDHCI := False;
  LoggingOutputExHandler:= @loggingoutputhandler;


end.

