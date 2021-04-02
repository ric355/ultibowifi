unit overrides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, platform, logoutput, globalconfig;

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);

implementation

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);
begin
//  if (atag <> 'USB') and (atag <> 'Device') then
    if (atag = 'WIFIdevice') then
      log('('+atag+')' + ' ' + acontent);
end;


initialization
  MMC_AUTO_DEVICE_CREATE := False;
  MMC_AUTOSTART := False;
  BCM2710_REGISTER_SDHCI := False;


end.

