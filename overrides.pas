unit overrides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, platform, logoutput, globalconfig, globalconst;

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);

implementation

procedure myloggingoutputhandler(AFacility,ASeverity:LongWord;const ATag,AContent:String);
begin
//  if (atag <> 'USB') and (atag <> 'Device') then
    if (atag = 'WIFIdevice') or (atag = 'MMC') or (atag = 'Network') then
      log('('+atag+')' + ' ' + acontent);
end;


initialization
  
  //MMC_AUTO_DEVICE_CREATE := False; // No longer required, overwride the SDHCI initializatoin instead
  
  MMC_AUTOSTART := False;
  
  CONSOLE_REGISTER_LOGGING := True;
  CONSOLE_LOGGING_DEFAULT := True;
  CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_RIGHT;

  // at the moment we don't want auto init because the USB device where the firmware
  // is loaded from is not available until after initialisation.

  //WIFI_AUTO_INIT := False; // Moved to WifiDevice unit during development

  {$IFDEF RPI}
  BCM2708_REGISTER_SDIO:=True;
  BCM2708_REGISTER_SDHOST:=True;
  {$ENDIF}

  {$IFDEF RPI3}
  BCM2710_REGISTER_SDIO:=True;
  BCM2710_REGISTER_SDHOST:=True;
  {$ENDIF}

  {$IFDEF RPI4}
  BCM2711_REGISTER_SDIO:=True;
  {$ENDIF}
  
end.

