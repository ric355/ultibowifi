unit overrides;

{$mode objfpc}{$H+}

interface

uses
  GlobalConfig,
  GlobalConst;

implementation


initialization

  MMC_AUTOSTART := False;

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

