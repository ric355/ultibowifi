unit overrides;

{$mode objfpc}{$H+}

interface

uses
  GlobalConfig,
  GlobalConst;

implementation


initialization

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

