program wifi;

{$mode delphi}{$H+}

uses
  overrides,
  {$IFDEF RPI}
  RaspberryPi,
  BCM2835,
  BCM2708,
  {$ENDIF}
  {$IFDEF RPI3}
  RaspberryPi3,
  BCM2837,
  BCM2710,
  {$ENDIF}
  {$IFDEF RPI4}
  RaspberryPi4,
  BCM2838,
  BCM2711,
  {$ENDIF}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  StrUtils,
  SysUtils,
  Classes,
  ShellFilesystem,
  ShellUpdate,
  RemoteShell,
  logoutput,
  console,
  framebuffer,
  gpio,
  mmc,
  devices,
  wifidevice,
  Ultibo,
//  vishell,
  Logging,
  Network,
  Winsock2,
  font,
  HTTP,
  WebStatus,
  Serial,
  ip,
  transport;

//{$DEFINE SERIAL_LOGGING}

const
   // copied from font as it's in the implementation section there.
   FONT_LATIN1_8X16_DATA:TFontData8x16 = (
    Data:(($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $18, $3C, $3C, $3C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
          ($00, $66, $66, $66, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $6C, $FE, $6C, $6C, $6C, $FE, $6C, $6C, $00, $00, $00, $00),
          ($00, $10, $10, $7C, $D6, $D0, $D0, $7C, $16, $16, $D6, $7C, $10, $10, $00, $00),
          ($00, $00, $00, $00, $C2, $C6, $0C, $18, $30, $60, $C6, $86, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $6C, $38, $76, $DC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $0C, $18, $30, $30, $30, $30, $30, $30, $18, $0C, $00, $00, $00, $00),
          ($00, $00, $30, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $30, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $3C, $FF, $3C, $66, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $30, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $06, $0C, $18, $30, $60, $C0, $00, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $CE, $CE, $D6, $D6, $E6, $E6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $18, $38, $78, $18, $18, $18, $18, $18, $18, $7E, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $06, $0C, $18, $30, $60, $C0, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $06, $06, $3C, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $0C, $1E, $00, $00, $00, $00),
          ($00, $00, $FE, $C0, $C0, $C0, $FC, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $38, $60, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $FE, $C6, $06, $06, $0C, $18, $30, $30, $30, $30, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $7E, $06, $06, $06, $0C, $78, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $30, $00, $00, $00, $00),
          ($00, $00, $00, $06, $0C, $18, $30, $60, $30, $18, $0C, $06, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $FE, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $60, $30, $18, $0C, $06, $0C, $18, $30, $60, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $0C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $DE, $DE, $DE, $DC, $C0, $7C, $00, $00, $00, $00),
          ($00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00),
          ($00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
          ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00),
          ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00),
          ($00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00),
          ($00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $DE, $7C, $0C, $0E, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $64, $38, $0C, $06, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C0, $60, $30, $18, $0C, $06, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $00, $00, $00, $00),
          ($10, $38, $6C, $C6, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00),
          ($00, $30, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $64, $60, $F0, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00),
          ($00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00),
          ($00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00),
          ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $1E, $00),
          ($00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
          ($00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $0E, $18, $18, $18, $70, $18, $18, $18, $18, $0E, $00, $00, $00, $00),
          ($00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00),
          ($00, $00, $70, $18, $18, $18, $0E, $18, $18, $18, $18, $70, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82, $FE, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $18, $18, $18, $3C, $3C, $3C, $18, $00, $00),
          ($00, $00, $00, $00, $10, $7C, $D6, $D0, $D0, $D0, $D6, $7C, $10, $00, $00, $00),
          ($00, $00, $38, $6C, $60, $60, $F0, $60, $60, $66, $F6, $6C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $C6, $7C, $6C, $6C, $7C, $C6, $00, $00, $00, $00, $00, $00),
          ($00, $00, $66, $66, $3C, $18, $7E, $18, $7E, $18, $18, $18, $00, $00, $00, $00),
          ($00, $00, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $00, $00, $00),
          ($00, $7C, $C6, $60, $38, $6C, $C6, $C6, $6C, $38, $0C, $C6, $7C, $00, $00, $00),
          ($00, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $42, $99, $A5, $A1, $A5, $99, $42, $3C, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $6C, $6C, $3E, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $36, $6C, $D8, $6C, $36, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $FE, $06, $06, $06, $06, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $42, $B9, $A5, $B9, $A5, $A5, $42, $3C, $00, $00, $00, $00, $00),
          ($FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $38, $6C, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $7E, $00, $00, $00, $00),
          ($38, $6C, $18, $30, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($38, $6C, $18, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $F6, $C0, $C0, $C0, $00),
          ($00, $00, $7F, $D6, $D6, $76, $36, $36, $36, $36, $36, $36, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $6C, $38, $00),
          ($30, $70, $30, $30, $78, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $6C, $38, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $D8, $6C, $36, $6C, $D8, $00, $00, $00, $00, $00, $00),
          ($60, $E0, $60, $60, $F6, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
          ($60, $E0, $60, $60, $F6, $0C, $18, $30, $6E, $DB, $06, $0C, $1F, $00, $00, $00),
          ($70, $D8, $30, $D8, $76, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
          ($00, $00, $00, $00, $30, $30, $00, $30, $30, $30, $60, $C6, $C6, $7C, $00, $00),
          ($60, $30, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($0C, $18, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($76, $DC, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($38, $6C, $38, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $3E, $78, $D8, $D8, $FC, $D8, $D8, $D8, $D8, $DE, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $0C, $66, $3C, $00),
          ($60, $30, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($0C, $18, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $FE, $66, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($00, $6C, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($60, $30, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $08, $00, $00, $00),
          ($06, $0C, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($18, $3C, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
          ($76, $DC, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($60, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $3C, $18, $3C, $66, $00, $00, $00, $00, $00, $00),
          ($00, $00, $7E, $C6, $CE, $CE, $DE, $F6, $E6, $E6, $C6, $FC, $00, $00, $00, $00),
          ($60, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($06, $0C, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $F0, $60, $7C, $66, $66, $66, $66, $7C, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $CC, $C6, $C6, $C6, $D6, $DC, $80, $00, $00, $00),
          ($00, $60, $30, $18, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $38, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7E, $DB, $1B, $7F, $D8, $DB, $7E, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $6C, $38, $00),
          ($00, $60, $30, $18, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $18, $3C, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $78, $30, $78, $0C, $7E, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $00, $7E, $00, $18, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7E, $CE, $DE, $FE, $F6, $E6, $FC, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $30, $78, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
          ($00, $00, $F0, $60, $60, $7C, $66, $66, $66, $66, $7C, $60, $60, $F0, $00, $00),
          ($00, $00, $00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00))
    );

var
  SSID : string;
  key : string;
  Country : string;
  TopWindow : THandle;
  Winsock2TCPClient : TWinsock2TCPClient;
  IPAddress : string;
  i : integer;
  HTTPListener : THTTPListener;
  ScanResultList : TStringList;
  Status : Longword;
  CYW43455Network: PCYW43455Network;
  BSSIDStr : string;

procedure WIFIScanCallback(ssid : string; ScanResultP : pwl_escan_result);
var
  ssidstr : string;
begin
  ssidstr := ssid + ' ' + inttohex(ScanResultP^.bss_info[1].BSSID.octet[0],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[1],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[2],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[3],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[4],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[5],2);

  if (ScanResultList <> nil) and (ScanResultList.Indexof(ssidstr) < 0) then
    ScanResultList.Add(ssidstr);
end;

procedure WaitForIP;
{$ifndef oldipdetect}
var
  WiredAdapter : twiredadapter;
  IPTransport : TIPTransport;
  IPTransportAdapter : TIPTransportAdapter;
{$endif}
begin
  {$ifndef oldipdetect}
  // locate network WiredAdapter
  WiredAdapter := TWiredAdapter(AdapterManager.GetAdapterByDevice(PNetworkDevice(CYW43455Network), false, 0));

  // locate IP transport
  IPTransport := TIPTransport(TransportManager.GetTransportByName(IP_TRANSPORT_NAME, false, 0));

  // locate IP transport WiredAdapter
  IPTransportAdapter := TIPTransportAdapter(IPTransport.GetAdapterByNext(nil,False,False,NETWORK_LOCK_READ));
  while IPTransportAdapter <> nil do
  begin
    if IPTransportAdapter.Adapter = tnetworkadapter(WiredAdapter) then
      break;

    IPTransportAdapter := TIPTransportAdapter(IPTransport.GetAdapterByNext(IPTransportAdapter,False,False,NETWORK_LOCK_READ));
  end;

  // wait for IP address to update
  IPAddress := '0.0.0.0';
  while IPAddress = '0.0.0.0' do
  begin
    sleep(200);
    try
      IPAddress := InAddrToString(InAddrToNetwork(IPTransportAdapter.Address));
    except
      // The InAddrToNetwork call here^^^ sometimes crashes. Maybe the RTL
      // is changing the objects after we've looked them up.
      IPAddress := '0.0.0.0';
    end;
  end;
 {$else}

  Winsock2TCPClient:=TWinsock2TCPClient.Create;

  while (true) do
  begin
    sleep(200);
    if (Winsock2TCPClient.LocalAddress <> IPAddress)
       and (length(Winsock2TCPClient.LocalAddress) > 0)
       and (Winsock2TCPClient.LocalAddress <> ' ') then
    begin
      IPAddress := Winsock2TCPClient.LocalAddress;
      break;
    end;
  end;

{$endif}

  ConsoleWindowWriteln(TopWindow, 'IP Address=' + IPAddress);
end;

procedure DumpIP;
var
  i, j, c : integer;
  s : string;
begin
  for i := 0 to 15 do
  begin
    s := '';
    for c := 1 to length(ipaddress) do
    begin
      for j := 7 downto 0 do
      begin
        if (FONT_LATIN1_8X16_DATA.data[ord(ipaddress[c]), i] and (1 shl j) = (1 shl j)) then
          s := s + '#'
        else
          s := s + ' ';
      end;
    end;
    consolewindowwriteln(TopWindow, s);
  end;
end;

procedure CPUUtilisation(window : TWindowHandle; core : integer);
var
  cpupercent : double;
begin
  cpupercent := CPUGetPercentage(core);
  ConsoleWindowWriteEx(window, 'CPU' + inttostr(core) + ' '  + floattostr(cpupercent) + '%      ', 1, 3+core, COLOR_BLACK, COLOR_WHITE);
end;

var
  BSSID : ether_addr;
  LedStatus : boolean;
  CPUWindow : TWindowHandle;
  WIFIDeviceP : PWIFIDevice;


begin
  ConsoleFramebufferDeviceAdd(FramebufferDeviceGetDefault);
  CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_RIGHT;
  TopWindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT,TRUE);
  CPUWindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMLEFT, FALSE);

  LOGGING_INCLUDE_TICKCOUNT := True;
  {$IFDEF SERIAL_LOGGING}
  SERIAL_REGISTER_LOGGING := True;
  SerialLoggingDeviceAdd(SerialDeviceGetDefault);
  LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
  {$ELSE}
  LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
  LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
  {$ENDIF}

  // Filter the logs so we only see the WiFi and MMC device events
  // (Primarily development use, otherwise you don't see network events etc)
  //LoggingOutputExHandler:= @myloggingoutputhandler; 

  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  WebStatusRegister(HTTPListener,'','',True);


  WIFI_LOG_ENABLED := true;

  // Because we disabled auto start of the MMC subsystem we need to start the SD card driver
  // now to provide access to the firmware files on the SD card.
  
  WIFIPreInit;

  // We've gotta wait for the file system to be alive because that's where the firmware is.
  // Because the WIFI uses the Arasan host, the only way you'll get a drive C
  // is if you use USB boot. So that's a pre-requisite at the moment until we make the
  // SD card work off the other SDHost controller.

  ConsoleWindowWriteln(TopWindow, 'Waiting for file system...');

  while not directoryexists('c:\') do
  begin
    Sleep(0);
  end;
  ConsoleWindowWriteln(TopWindow, 'File system ready. Initialize Wifi Device.');

  // check firmware folder is present on the card.
  if (not (DirectoryExists('c:\firmware'))) then
    ConsoleWindowWriteln(TopWindow, 'You must copy the WIFI firmware to a folder called c:\firmware.');

  try
    // WIFIInit has to be done from the main application because the initialisation
    // process needs access to the c: drive in order to load the firmware, regulatory file
    // and configuration file.
    // There is the option of adding the files as binary blobs to be compiled into
    // the kernel, but that would need to be an option I think really (easily done
    // by choosing to add a specific unit to the uses clause)
    // We'll need to work out what the best solution is later.
    
    WIFIInit;

    // warning, after wifiinit is called, the deviceopen() stuff will happen on
    // a different thread, so the code below will execute regardless of whether
    // the device is open or not. Consequently we are going to spin until the
    // wifi device has been fully initialized. This is a bit of a dirty hack
    // but hopefully we can change it to a proper 'link is up' check once the
    // whole network device integration stuff is complete.
    // Certainly can't stay the way it is.

    ConsoleWindowWriteln(TopWindow, 'Waiting for Wifi Device to be opened.');
    
    // spin until the wifi device is actually ready to do stuff.
    repeat
      CYW43455Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
      if CYW43455Network = nil then
        Sleep(100);
    until CYW43455Network <> nil;
    
    while CYW43455Network^.Network.NetworkState <> NETWORK_STATE_OPEN do
    begin
      Sleep(0);
    end;

    WIFIDeviceP := WIFIDeviceFind(0);

    if (SysUtils.GetEnvironmentVariable('WIFISCAN') = '1') then
    begin
      ConsoleWindowWriteln(TopWindow, 'Performing a WIFI network scan...');
      ScanResultList := TStringList.Create;

      WirelessScan(@WIFIScanCallback);

      for i := 0 to ScanResultList.Count-1 do
        ConsoleWindowWriteln(TopWindow, 'Found access point: ' + ScanResultList[i]);

      ScanResultList.Free;
    end
    else
      ConsoleWindowWriteln(TopWindow, 'Network scan not enabled in cmdline.txt (add the WIFISCAN=1 entry)');

    SSID := SysUtils.GetEnvironmentVariable('SSID');
    key := SysUtils.GetEnvironmentVariable('KEY');
    Country := SysUtils.GetEnvironmentVariable('COUNTRY');
    BSSIDStr := SysUtils.GetEnvironmentVariable('BSSID');

    ConsoleWindowWriteln(topwindow, 'Attempting to join WIFI network ' + SSID + ' (Country='+Country+')');

    if (Key = '') then
      ConsoleWindowWriteln(TopWindow, 'Warning: Key not specified - expecting the network to be unencrypted.');

    if (SSID = '') or (Country='') then
       ConsoleWindowWriteln(TopWindow, 'Cant join a network without SSID, Key, and Country Code.')
    else
    begin
      if (BSSIDStr <> '') then
      begin
        ConsoleWindowWriteln(TopWindow, 'Using BSSID configuration ' + BSSIDStr + ' from cmdline.txt');
        bssid.octet[0] := hex2dec(copy(BSSIDStr, 1, 2));
        bssid.octet[1] := hex2dec(copy(BSSIDStr, 4, 2));
        bssid.octet[2] := hex2dec(copy(BSSIDStr, 7, 2));
        bssid.octet[3] := hex2dec(copy(BSSIDStr, 10, 2));
        bssid.octet[4] := hex2dec(copy(BSSIDStr, 13, 2));
        bssid.octet[5] := hex2dec(copy(BSSIDStr, 16, 2));
      end
      else
        ConsoleWindowWriteln(TopWindow, 'Letting the Cypress firmware determine the best network interface from the SSID');

      status := WirelessJoinNetwork(SSID, Key, Country, WIFIJoinBlocking, WIFIReconnectAlways, BSSID, (BSSIDStr <> ''));
      IPAddress := '0.0.0.0';
      if (status = WIFI_STATUS_SUCCESS) then
      begin

        ConsoleWindowWriteln(topwindow, 'Network joined, waiting for an IP address...');

        WaitForIP;

        DumpIP;
      end
      else
      begin
        ConsoleWindowWriteLn(topwindow,'Failed to join the WIFI network. Status='+inttostr(status));
        ConsoleWindowWriteln(topwindow, 'Waiting for auto retry...');

        WaitForIP;

        DumpIP;
      end;

      ConsoleWindowWriteln(CPUWindow, 'CPU Utilisation');

      // Setup a slow blink of the activity LED to give an indcation that the Pi is still alive
      ActivityLEDEnable;

      LedStatus := true;
      while true do
      begin
        if (LedStatus) then
          ActivityLEDOn
        else
          ActivityLEDOff;

        LedStatus := not LedStatus;

        CPUUtilisation(CPUWindow, 0);
        {$ifndef RPI}
        CPUUtilisation(CPUWindow, 1);
        CPUUtilisation(CPUWindow, 2);
        CPUUtilisation(CPUWindow, 3);
        {$endif}

        {$ifndef notxglom}
        if (WIFIDeviceP <> nil) then
        begin
          ConsoleWindowWriteEx(CPUWindow, 'Received Glom Descriptors ' + inttostr(WIFIDeviceP^.ReceiveGlomPacketCount) + '    ', 1, 8, COLOR_BLACK, COLOR_WHITE);
          ConsoleWindowWriteEx(CPUWindow, 'Received Glom Bytes ' + inttostr(WIFIDeviceP^.ReceiveGlomPacketSize) + '    ', 1, 9, COLOR_BLACK, COLOR_WHITE);
        end;
        {$endif}

        Sleep(500);
      end;

    end;

  except
    on e : exception do
      ConsoleWindowWriteln(TopWindow, 'Exception: ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
  end;

end.

