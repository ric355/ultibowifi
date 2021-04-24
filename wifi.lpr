program wifi;

{$mode delphi}{$H+}

uses
  overrides,
  {$IFDEF ZERO}
  RaspberryPi,
  BCM2835,
  BCM2708,
  {$ENDIF}
  {$IFDEF RPI3}
  RaspberryPi3,
  BCM2837,
  BCM2710,
  {$ENDIF}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
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
  Winsock2;

var
  res : longword;
  SSID : string;
  key : string;
  Country : string;
  topwindow : THandle;
  Winsock2TCPClient : TWinsock2TCPClient;
  IPAddress : string;


begin
  ConsoleFramebufferDeviceAdd(FramebufferDeviceGetDefault);
  topwindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_TOP,TRUE);

  CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_BOTTOM;
  LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
  LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));

  LoggingOutputExHandler:= @myloggingoutputhandler;


  WIFI_LOG_ENABLED := true;

//  SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
//  log('Setting update path to ' + SHELL_UPDATE_LOCAL_PATH);


  // We've gotta wait for the file system to be alive because that's where the firmware is.
  // Note at the moment the firmware is hard coded to a pi3b.
  // Also note that because the WIFI uses the Arasan host, the only way you'll get a drive C
  // is if you use USB boot. So that's a pre-requisite at the moment until we make the
  // SD card work off the other SDHost controller.

  ConsoleWindowWriteln(topwindow, 'Waiting for file system...');
  while not directoryexists('c:\') do
  begin
  end;
  ConsoleWindowWriteln(topwindow, 'File system ready. Initialize Wifi Device.');

  try
    // WIFIInit has to be done from the main application because the initialisation
    // process needs access to the c: drive in order to load the firmware, regulatory file
    // and configuration file.
    // There is the option of adding the files as binary blobs to be compiled into
    // the kernel, but that would need to be an option I think really (easily done
    // by choosing to add a specific unit to the uses clause)
    // We'll need to work out what the best solution is later. For now the overrides.pas
    // file turns off auto WIFI init so we can call it from here. Note in order to
    // do that we had to add a new global const, so now that has to be rebuilt
    // into the RTL.

    WIFIInit;

    // warning, after wifiinit is called, the deviceopen() stuff will happen on
    // a different thread, so the code below will execute regardless of whether
    // the device is open or not. Consequently we are going to spin until the
    // wifi device has been fully initialized. This is a bit of a dirty hack
    // but hopefully we can change it to a proper 'link is up' check once the
    // whole network device integration stuff is complete.
    // Certainly can't stay the way it is.

    // Actually, maybe the join call should not be here at all. It should just
    // be part of the initialisation. The scan function does need to know if
    // the link is up though.

    // spin until the wifi device is actually ready to do stuff.

    while not (WIFIIsReady) do
    begin
    end;

    ConsoleWindowWriteln(topwindow, 'Performing a WIFI network scan...');

    WirelessScan;

    SSID := SysUtils.GetEnvironmentVariable('SSID');
    key := SysUtils.GetEnvironmentVariable('KEY');
    Country := SysUtils.GetEnvironmentVariable('COUNTRY');

    ConsoleWindowWriteln(topwindow, 'Attempting to join WIFI network ' + SSID + ' (Country='+Country+')');

    if (SSID = '') or (Key = '') or (Country='') then
       ConsoleWindowWriteln(topwindow, 'Cant join a network without SSID, Key, and Country Code.')
    else
      WirelessJoinNetwork(SSID, Key, Country);

    Winsock2TCPClient:=TWinsock2TCPClient.Create;
    IPAddress := '0.0.0.0';

    while (true) do
    begin
      sleep(1000);
      if (Winsock2TCPClient.LocalAddress <> IPAddress) then
      begin
        ConsoleWindowWriteLn(topwindow, 'IP address='+Winsock2TCPClient.LocalAddress);
        IPAddress := Winsock2TCPClient.LocalAddress;
      end;
    end;

  except
    on e : exception do
      ConsoleWindowWriteln(topwindow, 'Exception: ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
  end;

end.

