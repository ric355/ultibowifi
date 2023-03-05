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
  syscalls,
  ip,
  transport,
  Iphlpapi
  ;

//{$DEFINE SERIAL_LOGGING}

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
  StdoutToLogging : boolean = false;
  LoggingLine : string = '';


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


function GetMACAddress(const AdapterName: String): String;
// Get the MAC address of an adapter using the IP Helper API
var
  Size:LongWord;
  Count: LongWord;
  Name: String;
  IfTable: PMIB_IFTABLE;
  IfRow: PMIB_IFROW;
  HardwareAddress: THardwareAddress;
begin
  Result := '';

  // Get number of network interfaces
  GetNumberOfInterfaces(Count);

  if Count > 0 then
  begin
    Size := SizeOf(MIB_IFTABLE) + (Count * SizeOf(MIB_IFROW));
    IfTable := GetMem(Size);
    if IfTable <> nil then
    begin
      // Get the network interface table
      if GetIfTable(IfTable, Size, False) = ERROR_SUCCESS then
      begin
        // Get first row
        IfRow := @IfTable^.table[0];

        Count := 0;
        while Count < IfTable^.dwNumEntries do
        begin
          Name := IfRow^.wszName;

          // Check name
          if Uppercase(Name) = Uppercase(AdapterName) then
          begin
            // Check address size
            if IfRow^.dwPhysAddrLen = SizeOf(THardwareAddress) then
            begin
              System.Move(IfRow^.bPhysAddr[0], HardwareAddress[0], HARDWARE_ADDRESS_SIZE);
              Result := HardwareAddressToString(HardwareAddress);
              Exit;
            end;
          end;

          // Get next row
          Inc(Count);
          Inc(IfRow);
        end;
      end;

      FreeMem(IfTable);
    end;
  end;
end;

function GetIPAddress(const AdapterName: String): String;
// Get the IP address of an adapter using the IP Helper API
var
  Size: LongWord;
  Count: Integer;
  Address: String;
  MACAddress: String;
  IpNetTable: PMIB_IPNETTABLE;
  IpNetRow: PMIB_IPNETROW;
  IPAddress: TInAddr;
  HardwareAddress: THardwareAddress;
begin
  Result := '';

  // Get the MAC address
  MACAddress := GetMacAddress(AdapterName);
  if Length(MACAddress) = 0 then
    Exit;

  // Get the ip net table (ARP table)
  // First get the size
  Size := 0;
  IpNetTable := nil;
  if (GetIpNetTable(nil, Size, False) = ERROR_INSUFFICIENT_BUFFER) and (Size > 0) then // First call with zero size
  begin
    IpNetTable := GetMem(Size);
  end;
  if IpNetTable <> nil then
  begin
    // Now get the table
    if GetIpNetTable(IpNetTable, Size, False) = ERROR_SUCCESS then
    begin
      // Get first row
      IpNetRow := @IpNetTable^.table[0];

      Count := 0;
      while Count < IpNetTable^.dwNumEntries do
      begin
        // Check address size
        if IpNetRow^.dwPhysAddrLen = SizeOf(THardwareAddress) then
        begin
          // Get the address
          System.Move(IpNetRow^.bPhysAddr[0], HardwareAddress[0], HARDWARE_ADDRESS_SIZE);
          Address := HardwareAddressToString(HardwareAddress);

          // Check the address
          if Uppercase(Address) = Uppercase(MACAddress) then
          begin
            IPAddress.S_addr := IpNetRow^.dwAddr;
            Result := InAddrToString(IPAddress);
          end;
        end;

        // Get next row
        Inc(Count);
        Inc(IpNetRow);
      end;
    end;

    FreeMem(IpNetTable);
  end;
end;

procedure WaitForIP;
var
  WiredAdapter : TWiredAdapter;
begin
  // locate network WiredAdapter (the wireless interface is still supported by a wiredadapter object at present)
  WiredAdapter := TWiredAdapter(AdapterManager.GetAdapterByDevice(PNetworkDevice(CYW43455Network), false, 0));

  IPAddress := GetIPAddress(WiredAdapter.Name);
  while (IPAddress = '') or (IPAddress = '0.0.0.0') do
  begin
    sleep(200);
    IPAddress := GetIPAddress(WiredAdapter.Name);
  end;

  ConsoleWindowWriteln(TopWindow, 'IP Address=' + IPAddress);
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
  HeapStatus : TFPCHeapStatus;
  UseSupplicant : boolean;

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
  StdoutToLogging := true;
  if (SysUtils.GetEnvironmentVariable('FILESYS_LOGGING_FILE') <> '') then
  begin
    StdOutToLogging := true;
    LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
  end
  else
  begin
    CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_RIGHT;
    LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
    LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
  end;
  {$ENDIF}

  // intercept stdio for logging purposes.
  // supplicant writes to stdio for info and debug etc.
  ConsoleWindowRedirectOutput(TopWindow);

  // supplicant can be turned off by setting SUPPLICANT=FALSE in cmdline.txt
  UseSupplicant := not (uppercase(SysUtils.GetEnvironmentVariable('SUPPLICANT')) = 'FALSE');

  if (UseSupplicant) then
    Writeln('Using software supplicant for connection method.')
  else
    Writeln('Using Broadcom Firmware supplicant for connection method.');


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
    // wifi device has been fully initialized.

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

    if (UseSupplicant) then
    begin
      ConsoleWindowWriteln(TopWindow, 'Attempting to join WIFI network using configuration in c:\wpa_supplicant.conf');

      WirelessJoinNetWork(WIFIJoinBlocking);
      ConsoleWindowWriteln(TopWindow, 'Waiting for an IP address...');

      WaitForIP;
      ConsoleWindowWriteln(TopWindow, 'The IP address is ' + IPAddress);
    end
    else
    begin
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

      if (Key = '') then
        ConsoleWindowWriteln(TopWindow, 'Warning: Key not specified - expecting the network to be unencrypted.');

      if (SSID = '') or (Country='') then
         ConsoleWindowWriteln(TopWindow, 'Cant join a network without SSID, Key, and Country Code.')
      else
      begin
        ConsoleWindowWriteln(topwindow, 'Attempting to join WIFI network ' + SSID + ' (Country='+Country+')');

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

        IPAddress := '0.0.0.0';
        status := FirmwareWirelessJoinNetwork(SSID, Key, Country, WIFIJoinBlocking, WIFIReconnectAlways, BSSID, (BSSIDStr <> ''));

        if (status = WIFI_STATUS_SUCCESS) then
        begin
          ConsoleWindowWriteln(topwindow, 'Network joined, waiting for an IP address...');
          WaitForIP;
          ConsoleWindowWriteln(TopWindow, 'The IP address is ' + IPAddress);
        end
        else
        begin
          ConsoleWindowWriteLn(topwindow,'Failed to join the WIFI network. Status='+inttostr(status));
          ConsoleWindowWriteln(topwindow, 'Waiting for auto retry...');
          WaitForIP;
          ConsoleWindowWriteln(TopWindow, 'The IP address is ' + IPAddress);
        end;
      end;

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

      Sleep(1000);
      HeapStatus := GetFPCHeapStatus;
      ConsoleWindowWriteEx(CPUWindow, 'Memory Used ' + inttostr(HeapStatus.CurrHeapUsed) + '    ', 1, 10, COLOR_BLACK, COLOR_WHITE);
      ConsoleWindowWriteEx(CPUWindow, 'Received Glom Bytes ' + inttostr(WIFIDeviceP^.ReceiveGlomPacketSize) + '    ', 1, 9, COLOR_BLACK, COLOR_WHITE);

    end;

  except
    on e : exception do
      ConsoleWindowWriteln(TopWindow, 'Exception: ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
  end;

end.

