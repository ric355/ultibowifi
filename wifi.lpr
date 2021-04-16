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
  Logging;

var
  res : longword;
  SDHCI : PSDHCIHost;


function SDHCIHostCallback(aSDHCI:PSDHCIHost;Data:Pointer):LongWord;
begin
  // This function makes an assumption that there is only going to be 1 SDHCI host
  // present in the system. This is only temporary code anyway; we just need it
  // because all of the device creation is being done outside of the RTL until we
  // get to a point where it can be properly integrated.

  log('SDHCI Host Desc ' + asdhci^.Device.DeviceDescription
             + ' Name ' + aSDHCI^.Device.DeviceName
             + ' Clock ' + inttostr(aSDHCI^.Clock)
             + ' Dev id ' + inttohex(aSDHCI^.Device.DeviceId, 8) );
  SDHCI := aSDHCI;

  log('SDHCI is ' + inttohex(longword(SDHCI), 8));
  if (SDHCI^.Device.Signature <> DEVICE_SIGNATURE) then
    log('SDHCI Signature not valid!')
  else
    log('Got a good SDHCI signature');
end;

procedure CreateWIFIDevice;
var
  WIFI:PWIFIDevice;
  Status:LongWord;
begin

  // Create and register a WIFI device

  try
  log('WIFIDeviceCreate');
  WIFI:=WIFIDeviceCreate;
  if WIFI = nil then
   begin
    log('Failed to create new WIFI device');
    Exit;
   end;

  log('DeviceRegister');

  Status := WIFIDeviceRegister(WIFI);
  if Status <> ERROR_SUCCESS then
   begin
    log('SDHCI Failed to register new WIFI device ' + inttostr(status));
    WIFIDeviceDestroy(WIFI);
    Exit;
   end;


  log('Update WIFI Device');

  WIFI^.Device.DeviceBus:=DEVICE_BUS_SD;
  WIFI^.Device.DeviceType:=WIFI_TYPE_SDIO;
  WIFI^.Device.DeviceFlags:=WIFI_FLAG_NONE;
  WIFI^.Device.DeviceData:= SDHCI;
  WIFI^.Device.DeviceDescription:='WIFI Device';

//  WIFI^.WIFIState:=MMC_STATE_EJECTED;
  WIFI^.DeviceInitialize:=nil; //SDHCI^.DeviceInitialize;
//  WIFI^.DeviceDeinitialize:=SDHCI^.DeviceDeinitialize;
//  WIFI^.DeviceGetCardDetect:=SDHCI^.DeviceGetCardDetect;
//  WIFI^.DeviceGetWriteProtect:=SDHCI^.DeviceGetWriteProtect;
//  WIFI^.DeviceSendCommand:=SDHCI^.DeviceSendCommand;
//  WIFI^.DeviceSetIOS:=SDHCI^.DeviceSetIOS;

  log('WIFIDeviceInitialize');

  {Initialize WIFI}
  Status:=WIFIDeviceInitialize(WIFI);
  if (Status <> WIFI_STATUS_SUCCESS)then
   begin
    log('WIFI Failed to initialize new WIFI device ' + inttostr(status));
    WIFIDeviceDestroy(WIFI);
    Exit;
   end;

  except
    on e : exception do
      log('exception + ' + e.message + ' address ' + inttohex(longword(exceptaddr), 8));
  end;
end;


{$ifdef ZERO}
procedure registersdhci;
var
  BCM2708SDHCIHost:PBCM2708SDHCIHost;
  Status:LongWord;

begin
  BCM2708SDHCIHost:=PBCM2708SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2708SDHCIHost)));
  if BCM2708SDHCIHost <> nil then
   begin
    {Update SDHCI}
    {Device}
    BCM2708SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2708SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
    BCM2708SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_AUTO_CMD12 or SDHCI_FLAG_AUTO_CMD23;
    BCM2708SDHCIHost.SDHCI.Device.DeviceData:=nil;
    BCM2708SDHCIHost.SDHCI.Device.DeviceDescription:=BCM2708_EMMC_DESCRIPTION;
    {SDHCI}
    BCM2708SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
    BCM2708SDHCIHost.SDHCI.HostStart:=BCM2708SDHCIHostStart;
    BCM2708SDHCIHost.SDHCI.HostStop:=BCM2708SDHCIHostStop;
    BCM2708SDHCIHost.SDHCI.HostReadByte:=BCM2708SDHCIHostReadByte;
    BCM2708SDHCIHost.SDHCI.HostReadWord:=BCM2708SDHCIHostReadWord;
    BCM2708SDHCIHost.SDHCI.HostReadLong:=BCM2708SDHCIHostReadLong;
    BCM2708SDHCIHost.SDHCI.HostWriteByte:=BCM2708SDHCIHostWriteByte;
    BCM2708SDHCIHost.SDHCI.HostWriteWord:=BCM2708SDHCIHostWriteWord;
    BCM2708SDHCIHost.SDHCI.HostWriteLong:=BCM2708SDHCIHostWriteLong;
    BCM2708SDHCIHost.SDHCI.HostSetClockDivider:=nil;
    BCM2708SDHCIHost.SDHCI.HostSetControlRegister:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceInitialize:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceDeinitialize:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceGetCardDetect:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceSendCommand:=nil;
    BCM2708SDHCIHost.SDHCI.DeviceSetIOS:=nil;
    {Driver}
    BCM2708SDHCIHost.SDHCI.Address:=Pointer(BCM2835_SDHCI_REGS_BASE);

    {Register SDHCI}
    Status:=SDHCIHostRegister(@BCM2708SDHCIHost.SDHCI);
    if Status <> ERROR_SUCCESS then
     begin
      WIFILogError(nil,'BCM2708: Failed to register new SDHCI host: ' + ErrorToString(Status));
     end;
   end
  else
   begin
    WIFILogError(nil,'BCM2708: Failed to create new SDHCI host');
   end;

end;

{$endif}

{$ifdef RPI3}
procedure RegisterSDHCI;
var
  BCM2710SDHCIHost:PBCM2710SDHCIHost;
  Status:LongWord;

begin
  BCM2710SDHCIHost:=PBCM2710SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2710SDHCIHost)));
  if BCM2710SDHCIHost <> nil then
   begin
    {Update SDHCI}
    {Device}
    BCM2710SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2710SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
    BCM2710SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_AUTO_CMD12 or SDHCI_FLAG_AUTO_CMD23;
    BCM2710SDHCIHost.SDHCI.Device.DeviceData:=nil;
    BCM2710SDHCIHost.SDHCI.Device.DeviceDescription:=BCM2710_EMMC_DESCRIPTION;
    {SDHCI}
    BCM2710SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
    BCM2710SDHCIHost.SDHCI.HostStart:=BCM2710SDHCIHostStart;
    BCM2710SDHCIHost.SDHCI.HostStop:=BCM2710SDHCIHostStop;
    BCM2710SDHCIHost.SDHCI.HostReadByte:=BCM2710SDHCIHostReadByte;
    BCM2710SDHCIHost.SDHCI.HostReadWord:=BCM2710SDHCIHostReadWord;
    BCM2710SDHCIHost.SDHCI.HostReadLong:=BCM2710SDHCIHostReadLong;
    BCM2710SDHCIHost.SDHCI.HostWriteByte:=BCM2710SDHCIHostWriteByte;
    BCM2710SDHCIHost.SDHCI.HostWriteWord:=BCM2710SDHCIHostWriteWord;
    BCM2710SDHCIHost.SDHCI.HostWriteLong:=BCM2710SDHCIHostWriteLong;
    BCM2710SDHCIHost.SDHCI.HostSetClockDivider:=nil;
    BCM2710SDHCIHost.SDHCI.HostSetControlRegister:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceInitialize:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceDeinitialize:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceGetCardDetect:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceSendCommand:=nil;
    BCM2710SDHCIHost.SDHCI.DeviceSetIOS:=nil;
    {Driver}
    BCM2710SDHCIHost.SDHCI.Address:=Pointer(BCM2837_SDHCI_REGS_BASE);

    {Register SDHCI}
    Status:=SDHCIHostRegister(@BCM2710SDHCIHost.SDHCI);
    if Status <> ERROR_SUCCESS then
     begin
      WIFILogError(nil,'BCM2710: Failed to register new SDHCI host: ' + ErrorToString(Status));
     end;
   end
  else
   begin
    WIFILogError(nil,'BCM2710: Failed to create new SDHCI host');
   end;

end;
{$endif}

begin
  RegisterSDHCI;

  CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_FULLSCREEN;
  LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
  LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
  LoggingOutput('Application Started');

  LoggingOutputExHandler:= @myloggingoutputhandler;


  WIFI_LOG_ENABLED := true;

  SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
  log('Setting update path to ' + SHELL_UPDATE_LOCAL_PATH);


  // We've gotta wait for the file system to be alive because that's where the firmware is.
  // Note at the moment the firmware is hard coded to a pi3b.
  // Also note that because the WIFI uses the Arasan host, the only way you'll get a drive C
  // is if you use USB boot. So that's a pre-requisite at the moment until we make the
  // SD card work off the other SDHost controller.

  log('Waiting for file system...');
  while not directoryexists('c:\') do
  begin
  end;

  try
    log('Enumerate SDHCI hosts');
    SDHCIHostEnumerate(@SDHCIHostCallback, nil);
    log('Finished enumerating SDHCI hosts');

    SDHCIHostStart(SDHCI);

    log('Calling device create');

    CreateWIFIDevice;

    log('devicecreate completed');

    while (true) do
    begin
    end;

  except
    on e : exception do
      log('Exception: ' + e.message);
  end;

end.

