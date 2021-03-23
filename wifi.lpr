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
//  ShellFilesystem,
//  ShellUpdate,
  RemoteShell,
  logoutput,
  console,
  framebuffer,
  gpio,
//  mmc,
  devices,
  wifidevice,
  Ultibo, sdhcihost;

const
  SD_FUNC_BUS = 0;
  SD_RD = 0;
  SD_WR = 1;



var
  res : longword;
//  MyMMCP : PMMCDevice;
  SDHCI : PSDHCIHost;

function sdhcihostfunc(aSDHCI:PSDHCIHost;Data:Pointer):LongWord;
begin
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

procedure createdevice;
var
  WIFI:PWIFIDevice;
  Status:LongWord;
//  Capabilities:LongWord;
begin
  {Create MMC}
  try
  log('wifidevicecreate');
  WIFI:=WIFIDeviceCreate;
  if WIFI = nil then
   begin
    log('Failed to create new WIFI device');
    Exit;
   end;

  log('deviceregister');
  {Register WIFI}
  Status := WIFIDeviceRegister(WIFI);
  if Status <> SDHCI_STATUS_SUCCESS then
   begin
    log('SDHCI Failed to register new WIFI device ' + inttostr(status));
    WIFIDeviceDestroy(WIFI);
    Exit;
   end;


  log('update wifi');

  {Update MMC}
  {Device}
  WIFI^.Device.DeviceBus:=DEVICE_BUS_SD;
  WIFI^.Device.DeviceType:=WIFI_TYPE_SDIO;
  WIFI^.Device.DeviceFlags:=WIFI_FLAG_NONE;
  WIFI^.Device.DeviceData:= SDHCI;
  WIFI^.Device.DeviceDescription:='WIFI Device';
  {MMC}

  log('init values');
//  WIFI^.WIFIState:=MMC_STATE_EJECTED;
  WIFI^.DeviceInitialize:=nil; //SDHCI^.DeviceInitialize;
//  WIFI^.DeviceDeinitialize:=SDHCI^.DeviceDeinitialize;
//  WIFI^.DeviceGetCardDetect:=SDHCI^.DeviceGetCardDetect;
//  WIFI^.DeviceGetWriteProtect:=SDHCI^.DeviceGetWriteProtect;
//  WIFI^.DeviceSendCommand:=SDHCI^.DeviceSendCommand;
//  WIFI^.DeviceSetIOS:=SDHCI^.DeviceSetIOS;

  log('deviceiniitialize');

  {Initialize WIFI}
  Status:=WIFIDeviceInitialize(WIFI);
  if (Status <> WIFI_STATUS_SUCCESS)then
   begin
    log('WIFI Failed to initialize new WIFI device ' + inttostr(status));
    WIFIDeviceDestroy(WIFI);
    Exit;
   end;

  log('check mmc type');

  {Check MMC Type}
  if WIFI^.Device.DeviceType = WIFI_TYPE_SDIO then
   begin
    log('device type is SDIO');
    {Destroy Storage}
//    StorageDeviceDestroy(MMC.Storage);
//    MMC.Storage:=nil;
   end;

  except
    on e : exception do
      log('exception + ' + e.message + ' address ' + inttohex(longword(exceptaddr), 8));
  end;
end;



procedure registersdhci;
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
    Status:=SDHCINewHostRegister(@BCM2710SDHCIHost.SDHCI);
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

var
  i : integer;

begin
  registersdhci;

  ConsoleFramebufferDeviceAdd(FramebufferDeviceGetDefault);
  WindowHandle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

//  LoggingOutputExHandler:= @loggingoutputhandler;

  WIFI_LOG_ENABLED := true;
  SDHCI_LOG_ENABLED := true;

  log('application started');

(*  SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
  log('Setting update path to ' + SHELL_UPDATE_LOCAL_PATH);

  log('Waiting for file system...');*)

  (*

  Create a device to represent the wifi sdio device. Need a proper name for it.
  We might be able to use MMCDevice as a basis for this but may have to define a new
  device structre instead. Not exactly sure what it needs yet.

  We'll use WIFI0 for the device id.

  SDHCIHostStart creates the devices. We can change that or maybe just try and do it here.


  Once the device is created, we can try initialising it and reading and writing from it

  Going to need to create a new device driver. Have tried to use the MMC one but there is
  too much storage related stuff in it so it needs to be changed.




  while not directoryexists('c:\') do
  begin
  end;*)

  sleep(1000);

  try
    (*
    NOTE: With this code inline rather than being a device driver implementation, we must
    wait for the c: drive to be present otherwise when we get here the SDHCI host won't
    have been initialised. Calling SDHCIHostEnumerate() does it but there must be an init
    call to be made instead I imagine. For now we can just wait until the c: drive is ready.
    We are enumerating the hosts anyway for the heck of it.
    *)


    log('Enumerate SDHCI hosts');
    SDHCIHostEnumerate(@sdhcihostfunc, nil);
    log('Finished enumerating SDHCI hosts');



//    MMCInit;

(*    log('Enumerating MMC devices');
    MMCDeviceEnumerate(@mmcdevice, nil);
    log('Finished enumerating MMC devices');*)

    log('Calling device create');

    createdevice;

    log('devicecreate completed');

(*    log('enumerating devices again');
    MMCDeviceEnumerate(@mmcdevice, nil);
    log('enumeration completed');*)

//    SDIODeviceReadWriteDirect(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord;


    while (true) do
    begin

    end;

  except
    on e : exception do
      log('Exception: ' + e.message);
  end;

end.

