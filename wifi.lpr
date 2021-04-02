program wifi;

{$mode delphi}{$H+}

(*
Prerequisites for running this app:
1. You must boot from a USB drive
2. Pi3b+  (unless you change the hard coded filename to the right one for your device)
3. Must be a device with onboard wifi support (*not* a USB wifi device)
4. USB drive must contain c:\firmware with the correct firmware file in it

This is a very rough and ready Ultibo application which creates a wifi device and attempts
to get the Arasan controller to talk to the Cypress WFI chip.
There are a lot of issues with it at the moment but it goes through the motions of
creating the necessary devices, initialising the Cypress WIFI chip (that stuff
does work) and attempting to upload the firmware to it.
It may work on a pi zero to some extent, but the firmware it loads is a specific
pi3b+ version (id 0x4345 rev 6) at the moment so that would need to be changed
for other devices (not that it matters at this point because the firmware doesn't
actually load properly yet!). See the hard coded filename in WIFIDeviceDownloadFirmware.

In the overrides file, there are three overrides:

MMC_AUTO_DEVICE_CREATE := False;
MMC_AUTOSTART := False;
BCM2710_REGISTER_SDHCI := False;

The first of these is a custom setting and is something that will need to be resolved
in due course. It prevents the mmc.pas unit from creating the MMC device automatically
as this prevents the WIFI device from working. This is needed because this code
uses the SDHCI device code in mmc.pas, and that code automatically creates the MMC
device by default when SDHCIHostStart is called.
That means the mmc.pas file in this repo need to be compiled into the RTL instead
of the standard one.

Within this file you will find code that creates the wifi device, and code that
registers and starts an SDHCI device. The SDHCI device is created here because
it gives us better control over Arasan controller; it requires various GPIO's to be
set differently to connect it to the Cypress chip instead of to the MMC device.
This is another thing that needs a proper solution in due course.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy (and not well understood if I'm honest!) initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

At present the system can identify and select the wifi chip, read the chip id,
scan the cores to establish key addresses, scan the ram for similar purpose,
upload firmware to the device, and re-download the firmware for verification
purposes.

Unfortunately there are some rather nasty problems to address which appear to
be around the interrupt handler for cmd53. It will frequently stop working at
random points once cmd53 starts being used. So far it has resisted all attempts
to find the root cause of the problem as any attempt at logging tends to make
the crashing worse. This is possibly down to non-threadsafe logging functions
and their incompatibility with the interrupt handler, but I have tried
to fix this issue using what I think is the the correct splinlock calls, to no avail,
so not sure what the exact problem is.

I am fairly sure there is some sort of memory scribble going on as occasionally
the system will generate an unhandled exception at 0x00000000 from the platform
unit, although this is pretty rare. It usually simply stops, to the extent that
wired network access is lost. Therefore it is likely that either the interrupt
handler has crashed or has deadlocked in some way. Threads not using
interrupts do keep on running when it fails - I had a thread flashing the onboard
LED and that does not die.

Note that this application changes the update path for the kernel. This is
specific to my own development platform where I run a dual boot system sometimes
(I just didn't want to restructure the existing files on the drive so used the same
location).
You will need to change this if you want to do remote kernel updates to the root folder.
Just delete this line;
SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
From the main program below.
*)

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
  Ultibo;

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
    Status:=SDHCINewHostRegister(@BCM2708SDHCIHost.SDHCI);
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

  ConsoleFramebufferDeviceAdd(FramebufferDeviceGetDefault);
  WindowHandle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

  LoggingOutputExHandler:= @myloggingoutputhandler;


  MMC_DEFAULT_LOG_LEVEL := MMC_LOG_LEVEL_DEBUG;
  mmc_log_enabled := true;
  WIFI_LOG_ENABLED := true;

  ConsoleWindowWriteLn(windowhandle, 'Application started.');

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

