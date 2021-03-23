(*
we have some sort of data response but the command object is set to nil after the command executes
(on the sdhci). So when the data comes it is not processed as there is a specific check for a current
command. Need to alter the code so that the command we use is not a local variable and is instead
a global one. that will make that code valid if we don't nil out the sdhci.command value.
but it may still not work - but we do need to see what happens next.

Have testd this change but it did not work.
The system crashes and we can't see what the problem is either on telnet or the screen
going to have to reverse it and try something else.

*)


unit wifidevice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Devices,
  GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,DMA,sdhcihost,gpio;

const
  WIFI_DATA_READ		= 1;
  WIFI_DATA_WRITE		= 2;

  WIFI_BAK_BLK_BYTES = 64;   // backplane block size
  WIFI_RAD_BLK_BYTES = 512;  // radio block size

  BUS_BAK_BLKSIZE_REG = $110;   // register for backplane block size (2 bytes)
  BUS_RAD_BLKSIZE_REG = $210;   // register for radio block size (2 bytes)
  BAK_WIN_ADDR_REG = $1000a;    // register for backplane window address

  BAK_BASE_ADDR = $18000000;   // chipcommon base address

(*
  #define BUS_INTEN_REG           0x004   // SDIOD_CCCR_INTEN
  #define BUS_INTPEND_REG         0x005   // SDIOD_CCCR_INTPEND
  #define BUS_BI_CTRL_REG         0x007   // SDIOD_CCCR_BICTRL        Bus interface control
  #define BUS_SPEED_CTRL_REG      0x013   // SDIOD_CCCR_SPEED_CONTROL Bus speed control
  #define BUS_BRCM_CARDCAP        0x0f0   // SDIOD_CCCR_BRCM_CARDCAP
  #define BUS_BAK_BLKSIZE_REG     0x110   // SDIOD_CCCR_F1BLKSIZE_0   Backplane blocksize
  #define BUS_RAD_BLKSIZE_REG     0x210   // SDIOD_CCCR_F2BLKSIZE_0   WiFi radio blocksize*)


  // Backplane window
  SB_32BIT_WIN = $8000;


  {WIFI Bus Widths}
  WIFI_BUS_WIDTH_1	= 0;
  WIFI_BUS_WIDTH_4      = 2;

  WIFI_RSP_R1_APP_CMD			    = (1 shl 5);


  WIFI_NAME_PREFIX = 'WIFI';    {Name prefix for WIFI Devices}

  OPERATION_IO_RW_DIRECT = 6;

  {WIFI/SD Status Codes}
  WIFI_STATUS_SUCCESS                   = 0;  {Function successful}
  WIFI_STATUS_TIMEOUT                   = 1;  {The operation timed out}
  WIFI_STATUS_NO_MEDIA                  = 2;  {No media present in device}
  WIFI_STATUS_HARDWARE_ERROR            = 3;  {Hardware error of some form occurred}
  WIFI_STATUS_INVALID_DATA              = 4;  {Invalid data was received}
  WIFI_STATUS_INVALID_PARAMETER         = 5;  {An invalid parameter was passed to the function}
  WIFI_STATUS_INVALID_SEQUENCE          = 6;  {Invalid sequence encountered}
  WIFI_STATUS_OUT_OF_MEMORY             = 7;  {No memory available for operation}
  WIFI_STATUS_UNSUPPORTED_REQUEST       = 8;  {The request is unsupported}
  WIFI_STATUS_NOT_PROCESSED             = 9;  {The WIFI transfer has not yet been processed}



  {WIFI Device Types}
  WIFI_TYPE_NONE      = 0;
  WIFI_TYPE_SDIO      = 3; {An SDIO specification card}


  {WIFI Device Flags}
  WIFI_FLAG_NONE              = $00000000;

  {MMC Operation Condition Register (OCR) values} {See: Section 5.1 of SD Physical Layer Simplified Specification V4.10}
  WIFI_OCR_BUSY		   = $80000000; {Busy Status - 0 = Initializing / 1 = Initialization Complete}
  WIFI_OCR_HCS		   = $40000000; {Card Capacity Status - 0 = SDSC / 1 = SDHC or SDXC}
  WIFI_OCR_UHS_II        = $20000000; {UHS-II Card Status - 0 = Non UHS-II Card / 1 = UHS-II Card}
  WIFI_OCR_S18A          = $01000000; {Switching to 1.8V Accepted - 0 = Continue current voltage signaling / 1 = Ready for switching signal voltage}
  WIFI_OCR_VOLTAGE_MASK  = $007FFF80;
  WIFI_OCR_ACCESS_MODE   = $60000000; //To Do //??



type


{WIFI Device}
  PWIFIDevice = ^TWIFIDevice;

  TWIFIDeviceInitialize = function(WIFI:PWIFIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
  TWIFIDeviceSetIOS = function(WIFI:PWIFIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
  TWIFIDeviceSendCommand = function(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}


  TWIFIDevice = record
   {Device Properties}
   Device:TDevice;                                  {The Device entry for this WIFI device}
   {WIFI Properties}
   WIFIId:LongWord;                                  {Unique Id of this WIFI device in the MMC }
//   MMCState:LongWord;                               {MMC state (eg MMC_STATE_INSERTED)}
   DeviceInitialize:TWIFIDeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
//   DeviceDeinitialize:TMMCDeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
//   DeviceGetCardDetect:TMMCDeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
//   DeviceGetWriteProtect:TMMCDeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
   DeviceSendCommand:TWIFIDeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard MMC device interface (Or nil if the default method is suitable)}
   DeviceSetIOS:TWIFIDeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard MMC device interface (Or nil if the default method is suitable)}
   {Statistics Properties}
   CommandCount:LongWord;
   CommandErrors:LongWord;
   {Driver Properties}
   Lock:TMutexHandle;                               {Device lock}
   Version:LongWord;
   Clock:LongWord;
   Timing:LongWord;
   BusWidth:LongWord;
   Voltages:LongWord;
   Capabilities:LongWord;
   {Register Properties}                            {See: Table 3-2: SD Memory Card Registers of SD Physical Layer Simplified Specification Version 4.10}
   InterfaceCondition:LongWord;                     {Interface Condition Result}
   OperationCondition:LongWord;                     {Operation Condition Register (OCR)} {See: Section 5.1 of SD Physical Layer Simplified Specification Version 4.10}
   RelativeCardAddress:LongWord;                    {Relative Card Address (RCA) (Word)} {See: Section 5.4 of SD Physical Layer Simplified Specification Version 4.10}
   CardSpecific:array[0..3] of LongWord;            {Card Specific Data (CSD)}           {See: Section 5.3 of SD Physical Layer Simplified Specification Version 4.10}
   CardIdentification:array[0..3] of LongWord;      {Card Identification Data (CID)}     {See: Section 5.2 of SD Physical Layer Simplified Specification Version 4.10}
   CardStatus:LongWord;                             {Card Status Register (CSR)}         {See: Section 4.10.1 of SD Physical Layer Simplified Specification Version 4.10}
   DriverStage:LongWord;                            {Driver Stage Register (DSR) (Word)} {See: Section 5.5 of SD Physical Layer Simplified Specification Version 4.10}
   SDStatus:array[0..15] of LongWord;               {SD Status Register (SSR)}           {See: Section 4.10.2 of SD Physical Layer Simplified Specification Version 4.10}
   SDSwitch:array[0..15] of LongWord;               {SD Switch Status}                   {See: Section 4.3.10 of SD Physical Layer Simplified Specification Version 4.10}
   SDConfiguration:array[0..1] of LongWord;         {SD Configuration Register (SCR)}    {See: Section 5.6 of SD Physical Layer Simplified Specification Version 4.10}
   {Configuration Properties}
//   CardSpecificData:TMMCCardSpecificData;
//   CardIdentificationData:TMMCCardIdentificationData;
//   SDStatusData:TSDStatusData;
//   SDSwitchData:TSDSwitchData;
//   SDConfigurationData:TSDConfigurationData;
   {Storage Properties}
//   Storage:PStorageDevice;                          {The Storage entry for this MMC (Where Applicable)}
   {Internal Properties}
   Prev:PWIFIDevice;                                 {Previous entry in WIFI table}
   Next:PWIFIDevice;                                 {Next entry in WIFI table}
  end;



function WIFIDeviceCreate:PWIFIDevice;
function WIFIDeviceCreateEx(Size:LongWord):PWIFIDevice;
function WIFIDeviceDestroy(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceCheck(WIFI:PWIFIDevice):PWIFIDevice;
function WIFIDeviceInitialize(WIFI:PWIFIDevice):LongWord;

function WIFIDeviceRegister(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceFind(WIFIId:LongWord):PWIFIDevice;

function SDIOWIFIDeviceReset(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceGoIdle(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceSetBusWidth(WIFI:PWIFIDevice;Width:LongWord):LongWord;
function SDWIFIDeviceSendInterfaceCondition(WIFI:PWIFIDevice):LongWord;
function SDIOWIFIDeviceSendOperationCondition(WIFI:PWIFIDevice;Probe:Boolean):LongWord;
function SDIOWIFIDeviceReadWriteDirect(WIFI:PWIFIDevice;Write:Boolean;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord;
function SDIOWIFIDeviceReadWriteExtended(WIFI:PWIFIDevice; Write:Boolean;
            Operation, Address : LongWord;
            Increment : Boolean; Buffer : Pointer;
            BlockCount, BlockSize : LongWord) : LongWord;
function WIFIDeviceSendCommand(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;
function WIFIDeviceSetClock(WIFI:PWIFIDevice;Clock:LongWord):LongWord;
function WIFIDeviceSetBackplaneWindow(WIFI : PWIFIDevice; addr : longword) : longword;
function WIFIDeviceSendCardIdentification(WIFI:PWIFIDevice):LongWord;



function WIFIDeviceSendApplicationCommand(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;

procedure WIFILogError(WIFI:PWIFIDevice;const AText:String); inline;
function WIFIDeviceSetIOS(WIFI:PWIFIDevice):LongWord;



var
  WIFI_LOG_ENABLED : boolean = true;

implementation

const
  {WIFI logging}
  WIFI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {WIFI debugging messages}
  WIFI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {WIFI informational messages, such as a device being attached or detached}
  WIFI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {WIFI warning messages}
  WIFI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {WIFI error messages}
  WIFI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No WIFI messages}

var
  WIFI_DEFAULT_LOG_LEVEL:LongWord = WIFI_LOG_LEVEL_DEBUG; {Minimum level for WIFI messages.  Only messages with level greater than or equal to this will be printed}
  WIFIDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;

  WIFIDeviceTable:PWIFIDevice;
  WIFIDeviceTableCount:LongWord;

  WIFIInitialized:Boolean;

procedure WIFILog(Level:LongWord;WIFI:PWIFIDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < WIFI_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = WIFI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = WIFI_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = WIFI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'WIFI: ';

 {Check WIFI}
 if WIFI <> nil then
  begin
   WorkBuffer:=WorkBuffer + WIFI_NAME_PREFIX + IntToStr(WIFI^.WIFIId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_DEVICES,LogLevelToLoggingSeverity(Level),'WIFIdevice',WorkBuffer + AText);
end;

procedure WIFILogError(WIFI:PWIFIDevice;const AText:String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_ERROR,WIFI,AText);
end;

{==============================================================================}

procedure WIFILogDebug(WIFI: PWIFIDevice;const AText:String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_DEBUG,WIFI,AText);
end;

procedure WIFILogInfo(WIFI: PWIFIDevice;const AText:String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_INFO,WIFI,AText);
end;

function WIFIDeviceCreate:PWIFIDevice;
{Create a new WIFI entry}
{Return: Pointer to new WIFI entry or nil if WIFI could not be created}
begin
 {}
 Result:=WIFIDeviceCreateEx(SizeOf(TWIFIDevice));
end;

{==============================================================================}

function WIFIDeviceCreateEx(Size:LongWord):PWIFIDevice;
{Create a new WIFI entry}
{Size: Size in bytes to allocate for new WIFI (Including the WIFI entry)}
{Return: Pointer to new WIFI entry or nil if WIFI could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TWIFIDevice) then Exit;

 {Create WIFI}
 Result:=PWIFIDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result^.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result^.Device.DeviceType:=WIFI_TYPE_SDIO;
 Result^.Device.DeviceFlags:=WIFI_FLAG_NONE;  // may need to change
 Result^.Device.DeviceData:=nil;

 {Update WIFI}
 Result^.WIFIId:=DEVICE_ID_ANY;
// Result^.WIFIState:=WIFI_STATE_EJECTED;
 Result^.DeviceInitialize:=nil;
// Result^.DeviceDeinitialize:=nil;
// Result^.DeviceGetCardDetect:=nil;
// Result^.DeviceGetWriteProtect:=nil;
 Result^.DeviceSendCommand:=nil;
 Result^.DeviceSetIOS:=nil;
 Result^.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result^.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result^.Lock = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'Failed to create lock for WIFI device');
   WIFIDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

function WIFIDeviceDestroy(WIFI:PWIFIDevice):LongWord;
{Destroy an existing WIFI entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;
 if WIFI^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check WIFI}
 Result:=ERROR_IN_USE;
 if WIFIDeviceCheck(WIFI) = WIFI then Exit;

 {Check State}
 if WIFI^.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if WIFI^.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(WIFI^.Lock);
  end;

 {Destroy WIFI}
 Result:=DeviceDestroy(@WIFI^.Device);
end;

function WIFIDeviceCheck(WIFI:PWIFIDevice):PWIFIDevice;
{Check if the supplied WIFI device is in the table}
var
 Current:PWIFIDevice;
begin
 {}
 Result:=nil;

 {Check WIFI}
 if WIFI = nil then Exit;
 if WIFI^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(WIFIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get WIFI}
    Current:= WIFIDeviceTable;
    while Current <> nil do
     begin
      {Check WIFI}
      if Current = WIFI then
       begin
        Result:=WIFI;
        Exit;
       end;

      {Get Next}
      Current:=Current^.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(WIFIDeviceTableLock);
   end;
  end;
end;


{Initialization Functions}
procedure WIFIInit;
var
  i : integer;
begin
 {}
 {Check Initialized}
 if WIFIInitialized then Exit;

 {Initialize Logging}
 WIFI_LOG_ENABLED:=(WIFI_DEFAULT_LOG_LEVEL <> WIFI_LOG_LEVEL_NONE);

 {Initialize WIFI Device Table}
 WIFIDeviceTable:=nil;
 WIFIDeviceTableLock:=CriticalSectionCreate;
 WIFIDeviceTableCount:=0;
 if WIFIDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'Failed to create WIFI device table lock');
  end;

 (* disconnect emmc from SD card (connect sdhost instead) *)
 for i := 48 to 53 do
   GPIOFunctionSelect(i,GPIO_FUNCTION_ALT0);

 (* connect emmc to wifi *)
 for i := 34 to 39 do
 begin
   GPIOFunctionSelect(i,GPIO_FUNCTION_ALT3);

   if (i = 34) then
     GPIOPullSelect(i, GPIO_PULL_NONE)
   else
     GPIOPullSelect(i, GPIO_PULL_UP);
 end;

 // init 32khz oscillator.
 SysGPIOPullSelect(SD_32KHZ_PIN, GPIO_PULL_NONE);
 GPIOFunctionSelect(SD_32KHZ_PIN, GPIO_FUNCTION_ALT0);

 // turn on wlan power
 GPIOFunctionSelect(WLAN_ON_PIN, GPIO_FUNCTION_OUT);
 GPIOOutputSet(WLAN_ON_PIN, GPIO_LEVEL_HIGH);



 //this is already done elsewhere but we have a dependency.
 // will probably need to do something about creation order.
 {Initialize SDHCI Host Table}
(* SDHCIHostTable:=nil;
 SDHCIHostTableLock:=CriticalSectionCreate;
 SDHCIHostTableCount:=0;
 if SDHCIHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then MMCLogError(nil,'Failed to create SDHCI host table lock');
  end;*)

  WIFIInitialized:=True;
end;

function WIFIDeviceSetClock(WIFI:PWIFIDevice;Clock:LongWord):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'WIFI Set Clock (Clock=' + IntToStr(Clock) + ')');

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Clock}
 if Clock > SDHCI^.MaximumFrequency then
  begin
   Clock:=SDHCI^.MaximumFrequency;
  end;
 if Clock < SDHCI^.MinimumFrequency then
  begin
   Clock:=SDHCI^.MinimumFrequency;
  end;

 {Set Clock}
 WIFI^.Clock:=Clock;

 {Set IOS}
 Result:=WIFIDeviceSetIOS(WIFI);

 //See: mmc_set_clock in U-Boot mmc.c
 //See:
end;


function WIFIDeviceInitialize(WIFI:PWIFIDevice):LongWord;
{Reference: Section 3.6 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 SDHCI:PSDHCIHost;
 Command : TSDIOCommand;
 rcaraw : longword;
 updatevalue : word;
 ioreadyvalue : word;
 chipid : word;
 buf : array[1..500] of byte;
 str : string;
 i : integer;
 chipidbuf : array[1..2] of byte;
 bytevalue : byte;
begin
 {}
 WIFILogInfo(nil,'WIFI Initialize');

 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {Check Initialize}
 if Assigned(WIFI^.DeviceInitialize) then
  begin
   WIFILogDebug(nil,'WIFI^.DeviceInitialize');
   Result:=WIFI^.DeviceInitialize(WIFI);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   WIFILogDebug(nil,'Default initialize method');

   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then
   begin
    WIFILogError(nil, 'The SDHCI host is nil');
    Exit;
   end;

   WIFILogInfo(nil,'Set initial power');

   // should already have been done elsewhere.
   {Set Initial Power}
   Result:=SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI^.Voltages) - 1);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
     WIFILogError(nil,'failed to Set initial power');

     Exit;
    end;

   {Set Initial Bus Width}
   // don't think this is needed for this device. it fails any
   // come back to this later
   // might not be an application command but a normal one.
   // wil know more once we can see what sort of speed we can get out of it.
(*   Result:=WIFIDeviceSetBusWidth(WIFI,WIFI_BUS_WIDTH_4);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
     WIFILogError(WIFI, 'Failed to set WIFI bus width');
//     Exit;
    end;*)

   {Set Initial Clock}
   WIFILogInfo(nil,'Set device clock');
   Result:=WIFIDeviceSetClock(WIFI,SDIO_BUS_SPEED_DEFAULT);
   if Result <> WIFI_STATUS_SUCCESS then
     wifilogError(nil, 'failed to set the clock speed to default')
   else
     wifiloginfo(nil, 'Set device clock succeeded');

   {Perform an SDIO Reset}
   wifiloginfo(nil, 'SDIO WIFI Device Reset');
   SDIOWIFIDeviceReset(WIFI);

   wifiloginfo(nil, 'WIFI Device Go Idle');
   {Set the Card to Idle State}
   Result:= WIFIDeviceGoIdle(WIFI);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
     wifilogdebug(nil, 'go idle returned fail but lets plough on anyway...');
    end
   else
     wifiloginfo(nil, 'Go Idle succeeded');


   wifiloginfo(nil, 'send interface condition req');
   {Get the Interface Condition}
   SDWIFIDeviceSendInterfaceCondition(WIFI);

   wifiloginfo(nil, 'send operation condition');
   {Check for an SDIO Card}
   if SDIOWIFIDeviceSendOperationCondition(WIFI,True) = WIFI_STATUS_SUCCESS then
    begin
     wifiloginfo(nil, 'send operation condition successful');
     {SDIO Card}
    end
    else
      wifilogerror(nil, 'send operation condition failed');

   WIFI^.Device.DeviceBus:=DEVICE_BUS_SD;
   WIFI^.Device.DeviceType:=WIFI_TYPE_SDIO;
   WIFI^.RelativeCardAddress:=0;
   WIFI^.OperationCondition := $200000;

   {$IFDEF MMC_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Initialize Card Type is SDIO');
   {$ENDIF}


   // Note this probably doesn't work as the wifi device does not respond to
   // cmd5 during initialisation (and possibly not at all for all I know!)
   {Get the Operation Condition}
   wifiloginfo(nil, 'get operation condition');
   Result:=SDIOWIFIDeviceSendOperationCondition(WIFI,False);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
//     Exit;
    end;


   wifiloginfo(nil, 'set card relative address with cmd3');
   {send CMD3 get relative address}
   FillChar(Command,SizeOf(TSDIOCommand),0);
   Command.Command:=SDIO_CMD_SET_RELATIVE_ADDR;
   Command.Argument:=0;
   Command.ResponseType:=SDIO_RSP_R6;
   Command.Data:=nil;

   Result := WIFIDeviceSendCommand(WIFI, @Command);
   rcaraw := command.response[0];
   WIFI^.RelativeCardAddress := (rcaraw shr 16) and $ff;
   if (Result = WIFI_STATUS_SUCCESS) then
      wifiloginfo(nil,' Card relative address is ' + inttohex((rcaraw shr 16) and $ff, 2))
   else
     wifilogerror(nil, 'Could not set relative card address; error='+inttostr(Result));


   Result := WIFIDeviceSendCardIdentification(WIFI);
   // bits 64 to 103 are the name.
   // this is a 4 entry array of 4 byte values ergo 16 bytes.
   // bits 64 to 103 are in entry 2. Not sure which end of the array that will be
   // lets try both.
   wifiloginfo(nil, 'CID ' + inttohex( WIFI^.CardIdentification[0], 8)
             + ' ' + inttohex( WIFI^.CardIdentification[1], 8)
             + ' ' + inttohex( WIFI^.CardIdentification[2], 8)
             + ' ' + inttohex( WIFI^.CardIdentification[3], 8));


   wifiloginfo(nil, 'Selecting wifi device with cmd7');

   FillChar(Command,SizeOf(TSDIOCommand),0);
   Command.Command:= SDIO_CMD_SELECT_CARD;
   Command.Argument:= rcaraw;
   Command.ResponseType:=SDIO_RSP_R1;
   Command.Data:=nil;

   Result := WIFIDeviceSendCommand(WIFI, @Command);

   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     wifiloginfo(nil, 'Device successfully selected response[0]=' + inttohex(command.response[0], 8));
     // for an I/O only card, the status bits are fixed at 0x0f (bits 12:9 of response[0])
     if (((command.response[0] shr 9) and $f) = $f) then
       wifiloginfo(nil, 'The card correctly reads as I/O only')
     else
       wifilogerror(nil, 'Something went wrong with the status bits');
   end
   else
     wifilogerror(nil, 'Failed to select the card at rca='+inttohex((rcaraw shr 16) and $ff, 8));


   wifiloginfo(nil, 'setting bus speed via common control registers');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0,SDIO_CCCR_SPEED,3,nil);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated bus speed register')
   else
     wifilogerror(nil, 'Failed to update bus speed register');



   wifiloginfo(nil, 'setting bus interface via common control registers');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0,SDIO_CCCR_IF, $42,nil);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated bus interface control')
   else
     wifilogerror(nil, 'Failed to update bus interface control');


   wifiloginfo(nil, 'setting backplane block size');

   // by setting blocksize to zero, the blockcount is just the number of bytes.
   updatevalue := 64;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BUS_BAK_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BUS_BAK_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated backplane block size')
   else
     wifilogerror(nil, 'Failed to update backplane block size');

   wifiloginfo(nil, 'setting radio block size');

   updatevalue := 512;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BUS_RAD_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BUS_RAD_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated radio block size')
   else
     wifilogerror(nil, 'Failed to update radio block size');


   wifiloginfo(nil, 'io enable');
   ioreadyvalue := 0;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0,SDIO_CCCR_IOEx,  1 shl 1,nil);   // yuck. 1 shl fd_func_bak (not defined yet).
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'io enable successfully set')
   else
     wifilogerror(nil, 'io enable could not be set');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,0,SDIO_CCCR_IORx,  0, @ioreadyvalue);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'ioready value read and is ' + inttohex(ioreadyvalue, 4))
   else
     wifilogerror(nil, 'ioready value could not be read');

   // set backplane window
  wifiloginfo(nil, 'set backplane window ' + inttohex(bak_base_addr, 8));
  Result := WIFIDeviceSetBackplaneWindow(WIFI, BAK_BASE_ADDR);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated backplane window')
   else
     wifilogerror(nil, 'Failed to update backplane window');


(*   sdio_cmd53_read(SD_FUNC_BAK, SB_32BIT_WIN, u32d.bytes, 4);*)
  chipid := 0;

(*  str := '';
  for i := 1 to 500 do
  begin
    Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1, i-1,  0, @bytevalue);
    if (Result = WIFI_STATUS_SUCCESS) then
    begin
      str := str + ' ' + inttohex(bytevalue, 2);
      if i mod 20 = 0 then
      begin
        wifilogerror(nil, str);
        str := '';
      end;
    end
    else
      wifilogerror(nil, 'failed to read byte from device');
  end;
  wifilogerror(nil, str);
*)

  wifiloginfo(nil, 'getting chip id');

  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,0,  0, @chipid);
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,1,  0, pbyte(@chipid)+1);
  if (Result = WIFI_STATUS_SUCCESS) then
    wifiloginfo(nil, 'WIFI Chip ID is 0x'+inttohex(chipid, 4));

(*
function SDIOWIFIDeviceReadWriteExtended(WIFI:PWIFIDevice; Write:Boolean;
            Operation, Address : LongWord;
            Increment : Boolean; Buffer : Pointer;
            BlockCount, BlockSize : LongWord) : LongWord;

*)
  SDHCI_LOG_ENABLED := true;

  fillchar(buf, sizeof(buf), 0);
  Result:=SDIOWIFIDeviceReadWriteExtended(WIFI, false,
                  1, SB_32BIT_WIN, true, @buf[1], 1, 64);
  if (Result = WIFI_STATUS_SUCCESS) then
  begin
   str := '';
   for i := 1 to 500 do
   begin
     str := str + ' ' + inttohex(buf[i], 2);
     if i mod 20 = 0 then
     begin
       wifilogdebug(nil, str);
       str := '';
     end;
   end;
   wifilogdebug(nil, str);

    wifilogdebug(nil, 'End of reading chip id block');
  end
  else
    wifilogdebug(nil, 'Failed to read the chip id');

  SDHCI_LOG_ENABLED := false;

(*  fillchar(buf, sizeof(buf), 0);
  Result:=SDIOWIFIDeviceReadWriteExtended(WIFI, true,
                  1, SB_32BIT_WIN-200, true, @buf[1], 500, 0);
  str := '';
  for i := 1 to 500 do
  begin
    str := str + ' ' + inttohex(buf[i], 2);
    if i mod 20 = 0 then
    begin
      wifilogdebug(nil, str);
      str := '';
    end;
  end;
  wifilogdebug(nil, str);   *)

   Result:=WIFI_STATUS_SUCCESS;
  end;
end;

function WIFIDeviceSetBackplaneWindow(WIFI : PWIFIDevice; addr : longword) : longword;
begin
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BAK_WIN_ADDR_REG, addr and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BAK_WIN_ADDR_REG+1,(addr shr 8) and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0, BAK_WIN_ADDR_REG+2,(addr shr 16) and $ff,nil);

 if (Result = WIFI_STATUS_SUCCESS) then
   wifilogdebug(nil, 'backplanewindow updated to ' + inttohex(addr, 8))
 else
   wifilogdebug(nil, 'something went wrong in setbackplanewindow');
end;

function WIFIDeviceRegister(WIFI:PWIFIDevice):LongWord;
{Register a new WIFI device in the table}
var
 WIFIId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;
 if WIFI^.WIFIId <> DEVICE_ID_ANY then Exit;
 if WIFI^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check WIFI}
 Result:=ERROR_ALREADY_EXISTS;
 if WIFIDeviceCheck(WIFI) = WIFI then Exit;

 {Check State}
 if WIFI^.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert WIFI}
 if CriticalSectionLock(WIFIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update WIFI}
    WIFIId:=0;
    while WIFIDeviceFind(WIFIId) <> nil do
     begin
      Inc(WIFIId);
     end;
    WIFI^.WIFIId:=WIFIId;

    {Update Device}
    WIFI^.Device.DeviceName:=WIFI_NAME_PREFIX + IntToStr(WIFI^.WIFIId);
    WIFI^.Device.DeviceClass:=DEVICE_CLASS_SD;

    {Register Device}
    Result:=DeviceRegister(@WIFI^.Device);
    WIFILogError(nil, 'deviceregister result = ' + inttostr(result));
    if Result <> ERROR_SUCCESS then
     begin
      WIFI^.WIFIId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link WIFI}
    if WIFIDeviceTable = nil then
     begin
      WIFIDeviceTable:=WIFI;
     end
    else
     begin
      WIFI^.Next:=WIFIDeviceTable;
      WIFIDeviceTable^.Prev:=WIFI;
      WIFIDeviceTable:=WIFI;
     end;

    {Increment Count}
    Inc(WIFIDeviceTableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(WIFIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

function WIFIDeviceFind(WIFIId:LongWord):PWIFIDevice;
var
 WIFI:PWIFIDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if WIFIId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(WIFIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get WIFI}
    WIFI:=WIFIDeviceTable;
    while WIFI <> nil do
     begin
      {Check State}
      if WIFI^.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if WIFI^.WIFIId = WIFIId then
         begin
          Result:=WIFI;
          Exit;
         end;
       end;

       {Get Next}
      WIFI:=WIFI^.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(WIFIDeviceTableLock);
   end;
  end;
end;

function WIFIDeviceSetBusWidth(WIFI:PWIFIDevice;Width:LongWord):LongWord;
{Reference: Section 3.4 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 Status:LongWord;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFI^.BusWidth := Width;

 WIFILogDebug(nil,'WIFI Set Bus Width (Width=' + IntToStr(Width) + ')');

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SD_CMD_APP_SET_BUS_WIDTH;
 Command.Argument:=0;
 case Width of
  WIFI_BUS_WIDTH_1:Command.Argument:=WIFI_BUS_WIDTH_1;
  WIFI_BUS_WIDTH_4:Command.Argument:=WIFI_BUS_WIDTH_4;
  else
   begin
    Exit;
   end;
 end;
 Command.ResponseType:=SDIO_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 wifilogdebug(nil, 'setbuswidth sendapplicationcommand will not be sent yet as I dont think it is needed');
 Status:=WIFIDeviceSendApplicationCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   wifilogdebug(nil, 'setbuswidth sendapplicationcommand failed ' + inttostr(result));
   Result:=Status;
   Exit;
  end;

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_app_set_bus_width in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c

end;

function SDIOWIFIDeviceReset(WIFI:PWIFIDevice):LongWord;
{See: SDIO Simplified Specification V2.0, 4.4 Reset for SDIO}
var
 Abort:Byte;
 Status:LongWord;
begin
 {}
 WIFILOGDebug(WIFI, 'SDIO WIFI Reset');

 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {Get Abort Value}
 WIFILOGDebug(WIFI, 'get abort value');

 Status:=SDIOWIFIDeviceReadWriteDirect(WIFI,False,0,SDIO_CCCR_ABORT,0,@Abort);
 MicrosecondDelay(20000);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   WIFILOGDebug(WIFI, 'WIFI Device Reset - SDIO_CCR_ABORT returned non zero result of ' + inttostr(status));

   Abort:=$08;
  end
 else
  begin
   wifilogdebug(nil, 'abort value success status');
   Abort:=Abort or $08;
  end;

 {Set Abort Value}
 WIFILogDebug(WIFI, 'Set abort value');
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0,SDIO_CCCR_ABORT,Abort,nil);
 MicrosecondDelay(20000);
 WIFILogDebug(WIFI, 'Result of setting abort='+inttostr(Result));

 //See: sdio_reset in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;

function WIFIDeviceGoIdle(WIFI:PWIFIDevice):LongWord;
var
 Status:LongWord;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'WIFI Go Idle');

 {Delay 1ms}
 MicrosecondDelay(1000);

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_GO_IDLE_STATE;
 Command.Argument:=0;
 Command.ResponseType:=SDIO_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   WIFILogDebug(nil,'WIFI failed to go idle');

   Result:=Status;
   Exit;
  end;

 wifilogdebug(nil, 'WIFI successfully went idle');

 {Delay 2ms}
 MicrosecondDelay(2000);

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_go_idle in U-Boot mmc.c
 //     mmc_go_idle in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
end;

function SDWIFIDeviceSendInterfaceCondition(WIFI:PWIFIDevice):LongWord;
{See: 4.3.13 of SD Physical Layer Simplified Specification V4.10

 CMD8 (SEND_IF_COND) must be invoked to support SD 2.0 cards
 The card must be in Idle State before issuing this command

 This command will fail harmlessly for SD 1.0 cards
}
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'SD Send Interface Condition');

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_SEND_IF_COND;
 Command.Argument:=SDIO_SEND_IF_COND_CHECK_PATTERN;
 if (SDHCI^.Voltages and SDIO_SEND_IF_COND_VOLTAGE_MASK) <> 0 then
  begin
   {Set bit 8 if the host supports voltages between 2.7 and 3.6 V}
   Command.Argument:=(1 shl 8) or SDIO_SEND_IF_COND_CHECK_PATTERN;
  end;
 Command.ResponseType:=SDIO_RSP_R7;
 Command.Data:=nil;

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Response}
   if (Command.Response[0] and $FF) <> SDIO_SEND_IF_COND_CHECK_PATTERN then
    begin
     wifilogError(nil,'SD Send Interface Condition failure (Response=' + IntToHex(Command.Response[0] and $FF,8) + ')');
     Exit;
    end
    else
      wifilogdebug(nil, 'WIFI Send interface condition check pattern matches');

   {Get Response}
   wifiLogdebug(nil,'WIFI Send Interface Condition Response0=' + IntToHex(Command.Response[0] and $FF,8)
     + 'Response1=' + IntToHex(Command.Response[1] and $FF,8));
   WIFI^.InterfaceCondition:=Command.Response[0];

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_send_if_cond in U-Boot mmc.c
 //See: mmc_send_if_cond in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
end;


function SDIOWIFIDeviceSendOperationCondition(WIFI:PWIFIDevice;Probe:Boolean):LongWord;
var
 Status:LongWord;
 Timeout:Integer;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'SDIO Send Operation Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_SEND_OP_COND;
 Command.Argument:=0;
 if not(Probe) then
   Command.Argument:=WIFI^.OperationCondition;

 Command.ResponseType:=SDIO_RSP_R4;
 Command.Data:=nil;

 {Setup Timeout}
 Timeout:=100;
 wifilogdebug(nil, 'waiting for non-busy signal from wifi device');
 while Timeout > 0 do
  begin
   {Send Command}
   Status:=WIFIDeviceSendCommand(WIFI,@Command);
   if Status <> WIFI_STATUS_SUCCESS then
    begin
     wifilogerror(nil, 'sendoperationcondition devicesendcommand returned failed status ' + inttostr(status));
     Result:=Status;
     Exit;
    end;

   {Single pass only on probe}
   if Probe then Break;

   if (Command.Response[0] and WIFI_OCR_BUSY) <> 0 then Break;

   Dec(Timeout);
   if Timeout = 0 then
    begin
     if WIFI_LOG_ENABLED then WifiLogError(nil,'SDIO Send Operation Condition Busy Status Timeout');
     Exit;
    end;
   MillisecondDelay(10);
  end;

 wifilogdebug(nil, 'wifi device is ready for action');


 {Get Response}
  wifilogdebug(nil, 'operation condition returned as ' + inttostr(command.response[0]));
  WIFI^.OperationCondition:=Command.Response[0];
  //To Do //SD_OCR_CCS etc (see: MMC/SD)

  Result:=WIFI_STATUS_SUCCESS;

  //See: mmc_send_io_op_cond in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
end;

function WIFIDeviceSetIOS(WIFI:PWIFIDevice):LongWord;
var
 Value:Byte;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'WIFI Set IOS');

 {Check Set IOS}
 if Assigned(WIFI^.DeviceSetIOS) then
  begin
   Result:=WIFI^.DeviceSetIOS(WIFI);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Set Control Register}
   SDHCIHostSetControlRegister(SDHCI);

   {Check Clock}
   if WIFI^.Clock <> SDHCI^.Clock then
    begin
     SDHCIHostSetClock(SDHCI,WIFI^.Clock);
     SDHCI^.Clock:=WIFI^.Clock;
    end;

   {Set Power}
   SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI^.Voltages) - 1);

   {Set Bus Width}
   Value:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);
(*   if WIFI^.BusWidth = WIFI_BUS_WIDTH_8 then
    begin
     Value:=Value and not(SDHCI_CTRL_4BITBUS);
     if (SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300) or ((SDHCI^.Quirks2 and SDHCI_QUIRK2_USE_WIDE8) <> 0) then
      begin
       Value:=Value or SDHCI_CTRL_8BITBUS;
      end;
    end
   else
    begin*)
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       Value:=Value and not(SDHCI_CTRL_8BITBUS);
      end;

     if WIFI^.BusWidth = WIFI_BUS_WIDTH_4 then
      begin
       Value:=Value or SDHCI_CTRL_4BITBUS;
      end
     else
      begin
       Value:=Value and not(SDHCI_CTRL_4BITBUS);
      end;
    (*end;*)

   {Check Clock}
   if WIFI^.Clock > 26000000 then
    begin
     Value:=Value or SDHCI_CTRL_HISPD;
    end
   else
    begin
     Value:=Value and not(SDHCI_CTRL_HISPD);
    end;
   if (SDHCI^.Quirks and SDHCI_QUIRK_NO_HISPD_BIT) <> 0 then
    begin
     Value:=Value and not(SDHCI_CTRL_HISPD);
    end;
   //To Do //More here (Reset SD Clock Enable / Re-enable SD Clock) //See: bcm2835_mmc_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
                 //Even more quirks                                       //See: sdhci_do_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
   SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Value);

   Result:=WIFI_STATUS_SUCCESS;
  end;

 //See: mmc_set_ios in mmc.c
 //     sdhci_set_ios in sdhci.c
 //See: bcm2835_mmc_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_do_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
end;


function SDIOWIFIDeviceReadWriteDirect(WIFI:PWIFIDevice; Write:Boolean; Operation,Address:LongWord; Input:Byte; Output:PByte):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'sdio read write direct address='+inttohex(address, 8) + ' value='+inttohex(input, 2));

 {$IFDEF MMC_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'SDIO Read Write Direct');
 {$ENDIF}

 {Get SDHCI}
 WIFILogDebug(nil,'get sdhci');

 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Operation}
 if Operation > 7 then Exit;

 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;

 WIFILogDebug(nil,'setup command');

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_RW_DIRECT;
 Command.Argument:=0;
 Command.ResponseType:=SDIO_RSP_R5;
 Command.Data:=nil;

 WIFILogDebug(nil,'setup argument. write='+booltostr(write, true));

 {Setup Argument}
 if Write then Command.Argument:=$80000000;
 Command.Argument:=Command.Argument or (Operation shl 28);
 if Write and (Output <> nil) then Command.Argument:=Command.Argument or $08000000;
 Command.Argument:=Command.Argument or (Address shl 9);
 Command.Argument:=Command.Argument or Input;

 WIFILogDebug(nil,'send command. argument ended up being ' + inttohex(command.argument, 8));

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   WIFILogDebug(nil,'status is not success (=' + inttostr(result) + ')');

   Result:=Status;
   Exit;
  end;

 {Check Result}
 WIFILogDebug(nil,'check result (response[0]='+inttostr(command.response[0]));

 if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then Exit;
 if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then Exit;
 if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then Exit;

 {Get Output}
 if Output <> nil then
  begin
     WIFILogDebug(nil,'get output');
     Output^:=Command.Response[0] and $FF;
  end;

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_io_rw_direct_host in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;

{SDIO_CMD_RW_DIRECT argument format:
      [31] R/W flag
      [30:28] Function number
      [27] RAW flag
      [25:9] Register address
      [7:0] Data}

function SDIOWIFIDeviceReadWriteExtended(WIFI:PWIFIDevice; Write:Boolean;
            Operation, Address : LongWord;
            Increment : Boolean; Buffer : Pointer;
            BlockCount, BlockSize : LongWord) : LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
 SDIOData : TSDIOData;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'SDIO Read Write Extended');

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 wifilogdebug(nil, 'check operation; operation='+inttostr(operation));
 {Check Operation}
 if Operation > 7 then Exit;

 wifilogdebug(nil, 'check address='+inttohex(address, 8));
 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;

 WIFILogDebug(nil,'setup command');

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_RW_EXTENDED;
 Command.Argument:=0;
 Command.ResponseType:=SDIO_RSP_R5;
 Command.Data:=nil;

 Command.Data := @SDIOData;
 SDIOData.Data := Buffer;
 SDIOData.Blocksize := BlockSize;
 SDIOData.BlockCount := blockcount;
 SDIOData.Flags := WIFI_DATA_READ;

 WIFILogDebug(nil,'setup argument. write='+booltostr(write, true));

 {SDIO_CMD_RW_EXTENDED argument format:
       [31] R/W flag
       [30:28] Function number
       [27] Block mode
       [26] Increment address
       [25:9] Register address
       [8:0] Byte/block count}

 {Setup Argument}
 if Write then Command.Argument:=$80000000;
 Command.Argument:=Command.Argument or (Operation shl 28);   // adds in function number

 if (blockcount > 0) then       // block mode enabled by supplying non-zero value for block count
 begin
//   command.Argument := Command.Argument or (1 shl 27);
 end;

 // block size is just byte count if blocksize is zero.
 Command.Argument := Command.Argument or BlockSize;

 if increment then
    Command.Argument := Command.Argument or (1 shl 26);     // add in increment flag

 Command.Argument:=Command.Argument or (Address shl 9);

 WIFILogDebug(nil,'send command. argument ended up being ' + inttohex(command.argument, 8));

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   WIFILogDebug(nil,'status is not success (=' + inttostr(result) + ')');

   Result:=Status;
   Exit;
  end;

 {Check Result}
 WIFILogDebug(nil,'check result command.response[0]='+inttohex(command.response[0], 8));

 if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then
  begin
   wifilogdebug(nil, 'command response contains R5 Error');
   Exit;
  end;
 if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then
  begin
   wifilogdebug(nil, 'command response contains R5 function number');
   Exit;
  end;
 if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then
  begin
   wifilogdebug(nil, 'command response contains R5 out of range');
   Exit;
  end;

 WIFILogDebug(nil,'returning success');

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_io_rw_extended in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;


function WIFIDeviceSendCommand(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;
var
 Mask:LongWord;
 Mode:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Timeout:LongWord;
 Address:LongWord;
 SDHCI:PSDHCIHost;
 DataP : PSDIOData;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'WIFI Send Command ' + inttostr(command^.Command) + ' status='+inttostr(command^.Status));

 {Check Send Command}
 if Assigned(WIFI^.DeviceSendCommand) then
  begin
   WIFILogDebug(nil,'Assigned wifi sendcommand true');

   Result:=WIFI^.DeviceSendCommand(WIFI,Command);
  end
 else
  begin
   {Default Method}
   {Check Command}
//   WIFILogDebug(nil,'using default sendcommand method');
   if Command = nil then Exit;

   DataP := Command^.Data;

   {Get SDHCI}
   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Acquire the Lock}
   if MutexLock(WIFI^.Lock) = ERROR_SUCCESS then
    begin
     try
      {Setup Status}
      Command^.Status:=WIFI_STATUS_NOT_PROCESSED;
      try
       {Wait Timeout (10ms)}
       Timeout:=1000;
       Mask:=SDHCI_CMD_INHIBIT;
       if (Command^.Data <> nil) or ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         Mask:=Mask or SDHCI_DATA_INHIBIT;
        end;

       {We shouldn't wait for data inihibit for stop commands, even though they might use busy signaling}
       if Command^.Command = SDIO_CMD_STOP_TRANSMISSION then
        begin
         Mask:=Mask and not(SDHCI_DATA_INHIBIT);
        end;

//       wifilogdebug(nil, 'wait for command inhibit status='+inttostr(command^.status));
       {Wait for Command Inhibit and optionally Data Inhibit to be clear}
       while (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and Mask) <> 0 do
        begin
         if Timeout = 0 then
          begin
           WIFILogError(nil,'WIFI Send Command Inhibit Timeout');
           Command^.Status:=WIFI_STATUS_TIMEOUT;
           Exit;
          end;

         Dec(Timeout);
         MicrosecondDelay(10);
        end;

//       wifilogdebug(nil, 'check response type status='+inttostr(command^.status));

       {Check Response Type}
       if ((Command^.ResponseType and SDIO_RSP_136) <> 0) and ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         if WIFI_LOG_ENABLED then WifiLogError(nil,'MMC Send Command Invalid Response Type');
         Command^.Status:=WIFI_STATUS_INVALID_PARAMETER;
         Exit;
        end;

//       wifilogdebug(nil, 'setup command flags');

       {Setup Command Flags}
       if (Command^.ResponseType and SDIO_RSP_PRESENT) = 0 then
        begin
         Flags:=SDHCI_CMD_RESP_NONE;
        end
       else if (Command^.ResponseType and SDIO_RSP_136) <> 0 then
        begin
         Flags:=SDHCI_CMD_RESP_LONG;
        end
       else if (Command^.ResponseType and SDIO_RSP_BUSY) <> 0 then
        begin
         Flags:=SDHCI_CMD_RESP_SHORT_BUSY;
        end
       else
        begin
         Flags:=SDHCI_CMD_RESP_SHORT;
        end;

       if (Command^.ResponseType and SDIO_RSP_CRC) <> 0 then
        begin
         Flags:=Flags or SDHCI_CMD_CRC;
        end;
       if (Command^.ResponseType and SDIO_RSP_OPCODE) <> 0 then
        begin
         Flags:=Flags or SDHCI_CMD_INDEX;
        end;
       {CMD19 is special in that the Data Present Select should be set}
       if (Command^.Data <> nil) or (Command^.Command = SDIO_CMD_SEND_TUNING_BLOCK) or (Command^.Command = SDIO_CMD_SEND_TUNING_BLOCK_HS200) then
        begin
         wifilogdebug(nil, 'adding sdhci_cmd_data flag to the flags for this command');
         Flags:=Flags or SDHCI_CMD_DATA;
        end;

//       wifilogdebug(nil, 'write timeout control');

       {Write Timeout Control}
       if (Command^.Data <> nil) or ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_TIMEOUT_CONTROL (Value=' + IntToHex(SDHCI_TIMEOUT_VALUE,8) + ')');
         {$ENDIF}
         SDHCIHostWriteByte(SDHCI,SDHCI_TIMEOUT_CONTROL,SDHCI_TIMEOUT_VALUE);
        end;

//       wifilogdebug(nil, 'process data if present');

       {Check Data}
       if (command^.data = nil) then
        begin
         wifilogdebug(nil, 'writing a standard command; status='+inttostr(command^.status));

         {Setup Transfer Mode}
         Mode:=SDHCIHostReadWord(SDHCI,SDHCI_TRANSFER_MODE);

         {Clear Auto CMD settings for non data CMDs}
         Mode:=Mode and not(SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23);

         {Clear Block Count, Multi, Read and DMA for non data CMDs}
         Mode:=Mode and not(SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI or SDHCI_TRNS_READ or SDHCI_TRNS_DMA);

         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         {$ENDIF}

//         WIFILogDebug(nil,'write argument=' + inttohex(command^.argument, 8));

         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer Mode}
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,8) + ')');
         {$ENDIF}

//         WIFILogDebug(nil,'value='+inttohex(mode,8) + ' status='+inttostr(command^.status));

         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
//         WIFILogDebug(nil,'now value='+inttohex(mode,8) + ' status='+inttostr(command^.status));
        end
       else
        begin
         WIFILogDebug(nil,'Entered an area of code we have not updated!');

         {Setup Data}
         Command^.Data^.BlockOffset:=0;
         Command^.Data^.BlocksRemaining:=Command^.Data^.BlockCount;
         Command^.Data^.BytesTransfered:=0;

         wifilogdebug(nil, 'block based reqeust being made. blockcount='+inttostr(command^.data^.blockcount) + ' blocksize='+inttostr(command^.data^.blocksize));

         {Setup Transfer Mode}
         Mode:=SDHCI_TRNS_BLK_CNT_EN;
         if (Command^.Data^.BlockCount > 0) then
          begin
           Mode:=Mode or SDHCI_TRNS_MULTI;

           Mode:=Mode or SDHCI_TRNS_AUTO_CMD12; //To Do //Testing (This works, need to sort out properly where it fits, plus SDHCI_TRNS_AUTO_CMD23)

           //To Do //SDHCI_TRNS_AUTO_CMD12 //SDHCI_TRNS_AUTO_CMD23 //SDHCI_ARGUMENT2 //See: sdhci_set_transfer_mode
                   //See 1.15 Block Count in the SD Host Controller Simplified Specifications
          end;
         if (Command^.Data^.Flags and WIFI_DATA_READ) <> 0 then
          begin
           Mode:=Mode or SDHCI_TRNS_READ;
          end;

         Mode := Mode or SDHCI_TRNS_R5;  // should be an R5 response for an SDIO device

         {Setup DMA Address}
         //mode |= SDHCI_TRNS_DMA;
         //Address:=
         //To Do

         {Setup Interrupts}
         SDHCI^.Interrupts:=SDHCI^.Interrupts or (SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL);
         SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI^.Interrupts);
         SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI^.Interrupts);
         //To Do //Different for DMA //Should we disable these again after the command ? //Yes, probably

         {Write DMA Address}
         //To Do
         //SDHCIHostWriteLong(SDHCI,SDHCI_DMA_ADDRESS,Address);

         {Write Block Size}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_SIZE (Value=' + IntToStr(Command^.Data^.BlockSize) + ')');

         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize));

         {Write Block Count}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_COUNT (Value=' + IntToStr(Command^.Data^.BlockCount) + ')');
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,Command^.Data^.BlockCount);

         {Write Argument}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer Mode}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,8) + ')');
         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
        end;

//        wifilogdebug(nil, 'setup command status='+inttostr(command^.status));

       {Setup Command}
       SDHCI^.Command:=Command;
//       wifilogdebug(nil, 'now setup command status='+inttostr(command^.status));
       try
        {Write Command}
        WIFILogDebug(nil,'WIFI Send Command SDHCI_COMMAND cmd=' + inttostr(command^.command) + '  value written to cmd register=' + IntToHex(SDHCIMakeCommand(Command^.Command,Flags),8) + ') status='+inttostr(command^.status));

        // this isn't right but it might trigger the interrupt.
//        command^.data := nil;

        SDHCIHostWriteWord(SDHCI,SDHCI_COMMAND,SDHCIMakeCommand(Command^.Command,Flags));

        {Wait for Completion}   // short timeout for a read command.
        if true then
         begin
          {Wait for Signal with Timeout (100ms)}
          Status:=SemaphoreWaitEx(SDHCI^.Wait,1000);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              WIFILogDebug(nil,'WIFI Send Command Response (short) Timeout');
              Command^.Status:=SDHCI_STATUS_TIMEOUT;
              Exit;
             end
            else
             begin
              WIFILogDebug(nil,'WIFI Send Command Response (short) Failure semaphorewaitexresult='+inttostr(status));
              Command^.Status:=SDHCI_STATUS_HARDWARE_ERROR;
              Exit;
             end;
           end
          else
          begin
//           command^.data := DataP;

           wifilogdebug(nil, 'semaphore wait succeeded command=' + inttostr(command^.Command) + ' status=' + inttostr(command^.Status));
          end;
         end
        else
         begin
          {Wait for Signal with Timeout (5000ms)}
          wifilogdebug(nil, 'wait for semaphore 5000');
          Status:=SemaphoreWaitEx(SDHCI^.Wait,5000);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              WIFILogError(nil,'WIFI Send Data Response Timeout');
              Command^.Status:=SDHCI_STATUS_TIMEOUT;
              Exit;
             end
            else
             begin
              WIFILogError(nil,'MMC Send Data Response Failure');
              Command^.Status:=SDHCI_STATUS_HARDWARE_ERROR;
              Exit;
             end;
           end
           else
            wifilogdebug(nil, 'wait returned success');
         end;
       finally
        {Reset Command}
        SDHCI^.Command:=nil;
       end;
      finally
       {Check Status}
//       wifilogdebug(nil, 'WIFI command status='+inttostr(command^.status));
       if Command^.Status <> WIFI_STATUS_SUCCESS then //To Do //More see: sdhci_tasklet_finish //SDHCI_QUIRK_RESET_AFTER_REQUEST and SDHCI_QUIRK_CLOCK_BEFORE_RESET
        begin
    (* don't think this is needed for wifi? as we know some commands won't respond
        accordng to the lean2 blog.*)
         SDHCIHostReset(SDHCI,SDHCI_RESET_CMD);
         SDHCIHostReset(SDHCI,SDHCI_RESET_DATA);
        end;
      end;

     finally
      {Release the Lock}
      MutexUnlock(WIFI^.Lock);
     end;
    end;

   WIFILogDebug(nil,'WIFI Send Command completed: ' + SDHCIStatusToString(Command^.Status));
   if Command^.Status = WIFI_STATUS_SUCCESS then Result:=WIFI_STATUS_SUCCESS;
  end;

 //See: mmc_send_cmd in mmc.c
 //     sdhci_send_command in sdhci.c
 //See: bcm2835_mmc_send_command in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_send_command in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
end;


function WIFIDeviceSendApplicationCommand(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 ApplicationCommand:TSDIOCommand;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if WIFI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'SD Send Application Command');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Application Command}
 FillChar(ApplicationCommand,SizeOf(TSDIOCommand),0);
 ApplicationCommand.Command:=SDIO_CMD_APP_CMD;
 ApplicationCommand.Argument:=(WIFI^.RelativeCardAddress shl 16);
 ApplicationCommand.ResponseType:= SDIO_RSP_R1;
 ApplicationCommand.Data:=nil;

 {Send Application Command}
 Status:=WIFIDeviceSendCommand(WIFI,@ApplicationCommand);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Response}
  if (ApplicationCommand.Response[0] and WIFI_RSP_R1_APP_CMD) = 0 then
   begin
    if WIFI_LOG_ENABLED then WifiLogError(nil,'SD Send Application Command Not Supported');
    Command^.Status:=WIFI_STATUS_UNSUPPORTED_REQUEST;
    Exit;
   end;

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=WIFI_STATUS_SUCCESS;

 //See: mmc_wait_for_app_cmd in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
end;

function WIFIDeviceSendCardIdentification(WIFI:PWIFIDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if WIFI = nil then Exit;

 WIFILogDebug(nil,'WIFI Send Card Identification');

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TSDIOCommand),0);
 Command.Command:=SDIO_CMD_SEND_CID;
 Command.Argument:=(WIFI^.RelativeCardAddress shl 16);
 Command.ResponseType:=SDIO_RSP_R2;
 Command.Data:=nil;

 {Send Command}
 Status:=WIFIDeviceSendCommand(WIFI,@Command);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 System.Move(Command.Response,WIFI^.CardIdentification,SizeOf(LongWord) * 4);

 Result:=WIFI_STATUS_SUCCESS;
end;



initialization
  WIFIInit;

end.

