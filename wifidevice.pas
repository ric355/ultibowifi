unit wifidevice;

{$mode objfpc}{$H+}
{$ifdef supplicant}
{$ifdef RPI}
{$linklib wpasupplicant/pizero/libwpa_supplicant_pizero.a}
{$endif}
{$ifdef RPI3}
{$linklib wpasupplicant/pi3/libwpa_supplicant_pi3.a}
{$endif}
{$ifdef RPI4}
{$linklib wpasupplicant/pi4/libwpa_supplicant_pi4.a}
{$endif}
{$endif}


//{$DEFINE CYW43455_DEBUG}

interface

uses
  mmc,
  Classes, SysUtils, Devices, Ultibo,
  GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,DMA,gpio,Network, {$IFDEF RPI4}BCM2711{$ENDIF} {$IFDEF RPI3}BCM2710{$ENDIF} {$IFDEF RPI}BCM2708{$ENDIF};

const
  {wpa supplicant logging}
  MSG_EXCESSIVE = 0;
  MSG_MSGDUMP = 1;
  MSG_DEBUG = 2;
  MSG_INFO = 3;
  MSG_WARNING = 4;
  MSG_ERROR = 5;


  EAP_OVER_LAN_PROTOCOL_ID = $888E;
  MAC_ADDRESS_LEN = 6;
  WPA2_SECURITY = $00400000;        // Flag to enable WPA2 Security
  AES_ENABLED   = $0004;            // Flag to enable AES Encryption
  WHD_SECURITY_WPA2_AES_PSK = (WPA2_SECURITY or AES_ENABLED);
  WLC_SET_WSEC = 134;
  DEFAULT_EAPOL_KEY_PACKET_TIMEOUT = 2500; // in milliseconds
  WSEC_MAX_PSK_LEN = 64;
  WSEC_PASSPHRASE = (1 shl 0);
  WLC_SET_WSEC_PMK = 268;
  WL_AUTH_OPEN_SYSTEM = 0;
  WL_AUTH_SAE = 3;
  WLC_SET_INFRA = 20;
  WLC_SET_AUTH = 22;
  WLC_SET_WPA_AUTH = 165;
  WPA2_AUTH_PSK = $0080;
  WPA2_AUTH_PSK_SHA256 = $8000;
  WLC_SET_SSID = 26;
  WLC_SET_BSSID = 24;

  CYW43455_NETWORK_DESCRIPTION = 'Cypress 43455 SDIO Wireless Network Adapter';

  //cyw43455 event flags bits (see whd_event_msg record)
  CYW43455_EVENT_FLAG_LINK        = $01;  // bit 0
  CYW43455_EVENT_FLAG_FLUSHTXQ    = $02;  // bit 1
  CYW43455_EVENT_FLAG_GROUP       = $04;  // but 2

  CHIP_ID_PI_ZEROW = $a9a6;

  IOCTL_MAX_BLKLEN = 2048;
  SDPCM_HEADER_SIZE = 8;
  CDC_HEADER_SIZE = 16;
  IOCL_LEN_BYTES = 4;
  ETHERNET_HEADER_BYTES = 14;

  RECEIVE_REQUEST_PACKET_COUNT = 16;

  // wifi control commands
  WLC_GET_VAR = 262;
  WLC_SET_VAR = 263;

  WHD_MSG_IFNAME_MAX = 16;

  ETHER_ADDR_LEN = 6;
  BSS_TYPE_ANY = 2;
  SCAN_TYPE_PASSIVE = 1;
  SSID_MAX_LEN = 32;
  WLC_E_LAST = 152;
  WL_EVENTING_MASK_LEN = (WLC_E_LAST + 7) div 8;
  MCSSET_LEN = 16;
  DOT11_IE_ID_RSN = 48;



  BUS_FUNCTION = 0;
  BACKPLANE_FUNCTION = 1;
  WLAN_FUNCTION = 2;

  RECEIVE_BUFFER_DEFAULT_BLOCK_SIZE = 512;
  WLC_SET_PASSIVE_SCAN = 49;

  // sdio core regs
  Intstatus	= $20;
  	Fcstate		= 1 shl 4;
  	Fcchange	= 1 shl 5;
  	FrameInt	= 1 shl 6;
  	MailboxInt	= 1 shl 7;
  Intmask		= $24;
  Sbmbox		= $40;
  Sbmboxdata	= $48;
  Hostmboxdata= $4c;
  Fwready = $80;

  Gpiopullup	= $58;
  Gpiopulldown	= $5c;
  Chipctladdr	= $650;
  Chipctldata	= $654;

  Pullups	= $1000f;



  FIRMWARE_CHUNK_SIZE = 2048;
  FIRWMARE_OPTIONS_COUNT = 8;
  FIRMWARE_FILENAME_ROOT = 'c:\firmware\';

  {SDIO Bus Speeds (Hz)}
  SDIO_BUS_SPEED_DEFAULT   = 0;
  SDIO_BUS_SPEED_HS26      = 26000000;
  SDIO_BUS_SPEED_HS52      = 52000000;
  SDIO_BUS_SPEED_DDR       = 52000000;
  SDIO_BUS_SPEED_HS          = 50000000;
  SDIO_BUS_SPEED_HS200     = 200000000;

  {$IFDEF RPI}
  WLAN_ON_PIN = GPIO_PIN_41;
  {$ENDIF}
  {$IF DEFINED(RPI3) or DEFINED(RPI4)}
  WLAN_ON_PIN = VIRTUAL_GPIO_PIN_1;
  {$ENDIF}
  SD_32KHZ_PIN = GPIO_PIN_43;

  {SDIO Device States}
  SDIO_STATE_EJECTED  = 0;
  SDIO_STATE_INSERTED = 1;

  SDIO_STATE_MAX      = 1;

  {Maximum block count for SDIO}
  SDIO_MAX_BLOCK_COUNT = 65535;

  {SDIO Modes}
  SDIO_MODE_HS		= (1 shl 0);
  SDIO_MODE_HS_52MHz	= (1 shl 1);
  SDIO_MODE_4BIT		= (1 shl 2);
  SDIO_MODE_8BIT		= (1 shl 3);
  SDIO_MODE_SPI		= (1 shl 4);
  SDIO_MODE_HC		= (1 shl 5);
  SDIO_MODE_DDR_52MHz	= (1 shl 6);

  {SDHCI Host States}
  SDHCI_STATE_DISABLED = 0;
  SDHCI_STATE_ENABLED  = 1;

  SDHCI_STATE_MAX      = 1;


  {SDHCI specific constants}
  SDHCI_NAME_PREFIX = 'SDHCI';     {Name prefix for SDHCI Devices}

  {SDHCI Host Types}
  SDHCI_TYPE_NONE      = 0;
  SDHCI_TYPE_MMC       = 1; {An MMC specification host controller}
  SDHCI_TYPE_SD        = 2; {An SD specification host controller}
  SDHCI_TYPE_MMCI      = 3; {An MMCI specification host controller}

  SDHCI_TYPE_MAX       = 3;

  {SDHCI Type Names}
  SDHCI_TYPE_NAMES:array[SDHCI_TYPE_NONE..SDHCI_TYPE_MAX] of String = (
   'SDHCI_TYPE_NONE',
   'SDHCI_TYPE_MMC',
   'SDHCI_TYPE_SD',
   'SDHCI_TYPE_MMCI');


  {SDHCI State Names}
  SDHCI_STATE_NAMES:array[SDHCI_STATE_DISABLED..SDHCI_STATE_MAX] of String = (
   'SDHCI_STATE_DISABLED',
   'SDHCI_STATE_ENABLED');

  {SDHCI Host Flags}
  SDHCI_FLAG_NONE          = $00000000;
  SDHCI_FLAG_SDMA          = $00000001;
  SDHCI_FLAG_ADMA          = $00000002;
  SDHCI_FLAG_SPI           = $00000004;
  SDHCI_FLAG_CRC_ENABLE    = $00000008;
  SDHCI_FLAG_NON_STANDARD  = $00000010; {Host Controller uses a non standard interface (not supporting SDHCI register layout)}
  SDHCI_FLAG_AUTO_CMD12    = $00000020; {Host Controller supports Auto CMD12 (Stop Transmission)}
  SDHCI_FLAG_AUTO_CMD23    = $00000040; {Host Controller supports Auto CMD23 (Set Block Count)}
  //To Do //More //DMA shared, DMA align etc


  {SDIO Directions} //To Do //??
  SDIO_DATA_READ		= 1;
  SDIO_DATA_WRITE		= 2;

  {SDIO Response Types (From: /include/linux/mmc/mmc.h)}
  {Native}
  SDIO_RSP_PRESENT = (1 shl 0);
  SDIO_RSP_136	 = (1 shl 1); {136 bit response}
  SDIO_RSP_CRC	 = (1 shl 2); {Expect valid crc}
  SDIO_RSP_BUSY	 = (1 shl 3); {Card may send busy}
  SDIO_RSP_OPCODE	 = (1 shl 4); {Response contains opcode}

  {These are the native response types, and correspond to valid bit patterns of the above flags. One additional valid pattern is all zeros, which means we don't expect a respons}
  SDIO_RSP_NONE	= (0);
  SDIO_RSP_R1	    = (SDIO_RSP_PRESENT or SDIO_RSP_CRC or SDIO_RSP_OPCODE);
  SDIO_RSP_R1B    = (SDIO_RSP_PRESENT or SDIO_RSP_CRC or SDIO_RSP_OPCODE or SDIO_RSP_BUSY);
  SDIO_RSP_R2	    = (SDIO_RSP_PRESENT or SDIO_RSP_136 or SDIO_RSP_CRC);
  SDIO_RSP_R3	    = (SDIO_RSP_PRESENT);
  SDIO_RSP_R4	    = (SDIO_RSP_PRESENT);
  SDIO_RSP_R5	    = (SDIO_RSP_PRESENT or SDIO_RSP_CRC or SDIO_RSP_OPCODE);
  SDIO_RSP_R6	    = (SDIO_RSP_PRESENT or SDIO_RSP_CRC or SDIO_RSP_OPCODE);
  SDIO_RSP_R7	    = (SDIO_RSP_PRESENT or SDIO_RSP_CRC or SDIO_RSP_OPCODE);


  {SDHCI Voltage Values}
  SDHCI_VDD_165_195	= $00000080; {VDD voltage 1.65 - 1.95}
  SDHCI_VDD_20_21		= $00000100; {VDD voltage 2.0 ~ 2.1}
  SDHCI_VDD_21_22		= $00000200; {VDD voltage 2.1 ~ 2.2}
  SDHCI_VDD_22_23		= $00000400; {VDD voltage 2.2 ~ 2.3}
  SDHCI_VDD_23_24		= $00000800; {VDD voltage 2.3 ~ 2.4}
  SDHCI_VDD_24_25		= $00001000; {VDD voltage 2.4 ~ 2.5}
  SDHCI_VDD_25_26		= $00002000; {VDD voltage 2.5 ~ 2.6}
  SDHCI_VDD_26_27		= $00004000; {VDD voltage 2.6 ~ 2.7}
  SDHCI_VDD_27_28		= $00008000; {VDD voltage 2.7 ~ 2.8}
  SDHCI_VDD_28_29		= $00010000; {VDD voltage 2.8 ~ 2.9}
  SDHCI_VDD_29_30		= $00020000; {VDD voltage 2.9 ~ 3.0}
  SDHCI_VDD_30_31		= $00040000; {VDD voltage 3.0 ~ 3.1}
  SDHCI_VDD_31_32		= $00080000; {VDD voltage 3.1 ~ 3.2}
  SDHCI_VDD_32_33		= $00100000; {VDD voltage 3.2 ~ 3.3}
  SDHCI_VDD_33_34		= $00200000; {VDD voltage 3.3 ~ 3.4}
  SDHCI_VDD_34_35		= $00400000; {VDD voltage 3.4 ~ 3.5}
  SDHCI_VDD_35_36		= $00800000; {VDD voltage 3.5 ~ 3.6}

  {SDHCI Controller Registers}
  SDHCI_DMA_ADDRESS	     = $00;
  SDHCI_BLOCK_SIZE	     = $04;
  SDHCI_BLOCK_COUNT	     = $06;
  SDHCI_ARGUMENT		     = $08;
  SDHCI_TRANSFER_MODE	 = $0C;
  SDHCI_COMMAND		     = $0E;
  SDHCI_RESPONSE		     = $10;
  SDHCI_BUFFER		     = $20;
  SDHCI_PRESENT_STATE	 = $24;
  SDHCI_HOST_CONTROL	     = $28;
  SDHCI_POWER_CONTROL	 = $29;
  SDHCI_BLOCK_GAP_CONTROL = $2A;
  SDHCI_WAKE_UP_CONTROL	 = $2B;
  SDHCI_CLOCK_CONTROL	 = $2C;
  SDHCI_TIMEOUT_CONTROL	 = $2E;
  SDHCI_SOFTWARE_RESET	 = $2F;
  SDHCI_INT_STATUS	     = $30;
  SDHCI_INT_ENABLE	     = $34;
  SDHCI_SIGNAL_ENABLE	 = $38;
  SDHCI_ACMD12_ERR	     = $3C;
  {3E-3F reserved}
  SDHCI_CAPABILITIES      = $40;
  SDHCI_CAPABILITIES_1	 = $44;
  SDHCI_MAX_CURRENT	     = $48;
  {4C-4F reserved for more max current}
  SDHCI_SET_ACMD12_ERROR	 = $50;
  SDHCI_SET_INT_ERROR	 = $52;
  SDHCI_ADMA_ERROR	     = $54;
  {55-57 reserved}
  SDHCI_ADMA_ADDRESS	     = $58;
  {60-FB reserved}
  SDHCI_SLOT_INT_STATUS	 = $FC;
  SDHCI_HOST_VERSION	     = $FE;

  {SDHCI Transfer Modes}
  SDHCI_TRNS_DMA		    = $01;
  SDHCI_TRNS_BLK_CNT_EN	= $02;
  SDHCI_TRNS_AUTO_CMD12	= $04;  {SDHCI_TRNS_ACMD12}
  SDHCI_TRNS_AUTO_CMD23	= $08;
  SDHCI_TRNS_READ	    = $10;
  SDHCI_TRNS_MULTI	    = $20;
  SDHCI_TRNS_R5	    = $40;

  {SDHCI Command Values}
  SDHCI_CMD_RESP_MASK = $03;
  SDHCI_CMD_CRC		 = $08;
  SDHCI_CMD_INDEX	 = $10;
  SDHCI_CMD_DATA		 = $20;
  SDHCI_CMD_ABORTCMD	 = $C0;

  {SDHCI Command Response Values}
  SDHCI_CMD_RESP_NONE	   = $00;
  SDHCI_CMD_RESP_LONG	   = $01;
  SDHCI_CMD_RESP_SHORT	   = $02;
  SDHCI_CMD_RESP_SHORT_BUSY = $03;

  {SDHCI Present State Values}
  SDHCI_CMD_INHIBIT	         = $00000001;
  SDHCI_DATA_INHIBIT	         = $00000002;
  SDHCI_DOING_WRITE	         = $00000100;
  SDHCI_DOING_READ	         = $00000200;
  SDHCI_SPACE_AVAILABLE	     = $00000400;
  SDHCI_DATA_AVAILABLE	     = $00000800;
  SDHCI_CARD_PRESENT	         = $00010000;
  SDHCI_CARD_STATE_STABLE	 = $00020000;
  SDHCI_CARD_DETECT_PIN_LEVEL = $00040000;
  SDHCI_WRITE_PROTECT	     = $00080000; {Set if Write Enabled / Clear if Write Protected}

  {SDHCI Host Control Values}
  SDHCI_CTRL_LED		    = $01;
  SDHCI_CTRL_4BITBUS	    = $02;
  SDHCI_CTRL_HISPD	    = $04;
  SDHCI_CTRL_DMA_MASK    = $18;
  SDHCI_CTRL_SDMA	    = $00;
  SDHCI_CTRL_ADMA1	    = $08;
  SDHCI_CTRL_ADMA32	    = $10;
  SDHCI_CTRL_ADMA64	    = $18;
  SDHCI_CTRL_8BITBUS 	= $20;
  SDHCI_CTRL_CD_TEST_INS = $40;
  SDHCI_CTRL_CD_TEST	    = $80;

  {SDHCI Power Control Values}
  SDHCI_POWER_ON		= $01;
  SDHCI_POWER_180	= $0A;
  SDHCI_POWER_300	= $0C;
  SDHCI_POWER_330	= $0E;

  {SDHCI Wakeup Control Values}
  SDHCI_WAKE_ON_INT	  = $01;
  SDHCI_WAKE_ON_INSERT = $02;
  SDHCI_WAKE_ON_REMOVE = $04;

  {SDHCI Clock Control Values}
  SDHCI_DIVIDER_SHIFT	= 8;
  SDHCI_DIVIDER_HI_SHIFT	= 6;
  SDHCI_DIV_MASK	        = $FF;
  SDHCI_DIV_MASK_LEN	    = 8;
  SDHCI_DIV_HI_MASK	    = $0300;
  SDHCI_CLOCK_CARD_EN	= $0004;
  SDHCI_CLOCK_INT_STABLE = $0002;
  SDHCI_CLOCK_INT_EN	    = $0001;

  {SDHCI Software Reset Values}
  SDHCI_RESET_ALL	= $01;
  SDHCI_RESET_CMD	= $02;
  SDHCI_RESET_DATA	= $04;

  {SDHCI Interrupt Values}
  SDHCI_INT_RESPONSE	    = $00000001;
  SDHCI_INT_DATA_END	    = $00000002;
  SDHCI_INT_BLK_GAP      = $00000004;
  SDHCI_INT_DMA_END	    = $00000008;
  SDHCI_INT_SPACE_AVAIL	= $00000010;
  SDHCI_INT_DATA_AVAIL	= $00000020;
  SDHCI_INT_CARD_INSERT	= $00000040;
  SDHCI_INT_CARD_REMOVE	= $00000080;
  SDHCI_INT_CARD_INT	    = $00000100;
  SDHCI_INT_ERROR	    = $00008000;
  SDHCI_INT_TIMEOUT	    = $00010000;
  SDHCI_INT_CRC		    = $00020000;
  SDHCI_INT_END_BIT	    = $00040000;
  SDHCI_INT_INDEX	    = $00080000;
  SDHCI_INT_DATA_TIMEOUT = $00100000;
  SDHCI_INT_DATA_CRC	    = $00200000;
  SDHCI_INT_DATA_END_BIT = $00400000;
  SDHCI_INT_BUS_POWER	= $00800000;
  SDHCI_INT_ACMD12ERR	= $01000000;
  SDHCI_INT_ADMA_ERROR	= $02000000;

  SDHCI_INT_NORMAL_MASK	= $00007FFF;
  SDHCI_INT_ERROR_MASK	= $FFFF8000;

  SDHCI_INT_CMD_MASK	    = (SDHCI_INT_RESPONSE or SDHCI_INT_TIMEOUT or SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX);
  SDHCI_INT_DATA_MASK	= (SDHCI_INT_DATA_END or SDHCI_INT_DMA_END or SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_END_BIT or SDHCI_INT_ADMA_ERROR or SDHCI_INT_BLK_GAP);
  SDHCI_INT_ALL_MASK	    = (LongWord(-1));

  {SDHCI Capabilities Values}
  SDHCI_TIMEOUT_CLK_MASK	     = $0000003F;
  SDHCI_TIMEOUT_CLK_SHIFT     = 0;
  SDHCI_TIMEOUT_CLK_UNIT	     = $00000080;
  SDHCI_CLOCK_BASE_MASK	     = $00003F00;
  SDHCI_CLOCK_V3_BASE_MASK    = $0000FF00;
  SDHCI_CLOCK_BASE_SHIFT	     = 8;
  SDHCI_CLOCK_BASE_MULTIPLIER = 1000000;
  SDHCI_MAX_BLOCK_MASK	     = $00030000;
  SDHCI_MAX_BLOCK_SHIFT       = 16;
  SDHCI_CAN_DO_8BIT	         = $00040000;
  SDHCI_CAN_DO_ADMA2	         = $00080000;
  SDHCI_CAN_DO_ADMA1	         = $00100000;
  SDHCI_CAN_DO_HISPD	         = $00200000;
  SDHCI_CAN_DO_SDMA	         = $00400000;
  SDHCI_CAN_VDD_330	         = $01000000;
  SDHCI_CAN_VDD_300	         = $02000000;
  SDHCI_CAN_VDD_180	         = $04000000;
  SDHCI_CAN_64BIT	         = $10000000;

  {SDHCI Host Version Values}
  SDHCI_VENDOR_VER_MASK	= $FF00;
  SDHCI_VENDOR_VER_SHIFT	= 8;
  SDHCI_SPEC_VER_MASK	= $00FF;
  SDHCI_SPEC_VER_SHIFT	= 0;
  SDHCI_SPEC_100	        = 0;
  SDHCI_SPEC_200	        = 1;
  SDHCI_SPEC_300	        = 2;

  //SDHCI_GET_VERSION(x) (x->version and SDHCI_SPEC_VER_MASK);

  {SDHCI Clock Dividers}
  SDHCI_MAX_CLOCK_DIV_SPEC_200	 = 256;
  SDHCI_MAX_CLOCK_DIV_SPEC_300	 = 2046;

  {From Linux /include/linux/mmc/sdhci.h}
  SDHCI_QUIRK_CLOCK_BEFORE_RESET			= (1 shl 0); {Controller doesn't honor resets unless we touch the clock register}
  SDHCI_QUIRK_FORCE_DMA				    = (1 shl 1); {Controller has bad caps bits, but really supports DMA}
  SDHCI_QUIRK_NO_CARD_NO_RESET			= (1 shl 2); {Controller doesn't like to be reset when there is no card inserted.}
  SDHCI_QUIRK_SINGLE_POWER_WRITE			= (1 shl 3); {Controller doesn't like clearing the power reg before a change}
  SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS		= (1 shl 4); {Controller has flaky internal state so reset it on each ios change}
  SDHCI_QUIRK_BROKEN_DMA				    = (1 shl 5); {Controller has an unusable DMA engine}
  SDHCI_QUIRK_BROKEN_ADMA				= (1 shl 6); {Controller has an unusable ADMA engine}
  SDHCI_QUIRK_32BIT_DMA_ADDR			    = (1 shl 7); {Controller can only DMA from 32-bit aligned addresses}
  SDHCI_QUIRK_32BIT_DMA_SIZE			    = (1 shl 8); {Controller can only DMA chunk sizes that are a multiple of 32 bits}
  SDHCI_QUIRK_32BIT_ADMA_SIZE			= (1 shl 9); {Controller can only ADMA chunks that are a multiple of 32 bits}
  SDHCI_QUIRK_RESET_AFTER_REQUEST		= (1 shl 10); {Controller needs to be reset after each request to stay stable}
  SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER	= (1 shl 11); {Controller needs voltage and power writes to happen separately}
  SDHCI_QUIRK_BROKEN_TIMEOUT_VAL			= (1 shl 12); {Controller provides an incorrect timeout value for transfers}
  SDHCI_QUIRK_BROKEN_SMALL_PIO			= (1 shl 13); {Controller has an issue with buffer bits for small transfers}
  SDHCI_QUIRK_NO_BUSY_IRQ				= (1 shl 14); {Controller does not provide transfer-complete interrupt when not busy}
  SDHCI_QUIRK_BROKEN_CARD_DETECTION		= (1 shl 15); {Controller has unreliable card detection}
  SDHCI_QUIRK_INVERTED_WRITE_PROTECT		= (1 shl 16); {Controller reports inverted write-protect state}
  SDHCI_QUIRK_PIO_NEEDS_DELAY			= (1 shl 18); {Controller does not like fast PIO transfers}
  SDHCI_QUIRK_FORCE_BLK_SZ_2048			= (1 shl 20); {Controller has to be forced to use block size of 2048 bytes}
  SDHCI_QUIRK_NO_MULTIBLOCK			    = (1 shl 21); {Controller cannot do multi-block transfers}
  SDHCI_QUIRK_FORCE_1_BIT_DATA			= (1 shl 22); {Controller can only handle 1-bit data transfers}
  SDHCI_QUIRK_DELAY_AFTER_POWER			= (1 shl 23); {Controller needs 10ms delay between applying power and clock}
  SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK	= (1 shl 24); {Controller uses SDCLK instead of TMCLK for data timeouts}
  SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN		= (1 shl 25); {Controller reports wrong base clock capability}
  SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC		= (1 shl 26); {Controller cannot support End Attribute in NOP ADMA descriptor}
  SDHCI_QUIRK_MISSING_CAPS			    = (1 shl 27); {Controller is missing device caps. Use caps provided by host}
  SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12		= (1 shl 28); {Controller uses Auto CMD12 command to stop the transfer}
  SDHCI_QUIRK_NO_HISPD_BIT			    = (1 shl 29); {Controller doesn't have HISPD bit field in HI-SPEED SD card}
  SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC	= (1 shl 30); {Controller treats ADMA descriptors with length 0000h incorrectly}
  SDHCI_QUIRK_UNSTABLE_RO_DETECT			= (1 shl 31); {The read-only detection via SDHCI_PRESENT_STATE register is unstable}

  {SDHCI More Quirks/Bugs}
  {From Linux /include/linux/mmc/sdhci.h}
  SDHCI_QUIRK2_HOST_OFF_CARD_ON			= (1 shl 0);
  SDHCI_QUIRK2_HOST_NO_CMD23			    = (1 shl 1);
  SDHCI_QUIRK2_NO_1_8_V				    = (1 shl 2); {The system physically doesn't support 1.8v, even if the host does}
  SDHCI_QUIRK2_PRESET_VALUE_BROKEN		= (1 shl 3);
  SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON		= (1 shl 4);
  SDHCI_QUIRK2_BROKEN_HOST_CONTROL		= (1 shl 5); {Controller has a non-standard host control register}
  SDHCI_QUIRK2_BROKEN_HS200			    = (1 shl 6); {Controller does not support HS200}
  SDHCI_QUIRK2_BROKEN_DDR50			    = (1 shl 7); {Controller does not support DDR50}
  SDHCI_QUIRK2_STOP_WITH_TC			    = (1 shl 8); {Stop command(CMD12) can set Transfer Complete when not using MMC_RSP_BUSY}

  {Additions from U-Boot}
  SDHCI_QUIRK2_REG32_RW                  = (1 shl 28); {Controller requires all register reads and writes as 32bit} //To Do //Not Required ?
  SDHCI_QUIRK2_BROKEN_R1B                = (1 shl 29); {Response type R1B is broken}                                //To Do //Not Required ?
  SDHCI_QUIRK2_WAIT_SEND_CMD             = (1 shl 30); {Controller requires a delay between each command write}     //To Do //Not Required ?
  SDHCI_QUIRK2_USE_WIDE8                 = (1 shl 31); {????????}

  {SDHCI Host SDMA buffer boundary (Valid values from 4K to 512K in powers of 2)}
  SDHCI_DEFAULT_BOUNDARY_SIZE  = (512 * 1024);
  SDHCI_DEFAULT_BOUNDARY_ARG	  = (7);

  {SDHCI Timeout Value}
  SDHCI_TIMEOUT_VALUE  = $0E;

  {SDHCI/SD Status Codes}
  SDHCI_STATUS_SUCCESS                   = 0;  {Function successful}
  SDHCI_STATUS_TIMEOUT                   = 1;  {The operation timed out}
  SDHCI_STATUS_NO_MEDIA                  = 2;  {No media present in device}
  SDHCI_STATUS_HARDWARE_ERROR            = 3;  {Hardware error of some form occurred}
  SDHCI_STATUS_INVALID_DATA              = 4;  {Invalid data was received}
  SDHCI_STATUS_INVALID_PARAMETER         = 5;  {An invalid parameter was passed to the function}
  SDHCI_STATUS_INVALID_SEQUENCE          = 6;  {Invalid sequence encountered}
  SDHCI_STATUS_OUT_OF_MEMORY             = 7;  {No memory available for operation}
  SDHCI_STATUS_UNSUPPORTED_REQUEST       = 8;  {The request is unsupported}
  SDHCI_STATUS_NOT_PROCESSED             = 9;  {The SDHCI transfer has not yet been processed}

  {Application commands}
  SD_CMD_APP_SET_BUS_WIDTH	  = 6;
  SD_CMD_APP_SD_STATUS         = 13;
  SD_CMD_APP_SEND_NUM_WR_BLKS  = 22;
  SD_CMD_APP_SEND_OP_COND	  = 41;
  SD_CMD_APP_SEND_SCR		  = 51;

  {SDIO Commands (From: /include/linux/mmc/sdio.h)}
  SDIO_CMD_SEND_OP_COND       =   5;
  SDIO_CMD_RW_DIRECT          =  52;
  SDIO_CMD_RW_EXTENDED        =  53;

  {SDIO Response Values (From: /include/linux/mmc/sdio.h)}
  {R4}
  SDIO_RSP_R4_18V_PRESENT    = (1 shl 24);
  SDIO_RSP_R4_MEMORY_PRESENT = (1 shl 27);

  {R5}
  SDIO_RSP_R5_COM_CRC_ERROR	    = (1 shl 15);
  SDIO_RSP_R5_ILLEGAL_COMMAND	= (1 shl 14);
  SDIO_RSP_R5_ERROR		        = (1 shl 11);
  SDIO_RSP_R5_FUNCTION_NUMBER	= (1 shl 9);
  SDIO_RSP_R5_OUT_OF_RANGE		= (1 shl 8);


  {SDIO Commands}
  {Class 0}
  SDIO_CMD_SEND_RELATIVE_ADDR	  = 3;
  SDIO_CMD_SEND_IF_COND		  = 8;
  SDIO_CMD_SWITCH_VOLTAGE        = 11;

  {SD Send Interface Condition Values}
  SDIO_SEND_IF_COND_CHECK_PATTERN = $AA;
  SDIO_SEND_IF_COND_VOLTAGE_MASK  = $00FF8000;  {MMC_VDD_27_28, MMC_VDD_28_29, MMC_VDD_29_30, MMC_VDD_30_31, MMC_VDD_31_32, MMC_VDD_32_33, MMC_VDD_33_34, MMC_VDD_34_35, MMC_VDD_35_36}

  {SD Send Operation Condition Values}
  SDIO_SEND_OP_COND_VOLTAGE_MASK  = $00FF8000;  {MMC_VDD_27_28, MMC_VDD_28_29, MMC_VDD_29_30, MMC_VDD_30_31, MMC_VDD_31_32, MMC_VDD_32_33, MMC_VDD_33_34, MMC_VDD_34_35, MMC_VDD_35_36}


  {SDIO Card Common Control Registers (CCCR)}
  SDIO_CCCR_CCCR		= $00;
  SDIO_CCCR_SD		= $01;
  SDIO_CCCR_IOEx		= $02;
  SDIO_CCCR_IORx		= $03;
  SDIO_CCCR_IENx		= $04;	{Function/Master Interrupt Enable}
  SDIO_CCCR_INTx		= $05;	{Function Interrupt Pending}
  SDIO_CCCR_ABORT	= $06;	{function abort/card reset}
  SDIO_CCCR_IF		= $07;	{bus interface controls}
  SDIO_CCCR_CAPS		= $08;
  SDIO_CCCR_CIS		= $09;	{common CIS pointer (3 bytes)}
  {Following 4 regs are valid only if SBS is set}
  SDIO_CCCR_SUSPEND	= $0c;
  SDIO_CCCR_SELx		= $0d;
  SDIO_CCCR_EXECx	= $0e;
  SDIO_CCCR_READYx	= $0f;
  SDIO_CCCR_BLKSIZE	= $10;
  SDIO_CCCR_POWER	= $12;
  SDIO_CCCR_SPEED	= $13;
  SDIO_CCCR_UHS		= $14;
  SDIO_CCCR_DRIVE_STRENGTH = $15;

  {SDIO CCCR CCCR Register values}
  SDIO_CCCR_REV_1_00	= 0;	{CCCR/FBR Version 1.00}
  SDIO_CCCR_REV_1_10	= 1;	{CCCR/FBR Version 1.10}
  SDIO_CCCR_REV_1_20	= 2;	{CCCR/FBR Version 1.20}
  SDIO_CCCR_REV_3_00	= 3;	{CCCR/FBR Version 3.00}

  SDIO_SDIO_REV_1_00	= 0;	{SDIO Spec Version 1.00}
  SDIO_SDIO_REV_1_10	= 1;	{SDIO Spec Version 1.10}
  SDIO_SDIO_REV_1_20	= 2;	{SDIO Spec Version 1.20}
  SDIO_SDIO_REV_2_00	= 3;	{SDIO Spec Version 2.00}
  SDIO_SDIO_REV_3_00	= 4;	{SDIO Spec Version 3.00}

  {SDIO CCCR SD Register values}
  SDIO_SD_REV_1_01	= 0;	{SD Physical Spec Version 1.01}
  SDIO_SD_REV_1_10	= 1;	{SD Physical Spec Version 1.10}
  SDIO_SD_REV_2_00	= 2;	{SD Physical Spec Version 2.00}
  SDIO_SD_REV_3_00	= 3;	{SD Physical Spev Version 3.00}

  {SDIO CCCR IF Register values}
  SDIO_BUS_WIDTH_MASK	 = $03;	{data bus width setting}
  SDIO_BUS_WIDTH_1BIT	 = $00;
  SDIO_BUS_WIDTH_RESERVED = $01;
  SDIO_BUS_WIDTH_4BIT	 = $02;
  SDIO_BUS_ECSI		     = $20;	{Enable continuous SPI interrupt}
  SDIO_BUS_SCSI		     = $40;	{Support continuous SPI interrupt}

  SDIO_BUS_ASYNC_INT	     = $20;

  SDIO_BUS_CD_DISABLE     = $80;	{disable pull-up on DAT3 (pin 1)}

  {SDIO CCCR CAPS Register values}
  SDIO_CCCR_CAP_SDC	= $01;	{can do CMD52 while data transfer}
  SDIO_CCCR_CAP_SMB	= $02;	{can do multi-block xfers (CMD53)}
  SDIO_CCCR_CAP_SRW	= $04;	{supports read-wait protocol}
  SDIO_CCCR_CAP_SBS	= $08;	{supports suspend/resume}
  SDIO_CCCR_CAP_S4MI	= $10;	{interrupt during 4-bit CMD53}
  SDIO_CCCR_CAP_E4MI	= $20;	{enable ints during 4-bit CMD53}
  SDIO_CCCR_CAP_LSC	= $40;	{low speed card}
  SDIO_CCCR_CAP_4BLS	= $80;	{4 bit low speed card}

  {SDIO CCCR POWER Register values}
  SDIO_POWER_SMPC	= $01;	{Supports Master Power Control}
  SDIO_POWER_EMPC	= $02;	{Enable Master Power Control}

  {SDIO CCCR SPEED Register values}
  SDIO_SPEED_SHS		= $01;	{Supports High-Speed mode}
  SDIO_SPEED_BSS_SHIFT	= 1;
  SDIO_SPEED_BSS_MASK	= (7 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_SDR12	    = (0 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_SDR25	    = (1 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_SDR50	    = (2 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_SDR104	    = (3 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_DDR50	    = (4 shl SDIO_SPEED_BSS_SHIFT);
  SDIO_SPEED_EHS		= SDIO_SPEED_SDR25;	{Enable High-Speed}

  {SDIO CCCR UHS Register values}
  SDIO_UHS_SDR50	= $01;
  SDIO_UHS_SDR104	= $02;
  SDIO_UHS_DDR50	= $04;

  {SDIO CCCR DRIVE STRENGTH Register values}
  SDIO_SDTx_MASK		= $07;
  SDIO_DRIVE_SDTA	    = (1 shl 0);
  SDIO_DRIVE_SDTC	    = (1 shl 1);
  SDIO_DRIVE_SDTD	    = (1 shl 2);
  SDIO_DRIVE_DTSx_MASK	= $03;
  SDIO_DRIVE_DTSx_SHIFT	= 4;
  SDIO_DTSx_SET_TYPE_B	= (0 shl SDIO_DRIVE_DTSx_SHIFT);
  SDIO_DTSx_SET_TYPE_A	= (1 shl SDIO_DRIVE_DTSx_SHIFT);
  SDIO_DTSx_SET_TYPE_C	= (2 shl SDIO_DRIVE_DTSx_SHIFT);
  SDIO_DTSx_SET_TYPE_D	= (3 shl SDIO_DRIVE_DTSx_SHIFT);

  {SDIO Function Basic Registers (FBR)}
  //SDIO_FBR_BASE(f)	((f) * $100) {base of function f's FBRs}
  SDIO_FBR_STD_IF		= $00;
  SDIO_FBR_STD_IF_EXT	= $01;
  SDIO_FBR_POWER		    = $02;
  SDIO_FBR_CIS		    = $09;	{CIS pointer (3 bytes)}
  SDIO_FBR_CSA		    = $0C;	{CSA pointer (3 bytes)}
  SDIO_FBR_CSA_DATA	    = $0F;
  SDIO_FBR_BLKSIZE	    = $10;	{block size (2 bytes)}

  {SDIO FBR IF Register values}
  SDIO_FBR_SUPPORTS_CSA	= $40;	{supports Code Storage Area}
  SDIO_FBR_ENABLE_CSA	= $80;	{enable Code Storage Area}

  {SDIO FBR POWER Register values}
  SDIO_FBR_POWER_SPS	= $01;	{Supports Power Selection}
  SDIO_FBR_POWER_EPS	= $02;	{Enable (low) Power Selection}


  {SDIO Commands}
  {Class 1}
  SDIO_CMD_GO_IDLE_STATE	  = 0;
  SDIO_CMD_ALL_SEND_CID		  = 2;
  SDIO_CMD_SET_RELATIVE_ADDR	  = 3;
  SDIO_CMD_SET_DSR		  = 4;
  SDIO_CMD_SLEEP_AWAKE		  = 5;
  SDIO_CMD_SWITCH		  = 6;
  SDIO_CMD_SELECT_CARD		  = 7;
  SDIO_CMD_SEND_EXT_CSD		  = 8;
  SDIO_CMD_SEND_CSD		  = 9;
  SDIO_CMD_SEND_CID		  = 10;
  SDIO_CMD_READ_DAT_UNTIL_STOP    = 11;
  SDIO_CMD_STOP_TRANSMISSION	  = 12;
  SDIO_CMD_SEND_STATUS		  = 13;
  SDIO_CMD_BUS_TEST_R             = 14;
  SDIO_CMD_GO_INACTIVE_STATE      = 15;
  SDIO_CMD_BUS_TEST_W             = 19;
  SDIO_CMD_SPI_READ_OCR		  = 58;
  SDIO_CMD_SPI_CRC_ON_OFF	  = 59;

  {Class 2}
  SDIO_CMD_SET_BLOCKLEN		  = 16;
  SDIO_CMD_READ_SINGLE_BLOCK	  = 17;
  SDIO_CMD_READ_MULTIPLE_BLOCK  = 18;
  SDIO_CMD_SEND_TUNING_BLOCK    = 19;
  SDIO_CMD_SEND_TUNING_BLOCK_HS200 = 21;

  {Class 3}
  SDIO_CMD_WRITE_DAT_UNTIL_STOP = 20;

  {Class 4}
  SDIO_CMD_SET_BLOCK_COUNT      = 23;
  SDIO_CMD_WRITE_SINGLE_BLOCK   =	24;
  SDIO_CMD_WRITE_MULTIPLE_BLOCK =	25;
  SDIO_CMD_PROGRAM_CID          = 26;
  SDIO_CMD_PROGRAM_CSD          = 27;

  {Class 6}
  SDIO_CMD_SET_WRITE_PROT       = 28;
  SDIO_CMD_CLR_WRITE_PROT       = 29;
  SDIO_CMD_SEND_WRITE_PROT      = 30;

  {Class 5}
  SDIO_CMD_ERASE_GROUP_START	  = 35;
  SDIO_CMD_ERASE_GROUP_END	  = 36;
  SDIO_CMD_ERASE			      = 38;

  {Class 9}
  SDIO_CMD_FAST_IO              = 39;
  SDIO_CMD_GO_IRQ_STATE         = 40;

  {Class 7}
  SDIO_CMD_LOCK_UNLOCK          = 42;

  {Class 8}
  SDIO_CMD_APP_CMD			  = 55;
  SDIO_CMD_GEN_CMD              = 56;
  SDIO_CMD_RES_MAN			  = 62;

  SDIO_CMD62_ARG1			= $EFAC62EC;
  SDIO_CMD62_ARG2			= $00CBAEA7;

  // core control registers
  Ioctrl = $408;
  Resetctrl= $800;

  // socram regs
  Coreinfo = $00;
  Bankidx = $10;
  Bankinfo = $40;
  Bankpda = $44;

  // armcr4 regs
  Cr4Cap	= $04;
  Cr4Bankidx	= $40;
  Cr4Bankinfo	= $44;
  Cr4Cpuhalt	= $20;


  ARMcm3 = $82A;
  ARM7tdmi = $825;
  ARMcr4 = $83E;


  ATCM_RAM_BASE_ADDRESS = 8;

  // CCCR interrupt enable bits
  INTR_CTL_MASTER_EN  = 1;    // master interrupt enable bit
  INTR_CTL_FUNC1_EN = 2;      // function 1 interrupt enable bit
  INTR_CTL_FUNC2_EN = 4;      // function 2 interrupt enable bit

  // CCCR IO Enable bits
  SDIO_FUNC_ENABLE_1 = 2;
  SDIO_FUNC_ENABLE_2 = 4;

  WIFI_DATA_READ		= 1;
  WIFI_DATA_WRITE		= 2;

  WIFI_BAK_BLK_BYTES = 64;   // backplane block size
  WIFI_RAD_BLK_BYTES = 512;  // radio block size

  BUS_BAK_BLKSIZE_REG = $110;   // register for backplane block size (2 bytes)
  BUS_RAD_BLKSIZE_REG = $210;   // register for radio block size (2 bytes)
  BAK_WIN_ADDR_REG = $1000a;    // register for backplane window address


  Sbwsize = $8000;
  Sb32bit = $8000;


  BAK_BASE_ADDR = $18000000;   // chipcommon base address

  BAK_CHIP_CLOCK_CSR_REG = $1000e;
  ForceALP               = $01;	// active low-power clock */
  ForceHT                = $02;	// high throughput clock */
  ForceILP               = $04;	// idle low-power clock */
  ReqALP                 = $08;
  ReqHT	                 = $10;
  Nohwreq                = $20;
  ALPavail               = $40;
  HTavail                = $80;

  BAK_WAKEUP_REG = $1001e;


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


  MAX_GLOM_PACKETS = 20; // maximum number of packets allowed in glomming (superpackets).


type
  PWPAKey = ^TWPAKey;
  TWPAKey = packed record
    groupkeyid : longword;
    keylen : longword;
    key : array[0..103] of byte;
    algo : longword;
    flags : longword;
    pad1 : array[1..12] of byte;
    ivinitialised : longword;
    unused : longword;
    ivhigh : longword;
    ivlow : word;
    pad2 : array[1..10] of byte;
    ethaddr : array[0..5] of byte;
    pad3 : array[1..2] of byte;
  end;

  TMACAddress = array[0..MAC_ADDRESS_LEN-1] of byte;
  TWIFIJoinType = (WIFIJoinBlocking, WIFIJoinBackground);
  TWIFIReconnectionType = (WIFIReconnectNever, WIFIReconnectAlways);

  {$ifdef supplicant}
  pether_addr = ^ether_addr;
  {$endif}
  ether_addr = record
    octet : array[0..ETHER_ADDR_LEN-1] of byte;
  end;

  countryparams = record
    country_ie : array[1..4] of char;
    revision : longint;
    country_code : array[1..4] of char;
  end;

  wlc_ssid = record
    SSID_len : longword;
    SSID : array[0..31] of byte;
  end;

  chanrec = record
    chan : byte;
    other : byte;
  end;

  wl_scan_params = record
    ssid : wlc_ssid;
    bssid : ether_addr;
    bss_type : byte;
    scan_type : byte;
    nprobes : longword;
    active_time : longword;
    passive_time : longword;
    home_time : longword;
    channel_num : longword;
    channel_list : array[1..14] of chanrec;
    ssids : array[0..SSID_MAX_LEN-1] of word;
  end;


  wl_escan_params = record
    version : longword;
    action : word;
    sync_id : word;
    params : wl_scan_params;
  end;


  pwhd_tlv8_header = ^whd_tlv8_header;
  whd_tlv8_header = record
    atype : byte;
    length : byte;
  end;


  pwhd_tlv8_data = ^whd_tlv8_data;
  whd_tlv8_data = record
    atype : byte;
    length : byte;
    data : array[1..1] of byte;    // used as a pointer to a list.
  end;

  prsn_ie_fixed_portion = ^rsn_ie_fixed_portion;
  rsn_ie_fixed_portion = record
    tlv_header : whd_tlv8_header; //id, length
    version : word;
    group_key_suite : longword; // See whd_80211_cipher_t for values
    pairwise_suite_count : word;
    pairwise_suite_list : array[1..1] of longword;
  end;

  wsec_pmk = record
    key_len : word;
    flags : word;
    key : array[0..WSEC_MAX_PSK_LEN-1] of byte;
  end;

  wl_join_assoc_params = record
    bssid : ether_addr;
    bssid_cnt : word;
    chanspec_num : longword;
    chanspec_list : array[0..1] of word;
  end;

  wl_join_scan_params = record
    scan_type : byte;          // 0 use default, active or passive scan */
    nprobes : longint;         // -1 use default, number of probes per channel */
    active_time : longint;     // -1 use default, dwell time per channel for active scanning
    passive_time : longint;    // -1 use default, dwell time per channel for passive scanning
    home_time : longint;       // -1 use default, dwell time for the home channel between channel scans
  end;

  wl_extjoin_params = record
     ssid : wlc_ssid;                          // {0, ""}: wildcard scan */
     scan_params : wl_join_scan_params;
     assoc_params : wl_join_assoc_params;     // optional field, but it must include the fixed portion
                                              // of the wl_join_assoc_params_t struct when it does
                                              // present.
  end;


  PGlomDescriptor = ^TGlomDescriptor;
  TGlomDescriptor = record
    len : word;
    notlen : word;
    seq : byte;
    chan : byte;
    nextlen : byte; // unsure
    hdrlen : byte;
    flow : byte;
    credit : byte;
    reserved : word;
    packetlengths : array[0..MAX_GLOM_PACKETS-1] of word;     // puts an upper limit on number of coalesced packets but should be ok.
  end;

  PSDPCM_HEADER = ^SDPCM_HEADER;
  SDPCM_HEADER = record
    // sdpcm_sw_header     (hardware extension header)
     seq,                  // rx/tx sequence number
     chan,                 // 4 MSB channel number, 4 LSB aritrary flag
     nextlen,              // length of next data frame, reserved for Tx
     hdrlen,               // data offset
     flow,                 // flow control bits, reserved for tx
     credit : byte;        // maximum sequence number allowed by firmware for Tx
     reserved : word;      // reserved
  end;

  IOCTL_CMDP = ^IOCTL_CMD;
  IOCTL_CMD = record
    sdpcmheader : SDPCM_HEADER;
    cmd : longword;
    outlen,
    inlen : word;
    flags,
    status : longword;
    data : array[0..IOCTL_MAX_BLKLEN-1] of byte;
  end;

  IOCTL_GLOM_HDR = record
    len : word;
    reserved1,
    flags : byte;
    reserved2 : word;
    pad : word;
  end;

  IOCTL_GLOM_CMD = record
    glom_hdr : IOCTL_GLOM_HDR;
    cmd : IOCTL_CMD
  end;

  PIOCTL_MSG = ^IOCTL_MSG;
  IOCTL_MSG = record
    len : word;           // length in bytes to follow
    notlen : word;        // bitwise inverse of length (len + notlen = $ffff)
    case byte of
      1 : (cmd : IOCTL_CMD);
      2 : (glom_cmd : IOCTL_GLOM_CMD);
  end;


  TSDIODirection = (sdioRead, sdioWrite);

  byte4 = array[1..4] of byte;

  TFirmwareEntry = record
    chipid : word;
    chipidrev : word;
    firmwarefilename : string;
    configfilename : string;
    regufilename : string;
  end;

  PSDIOData = ^TSDIOData;
   PSDIOCommand = ^TSDIOCommand;
   TSDIOCommand = record
    {Command Properties}
    Command:Word;
    Argument:LongWord;
    ResponseType:LongWord;
    Response:array[0..3] of LongWord;
    Status:LongWord;
    Data:PSDIOData;
    {Host Properties}
    Timeout:LongWord; {Milliseconds}
    DataCompleted:Boolean;
    BusyCompleted:Boolean;
    TuningCompleted:Boolean;
    CommandCompleted:Boolean;
   end;

   {SDHCI Data}
   TSDIOData = record
    {Data Properties}
    Data:Pointer;
    Flags:LongWord;
    BlockSize:LongWord;
    BlockCount:LongWord;
    {Host Properties}
    BlockOffset:LongWord;
    BlocksRemaining:LongWord;
    BytesRemaining:LongWord;
    BytesTransfered:LongWord;
   end;

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
   NetworkP : PNetworkDevice;                         // the network this device is being associated with
   DeviceInitialize:TWIFIDeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
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

   // wifi chip data - some may not be needed.

   chipid : word;
   chipidrev : word;
   armcore : longword;
   chipcommon : longword;
   armctl : longword;
   armregs : longword;
   d11ctl : longword;
   socramregs : longword;
   socramctl : longword;
   socramrev : longword;
   sdregs : longword;
   sdiorev : longword;
   socramsize : longword;
   rambase : longword;
   dllctl : longword;
   resetvec : longword;

   {additional statistics}
   ReceiveGlomPacketCount:LongWord;              {number of Glom packets received}
   ReceiveGlomPacketSize:LongWord;               {total bytes received via Glom packets}

   {Internal Properties}
   Prev:PWIFIDevice;                                 {Previous entry in WIFI table}
   Next:PWIFIDevice;                                 {Next entry in WIFI table}
  end;

  pether_header = ^ether_header;
  ether_header = record
      destination_address : array[0..5] of byte;
      source_address : array[0..5] of byte;
      ethertype : word;
  end;

  whd_event_eth_hdr = record
      subtype : word;                 // Vendor specific..32769
      length : word;
      version : byte;                 // Version is 0
      oui : array[0..2] of byte;      //  OUI
      usr_subtype : word;             // user specific Data
  end;

  whd_event_msg = record
      version : word;
      flags : word;                                    // see flags below
      event_type : longword;                           // Message (see below)
      status : longword;                               // Status code (see below)
      reason : longword;                               // Reason code (if applicable)
      auth_type : longword;                            // WLC_E_AUTH
      datalen : longword;                              // data buf
      addr : array[0..5] of byte;                      // Station address (if applicable)
      ifname : array[0..WHD_MSG_IFNAME_MAX-1] of char; // name of the packet incoming interface
      ifidx : byte;                                    // destination OS i/f index
      bsscfgidx : byte;                                // source bsscfg index
  end;

  // used by driver msgs
  pwhd_event = ^whd_event;
  whd_event = record
      eth : ether_header;                  // 12 bytes
      eth_evt_hdr : whd_event_eth_hdr ;    // 10 bytes
      whd_event : whd_event_msg;           // 48 bytes
      // data portion follows */
  end;

  // do not change the order or values of the below as they are matched to the
  // event values as defined by Cypress.
//  WLC_E_NONE,
  TWIFIEvent = (
      WLC_E_SET_SSID=0,                  // indicates status of set SSID ,
      WLC_E_JOIN=1,                      // differentiates join IBSS from found (WLC_E_START) IBSS
      WLC_E_START=2,                     // STA founded an IBSS or AP started a BSS
      WLC_E_AUTH=3,                      // 802.11 AUTH request
      WLC_E_AUTH_IND=4,                  // 802.11 AUTH indication
      WLC_E_DEAUTH=5,                    // 802.11 DEAUTH request
      WLC_E_DEAUTH_IND=6,                // 802.11 DEAUTH indication
      WLC_E_ASSOC=7,                     // 802.11 ASSOC request
      WLC_E_ASSOC_IND=8,                 // 802.11 ASSOC indication
      WLC_E_REASSOC=9,                   // 802.11 REASSOC request
      WLC_E_REASSOC_IND=10,              // 802.11 REASSOC indication
      WLC_E_DISASSOC=11,                 // 802.11 DISASSOC request
      WLC_E_DISASSOC_IND=12,             // 802.11 DISASSOC indication
      WLC_E_QUIET_START=13,              // 802.11h Quiet period started
      WLC_E_QUIET_END=14,                // 802.11h Quiet period ended
      WLC_E_BEACON_RX=15,                // BEACONS received / lost indication
      WLC_E_LINK=16,                     // generic link indication
      WLC_E_MIC_ERROR=17,                // TKIP MIC error occurred
      WLC_E_NDIS_LINK=18,                // NDIS style link indication
      WLC_E_ROAM=19,                     // roam attempt occurred: indicate status & reason
      WLC_E_TXFAIL=20,                   // change in dot11FailedCount (txfail)
      WLC_E_PMKID_CACHE=21,              // WPA2 pmkid cache indication
      WLC_E_RETROGRADE_TSF=22,           // current AP's TSF value went backward
      WLC_E_PRUNE=23,                    // AP was pruned from join list for reason
      WLC_E_AUTOAUTH=24,                 // report AutoAuth table entry match for join attempt
      WLC_E_EAPOL_MSG=25,                // Event encapsulating an EAPOL message
      WLC_E_SCAN_COMPLETE=26,            // Scan results are ready or scan was aborted
      WLC_E_ADDTS_IND=27,                // indicate to host addts fail /success
      WLC_E_DELTS_IND=28,                // indicate to host delts fail/success
      WLC_E_BCNSENT_IND=29,              // indicate to host of beacon transmit
      WLC_E_BCNRX_MSG=30,                // Send the received beacon up to the host
      WLC_E_BCNLOST_MSG=31,              // indicate to host loss of beacon
      WLC_E_ROAM_PREP=32,                // before attempting to roam
      WLC_E_PFN_NET_FOUND=33,            // PFN network found event
      WLC_E_PFN_NET_LOST=34,             // PFN network lost event
      WLC_E_RESET_COMPLETE=35,
      WLC_E_JOIN_START=36,
      WLC_E_ROAM_START=37,
      WLC_E_ASSOC_START=38,
      WLC_E_IBSS_ASSOC=39,
      WLC_E_RADIO=40,
      WLC_E_PSM_WATCHDOG=41,             // PSM microcode watchdog fired
      WLC_E_CCX_ASSOC_START=42,          // CCX association start
      WLC_E_CCX_ASSOC_ABORT=43,          // CCX association abort
      WLC_E_PROBREQ_MSG=44,              // probe request received
      WLC_E_SCAN_CONFIRM_IND=45,         //
      WLC_E_PSK_SUP=46,                  // WPA Handshake
      WLC_E_COUNTRY_CODE_CHANGED=47,     //
      WLC_E_EXCEEDED_MEDIUM_TIME=48,     // WMMAC excedded medium time
      WLC_E_ICV_ERROR=49,                // WEP ICV error occurred
      WLC_E_UNICAST_DECODE_ERROR=50,     // Unsupported unicast encrypted frame
      WLC_E_MULTICAST_DECODE_ERROR=51,   // Unsupported multicast encrypted frame
      WLC_E_TRACE=52,                    //
      WLC_E_BTA_HCI_EVENT=53,            // BT-AMP HCI event
      WLC_E_IF=54,                       // I/F change (for wlan host notification)
      WLC_E_P2P_DISC_LISTEN_COMPLETE=55, // P2P Discovery listen state expires
      WLC_E_RSSI=56,                     // indicate RSSI change based on configured levels
      WLC_E_PFN_BEST_BATCHING=57,        // PFN best network batching event
      WLC_E_EXTLOG_MSG=58,               //
      WLC_E_ACTION_FRAME=59,             // Action frame reception
      WLC_E_ACTION_FRAME_COMPLETE=60,    // Action frame Tx complete
      WLC_E_PRE_ASSOC_IND=61,            // assoc request received
      WLC_E_PRE_REASSOC_IND=62,          // re-assoc request received
      WLC_E_CHANNEL_ADOPTED=63,          // channel adopted (xxx: obsoleted)
      WLC_E_AP_STARTED=64,               // AP started
      WLC_E_DFS_AP_STOP=65,              // AP stopped due to DFS
      WLC_E_DFS_AP_RESUME=66,            // AP resumed due to DFS
      WLC_E_WAI_STA_EVENT=67,            // WAI stations event
      WLC_E_WAI_MSG=68,                  // event encapsulating an WAI message
      WLC_E_ESCAN_RESULT=69,             // escan result event
      WLC_E_ACTION_FRAME_OFF_CHAN_COMPLETE=70,       // NOTE - This used to be WLC_E_WAKE_EVENT
      WLC_E_PROBRESP_MSG=71,             // probe response received
      WLC_E_P2P_PROBREQ_MSG=72,          // P2P Probe request received
      WLC_E_DCS_REQUEST=73,              //
      WLC_E_FIFO_CREDIT_MAP=74,          // credits for D11 FIFOs. [AC0,AC1,AC2,AC3,BC_MC,ATIM]
      WLC_E_ACTION_FRAME_RX=75,          // Received action frame event WITH wl_event_rx_frame_data_t header
      WLC_E_WAKE_EVENT=76,               // Wake Event timer fired, used for wake WLAN test mode
      WLC_E_RM_COMPLETE=77,              // Radio measurement complete
      WLC_E_HTSFSYNC=78,                 // Synchronize TSF with the host
      WLC_E_OVERLAY_REQ=79,              // request an overlay IOCTL/iovar from the host
      WLC_E_CSA_COMPLETE_IND=80,         //
      WLC_E_EXCESS_PM_WAKE_EVENT=81,     // excess PM Wake Event to inform host
      WLC_E_PFN_SCAN_NONE=82,            // no PFN networks around
      WLC_E_PFN_SCAN_ALLGONE=83,         // last found PFN network gets lost
      WLC_E_GTK_PLUMBED=84,              //
      WLC_E_ASSOC_IND_NDIS=85,           // 802.11 ASSOC indication for NDIS only
      WLC_E_REASSOC_IND_NDIS=86,         // 802.11 REASSOC indication for NDIS only
      WLC_E_ASSOC_REQ_IE=87,             //
      WLC_E_ASSOC_RESP_IE=88,            //
      WLC_E_ASSOC_RECREATED=89,          // association recreated on resume
      WLC_E_ACTION_FRAME_RX_NDIS=90,     // rx action frame event for NDIS only
      WLC_E_AUTH_REQ=91,                 // authentication request received
      WLC_E_TDLS_PEER_EVENT=92,          // discovered peer, connected/disconnected peer
      //WLC_E_MESH_DHCP_SUCCESS=92,      // DHCP handshake successful for a mesh interface. Note commented out as duplicate ID with previous in cypress code
      WLC_E_SPEEDY_RECREATE_FAIL=93,     // fast assoc recreation failed
      WLC_E_NATIVE=94,                   // port-specific event and payload (e.g. NDIS)
      WLC_E_PKTDELAY_IND=95,             // event for tx pkt delay suddently jump
      WLC_E_AWDL_AW=96,                  // AWDL AW period starts
      WLC_E_AWDL_ROLE=97,                // AWDL Master/Slave/NE master role event
      WLC_E_AWDL_EVENT=98,               // Generic AWDL event
      WLC_E_NIC_AF_TXS=99,               // NIC AF txstatus
      WLC_E_NAN=100,                     // NAN event
      WLC_E_BEACON_FRAME_RX=101,         //
      WLC_E_SERVICE_FOUND=102,           // desired service found
      WLC_E_GAS_FRAGMENT_RX=103,         // GAS fragment received
      WLC_E_GAS_COMPLETE=104,            // GAS sessions all complete
      WLC_E_P2PO_ADD_DEVICE=105,         // New device found by p2p offload
      WLC_E_P2PO_DEL_DEVICE=106,         // device has been removed by p2p offload
      WLC_E_WNM_STA_SLEEP=107,           // WNM event to notify STA enter sleep mode
      WLC_E_TXFAIL_THRESH=108,           // Indication of MAC tx failures (exhaustion of 802.11 retries) exceeding threshold(s)
      WLC_E_PROXD=109,                   // Proximity Detection event
      WLC_E_IBSS_COALESCE=110,           // IBSS Coalescing
      //WLC_E_MESH_PAIRED=110,           // Mesh peer found and paired     Note commented out as duplicate ID with previous in cypress code
      WLC_E_AWDL_RX_PRB_RESP=111,        // AWDL RX Probe response
      WLC_E_AWDL_RX_ACT_FRAME=112,       // AWDL RX Action Frames
      WLC_E_AWDL_WOWL_NULLPKT=113,       // AWDL Wowl nulls
      WLC_E_AWDL_PHYCAL_STATUS=114,      // AWDL Phycal status
      WLC_E_AWDL_OOB_AF_STATUS=115,      // AWDL OOB AF status
      WLC_E_AWDL_SCAN_STATUS=116,        // Interleaved Scan status
      WLC_E_AWDL_AW_START=117,           // AWDL AW Start
      WLC_E_AWDL_AW_END=118,             // AWDL AW End
      WLC_E_AWDL_AW_EXT=119,             // AWDL AW Extensions
      WLC_E_AWDL_PEER_CACHE_CONTROL0=120,//
      WLC_E_CSA_START_IND=121,           //
      WLC_E_CSA_DONE_IND=122,            //
      WLC_E_CSA_FAILURE_IND=123,         //
      WLC_E_CCA_CHAN_QUAL=124,           // CCA based channel quality report
      WLC_E_BSSID=125,                   // to report change in BSSID while roaming
      WLC_E_TX_STAT_ERROR=126,           // tx error indication
      WLC_E_BCMC_CREDIT_SUPPORT=127,     // credit check for BCMC supported
      WLC_E_PSTA_PRIMARY_INTF_IND=128,   // psta primary interface indication
      WLC_E_129=129,                     //
      WLC_E_BT_WIFI_HANDOVER_REQ=130,    // Handover Request Initiated
      WLC_E_SPW_TXINHIBIT=131,           // Southpaw TxInhibit notification
      WLC_E_FBT_AUTH_REQ_IND=132,        // FBT Authentication Request Indication
      WLC_E_RSSI_LQM=133,                // Enhancement addition for WLC_E_RSSI
      WLC_E_PFN_GSCAN_FULL_RESULT=134,   // Full probe/beacon (IEs etc) results
      WLC_E_PFN_SWC=135,                 // Significant change in rssi of bssids being tracked
      WLC_E_AUTHORIZED=136,              // a STA been authroized for traffic
      WLC_E_PROBREQ_MSG_RX=137,          // probe req with wl_event_rx_frame_data_t header
      WLC_E_PFN_SCAN_COMPLETE=138,       // PFN completed scan of network list
      WLC_E_RMC_EVENT=139,               // RMC Event
      WLC_E_DPSTA_INTF_IND=140,          // DPSTA interface indication
      WLC_E_RRM=141,                     // RRM Event
      WLC_E_142=142,                     //
      WLC_E_143=143,                     //
      WLC_E_144=144,                     //
      WLC_E_145=145,                     //
      WLC_E_ULP=146,                     // ULP entry event
      WLC_E_147=147,                     //
      WLC_E_148=148,                     //
      WLC_E_149=149,                     //
      WLC_E_150=150,                     //
      WLC_E_TKO=151,                     // TCP Keep Alive Offload Event
      WLC_E_LASTONE=152                  //
    );

  TWIFIEventSet = set of TWIFIEvent;

  PWIFIRequestItem = ^TWIFIRequestItem;

  wl_chanspec = integer;

  pwl_bss_info = ^wl_bss_info;
  wl_bss_info = record
      version : longword;                       // version field
      length : longword;                        // byte length of data in this record, starting at version and including IEs
      BSSID : ether_addr;                       // Unique 6-byte MAC address
      beacon_period : word;                     // Interval between two consecutive beacon frames. Units are Kusec
      capability : word;                        // Capability information
      SSID_len : byte;                          // SSID length
      SSID : array[0..31] of char;                // Array to store SSID

      // this is a sub struct in cypress driver.
          ratecount : longword;                 // Count of rates in this set
          rates : array[1..15] of byte;         // rates in 500kbps units, higher bit set if basic

      chanspec : wl_chanspec ;                   // Channel specification for basic service set
      atim_window : word;                       // Announcement traffic indication message window size. Units are Kusec
      dtim_period : byte;                       // Delivery traffic indication message period
      RSSI : integer;                           // receive signal strength (in dBm)
      phy_noise : shortint;                     // noise (in dBm)

      n_cap : byte;                             // BSS is 802.11N Capable
      nbss_cap : longword;                      // 802.11N BSS Capabilities (based on HT_CAP_*)
      ctl_ch : byte;                            // 802.11N BSS control channel number
      reserved32 : array[1..1] of longword;       // Reserved for expansion of BSS properties
      flags : byte;                             // flags
      reserved : array[1..3] of byte;           // Reserved for expansion of BSS properties
      basic_mcs : array[0..MCSSET_LEN-1] of byte; // 802.11N BSS required MCS set

      ie_offset : word;                         // offset at which IEs start, from beginning
      ie_length : longword;                     // byte length of Information Elements
      SNR : integer;                            // Average SNR(signal to noise ratio) during frame reception
  end;


  pwl_escan_result = ^wl_escan_result;
  wl_escan_result = record
    buflen : longword;
    version : longword;
    sync_id : word;
    bss_count : word;
    bss_info : array [1..1] of wl_bss_info; // used as pointer to a list.
  end;

  TWIFIScanUserCallback = procedure(ssid : string; ScanResultP : pwl_escan_result); cdecl;
  TWirelessEventCallback = procedure(Event : TWIFIEvent; EventRecordP : pwhd_event; RequestItemP : PWIFIRequestItem; datalength : longint);

  TWIFIRequestItem = record
    RequestID : Word;
    RegisteredEvents : TWIFIEventSet;
    MsgP : PIOCTL_MSG;
    Signal : TSemaphoreHandle;
    Callback : TWirelessEventCallback;
    UserDataP : Pointer;
    NextP : PWIFIRequestItem;
  end;

  TWIFIWorkerThread = class(TThread)
  private
    FRequestQueueP : PWIFIRequestItem;
    FLastRequestQueueP : PWIFIRequestItem;
    FQueueProtect : TCriticalSectionHandle;
    procedure ProcessDevicePacket(var responseP : PIOCTL_MSG;
                             NetworkEntryP : PNetworkEntry;
                             var bytesleft : longword;
                             var isfinished : boolean);
  public
    FWIFI : PWIFIDevice;
    constructor Create(CreateSuspended : Boolean; AWIFI : PWIFIDevice);
    destructor Destroy; override;
    function AddRequest(ARequestID : word; InterestedEvents : TWIFIEventSet;
                                   Callback : TWirelessEventCallback;
                                   UserDataP : Pointer) : PWIFIRequestItem;
    procedure DoneWithRequest(ARequestItemP : PWIFIRequestItem);
    function FindRequest(ARequestId : word) : PWIFIRequestItem;
    function FindRequestByEvent(AEvent : longword) : PWIFIRequestItem;
    procedure dumpqueue;
    procedure Execute; override;
  end;

  TWirelessReconnectionThread = class(TThread)
  private
    FConnectionLost : TSemaphoreHandle;
    FSSID : string;
    FBSSID : ether_addr;
    FUseBSSID : boolean;
    FKey : string;
    FCountry : string;
    FReconnectionType : TWIFIReconnectionType;
  public
    constructor Create;
    procedure SetConnectionDetails(aSSID, aKey, aCountry : string; BSSID : ether_addr; useBSSID : boolean; aReconnectionType : TWIFIReconnectionType);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  {$ifdef supplicant}
  TWPASupplicantThread = class(TThread)
  private
  public
    constructor Create;
    procedure Execute; override;
  end;
  {$endif}


  PCYW43455Network = ^TCYW43455Network;
  TCYW43455Network = record
   {Network Properties}
   Network:TNetworkDevice;
   {Driver Properties}

   ChipID:LongWord;
   ChipRevision:LongWord;
   PHYLock:TMutexHandle;
   ReceiveRequestSize:LongWord;                  {Size of each receive request buffer}
   TransmitRequestSize:LongWord;                 {Size of each transmit request buffer}
   ReceiveEntryCount:LongWord;                   {Number of entries in the receive queue}
   TransmitEntryCount:LongWord;                  {Number of entries in the transmit queue}
   ReceivePacketCount:LongWord;                  {Maximum number of packets per receive entry}
   TransmitPacketCount:LongWord;                 {Maximum number of packets per transmit entry}

   JoinCompleted: Boolean;
   OriginalHostStart: TSDHCIHostStart;
   {$ifdef supplicant}
   EAPOLCompleted : boolean;
   WPAKeys : array[0..1] of TWPAKey;
   NetworkUpSignal : TSemaphoreHandle;
   CountryCode : array[0..1] of char;
   {$endif}
  end;


function WIFIHostStart(SDHCI: PSDHCIHost): LongWord;

function WIFIDeviceCreate:PWIFIDevice;
function WIFIDeviceCreateEx(Size:LongWord):PWIFIDevice;
function WIFIDeviceDestroy(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceCheck(WIFI:PWIFIDevice):PWIFIDevice;
function WIFIDeviceInitialize(WIFI:PWIFIDevice):LongWord;

function WIFIDeviceRegister(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceFind(WIFIId:LongWord):PWIFIDevice;

function SDIOWIFIDeviceReset(WIFI:PWIFIDevice):LongWord;
function WIFIDeviceGoIdle(WIFI:PWIFIDevice):LongWord;
function SDWIFIDeviceSendInterfaceCondition(WIFI:PWIFIDevice):LongWord;
function SDIOWIFIDeviceSendOperationCondition(WIFI:PWIFIDevice;Probe:Boolean):LongWord;
function SDIOWIFIDeviceReadWriteDirect(WIFI:PWIFIDevice;Direction : TSDIODirection;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord;
function SDIOWIFIDeviceReadWriteExtended(WIFI:PWIFIDevice; Direction : TSDIODirection;
            Operation, Address : LongWord;
            Increment : Boolean; Buffer : Pointer;
            BlockCount, BlockSize : LongWord; trace : string =''): LongWord;
function WIFIDeviceSendCommand(WIFI:PWIFIDevice;Command:PSDIOCommand; txdata : PSDIOData = nil):LongWord;
function WIFIDeviceSetClock(WIFI:PWIFIDevice;Clock:LongWord):LongWord;
function WIFIDeviceSetBackplaneWindow(WIFI : PWIFIDevice; addr : longword) : longword;
function WIFIDeviceCoreScan(WIFI : PWIFIDevice) : longint;
procedure WIFIDeviceRamScan(WIFI : PWIFIDevice);
function WIFIDeviceDownloadFirmware(WIFI : PWIFIDevice) : Longword;

procedure sbreset(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);
procedure sbdisable(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);



function WIFIDeviceSendApplicationCommand(WIFI:PWIFIDevice;Command:PSDIOCommand):LongWord;

procedure WIFILogError(WIFI:PWIFIDevice;const AText:String); inline;
function WIFIDeviceSetIOS(WIFI:PWIFIDevice):LongWord;

{$ifdef supplicant}
{primary function for the user to call to join a wireless network}
function WirelessJoinNetwork(JoinType : TWIFIJoinType;
                             timeout : integer = 10000) : longword; cdecl;

{functions the supplicant needs to be able to call that are defined in this device driver}
procedure SendSupplicantL2Packet(PacketBufferP : PByte; Len : longword); cdecl; public name 'SendSupplicantL2Packet';
procedure UltiboTimeProc(var epoch : longint; var millisecond : integer); cdecl; public name 'UltiboTimeProc';
procedure UltiboMonotonicTimeProc(var epoch : longint; var millisecond : integer); cdecl; public name 'UltiboMonotonicTimeProc';
procedure WirelessScan(UserCallback : TWIFIScanUserCallback; WaitTime : Longint = 10000); cdecl; public name 'WirelessScan';
function SupplicantWirelessJoinNetwork(ssid : PChar; authkey : PByte;
                             authkeylen : longword;
                             bssid : Pether_addr;
                             usebssid : boolean = false) : longword; cdecl; public name 'SupplicantWirelessJoinNetwork';
procedure LockEAPOLPacketQueue(trace : pchar); cdecl; public name 'LockEAPOLPacketQueue';
procedure UnlockEAPOLPacketQueue; cdecl; public name 'UnlockEAPOLPacketQueue';
procedure LockSendPacketQueue; cdecl; public name 'LockSendPacketQueue';
procedure UnlockSendPacketQueue; cdecl; public name 'UnlockSendPacketQueue';
function UltiboSetKey(const ifname : PChar; priv : pointer; alg : byte;
		      const addr : PByte; key_idx : integer; set_tx : integer;
		      const seq : PByte; seq_len : integer;
		      const key : PByte; key_len : integer) : integer; cdecl; public name 'UltiboSetKey';
procedure UltiboEAPOLComplete; cdecl; public name 'UltiboEAPOLComplete';
function UltiboSetCountry(priv : pointer; ccode : PChar) : integer; cdecl; public name 'UltiboSetCountry';


{$else}
procedure WirelessScan(UserCallback : TWIFIScanUserCallback; WaitTime : Longint = 10000); cdecl;
function WirelessJoinNetwork(ssid : string; security_key : string;
                             countrycode : string;
                             JoinType : TWIFIJoinType;
                             ReconnectType : TWIFIReconnectionType;
                             bssid : ether_addr;
                             usebssid : boolean = false) : longword; cdecl;
{$endif}
function WirelessLeaveNetwork : longword;
procedure WIFIPreInit;
procedure WIFIInit;


var
  WIFI_LOG_ENABLED : boolean = true;

  // this will be moved to the implementation section eventually
  // just here because we currently need to create it externally
  // and that is because the SDHCI and WIFI Device are being created externally.
  WIFIWorkerThread : TWIFIWorkerThread;

var
  // Auto init variable, move to GlobalConfig or restructure during integration
  WIFI_AUTO_INIT : Boolean = False; //True; // Don't auto init during development

  // defines whether to inject waits into packet transmission when the firmware
  // credit value indicates internal buffers are filling up.
  CYW43455_USE_FIRMWARE_CREDIT_VALUE : Boolean = TRUE;

  {$ifdef supplicant}
  // Log level as used by the supplicant. Enables us to control it from the device
  // driver source instead of having to change the supplicant source.
  WPASupplicantLogLevel : integer = MSG_INFO; cvar;
  {$endif}



implementation

const
  {WIFI logging}
  WIFI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {WIFI debugging messages}
  WIFI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {WIFI informational messages, such as a device being attached or detached}
  WIFI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {WIFI warning messages}
  WIFI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {WIFI error messages}
  WIFI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No WIFI messages}


{$ifdef supplicant}


const
  {wpa supplicant crypto algorithms}
  WPA_ALG_NONE = 0;
  WPA_ALG_WEP = 1;
  WPA_ALG_TKIP = 2;
  WPA_ALG_CCMP = 3;
  WPA_ALG_IGTK = 4;
  WPA_ALG_PMK = 5;
  WPA_ALG_GCMP = 6;
  WPA_ALG_SMS4 = 7;
  WPA_ALG_KRK = 8;
  WPA_ALG_GCMP_256 = 9;
  WPA_ALG_CCMP_256 = 10;
  WPA_ALG_BIP_GMAC_128 = 11;
  WPA_ALG_BIP_GMAC_256 = 12;
  WPA_ALG_BIP_CMAC_256 = 13;

{$endif}

var
  WIFI_DEFAULT_LOG_LEVEL:LongWord = WIFI_LOG_LEVEL_DEBUG; {Minimum level for WIFI messages.  Only messages with level greater than or equal to this will be printed}
  WIFIDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;

  WIFIDeviceTable:PWIFIDevice;
  WIFIDeviceTableCount:LongWord;

  WIFIInitialized:Boolean;

  dodumpregisters : boolean = false;

  firmware : array[1..FIRWMARE_OPTIONS_COUNT] of TFirmwareEntry =
    (
      	    ( chipid : $4330; chipidrev : 3; firmwarefilename: 'fw_bcm40183b1.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename : ''),
    	    ( chipid : $4330; chipidrev : 4; firmwarefilename: 'fw_bcm40183b2.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename : ''),
    	    ( chipid : 43362; chipidrev : 0; firmwarefilename: 'fw_bcm40181a0.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename : ''),
    	    ( chipid : 43362; chipidrev : 1; firmwarefilename: 'fw_bcm40181a2.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename : ''),
    	    ( chipid : 43430; chipidrev : 1; firmwarefilename: 'brcmfmac43430-sdio.bin'; configfilename: 'brcmfmac43430-sdio.txt'; regufilename : ''),
{Pi Zero2W} ( chipid : 43430; chipidrev : 2; firmwarefilename: 'brcmfmac43436-sdio.bin'; configfilename: 'brcmfmac43436-sdio.txt'; regufilename : 'brcmfmac43436-sdio.clm_blob'),
    	    ( chipid : $4345; chipidrev : 6; firmwarefilename: 'brcmfmac43455-sdio.bin'; configfilename: 'brcmfmac43455-sdio.txt'; regufilename : 'brcmfmac43455-sdio.clm_blob'),
    	    ( chipid : $4345; chipidrev : 9; firmwarefilename: 'brcmfmac43456-sdio.bin'; configfilename: 'brcmfmac43456-sdio.txt'; regufilename : 'brcmfmac43456-sdio.clm_blob')
    );


  ioctl_txmsg, ioctl_rxmsg : IOCTL_MSG;
  txglom : boolean = false; // don't know what this is for yet.
  ioctl_reqid : longword = 1; // ioct request id used to match request to response.
                              // starts at 1 because 0 is reserved for an event entry.

  txseq : byte = 1; // ioctl tx sequence number.

  SDIOProtect : TSpinHandle;

  //FoundSDHCI : PSDHCIHost = nil; // don't really like this but only way at the moment.
  WIFI:PWIFIDevice;
  macaddress : TMACAddress;
  BackgroundJoinThread : TWirelessReconnectionThread = nil;

{$ifdef supplicant}
  EAPOLQueueLock : TRTLCriticalSection;
//  L2SendQueueLock : TRTLCriticalSection;
  SupplicantOperatingState : integer; cvar; external;
  WPASupplicantThread : TWPASupplicantThread;


{functions we need to call that are defined by wpa_supplicant}
procedure SetUltiboMacAddress(AMacAddress : PChar); cdecl; external;
function wpa_supplicant_main(confname : PChar) : integer; cdecl external;
procedure ultibo_driver_new_packet_data(srcaddr : pbyte; packetbuf : pbyte; len : word); cdecl; external;
procedure DoNetworkNotify(data : pointer); forward;
procedure UltiboEloopTerminate; cdecl; external;

{$endif}

procedure sbenable(WIFI : PWIFIDevice); forward;
function WirelessInit(WIFI : PWIFIDevice) : longword; forward;
procedure WIFILogInfo(WIFI: PWIFIDevice;const AText:String); forward;


procedure hexdump(p : pbyte; len : word; title : string = '');
var
  rows : integer;
  remainder : integer;
  i : integer;

  function line(bytecount : integer) : string;
  var
    s : string;
    asc : string;
    j : integer;
    b : byte;
  begin
     s := '';
     asc := '';

     s := s + inttohex(i*16, 4) + ' ';
     for j := 0 to bytecount-1 do
     begin
       b := (p+(i*16)+j)^;
       s := s + inttohex(b, 2) +' ' ;
       if (b in [28..126]) then
         asc := asc + chr(b)
       else
         asc := asc + '.';
     end;

     if (bytecount < 16) then
       for j := 15 downto bytecount do
         s := s + '   ';

     s := s + ' ' + asc;

     Result := s;
  end;

begin
  rows := len div 16;
  remainder := len mod 16;

  if (title <> '') then
    WIFILogInfo(nil, title + ' @ address 0x'+inttohex(longword(p),8) + ' for ' + inttostr(len) + ' bytes')
  else
    WIFILogInfo(nil, 'hexdump @ address 0x'+inttohex(longword(p),8) + ' for ' + inttostr(len) + ' bytes');
  for i := 0 to rows-1 do
  begin
     WIFILogInfo(nil, line(16));
  end;

  if (remainder > 0) then
  begin
    i:= rows;
    WIFILogInfo(nil, line(remainder));
  end;
end;

function NetSwapLong(v : longword) : longword; inline;
begin
  Result:= ((v and $ff) << 24) or ((v and $ff00) << 8) or ((v and $ff0000) >> 8) or ((v and $ff000000) >> 24);
end;

function NetSwapWord(v : word) : word; inline;
begin
 Result := ((v and $ff) << 8) or ((v and $ff00) >> 8);
end;

function buftostr(bufferp : pbyte; messagelen : word; nullterminated : boolean = false) : string;
var
 i : integer;
 b : byte;
begin
 try
  Result := '';
  for i := 0 to messagelen-1 do
  begin
    b := (bufferp+i)^;

    if (nullterminated) and (b = 0) then
      exit;

    if (b in [32..126]) then
      Result := Result + chr(b)
    else
      Result := Result + '[#'+inttostr(b)+']';
  end;

 except
   on e : exception do
     WIFILogError(nil, 'exception in buftostr ' + e.message + ' i='+inttostr(i));
 end;
end;

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

procedure dumpregisters(WIFI : PWIFIDevice);
type
  arrayptr = ^arraytype;
  arraytype = array[0..99] of longword;
  arrayptrbytes = ^arraybytes;
  arraybytes = array[0..1000] of byte;
var
  SDHCI : PSDHCIHost;
  r : arrayptr;
  rb : arrayptrbytes;
begin
   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);

    r := arrayptr(SDHCI^.address);
    rb := arrayptrbytes(SDHCI^.address);


    WIFILogInfo(nil, '32 bit block count 0x'+ inttohex(r^[0], 8));
    WIFILogInfo(nil, 'blocksize and count 0x'+ inttohex(r^[1], 8));
    WIFILogInfo(nil, 'arg 0x%x'+ inttohex(r^[2], 8));
    WIFILogInfo(nil, 'transfermode and command 0x'+ inttohex(r^[3], 8));
    WIFILogInfo(nil, 'response0 0x'+ inttohex(r^[4], 8));
    WIFILogInfo(nil, 'response1 0x'+ inttohex(r^[5], 8));
    WIFILogInfo(nil, 'response2 0x'+ inttohex(r^[6], 8));
    WIFILogInfo(nil, 'response3 0x'+ inttohex(r^[7], 8));

    WIFILogInfo(nil, 'present state 0x'+ inttohex(r^[9], 8));
    WIFILogInfo(nil, 'host ctrl1, pwr ctrl, block gap ctrl, wakeup ctrl0x'+ inttohex(r^[10], 8));


    WIFILogInfo(nil, 'host ctrl1 0x' +inttohex(rb^[SDHCI_HOST_CONTROL], 2));
    WIFILogInfo(nil, 'pwr ctrl 0x' +inttohex(rb^[SDHCI_POWER_CONTROL], 2));
    WIFILogInfo(nil, 'block gap ctrl 0x' +inttohex(rb^[SDHCI_BLOCK_GAP_CONTROL], 2));
    WIFILogInfo(nil, 'wakeup ctrl 0x' +inttohex(rb^[SDHCI_WAKE_UP_CONTROL], 2));

    WIFILogInfo(nil, 'clock ctrl, timeout ctrl, sw reset 0x'+ inttohex(r^[11], 8));

    WIFILogInfo(nil, 'clock ctrl byte 1 0x' +inttohex(rb^[SDHCI_CLOCK_CONTROL], 2));
    WIFILogInfo(nil, 'clock ctrl byte 2 0x' +inttohex(rb^[SDHCI_CLOCK_CONTROL+1], 2));
    WIFILogInfo(nil, 'timeout ctrl 0x' +inttohex(rb^[SDHCI_TIMEOUT_CONTROL], 2));
    WIFILogInfo(nil, 'sw reset 0x' +inttohex(rb^[SDHCI_SOFTWARE_RESET], 2));


    WIFILogInfo(nil, 'normal interrupt status, error interrupt status 0x'+ inttohex(r^[12], 8));
    WIFILogInfo(nil, 'normal interr enable, error interr enable 0x'+ inttohex(r^[13], 8));
    WIFILogInfo(nil, 'auto cmd status, host ctrl 2 0x'+ inttohex(r^[14], 8));
    WIFILogInfo(nil, 'capabilities part 1 0x'+ inttohex(r^[15], 8));
    WIFILogInfo(nil, 'capabilities part 2 0x'+ inttohex(r^[16], 8));
end;

{==============================================================================}

function WIFIHostStart(SDHCI: PSDHCIHost): LongWord;
// Overwridden SDHCIHostStart method for the Arasan SDHCI controller
var
  {$IFNDEF RPI4}
  i : integer;
  {$ENDIF}
  Network: PCYW43455Network;
  
  Capabilities:LongWord;
begin
  Result := ERROR_INVALID_PARAMETER;
  
  // Check SDHCI
  if SDHCI = nil then
    Exit;
   
  // Check Device Data 
  if SDHCI^.Device.DeviceData = nil then
    Exit;
   
  // Get Network   
  Network := PCYW43455Network(SDHCI^.Device.DeviceData);
  
  // Call original Host Start
  if Assigned(Network^.OriginalHostStart) then
  begin
    Result := Network^.OriginalHostStart(SDHCI);
    if Result <> ERROR_SUCCESS then
      Exit;
  end;  

  {$IFNDEF RPI4}
  // disconnect emmc from SD card (connect sdhost instead)
  // ZeroW/3B/3B+/3A+
  for i := 48 to 53 do
    GPIOFunctionSelect(i,GPIO_FUNCTION_ALT0);

  // connect emmc to wifi
  // ZeroW/3B/3B+/3A+
  for i := 34 to 39 do
  begin
    GPIOFunctionSelect(i,GPIO_FUNCTION_ALT3);

    if (i = 34) then
      GPIOPullSelect(i, GPIO_PULL_NONE)
    else
      GPIOPullSelect(i, GPIO_PULL_UP);
  end;
  {$ENDIF}

  {$IFNDEF RPI4}
  // init 32khz oscillator (WIFI_CLK)
  // ZeroW/3B/3B+/3A+
  GPIOPullSelect(SD_32KHZ_PIN, GPIO_PULL_NONE);
  GPIOFunctionSelect(SD_32KHZ_PIN, GPIO_FUNCTION_ALT0);
  {$ENDIF}

  {$IFDEF RPI}
  // turn on wlan power (WL_ON)
  // ZeroW
  GPIOFunctionSelect(WLAN_ON_PIN, GPIO_FUNCTION_OUT);
  GPIOOutputSet(WLAN_ON_PIN, GPIO_LEVEL_HIGH);
  {$ENDIF}
  {$IF DEFINED(RPI3) or DEFINED(RPI4)}
  // 3B/3B+/3A+/4B
  // turn on wlan power (WL_ON)
  VirtualGPIOFunctionSelect(WLAN_ON_PIN, GPIO_FUNCTION_OUT);
  VirtualGPIOOutputSet(WLAN_ON_PIN, GPIO_LEVEL_HIGH);
  {$ENDIF}

  // Perform operations normally done by SDHCIHostStart
  {Get Capabilities}
  Capabilities:=SDHCIHostReadLong(SDHCI,SDHCI_CAPABILITIES);
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Capabilities = ' + IntToHex(Capabilities,8));
  {$ENDIF}
                          
  {Check DMA Support}
  if ((Capabilities and SDHCI_CAN_DO_SDMA) = 0) and ((SDHCI^.Quirks and SDHCI_QUIRK_MISSING_CAPS) = 0) then
   begin
    if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Host does not support SDMA');
    
    SDHCI^.HostStop(SDHCI);
    Exit;
   end;
  
  {Check Clock Maximum}
  if SDHCI^.ClockMaximum <> 0 then
   begin
    SDHCI^.MaximumFrequency:=SDHCI^.ClockMaximum;
   end
  else
   begin
    if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
     begin
      SDHCI^.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_V3_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
     end
    else
     begin
      SDHCI^.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
     end;    
    SDHCI^.MaximumFrequency:=(SDHCI^.MaximumFrequency * SDHCI_CLOCK_BASE_MULTIPLIER);
   end;
  if SDHCI^.MaximumFrequency = 0 then
   begin
    if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Host does not specify a maximum clock frequency');
    
    SDHCI^.HostStop(SDHCI);
    Exit;
   end;
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum frequency = ' + IntToStr(SDHCI^.MaximumFrequency));
  {$ENDIF}
   
  {Check Clock Minimum}
  if SDHCI^.ClockMinimum <> 0 then
   begin
    SDHCI^.MinimumFrequency:=SDHCI^.ClockMinimum;
   end
  else
   begin
    if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
     begin
      SDHCI^.MinimumFrequency:=SDHCI^.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
     end
    else
     begin
      SDHCI^.MinimumFrequency:=SDHCI^.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
     end;    
   end;  
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host minimum frequency = ' + IntToStr(SDHCI^.MinimumFrequency));
  {$ENDIF}
  
  {Determine Voltages}
  SDHCI^.Voltages:=0;
  if (Capabilities and SDHCI_CAN_VDD_330) <> 0 then
   begin
    SDHCI^.Voltages:=SDHCI^.Voltages or MMC_VDD_32_33 or MMC_VDD_33_34;
   end;
  if (Capabilities and SDHCI_CAN_VDD_300) <> 0 then
   begin
    SDHCI^.Voltages:=SDHCI^.Voltages or MMC_VDD_29_30 or MMC_VDD_30_31;
   end;
  if (Capabilities and SDHCI_CAN_VDD_180) <> 0 then
   begin
    SDHCI^.Voltages:=SDHCI^.Voltages or MMC_VDD_165_195;
   end;
  {Check Presets}
  if SDHCI^.PresetVoltages <> 0 then
   begin
    SDHCI^.Voltages:=SDHCI^.Voltages or SDHCI^.PresetVoltages;
   end;
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host voltages = ' + IntToHex(SDHCI^.Voltages,8));
  {$ENDIF}
   
  {Determine Capabilities}
  SDHCI^.Capabilities:=MMC_CAP_SD_HIGHSPEED or MMC_CAP_MMC_HIGHSPEED;
  if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
   begin
    if (Capabilities and SDHCI_CAN_DO_8BIT) <> 0 then
     begin
      SDHCI^.Capabilities:=SDHCI^.Capabilities or MMC_CAP_8_BIT_DATA;
     end;
   end;
  {Check Presets}
  if SDHCI^.PresetCapabilities <> 0 then
   begin
    SDHCI^.Capabilities:=SDHCI^.Capabilities or SDHCI^.PresetCapabilities;
   end;
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host capabilities = ' + IntToHex(SDHCI^.Capabilities,8));
  {$ENDIF}
  
  {Determine Maximum Blocks}
  SDHCI^.MaximumBlockCount:=MMC_MAX_BLOCK_COUNT;
  {$IFDEF MMC_DEBUG}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum blocks = ' + IntToStr(SDHCI^.MaximumBlockCount));
  {$ENDIF}

  {$ifdef supplicant}
  Network^.EAPOLCompleted:=false;
  Network^.NetworkUpSignal := SemaphoreCreate(0);
  {$endif}

  {Host reset done by host start}
    
  {Enable Host}
  SDHCI^.SDHCIState:=SDHCI_STATE_ENABLED;
   
  {Notify Enable}
  NotifierNotify(@SDHCI^.Device,DEVICE_NOTIFICATION_ENABLE);
    
  Result:=ERROR_SUCCESS;  
end;

{==============================================================================}


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
 Result^.DeviceInitialize:=nil;
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

  WIFIDeviceRegister(Result);
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

function SDHCIEnum(SDHCI:PSDHCIHost;Data:Pointer):LongWord;

const
  {$IFDEF RPI}
  SDHCI_DESCRIPTION = BCM2708_EMMC_DESCRIPTION;
  {$ENDIF}
  {$IFDEF RPI3}
  SDHCI_DESCRIPTION = BCM2710_EMMC_DESCRIPTION;
  {$ENDIF}
  {$IFDEF RPI4}
  SDHCI_DESCRIPTION = BCM2711_EMMC0_DESCRIPTION;
  {$ENDIF}
 
begin
  Result := ERROR_INVALID_PARAMETER;
  
  if SDHCI = nil then
    Exit;
    
  if Data = nil then
    Exit;
  
  Result := ERROR_SUCCESS;
  
  if SDHCI^.Device.DeviceDescription = SDHCI_DESCRIPTION then
  begin
    PCYW43455Network(Data)^.Network.Device.DeviceData := SDHCI;
  end;  
end;

function CYW43455DeviceOpen(Network:PNetworkDevice):LongWord;
var
 Status : longword;
 Entry:PNetworkEntry;
begin
 Result := ERROR_INVALID_PARAMETER;
 
 WIFI:=WIFIDeviceCreate;
 if WIFI = nil then
  begin
   WIFILogError(nil,'Failed to create new WIFI device');
   Exit;
  end;

 if WIFI_LOG_ENABLED then WIFILogInfo(nil,'Update WIFI Device');

 if Network^.Device.DeviceData = nil then
 begin
   WIFILogError(nil,'There was no SDHCI Device to initialise the WIFI device with');
   exit;
 end;

 WIFI^.ReceiveGlomPacketCount:=0;
 WIFI^.ReceiveGlomPacketSize:=0;

 WIFI^.Device.DeviceBus:=DEVICE_BUS_SD;
 WIFI^.Device.DeviceType:=WIFI_TYPE_SDIO;
 WIFI^.Device.DeviceFlags:=WIFI_FLAG_NONE;
 WIFI^.Device.DeviceData:= Network^.Device.DeviceData;
 WIFI^.Device.DeviceDescription:='Cypress CYW34355 WIFI Device';

 WIFI^.DeviceInitialize:=nil;

 if WIFI_LOG_ENABLED then WIFILogInfo(nil,'WIFIDeviceInitialize');

 {Initialize WIFI}
 Status:=WIFIDeviceInitialize(WIFI);
 if (Status <> WIFI_STATUS_SUCCESS)then
  begin
   WIFILogError(nil,'WIFI Failed to initialize new WIFI device ' + inttostr(status));
   WIFIDeviceDestroy(WIFI);
  end;

 // set buffering sizes
 PCYW43455Network(Network)^.ReceiveRequestSize:=RECEIVE_REQUEST_PACKET_COUNT * sizeof(IOCTL_MSG); // space for 16 reads; actually more than that as a packet can't be as large as the IOCTL msg allocates
 PCYW43455Network(Network)^.TransmitRequestSize:=ETHERNET_MAX_PACKET_SIZE; //+ LAN78XX_TX_OVERHEAD; don't know what overhead we have yet.
 PCYW43455Network(Network)^.ReceiveEntryCount:=40;
 PCYW43455Network(Network)^.TransmitEntryCount:=4;
 PCYW43455Network(Network)^.ReceivePacketCount:=RECEIVE_REQUEST_PACKET_COUNT * IOCTL_MAX_BLKLEN div (ETHERNET_MIN_PACKET_SIZE);
 PCYW43455Network(Network)^.TransmitPacketCount:=1;

 WIFI^.NetworkP := Network;



 {Allocate Receive Queue Buffer}
 Network^.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PCYW43455Network(Network)^.ReceiveEntryCount);
 if Network^.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(Network,'LAN78XX: Failed to create receive queue buffer');

   Exit;
  end;

 {Allocate Receive Queue Semaphore}
 Network^.ReceiveQueue.Wait:=SemaphoreCreate(0);
 if Network^.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(Network,'LAN78XX: Failed to create receive queue semaphore');

   Exit;
  end;

 {Allocate Receive Queue Buffers}
 Entry:=BufferIterate(Network^.ReceiveQueue.Buffer,nil);
 while Entry <> nil do
  begin
   {Initialize Entry}
   Entry^.Size:=PCYW43455Network(Network)^.ReceiveRequestSize;

   // pi zero has a different data offset as the header is 4 bytes longer.
   // actually a firmware version thing. Other versions may be different too.
   if (WIFI^.ChipId = CHIP_ID_PI_ZEROW) and (WIFI^.ChipIdRev = 1) then
     Entry^.Offset:= ETHERNET_HEADER_BYTES + IOCL_LEN_BYTES + 4
   else
     Entry^.Offset:= ETHERNET_HEADER_BYTES + IOCL_LEN_BYTES; // + SDPCM_HEADER_SIZE + CDC_HEADER_SIZE;  // packet data starts after this point.

   Entry^.Count:=0;

   {Allocate Request Buffer}
   Entry^.Buffer:= GetMem(Entry^.Size); //may need something different to getmem here?
   if Entry^.Buffer = nil then
    begin
     if WIFI_LOG_ENABLED then WIFILogError(nil,'CY43455: Failed to allocate receive buffer');

     Exit;
    end;

   {Initialize Packets}
   SetLength(Entry^.Packets,PCYW43455Network(Network)^.ReceivePacketCount);

   {Initialize First Packet}
   Entry^.Packets[0].Buffer:=Entry^.Buffer;
   Entry^.Packets[0].Data:=Entry^.Buffer + Entry^.Offset;
   Entry^.Packets[0].Length:=Entry^.Size - Entry^.Offset;

   Entry:=BufferIterate(Network^.ReceiveQueue.Buffer,Entry);
  end;

 {Allocate Receive Queue Entries}
 SetLength(Network^.ReceiveQueue.Entries,PCYW43455Network(Network)^.ReceiveEntryCount);

 {Allocate Transmit Queue Buffer}
 Network^.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PCYW43455Network(Network)^.TransmitEntryCount);
 if Network^.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'CY43455: Failed to create transmit queue buffer');

   Exit;
  end;

 {Allocate Transmit Queue Semaphore}
 Network^.TransmitQueue.Wait:=SemaphoreCreate(PCYW43455Network(Network)^.TransmitEntryCount);
 if Network^.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'CY43455: Failed to create transmit queue semaphore');

   Exit;
  end;

 {Allocate Transmit Queue Buffers}
 Entry:=BufferIterate(Network^.TransmitQueue.Buffer,nil);
 while Entry <> nil do
  begin
   {Initialize Entry}
   Entry^.Size:=PCYW43455Network(Network)^.TransmitRequestSize;
   Entry^.Offset:=sizeof(SDPCM_HEADER)+8;
   Entry^.Count:=PCYW43455Network(Network)^.TransmitPacketCount;

   {Allocate Request Buffer}
   Entry^.Buffer:=GetMem(Entry^.Size);
   if Entry^.Buffer = nil then
    begin
     if WIFI_LOG_ENABLED then WIFILogError(nil,'CY43455: Failed to allocate wifi transmit buffer');

     Exit;
    end;

   {Initialize Packets}
   SetLength(Entry^.Packets,PCYW43455Network(Network)^.TransmitPacketCount);

   {Initialize First Packet}
   Entry^.Packets[0].Buffer:=Entry^.Buffer;
   Entry^.Packets[0].Data:=Entry^.Buffer + Entry^.Offset;
   Entry^.Packets[0].Length:=Entry^.Size - Entry^.Offset;

   Entry:=BufferIterate(Network^.TransmitQueue.Buffer,Entry);
  end;

 {Allocate Transmit Queue Entries}
 SetLength(Network^.TransmitQueue.Entries,PCYW43455Network(Network)^.TransmitEntryCount);

 // create the receive thread

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Creating WIFI Worker Thread');

 WIFIWorkerThread := TWIFIWorkerThread.Create(true, WIFI);
 WIFIWorkerThread.Start;

 BackgroundJoinThread := TWirelessReconnectionThread.Create;

 // call initialisation (loads country code etc)
 // note this code requires the worker thread to be running otherwise
 // responses won't be received.
 WirelessInit(WIFI);

 {$ifdef supplicant}
 //the wpa_supplicant must now be initialized, once we have a mac address (which is done in wirelessinit)
 //the supplicant runs its own event loop, so it runs in a thread to allow this.
 //later on we could consider moving the event loop into the thread execute and getting
 //rid of some C code.
 WPASupplicantThread := TWPASupplicantThread.Create;
 {$endif}


 {Set State to Open}
 Network^.NetworkState := NETWORK_STATE_OPEN;

 {Notify the State}
 NotifierNotify(@Network^.Device, DEVICE_NOTIFICATION_OPEN);

 Result := ERROR_SUCCESS;
end;

function CYW43455DeviceClose(Network:PNetworkDevice):LongWord;
begin
  if PCYW43455Network(Network)^.JoinCompleted then
    WirelessLeaveNetwork;

  {Set State to Closed}
  Network^.NetworkState := NETWORK_STATE_CLOSED;
 
  {Notify the State}
  NotifierNotify(@Network^.Device, DEVICE_NOTIFICATION_CLOSE); 

  Result := ERROR_SUCCESS;
end;

function CYW43455DeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
 var
  Status:LongWord;
  Device:PSDHCIHost;
begin

  {}
  Result:=ERROR_INVALID_PARAMETER;

  {Check Network}
  if Network = nil then Exit;
  if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

  {$IF DEFINED(LAN78XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'LAN78XX: Network Control');
  {$ENDIF}

  {Get Device}
  Device:=PSDHCIHost(Network^.Device.DeviceData);
  if Device = nil then Exit;

  {Acquire the Lock}
  if MutexLock(Network^.Lock) = ERROR_SUCCESS then
   begin
    try
     {Set Result}
     Result:=ERROR_OPERATION_FAILED;
     Status:=WIFI_STATUS_SUCCESS;

     {Check Request}
     case Request of
      NETWORK_CONTROL_CLEAR_STATS:begin
        {Clear Statistics}
        {Network}
        Network^.ReceiveBytes:=0;
        Network^.ReceiveCount:=0;
        Network^.ReceiveErrors:=0;
        Network^.TransmitBytes:=0;
        Network^.TransmitCount:=0;
        Network^.TransmitErrors:=0;
        Network^.StatusCount:=0;
        Network^.StatusErrors:=0;
        Network^.BufferOverruns:=0;
        Network^.BufferUnavailable:=0;
       end;
      NETWORK_CONTROL_GET_STATS:begin
        {Get Statistics}
        if Argument2 < SizeOf(TNetworkStatistics) then Exit;

        {Network}
        PNetworkStatistics(Argument1)^.ReceiveBytes:=Network^.ReceiveBytes;
        PNetworkStatistics(Argument1)^.ReceiveCount:=Network^.ReceiveCount;
        PNetworkStatistics(Argument1)^.ReceiveErrors:=Network^.ReceiveErrors;
        PNetworkStatistics(Argument1)^.TransmitBytes:=Network^.TransmitBytes;
        PNetworkStatistics(Argument1)^.TransmitCount:=Network^.TransmitCount;
        PNetworkStatistics(Argument1)^.TransmitErrors:=Network^.TransmitErrors;
        PNetworkStatistics(Argument1)^.StatusCount:=Network^.StatusCount;
        PNetworkStatistics(Argument1)^.StatusErrors:=Network^.StatusErrors;
        PNetworkStatistics(Argument1)^.BufferOverruns:=Network^.BufferOverruns;
        PNetworkStatistics(Argument1)^.BufferUnavailable:=Network^.BufferUnavailable;
       end;
      NETWORK_CONTROL_SET_MAC:begin
        {Set the MAC for this device}
        {$ifdef CYW43455_DEBUG}
        if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'set mac address to '
                         + inttohex(phardwareaddress(argument1)^[0], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[1], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[2], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[3], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[4], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[5], 2)
                         );
        {$endif}
        Status := WIFI_STATUS_SUCCESS;
       end;
      NETWORK_CONTROL_GET_MAC:begin
        {Get the MAC for this device}
        move(macaddress[0], PHardwareAddress(Argument1)^, sizeof(THardwareAddress));
        Status := WIFI_STATUS_SUCCESS;
       end;
      NETWORK_CONTROL_SET_LOOPBACK:begin
        {Set Loopback Mode}
        // loopback mode not supported yet.
       end;
      NETWORK_CONTROL_RESET:begin
        {Reset the device}
        //To Do
       end;
      NETWORK_CONTROL_DISABLE:begin
        {Disable the device}
        //To Do
       end;
      NETWORK_CONTROL_GET_HARDWARE:begin
        {Get Hardware address for this device}
       move(macaddress[0], PHardwareAddress(Argument1)^, sizeof(THardwareAddress));
       Status := WIFI_STATUS_SUCCESS;
       end;
      NETWORK_CONTROL_GET_BROADCAST:begin
        {Get Broadcast address for this device}
        PHardwareAddress(Argument1)^:=ETHERNET_BROADCAST;
       end;
      NETWORK_CONTROL_GET_MTU:begin
        {Get MTU for this device}
        Argument2:=ETHERNET_MTU;
       end;
      NETWORK_CONTROL_GET_HEADERLEN:begin
        {Get Header length for this device}
        Argument2:=ETHERNET_HEADER_SIZE;
       end;
      NETWORK_CONTROL_GET_LINK:begin
        {Get Link State for this device}
        if PCYW43455Network(Network)^.JoinCompleted then
          Argument2 := NETWORK_LINK_UP
        else
          Argument2 := NETWORK_LINK_DOWN;
       end;
      else
       begin
        Exit;
       end;
     end;

     {Check Status}
     if Status <> WIFI_STATUS_SUCCESS then Exit;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Release the Lock}
     MutexUnlock(Network^.Lock);
    end;
   end
  else
   begin
    Result:=ERROR_CAN_NOT_COMPLETE;
   end;

 Result := ERROR_SUCCESS;
end;

function CYW43455BufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(LAN78XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'LAN78XX: Buffer Allocate');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Set Result}
 Result:=ERROR_OPERATION_FAILED;

 {Wait for Entry (Transmit Buffer)}
 Entry:=BufferGet(Network^.TransmitQueue.Buffer);
 if Entry <> nil then
  begin
   {Update Entry}
   Entry^.Size:=PCYW43455Network(Network)^.TransmitRequestSize;
   Entry^.Offset:=sizeof(SDPCM_HEADER)+8;
   Entry^.Count:=PCYW43455Network(Network)^.TransmitPacketCount;

   {Update First Packet}
   Entry^.Packets[0].Buffer:=Entry^.Buffer;
   Entry^.Packets[0].Data:=Entry^.Buffer + Entry^.Offset;
   Entry^.Packets[0].Length:=Entry^.Size - Entry^.Offset;

   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if WIFI_LOG_ENABLED then
   begin
     WIFILogDebug(nil,'CYW43455:  Entry.Size = ' + IntToStr(Entry^.Size));
     WIFILogDebug(nil,'CYW43455:  Entry.Offset = ' + IntToStr(Entry^.Offset));
     WIFILogDebug(nil,'CYW43455:  Entry.Count = ' + IntToStr(Entry^.Count));
     WIFILogDebug(nil,'CYW43455:  Entry.Packets[0].Length = ' + IntToStr(Entry^.Packets[0].Length));
   end;
   {$ENDIF}

   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
   WIFILogError(nil, 'Failed to get a transmit buffer!');

end;

function CYW43455BufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'CYW43455: Buffer Release');
 {$ENDIF}

 {Check Entry}
 if Entry = nil then Exit;

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Acquire the Lock}
 if MutexLock(Network^.Lock) = ERROR_SUCCESS then
  begin
   try
    {Free Entry (Receive Buffer)}
    Result:=BufferFree(Entry);
   finally
    {Release the Lock}
    MutexUnlock(Network^.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

function CYW43455BufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'CYW43455: Buffer Receive');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Wait for Entry}
 if SemaphoreWait(Network^.ReceiveQueue.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network^.Lock) = ERROR_SUCCESS then
    begin
     try
      {Remove Entry}
      Entry:=Network^.ReceiveQueue.Entries[Network^.ReceiveQueue.Start];

      {Update Start}
      Network^.ReceiveQueue.Start:=(Network^.ReceiveQueue.Start + 1) mod PCYW43455Network(Network)^.ReceiveEntryCount;

      {Update Count}
      Dec(Network^.ReceiveQueue.Count);

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Network^.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

function CYW43455BufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
var
 Empty:Boolean;
begin
  {}
  Result:=ERROR_INVALID_PARAMETER;

  {Check Network}
  if Network = nil then Exit;
  if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

  {$IFDEF CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil,'CYW43455: Buffer Transmit');
  {$ENDIF}

  {Check Entry}
  if Entry = nil then Exit;
  if (Entry^.Count = 0) or (Entry^.Count > 1) then Exit;

  {Check State}
  Result:=ERROR_NOT_READY;
  if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

  {Wait for Entry}
  if SemaphoreWait(Network^.TransmitQueue.Wait) = ERROR_SUCCESS then
   begin
    {Acquire the Lock}
    if MutexLock(Network^.Lock) = ERROR_SUCCESS then
     begin
      try
       {Check Empty}
       Empty:=(Network^.TransmitQueue.Count = 0);

       {Add Entry}
       Network^.TransmitQueue.Entries[(Network^.TransmitQueue.Start + Network^.TransmitQueue.Count) mod PCYW43455Network(Network)^.TransmitEntryCount]:=Entry;

       {Update Count}
       Inc(Network^.TransmitQueue.Count);

       {Check Empty}
       if Empty then
        begin
         {Start Transmit}
         // this needs to be filled in with something!
         //LAN78XXTransmitStart(PLAN78XXNetwork(Network));
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       MutexUnlock(Network^.Lock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   end
  else
   begin
    Result:=ERROR_CAN_NOT_COMPLETE;
   end;
end;



{Initialization Functions}
procedure WIFIPreInit;

const
  {$IFDEF RPI}
  SDHOST_DESCRIPTION = 'BCM2835 SDHOST'; //BCM2708_SDHOST_DESCRIPTION;
  {$ENDIF}
  {$IFDEF RPI3}
  SDHOST_DESCRIPTION = 'BCM2837 SDHOST'; //BCM2710_SDHOST_DESCRIPTION;
  {$ENDIF}
  {$IFDEF RPI4}
  SDHOST_DESCRIPTION = BCM2711_EMMC2_DESCRIPTION;
  {$ENDIF}

var
  SDHCI: PSDHCIHost;
begin
  SDHCI := PSDHCIHost(DeviceFindByDescription(SDHOST_DESCRIPTION));
  if SDHCI <> nil then
    SDHCIHostStart(SDHCI);
end;

procedure WIFIInit;
var
  Network:PCYW43455Network;
  SDHCI : PSDHCIHost;
begin
 {Check Initialized}
 if WIFIInitialized then Exit;

 {Initialize Logging}
 WIFI_LOG_ENABLED:=(WIFI_DEFAULT_LOG_LEVEL <> WIFI_LOG_LEVEL_NONE);

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Initialize');

 SDIOProtect := SpinCreate;


 {Create Network}
 Network:=PCYW43455Network(NetworkDeviceCreateEx(SizeOf(TCYW43455Network)));
 if Network = nil then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'CYW43455: Failed to create new network device');
   Exit;
  end;

 {Update Network}
 {Device}
 Network^.Network.Device.DeviceBus:=DEVICE_BUS_SD;
 Network^.Network.Device.DeviceType:=NETWORK_TYPE_ETHERNET;
 Network^.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER or NETWORK_FLAG_RX_MULTIPACKET;
 Network^.Network.Device.DeviceData:=nil;
 Network^.Network.Device.DeviceDescription:=CYW43455_NETWORK_DESCRIPTION;
 {Network}
 Network^.Network.NetworkState:=NETWORK_STATE_CLOSED;
 Network^.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 Network^.Network.DeviceOpen:=@CYW43455DeviceOpen;
 Network^.Network.DeviceClose:=@CYW43455DeviceClose;
 Network^.Network.DeviceControl:=@CYW43455DeviceControl;
 Network^.Network.BufferAllocate:=@CYW43455BufferAllocate;
 Network^.Network.BufferRelease:=@CYW43455BufferRelease;
 Network^.Network.BufferReceive:=@CYW43455BufferReceive;
 Network^.Network.BufferTransmit:=@CYW43455BufferTransmit;

 {Register Network}
 if NetworkDeviceRegister(@Network^.Network) <> ERROR_SUCCESS then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'CYW43455: Failed to register new network device');

   {Destroy Network}
   NetworkDeviceDestroy(@Network^.Network);
   Exit;
  end;

 // We need to get an SDHCI Host to assign for the device data
 SDHCIHostEnumerate(@SDHCIEnum, Network);
 
 SDHCI := PSDHCIHost(Network^.Network.Device.DeviceData);
 if (SDHCI <> nil) then // if (FoundSDHCI <> nil) then
 begin
   // Save Network to Device Data
   SDHCI^.Device.DeviceData := Network;
   
   // Update host flags to include non standard
   SDHCI^.Device.DeviceFlags := SDHCI^.Device.DeviceFlags or SDHCI_FLAG_NON_STANDARD;
   
   // Insert new HostStart method and save original method
   Network^.OriginalHostStart := SDHCI^.HostStart;
   SDHCI^.HostStart := @WIFIHostStart;
   
   SDHCIHostStart(SDHCI);
 end  
 else
 begin
   WIFILogError(nil, 'Could not find an SDHCI host to initialize');
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network^.Network);
   exit;
 end;


 {Initialize WIFI Device Table}
 WIFIDeviceTable:=nil;
 WIFIDeviceTableLock:=CriticalSectionCreate;
 WIFIDeviceTableCount:=0;
 if WIFIDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if WIFI_LOG_ENABLED then WIFILogError(nil,'Failed to create WIFI device table lock');
  end;

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'WIFIInit Complete');
 {$endif}

 {$ifdef supplicant}
 // EAPOL queue locks for use with supplicant
 InitializeCriticalSection(EAPOLQueueLock);
// InitializeCriticalSection(L2SendQueueLock);
 {$endif}

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

 if WIFI_LOG_ENABLED then WIFILogInfo(nil,'WIFI Set Clock (Clock=' + IntToStr(Clock) + ')');

 {Get SDHCI}
 SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
 if SDHCI = nil then Exit;

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'clock='+inttostr(clock) + ' max freq='+inttostr(sdhci^.MaximumFrequency));
 {$endif}
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
 chipidrev : byte;
 bytevalue : byte;
 blocksize : byte;
 result1, result2 : longword;
 retries : word;
begin
 {}
 try
 if WIFI_LOG_ENABLED then WIFILogInfo(nil,'WIFIDeviceInitialize');

 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {Check Initialize}
 if Assigned(WIFI^.DeviceInitialize) then
  begin
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI^.DeviceInitialize');
   {$endif}
   Result:=WIFI^.DeviceInitialize(WIFI);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'Default initialize method');
   {$endif}

   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then
   begin
    WIFILogError(nil, 'The SDHCI host is nil');
    Exit;
   end;

   if WIFI_LOG_ENABLED then WIFILogInfo(nil,'Set initial power');

   // should already have been done elsewhere.
   {Set Initial Power}
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sdhci^.voltages is ' + inttostr(sdhci^.voltages) + 'firstbitsetof()='+inttostr(firstbitset(sdhci^.voltages)));
   {$endif}
   Result:=SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI^.Voltages));
   if Result <> WIFI_STATUS_SUCCESS then
    begin
     WIFILogError(nil,'failed to Set initial power');

     Exit;
    end;

   {Set Initial Clock}
   if WIFI_LOG_ENABLED then WIFILogInfo(nil,'Set device clock');
   Result:=WIFIDeviceSetClock(WIFI,SDIO_BUS_SPEED_DEFAULT);
   if Result <> WIFI_STATUS_SUCCESS then
     WIFILogError(nil, 'failed to set the clock speed to default')
   {$ifdef CYW43455_DEBUG}
   else
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Set device clock succeeded');
   {$else} ; {$endif}

   {Perform an SDIO Reset}
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'SDIO WIFI Device Reset');
   SDIOWIFIDeviceReset(WIFI);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Device Go Idle');
   {Set the Card to Idle State}
   WIFIDeviceGoIdle(WIFI);


   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'send interface condition request');
   {Get the Interface Condition}
   SDWIFIDeviceSendInterfaceCondition(WIFI);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'send operation condition');
   {Check for an SDIO Card}
   if SDIOWIFIDeviceSendOperationCondition(WIFI,True) = WIFI_STATUS_SUCCESS then
    begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'send operation condition successful');
     {$endif}
    end
    else
      WIFILogError(nil, 'send operation condition failed');

   WIFI^.Device.DeviceBus:=DEVICE_BUS_SD;
   WIFI^.Device.DeviceType:=WIFI_TYPE_SDIO;
   WIFI^.RelativeCardAddress:=0;
   WIFI^.OperationCondition := $200000;

   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Initialize Card Type is SDIO');
   {$ENDIF}


   {Get the Operation Condition}
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Get operation condition');
   Result:=SDIOWIFIDeviceSendOperationCondition(WIFI,False);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
//     Exit;
    end;


   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Set card relative address');

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
   begin
      if WIFI_LOG_ENABLED then
         WIFILogInfo(nil,' Card relative address is ' + inttohex((rcaraw shr 16) and $ff, 2))
   end
   else
     WIFILogError(nil, 'Could not set relative card address; error='+inttostr(Result));


   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Selecting wifi device with cmd7');

   FillChar(Command,SizeOf(TSDIOCommand),0);
   Command.Command:= SDIO_CMD_SELECT_CARD;
   Command.Argument:= rcaraw;
   Command.ResponseType:=SDIO_RSP_R1;
   Command.Data:=nil;

   Result := WIFIDeviceSendCommand(WIFI, @Command);

   if (Result = WIFI_STATUS_SUCCESS) then
   begin
    {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Device successfully selected response[0]=' + inttohex(command.response[0], 8));
    {$endif}
     // for an I/O only card, the status bits are fixed at 0x0f (bits 12:9 of response[0])
     if (((command.response[0] shr 9) and $f) = $f) then
     begin
     {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'The card correctly reads as I/O only')
     {$endif}
     end
     else
       WIFILogError(nil, 'Something went wrong with the status bits');
   end
   else
     WIFILogError(nil, 'Failed to select the card at rca='+inttohex((rcaraw shr 16) and $ff, 8));

   {Set Clock to high speed}
   if WIFI_LOG_ENABLED then WIFILogInfo(nil,'Set device clock');
   Result:=WIFIDeviceSetClock(WIFI,SDIO_BUS_SPEED_HS);
   if Result <> WIFI_STATUS_SUCCESS then
     WIFILogError(nil, 'failed to set the clock speed to default')
   else
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Set device clock succeeded');
     {$endif}
   end;

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'setting bus speed via common control registers');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite,BUS_FUNCTION,SDIO_CCCR_SPEED,3,nil);            // emmc sets this to 2.
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully updated bus speed register');
     {$endif}
   end
   else
     WIFILogError(nil, 'Failed to update bus speed register');


   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'setting bus interface via common control registers');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite,BUS_FUNCTION,SDIO_CCCR_IF, $2,nil);
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully updated bus interface control');
     {$endif}
   end
   else
     WIFILogError(nil, 'Failed to update bus interface control');

   if WIFI_LOG_ENABLED then WIFILogInfo(nil,'Waiting until the backplane is ready');
   blocksize := 0;
   retries := 0;
   repeat
     // attempt to set and read back the fn0 block size.
     result1 := SDIOWIFIDeviceReadWriteDirect(WIFI, sdioWrite, BUS_FUNCTION, SDIO_CCCR_BLKSIZE, WIFI_BAK_BLK_BYTES, nil);
     result2 := SDIOWIFIDeviceReadWriteDirect(WIFI, sdioRead, BUS_FUNCTION, SDIO_CCCR_BLKSIZE, 1, @blocksize);
     retries += 1;
     sleep(1);
   until ((result1 = WIFI_STATUS_SUCCESS) and (result2 = WIFI_STATUS_SUCCESS) and (blocksize = WIFI_BAK_BLK_BYTES)) or (retries > 500);

   if (retries > 500) then
     WIFILogError(nil, 'the backplane was not ready');

   // if we get here we have successfully set the fn0 block size in CCCR and therefore the backplane is up.

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'setting backplane block size');

   // set block sizes for fn1 and fn2 in their respective function registers.
   // note these are still writes to the common IO area (function 0).
   updatevalue := WIFI_BAK_BLK_BYTES;
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'setting backplane fn1 block size to 64');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, BUS_BAK_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, BUS_BAK_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'setting backplane fn2 (radio) block size to 512');
   updatevalue := 512;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, BUS_RAD_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, BUS_RAD_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   // we only check the last result here. Needs changing really.
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully updated backplane block sizes');
     {$endif}
   end
   else
     WIFILogError(nil, 'Failed to update backplane block sizes');

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'IO Enable backplane function 1');
   ioreadyvalue := 0;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite,BUS_FUNCTION,SDIO_CCCR_IOEx, 1 shl 1,nil);
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'io enable successfully set for function 1');
     {$endif}
   end
   else
     WIFILogError(nil, 'io enable could not be set for function 1');

   // at this point ether4330.c turns off all interrupts and then does to ioready check below.

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Waiting for IOReady function 1');
   ioreadyvalue := 0;
   while (ioreadyvalue and (1 shl 1)) = 0 do
   begin
     Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BUS_FUNCTION, SDIO_CCCR_IORx,  0, @ioreadyvalue);
     if (Result <> WIFI_STATUS_SUCCESS) then
     begin
       WIFILogError(nil, 'Could not read IOReady value');
       exit;
     end;
     sleep(10);
   end;

   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Reading the Chip ID');
   {$endif}

   chipid := 0;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,0,  0, @chipid);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,1,  0, pbyte(@chipid)+1);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,2,  0, @chipidrev);
   chipidrev := chipidrev and $f;
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Chip ID is 0x'+inttohex(chipid, 4) + ' rev ' + inttostr(chipidrev));
     WIFI^.chipid := chipid;
     WIFI^.chipidrev := chipidrev;
   end;


   // scan the cores to establish various key addresses
   WIFIDeviceCoreScan(WIFI);

   if (WIFI^.armctl = 0) or (WIFI^.dllctl = 0) or
     ((WIFI^.armcore = ARMcm3) and ((WIFI^.socramctl = 0) or (WIFI^.socramregs = 0))) then
   begin
     WIFILogError(nil, 'Corescan did not find essential cores!');
     exit;
   end;


   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Disable core');
   {$endif}

   if (WIFI^.armcore = ARMcr4) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sbreset armcr4 core');
     {$endif}

     sbreset(WIFI, WIFI^.armctl, Cr4Cpuhalt, Cr4CpuHalt)
   end
   else
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sbdisable armctl core');
     {$endif}

     sbdisable(WIFI, WIFI^.armctl, 0, 0);
   end;

   sbreset(WIFI, WIFI^.dllctl, 8 or 4, 4);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Device RAM scan');

   WIFIDeviceRamScan(WIFI);


   // Set clock on function 1
   Result := SDIOWIFIDeviceReadWriteDirect(WIFI, sdioWrite, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
   if (Result <> WIFI_STATUS_SUCCESS) then
     WIFILogError(nil, 'Unable to update config at chip clock csr register');
   MicrosecondDelay(10);

   // check active low power clock availability

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
   sleep(1);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ReqALP, nil);

   // now we keep reading them until we have some availability
   bytevalue := 0;
   while (bytevalue and (HTavail or ALPavail) = 0) do
   begin
     Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, @bytevalue);
     if (Result <> WIFI_STATUS_SUCCESS) then
       WIFILogError(nil, 'failed to read clock settings');
     MicrosecondDelay(10);
   end;

   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Clock availability is 0x' + inttohex(bytevalue, 2));
   {$endif}

   // finally we can clear active low power request. Not sure if any of this is needed to be honest.
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'clearing active low power clock request');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ForceALP, nil);

   MicrosecondDelay(65);

  WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.chipcommon);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Disable pullups');
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, gpiopullup, 0, nil);
  if (Result = WIFI_STATUS_SUCCESS) then
  begin
   {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully disabled SDIO extra pullups');
   {$endif}
  end
  else
    WIFILogError(nil, 'Failed to disable SDIO extra pullups');

  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, Gpiopulldown, 0, nil);
  if (Result = WIFI_STATUS_SUCCESS) then
  begin
   {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully disabled SDIO extra pulldowns');
    {$endif}
  end
  else
    WIFILogError(nil, 'Failed to disable SDIO extra pulldowns');


   if (WIFI^.chipid = $4330) or (WIFI^.chipid = 43362) then
   begin
    // there is other stuff from sbinit() to do here
    // however the chipids are not either 3b or zero as far as I can tell so
    // we won't do them until we find a device that needs them.
    // relates to power management by the look of it. PMU, drive strength.
   end;


   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Download WIFI firmware to Cypress SOC');
   Result := WIFIDeviceDownloadFirmware(WIFI);

   // Enable the device. This should boot the firmware we just loaded to the chip
   sbenable(WIFI);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Enabling interrupts for all functions');
   Result := SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, SDIO_CCCR_IENx, (INTR_CTL_MASTER_EN or INTR_CTL_FUNC1_EN or INTR_CTL_FUNC2_EN), nil );
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     {$ifdef CYW43455_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully enabled interrupts');
     {$endif}
   end
   else
     WIFILogError(nil, 'Failed  to enable interrupts');

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'End of WIFIDeviceInitialize');
   Result:=WIFI_STATUS_SUCCESS;
  end;

 except
   on e : exception do
   WIFILogError(nil, 'Exception ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8) + ' during wifiinitialize');
 end;
end;

function WIFIDeviceSetBackplaneWindow(WIFI : PWIFIDevice; addr : longword) : longword;
begin
 addr := addr and (not $7fff);

 {$ifdef CYW43455_SDIO_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'setting backplane address to ' + inttohex((addr shr 8) and $ff, 8) + ' '
                  + inttohex((addr shr 16) and $ff, 8) + ' '
                  + inttohex((addr shr 24) and $ff, 8));
 {$endif}

 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG, (addr shr 8) and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG+1,(addr shr 16) and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG+2,(addr shr 24) and $ff,nil);

 if (Result = WIFI_STATUS_SUCCESS) then
 begin
  {$ifdef CYW43455_SDIO_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'function ' + inttostr(1) + ' backplanewindow updated to ' + inttohex(addr, 8));
  {$endif}
 end
 else
   WIFILogError(nil, 'something went wrong in setbackplanewindow');
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
    if Result <> ERROR_SUCCESS then
     begin
      WIFILogError(nil, 'DeviceRegister returned ' + inttostr(result));
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

function SDIOWIFIDeviceReset(WIFI:PWIFIDevice):LongWord;
{See: SDIO Simplified Specification V2.0, 4.4 Reset for SDIO}
var
 Abort:Byte;
 Status:LongWord;
begin
 {}
 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(WIFI, 'SDIO WIFI Reset');
 {$endif}

 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {Get Abort Value}
 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(WIFI, 'get abort value');
 {$endif}

 Status:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BUS_FUNCTION,SDIO_CCCR_ABORT,0,@Abort);
 MicrosecondDelay(20000);
 if Status <> WIFI_STATUS_SUCCESS then
  begin
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(WIFI, 'WIFI Device Reset - SDIO_CCR_ABORT returned non zero result of ' + inttostr(status));
   {$endif}

   Abort:=$08;
  end
 else
  begin
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'abort value success status');
   {$endif}
   Abort:=Abort or $08;
  end;

 {Set Abort Value}
 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(WIFI, 'Set abort value');
 {$endif}

 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite,BUS_FUNCTION,SDIO_CCCR_ABORT,Abort,nil);
 MicrosecondDelay(20000);

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(WIFI, 'Result of setting abort='+inttostr(Result));
 {$endif}

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

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Go Idle');
 {$endif}

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
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI failed to go idle');
   {$endif}

   Result:=Status;
   Exit;
  end;

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'WIFI successfully went idle');
 {$endif}

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

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'SD Send Interface Condition');
 {$endif}

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
     WIFILogError(nil,'SD Send Interface Condition failure (Response=' + IntToHex(Command.Response[0] and $FF,8) + ')');
     Exit;
    end
    else
    begin
      {$ifdef CYW43455_DEBUG}
      if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'WIFI Send interface condition check pattern matches');
      {$endif}
    end;

   {Get Response}
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Interface Condition Response0=' + IntToHex(Command.Response[0] and $FF,8)
     + 'Response1=' + IntToHex(Command.Response[1] and $FF,8));
   {$endif}

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

 {$ifdef CYW43455_DEBUG}
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
 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'waiting for non-busy signal from wifi device');
 {$endif}

 while Timeout > 0 do
  begin
   {Send Command}
   Status:=WIFIDeviceSendCommand(WIFI,@Command);
   if Status <> WIFI_STATUS_SUCCESS then
    begin
     WIFILogError(nil, 'sendoperationcondition devicesendcommand returned failed status ' + inttostr(status));
     Result:=Status;
     Exit;
    end;

   {Single pass only on probe}
   if Probe then Break;

   if (Command.Response[0] and WIFI_OCR_BUSY) <> 0 then Break;

   Dec(Timeout);
   if Timeout = 0 then
    begin
     if WIFI_LOG_ENABLED then WIFILogError(nil,'SDIO Send Operation Condition Busy Status Timeout');
     Exit;
    end;
   MillisecondDelay(10);
  end;

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'wifi device is ready for action');
 {$endif}


 {Get Response}
 {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'operation condition returned as ' + inttostr(command.response[0]));
 {$endif}

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

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Set IOS');
 {$endif}

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
   SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI^.Voltages));

   {Set Bus Width}
   WIFI^.BusWidth := WIFI_BUS_WIDTH_4;
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
       {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'turn off 8 bit bus');
       {$endif}

       Value:=Value and not(SDHCI_CTRL_8BITBUS);
      end;

     if WIFI^.BusWidth = WIFI_BUS_WIDTH_4 then
      begin
       {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'set 4 bit bus');
       {$endif}

       Value:=Value or SDHCI_CTRL_4BITBUS;
      end
     else
      begin
       {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'turn off 4 bit bus');
       {$endif}

       Value:=Value and not(SDHCI_CTRL_4BITBUS);
      end;
    (*end;*)

   // block gap control
   SDHCIHostWriteByte(SDHCI, SDHCI_BLOCK_GAP_CONTROL, 0);
   SDHCIHostWriteByte(SDHCI, SDHCI_POWER_CONTROL, 0);

   {Update Bus Width}  
   SDHCI^.BusWidth:=WIFI^.BusWidth;

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

   {Update Timing}  
   SDHCI^.Timing:=WIFI^.Timing;

   Result:=WIFI_STATUS_SUCCESS;
  end;

 //See: mmc_set_ios in mmc.c
 //     sdhci_set_ios in sdhci.c
 //See: bcm2835_mmc_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_do_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
end;


function SDIOWIFIDeviceReadWriteDirect(WIFI:PWIFIDevice; Direction : TSDIODirection; Operation,Address:LongWord; Input:Byte; Output:PByte):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
begin
 {}
 SpinLock(SDIOProtect);
 try

   Result:=WIFI_STATUS_INVALID_PARAMETER;

   {Check WIFI}
   if WIFI = nil then Exit;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'sdio read write direct address='+inttohex(address, 8) + ' value='+inttohex(input, 2)
       + ' direction='+inttostr(ord(direction)) +' [0=read, 1=write]');
   {$endif}

   {Get SDHCI}
   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Check Operation}
   if Operation > 7 then Exit;

   {Check Address}
   if (Address and not($0001FFFF)) <> 0 then Exit;

   {Setup Command}
   FillChar(Command,SizeOf(TSDIOCommand),0);
   Command.Command:=SDIO_CMD_RW_DIRECT;
   Command.Argument:=0;
   Command.ResponseType:=SDIO_RSP_R5;
   Command.Data:=nil;

   {Setup Argument}
   if Direction = sdioWrite then Command.Argument:=$80000000;
   Command.Argument:=Command.Argument or (Operation shl 28);
   if (Direction = sdioWrite) and (Output <> nil) then Command.Argument:=Command.Argument or $08000000;
   Command.Argument:=Command.Argument or (Address shl 9);
   Command.Argument:=Command.Argument or Input;

   {Send Command}
   Status:=WIFIDeviceSendCommand(WIFI,@Command);
   if Status <> WIFI_STATUS_SUCCESS then
    begin
     {$ifdef CYW43455_SDHCI_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFIDeviceCommand returne fail status of ' + inttostr(result));
     {$endif}

     Result:=Status;
     Exit;
    end;

   {Check Result}
   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'check result (response[0]='+inttostr(command.response[0]));
   {$endif}

   if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then Exit;
   if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then Exit;
   if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then Exit;

   {Get Output}
   if Output <> nil then
       Output^:=Command.Response[0] and $FF;

   Result:=WIFI_STATUS_SUCCESS;

 finally
   SpinUnlock(SDIOProtect);
 end;

 //See: mmc_io_rw_direct_host in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;

{SDIO_CMD_RW_DIRECT argument format:
      [31] R/W flag
      [30:28] Function number
      [27] RAW flag
      [25:9] Register address
      [7:0] Data}

function SDIOWIFIDeviceReadWriteExtended(WIFI:PWIFIDevice;
            Direction: TSDIODirection;
            Operation, Address : LongWord;
            Increment : Boolean; Buffer : Pointer;
            BlockCount, BlockSize : LongWord;trace : string = '') : LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TSDIOCommand;
 SDIOData : TSDIOData;
begin
 {}
 SpinLock(SDIOProtect);

 try
   Result:=WIFI_STATUS_INVALID_PARAMETER;

   {Check WIFI}
   if WIFI = nil then Exit;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'SDIOReadWriteExtended ' + inttostr(ord(direction)) + ' [0=read, 1=write] ' + inttostr(operation)
     + ' address=0x' + inttohex(address, 8)
     + ' buf=0x'+inttohex(longword(buffer), 8)
     + ' blockcount='+inttostr(blockcount)
     + ' blocksize='+inttostr(blocksize)
     + ' callerid='+inttostr(callerid));
   {$endif}

   {Get SDHCI}
   SDHCI:=PSDHCIHost(WIFI^.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Check Operation}
   if Operation > 7 then Exit;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'check address='+inttohex(address, 8));
   {$endif}

   {Check Address}
   if (Address and not($0001FFFF)) <> 0 then Exit;

   {Setup Command}
   FillChar(Command,SizeOf(TSDIOCommand),0);
   Command.Command:=SDIO_CMD_RW_EXTENDED;
   Command.Argument:=0;
   Command.ResponseType:=SDIO_RSP_R1;
   Command.Data:=nil;

   Command.Data := @SDIOData;
   SDIOData.Data := Buffer;

   SDIOData.Blocksize := BlockSize;
   SDIOData.BlockCount := blockcount;

   if (direction = sdioWrite) then
     SDIOData.Flags := WIFI_DATA_WRITE
   else
     SDIOData.Flags := WIFI_DATA_READ;

   {SDIO_CMD_RW_EXTENDED argument format:
         [31] R/W flag
         [30:28] Function number
         [27] Block mode
         [26] Increment address
         [25:9] Register address
         [8:0] Byte/block count}

   {Setup Argument}
   if (direction = sdioWrite) then Command.Argument:=$80000000;
   Command.Argument:=Command.Argument or (Operation shl 28);   // adds in function number
   if increment then
      Command.Argument := Command.Argument or (1 shl 26);     // add in increment flag
   Command.Argument:=Command.Argument or (Address shl 9);

   if (blockcount = 0) then
     Command.Argument := Command.Argument or BlockSize        // byte mode; blocksize=bytes
   else
   begin
     Command.Argument := Command.Argument or (1 shl 27);      // set block mode bit
     Command.Argument := Command.Argument or BlockCount;
   end;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'send command. argument ended up being ' + inttohex(command.argument, 8));
   {$endif}

   Status:=WIFIDeviceSendCommand(WIFI,@Command);  // send command

   if Status <> WIFI_STATUS_SUCCESS then
    begin
     {$ifdef CYW43455_SDHCI_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFIDeviceSendCommand returned status of ' + inttostr(result));
     {$endif}

     Result:=Status;
     Exit;
    end;

   {Check Result}
   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'check result command.response[0]='+inttohex(command.response[0], 8));
   {$endif}

   if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then
    begin
     {$ifdef CYW43455_SDHCI_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'command response contains R5 Error');
     {$endif}

     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then
    begin
     {$ifdef CYW43455_SDHCI_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'command response contains R5 function number');
     {$endif}

     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then
    begin
     {$ifdef CYW43455_SDHCI_DEBUG}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'command response contains R5 out of range');
     {$endif}

     Exit;
    end;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'returning success');
   {$endif}

   Result:=WIFI_STATUS_SUCCESS;

 finally
   SpinUnlock(SDIOProtect);
 end;

 //See: mmc_io_rw_extended in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;


function WIFIDeviceSendCommand(WIFI:PWIFIDevice;Command:PSDIOCommand; txdata : PSDIOData = nil):LongWord;
var
 Mask:LongWord;
 TransferMode:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Timeout:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=WIFI_STATUS_INVALID_PARAMETER;

 {Check WIFI}
 if WIFI = nil then Exit;

 {$ifdef CYW43455_SDHCI_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command ' + inttostr(command^.Command) + ' status='+inttostr(command^.Status));
 {$endif}

 {Check Send Command}
 if Assigned(WIFI^.DeviceSendCommand) then
  begin
   Result:=WIFI^.DeviceSendCommand(WIFI,Command);
  end
 else
  begin
   {Default Method}
   {Check Command}
   if Command = nil then Exit;

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
       {Update Statistics}
       Inc(SDHCI^.RequestCount);

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

       {Check Response Type}
       if ((Command^.ResponseType and SDIO_RSP_136) <> 0) and ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         if WIFI_LOG_ENABLED then WIFILogError(nil,'MMC Send Command Invalid Response Type');
         Command^.Status:=WIFI_STATUS_INVALID_PARAMETER;
         Exit;
        end;

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
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'adding sdhci_cmd_data flag to the flags for this command');
         {$endif}

         Flags:=Flags or SDHCI_CMD_DATA;
        end;

       {Write Timeout Control}
       if (Command^.Data <> nil) or ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFISendCommand SDHCI_TIMEOUT_CONTROL (Value=' + IntToHex(SDHCI_TIMEOUT_VALUE,8) + ')');
         {$ENDIF}
         SDHCIHostWriteByte(SDHCI,SDHCI_TIMEOUT_CONTROL,SDHCI_TIMEOUT_VALUE);
        end;

       {Check Data}
       if (command^.data = nil) then
        begin
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'writing a standard command; status='+inttostr(command^.status));
         {$endif}

         {Setup Transfer Mode}
         TransferMode:=SDHCIHostReadWord(SDHCI,SDHCI_TRANSFER_MODE);

         {Clear Auto CMD settings for non data CMDs}
         TransferMode:=TransferMode and not(SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23);

         {Clear Block Count, Multi, Read and DMA for non data CMDs}
         TransferMode:=TransferMode and not(SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI or SDHCI_TRNS_READ or SDHCI_TRNS_DMA);

         {Write Argument}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         {$ENDIF}

         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer TransferMode}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(TransferMode,8) + ')');
         {$ENDIF}

         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,TransferMode);
        end
       else
        begin
         {Setup Data}
         Command^.Data^.BlockOffset:=0;
         if (Command^.Data^.BlockCount = 0) then
            Command^.Data^.BlocksRemaining := 1
         else
           Command^.Data^.BlocksRemaining:=Command^.Data^.BlockCount;  // not sure if code expects there always to be a block count
         Command^.Data^.BytesTransfered:=0;

         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'blockcount='+inttostr(command^.data^.blockcount) + ' blocksize='+inttostr(command^.data^.blocksize));
         {$endif}

         {Setup Transfer TransferMode}
         TransferMode := 0;
         if (Command^.Data^.BlockCount > 0) then
          begin
           {$ifdef CYW43455_SDHCI_DEBUG}
           if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'enabling block transfer TransferMode');
           {$endif}
           TransferMode:=SDHCI_TRNS_BLK_CNT_EN;
           TransferMode:=TransferMode or SDHCI_TRNS_MULTI;

          // TransferMode:=TransferMode or SDHCI_TRNS_AUTO_CMD12; //To Do //Testing (This works, need to sort out properly where it fits, plus SDHCI_TRNS_AUTO_CMD23)

           //To Do //SDHCI_TRNS_AUTO_CMD12 //SDHCI_TRNS_AUTO_CMD23 //SDHCI_ARGUMENT2 //See: sdhci_set_transfer_mode
                   //See 1.15 Block Count in the SD Host Controller Simplified Specifications
          end;
         if (txdata <> nil) then
         begin
            if ((txdata^.Flags and WIFI_DATA_READ) <> 0) then
              TransferMode := TransferMode or SDHCI_TRNS_READ;
         end
         else
         if (Command^.Data^.Flags and WIFI_DATA_READ) <> 0 then
          begin
           {$ifdef CYW43455_SDHCI_DEBUG}
           if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Add read flag to transfer mode');
           {$endif}

           TransferMode:=TransferMode or SDHCI_TRNS_READ;
          end;

//         TransferMode := TransferMode or SDHCI_TRNS_R5;  //should be an R5 response for an SDIO device (for a cmd53 -- needs some changes so it works generically)
// by not including this we should expect an R1 response.

         {Setup DMA Address}
         //TransferMode |= SDHCI_TRNS_DMA;
         //Address:=
         //To Do

         {Setup Interrupts}
         SDHCI^.Interrupts:=SDHCI^.Interrupts or (SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL or SDHCI_INT_DATA_END);
         SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI^.Interrupts);
         SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI^.Interrupts);
         //To Do //Different for DMA //Should we disable these again after the command ? //Yes, probably

         {Write DMA Address}
         //To Do
         //SDHCIHostWriteLong(SDHCI,SDHCI_DMA_ADDRESS,Address);

         {Write Block Size}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_SIZE (Value=' + IntToStr(Command^.Data^.BlockSize) + ') makeblocksize='+inttohex(SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize), 8));
         {$endif}

         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize));

         {Write Block Count}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_COUNT (Value=' + IntToStr(Command^.Data^.BlockCount) + ')');
         {$endif}

         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,Command^.Data^.BlockCount);

         {Write Argument}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         {$endif}

         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer TransferMode}
         {$ifdef CYW43455_SDHCI_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(TransferMode,8) + ')');
         {$endif}

         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,TransferMode);
        end;


       {Setup Command}
       // this cast is safe as the data types are the same size and shape but it should
       // not be persisted. The only 'easy' solution is to use the mmc command for all.
       // but I don't like that as it is not an mmc command.
       SDHCI^.Command:=PMMCCommand(Command);

       try
        {Write Command}

        {$ifdef CYW43455_SDHCI_DEBUG}
        if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_COMMAND cmd=' + inttostr(command^.command) + '  value written to cmd register=' + IntToHex(SDHCIMakeCommand(Command^.Command,Flags),8) + ') status='+inttostr(command^.status));
        {$endif}

        if (dodumpregisters) and (command^.Command = SDIO_CMD_RW_EXTENDED) then
        begin
          dumpregisters(WIFI);
          dodumpregisters := false;
        end;

        SDHCIHostWriteWord(SDHCI,SDHCI_COMMAND,SDHCIMakeCommand(Command^.Command,Flags));

        {Wait for Completion}   // short timeout for a read command.
        if SDHCI^.Command^.Data = nil then // need to go back to test for data=nil
         begin
          {Update Statistics}
          Inc(SDHCI^.CommandRequestCount);

          {Wait for Signal with Timeout (100ms)}
          Status:=SemaphoreWaitEx(SDHCI^.Wait,500);  // increased during debug
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              {$ifdef CYW43455_SDHCI_DEBUG}
              if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command Response (short) Timeout');
              {$endif}

              Command^.Status:=SDHCI_STATUS_TIMEOUT;
              Exit;
             end
            else
             begin
              {$ifdef CYW43455_SDHCI_DEBUG}
              if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command Response (short) Failure, result='+inttostr(status));
              {$endif}

              Command^.Status:=SDHCI_STATUS_HARDWARE_ERROR;
              Exit;
             end;
           end
          else
          begin
           {$ifdef CYW43455_SDHCI_DEBUG}
           if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'semaphore wait succeeded command=' + inttostr(command^.Command) + ' status=' + inttostr(command^.Status));
           {$endif}
          end;
         end
        else
         begin
          {Update Statistics}
          Inc(SDHCI^.DataRequestCount);

          {Update Statistics}
          Inc(SDHCI^.PIODataTransferCount);

          {Wait for Signal with Timeout (5000ms)}
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
           begin
            {$ifdef CYW43455_SDHCI_DEBUG}
            if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'wait returned success');
            {$endif}
           end;
         end;

       finally
        {Reset Command}
        SDHCI^.Command:=nil;
       end;
      finally
       {Check Reset Required}
       if Command^.Status <> WIFI_STATUS_SUCCESS then //To Do //More see: sdhci_tasklet_finish //SDHCI_QUIRK_RESET_AFTER_REQUEST and SDHCI_QUIRK_CLOCK_BEFORE_RESET
        begin
         SDHCIHostReset(SDHCI,SDHCI_RESET_CMD);
         SDHCIHostReset(SDHCI,SDHCI_RESET_DATA);
        end;
       
       {Check Status}
       if Command^.Status <> MMC_STATUS_SUCCESS then 
        begin
         {Update Statistics}
         Inc(SDHCI^.RequestErrors); 
         
         {Return Result}
         Result:=Command^.Status;
        end;
      end;

     finally
      {Release the Lock}
      MutexUnlock(WIFI^.Lock);
     end;
    end;

   {$ifdef CYW43455_SDHCI_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command completed: ' + MMCStatusToString(Command^.Status));
   {$endif}

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

 {$ifdef CYW43455_DEBUG}
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
    if WIFI_LOG_ENABLED then WIFILogError(nil,'SD Send Application Command Not Supported');
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


function WIFIDeviceCoreScan(WIFI : PWIFIDevice) : longint;
const
  corescansz = 512;
  CID_ID_MASK  =   $0000ffff;
  CID_REV_MASK  =  $000f0000;
  CID_REV_SHIFT  = 16;
  CID_TYPE_MASK  = $f0000000;
  CID_TYPE_SHIFT = 28;

var
 buf : array[0..511] of byte;
 i, coreid, corerev : integer;
 addr : longint;
 addressbytes : array[1..4] of byte;
 address : longword;
 chipidbuf : longword;
 {$ifdef CYW43455_DEBUG}
 chipid : word;
 chiprev : word;
 socitype : word;
 str : string;
 {$endif}
begin
 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Starting WIFI core scan');

 Result := WIFI_STATUS_INVALID_PARAMETER;

 // set backplane window
 fillchar(buf, sizeof(buf), 0);

 Result := WIFIDeviceSetBackplaneWindow(WIFI, BAK_BASE_ADDR);

 // read 32 bits containing chip id and other info
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,0,  0, pbyte(@chipidbuf));
 if (Result <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'failed to read the first byte of the chip id');

 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,1,  0, pbyte(@chipidbuf)+1);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,2,  0, pbyte(@chipidbuf)+2);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,sdioRead,BACKPLANE_FUNCTION,2,  0, pbyte(@chipidbuf)+3);

 {$ifdef CYW43455_DEBUG}
 chipid := chipidbuf  and CID_ID_MASK;
 chiprev := (chipidbuf and CID_REV_MASK) shr CID_REV_SHIFT;
 socitype := (chipidbuf and CID_TYPE_MASK) shr CID_TYPE_SHIFT;
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'chipid ' + inttohex(chipid,4) + ' chiprev ' + inttohex(chiprev, 4) + ' socitype ' + inttohex(socitype,4));
 {$endif}

 // read pointer to core info structure.
 // 63*4 is yucky. Could do with a proper address definition for it.
 Result := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioRead, BACKPLANE_FUNCTION, 63*4, true, @addressbytes[1], 0, 4);
 address := plongint(@addressbytes[1])^;

 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Core info pointer is read as ' + inttohex(address, 8));
 {$endif}

 // we must get the top 15 bits from the address and set the bakplane window to it
 WIFIDeviceSetBackplaneWindow(WIFI, address);

 address := (address and $7fff) or $8000;

 try
   // read the core info from the device
   Result := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioRead, BACKPLANE_FUNCTION, address, true, @buf[0], 8, 64);
   if (Result <> WIFI_STATUS_SUCCESS) then
   begin
     WIFILogError(nil, 'Failed to read Core information from the SDIO device.');
     exit;
   end;

   {$ifdef CYW43455_DEBUG}
   // dump the block into the log so we can take a look at it during development
   // this code will be deleted later.
   str := '';
   for i := 1 to 512 do
   begin
     str := str + ' ' + inttohex(buf[i-1], 2);
     if i mod 20 = 0 then
     begin
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, str);
       str := '';
     end;
   end;
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, str);
   {$endif}

   coreid := 0;
   corerev := 0;

   i := 0;

    while i < Corescansz do
    begin
       case buf[i] and $0f of
 	    $0F: begin
                   {$ifdef CYW43455_DEBUG}
                   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Reached end of descriptor');
                   {$endif}

                   break;
                 end;
            $01:	// core info */
                 begin
 		    if((buf[i+4] and $F) <> $01) then
 			    break;
 		    coreid := buf[i+1] or (buf[i+2] shl 8) and $FFF;
 		    i += 4;
 		    corerev := buf[i+3];
                 end;
            $05:	// address */
                 begin
 		    addr := (buf[i+1] shl 8) or (buf[i+2] shl 16) or (buf[i+3]<<24);
 		    addr := addr and (not $FFF);
  		    case coreid of
  		      $800:
                      begin
                         WIFI^.chipcommon := addr;
                      end;
                      ARMcm3,
  		      ARM7tdmi,
  		      ARMcr4:
                      begin
                         WIFI^.armcore := coreid;
                         if ((buf[i] and $c0) > 0) then
                         begin
                           if (WIFI^.armctl = 0) then
                             WIFI^.armctl := addr;
                         end
                         else
                         if (WIFI^.armregs = 0) then
                            WIFI^.armregs := addr;
                      end;

  		      $80E:
                      begin
                         if ((buf[i] and $c0) > 0) then
                           WIFI^.socramctl := addr
                         else
                         if (WIFI^.socramregs = 0) then
                           WIFI^.socramregs := addr;
                         WIFI^.socramrev := corerev;
                      end;

                      $829:
                      begin
                         if ((buf[i] and $c0) = 0) then
                           WIFI^.sdregs := addr;
                         WIFI^.sdiorev := corerev;
                      end;

                      $812:
                      begin
                         if ((buf[i] and $c0) > 0) then
                           WIFI^.dllctl := addr;
                      end;
                    end;
                 end;
       end;
      i := i + 4;
    end;

    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Corescan completed.');

    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then
    begin
      WIFILogDebug(nil,'chipcommon=0x' + inttohex(WIFI^.chipcommon,8));
      WIFILogDebug(nil,'armcore=0x' + inttohex(WIFI^.armcore,8));
      WIFILogDebug(nil,'armctl=0x' + inttohex(WIFI^.armctl,8));
      WIFILogDebug(nil,'armregs=0x' + inttohex(WIFI^.armregs,8));
      WIFILogDebug(nil,'socramctl=0x' + inttohex(WIFI^.socramctl,8));
      WIFILogDebug(nil,'socramregs=0x' + inttohex(WIFI^.socramregs,8));
      WIFILogDebug(nil,'socramrev=0x' + inttohex(WIFI^.socramrev,8));
      WIFILogDebug(nil,'sdregs=0x' + inttohex(WIFI^.sdregs,8));
      WIFILogDebug(nil,'sdiorev=0x' + inttohex(WIFI^.sdiorev,8));
      WIFILogDebug(nil,'dllctl=0x' + inttohex(WIFI^.dllctl,8));
    end;
    {$endif}

    Result := WIFI_STATUS_SUCCESS;

 except
   on e : exception do
     WIFILogError(nil, 'exception in corescan: ' + e.message);
 end;

end;

function cfgreadl(WIFI : PWIFIDevice; addr : longword; trace : string = '') : longword;
var
  v : longword;
begin
  v := 0;
  Result := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioRead, BACKPLANE_FUNCTION, (addr and $1ffff) or $8000, true, @v, 0, 4, trace);
  if (Result <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'Failed to read config item 0x'+inttohex(addr, 8) + ' result='+inttostr(Result));
  Result := v;
end;

procedure cfgwritel(WIFI : PWIFIDevice; addr : longword; v : longword; trace : string = '');
var
  Result : longword;
begin
 Result := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioWrite, BACKPLANE_FUNCTION, (addr and $1ffff) or $8000, true, @v, 0, 4, trace);
 if (Result <> WIFI_STATUS_SUCCESS) then
   WIFILogError(nil,'Failed to update config item 0x'+inttohex(addr, 8));
end;

procedure cfgw(WIFI : PWIFIDevice; offset : longword; value : byte);
var
  Result : longword;
begin
  Result := SDIOWIFIDeviceReadWriteDirect(WIFI, sdioWrite, BACKPLANE_FUNCTION, offset, value, nil);
  if (Result <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'Failed to write config item 0x'+inttohex(offset, 8));
end;

function cfgr(WIFI : PWIFIDevice; offset : longword) : byte;
var
  Res : longword;
  value : byte;
begin
  Res := SDIOWIFIDeviceReadWriteDirect(WIFI, sdioRead, BACKPLANE_FUNCTION, offset, 0, @value);
  if (Res <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'Failed to read config item 0x'+inttohex(offset, 8));

  Result := value;
end;

procedure sbdisable(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);
begin
 try
  WIFIDeviceSetBackplaneWindow(WIFI,  regs);

  if ((cfgreadl(WIFI, regs + Resetctrl) and 1) <> 0) then
  begin
    cfgwritel(WIFI, regs + Ioctrl, 3 or ioctl);
    cfgreadl(WIFI, regs + Ioctrl);
    exit;
  end;

  cfgwritel(WIFI, regs + Ioctrl, 3 or pre);
  cfgreadl(WIFI, regs + Ioctrl);
  cfgwritel(WIFI, regs + Resetctrl, 1);

  MicrosecondDelay(10);

  while((cfgreadl(WIFI, regs + Resetctrl) and 1) = 0) do
    begin
      MicrosecondDelay(10);
    end;

  cfgwritel(WIFI, regs + Ioctrl, 3 or ioctl);
  cfgreadl(WIFI, regs + Ioctrl);
 except
   on e : exception do
     WIFILogError(nil, 'exception in sbdisable 0x' + inttohex(longword(exceptaddr), 8));
 end;
end;

procedure sbmem(WIFI : PWIFIDevice; write : boolean; buf : pointer; len : longword; off : longword);
var
  n : longword;
  addr : longword;
  Res : longword;
begin
  n := (((off)+(Sbwsize)-1) div (Sbwsize) * (Sbwsize)) - off;
  if (n = 0) then
    n := Sbwsize;

  {$ifdef CYW43455_SDIO_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sbmem len='+inttostr(len) + ' n=' + inttostr(n) + ' offset=0x'+inttohex(off,8) + ' off&(sbwsize-1)=0x' + inttohex(off and (Sbwsize-1), 8));
  {$endif}

  while (len > 0) do
  begin
    if (n > len) then
      n := len;

    WIFIDeviceSetBackplaneWindow(WIFI, off);
    addr := off and (sbwsize-1);

    if (len >= 4) then
      addr := addr or $8000;

    if (n < WIFI_BAK_BLK_BYTES) then
      Res := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioWrite, BACKPLANE_FUNCTION, addr, true, buf, 0, n)
    else
    begin
      Res := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioWrite, BACKPLANE_FUNCTION, addr, true, buf, n div WIFI_BAK_BLK_BYTES, WIFI_BAK_BLK_BYTES);
      n := (n div WIFI_BAK_BLK_BYTES) * WIFI_BAK_BLK_BYTES;
    end;

    if (Res <> WIFI_STATUS_SUCCESS) then
    begin
      WIFILogError(nil, 'Error transferring to/from backplane 0x' + inttohex(addr,8) + ' ' + inttostr(n) + 'bytes (write='+booltostr(write, true)+')');
    end;

    off += n;
    buf += n;
    len -= n;
    n := Sbwsize;
  end;
end;

procedure sbreset(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);

begin
 sbdisable(WIFI, regs, pre, ioctl);
 WIFIDeviceSetBackplaneWindow(WIFI, regs);
 {$ifdef CYW43455_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sbreset entry regs 0x' + inttohex(regs, 8) + ' regs+ioctrl val 0x'
                   + inttohex(cfgreadl(WIFI, regs + IOCtrl), 8)
                   + ' regs+resetctrl val 0x ' + inttohex(cfgreadl(WIFI, regs + Resetctrl), 8));
 {$endif}

  while ((cfgreadl(WIFI, regs + Resetctrl) and 1) <> 0) do
  begin
    cfgwritel(WIFI, regs + Resetctrl, 0);
    MicrosecondDelay(40);
  end;

  cfgwritel(WIFI, regs + Ioctrl, 1 or ioctl);
  cfgreadl(WIFI, regs + Ioctrl);

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sbreset exit regs+ioctrl val 0x' + inttohex(cfgreadl(WIFI, regs + IOCtrl), 8)
                    + ' regs+resetctrl val 0x ' + inttohex(cfgreadl(WIFI, regs + Resetctrl), 8));
  {$endif}
end;

procedure WIFIDeviceRamScan(WIFI : PWIFIDevice);
var
 n, size : longword;
 r : longword;
 banks, i : longword;
begin
  if (WIFI^.armcore = ARMcr4) then
  begin
    r := WIFI^.armregs;
    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'ramscan armcr4 0x' + inttohex(r, 8));
    {$endif}

    WIFIDeviceSetBackplaneWindow(WIFI, r);
    r := (r and $7fff) or $8000;
    n := cfgreadl(WIFI, r + Cr4Cap);

    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'cr4 banks 0x' + inttohex(n, 8));
    {$endif}

    banks := ((n shr 4) and $F) + (n and $F);
    size := 0;

    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'banks='+inttostr(banks));
    {$endif}

    for i := 0 to banks - 1 do
    begin
       cfgwritel(WIFI, r + Cr4Bankidx, i);
       n := cfgreadl(WIFI, r + Cr4Bankinfo);
       {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
       {$endif}

       size += 8192 * ((n and $3F) + 1);
    end;
    WIFI^.socramsize := size;
    WIFI^.rambase := $198000;
    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Socram size=0x'+inttohex(size, 8) + ' rambase=0x'+inttohex(WIFI^.rambase, 8));
    {$endif}

    exit;
  end;

  sbreset(WIFI, WIFI^.socramctl, 0, 0);
  r := WIFI^.socramregs;
  WIFIDeviceSetBackplaneWindow(WIFI, r);
  n := cfgreadl(WIFI, r + Coreinfo);

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil ,'socramrev ' + inttostr(WIFI^.socramrev) + ' coreinfo 0x' + inttohex(n, 8));
  {$endif}

  banks := (n>>4) and $F;
  size := 0;
  for i := 0 to banks-1 do
  begin
    cfgwritel(WIFI, r + Bankidx, i);
    n := cfgreadl(WIFI, r + Bankinfo);
    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
    {$endif}

    size += 8192 * ((n and $3F) + 1);
  end;
  WIFI^.socramsize := size;
  WIFI^.rambase := 0;
  if(WIFI^.chipid = 43430) then
  begin
    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'updating bankidx values for 43430');
    {$endif}

    cfgwritel(WIFI, r + Bankidx, 3);
    cfgwritel(WIFI, r + Bankpda, 0);
  end;

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Socram size=0x'+inttohex(size, 8) + ' rambase=0x'+inttohex(WIFI^.rambase, 8));
  {$endif}
end;


(*
 * Condense config file contents (in buffer buf with length n)
 * to 'var=value\0' list for firmware:
 *	- remove comments (starting with '#') and blank lines
 *	- remove carriage returns
 *	- convert newlines to nulls
 *	- mark end with two nulls
 *	- pad with nulls to multiple of 4 bytes total length
 *)

function condense(buf : pchar; n : integer) : integer;
var
 p, ep, lp, op : pchar;
 c : char;
 skipping : boolean;
 // i : integer;
begin

  Result := 0;
  skipping := false;      // true if in a comment
  ep := buf + n;          // end of input
  op := buf;              // end of output
  lp := buf;              // start of current output line

  p := buf;
  while (p < ep) do
  begin
    c := p^;

    case c of
      '#' : skipping := true;
      #0,
      #10 : begin
              skipping := false;
    	      if (op <> lp) then
              begin
                op^ := #0;
                op += 1;
                lp := op;
              end;
            end;
      #13: ; //do nothing (don't include in output)
    else
      if (not skipping) then
      begin
        op^ := c;
        op += 1;
      end;
    end;

    p += 1;
  end;

  if( not skipping) and (op <> lp) then
  begin
    op^ := #0;
    op+=1;
  end;

  op^ := #0;
  op+=1;

  // pad with nulls to multiple of 4 bytes length
  // note input has to be dword aligned to avoid a crash here.

  n := op - buf;
  while (n and 3 <> 0) do
  begin
    op^ := #0;
    op += 1;
    n += 1;
  end;
  Result := n;
end;

procedure put4(var p : byte4; v : longword);
begin
  p[1] := byte(v and $ff);
  p[2] := byte(v >> 8);
  p[3] := byte(v >> 16);
  p[4] := byte(v >> 24);
end;

procedure put4_2(p  : pbyte; v : longword);
begin
  p^ := byte(v and $ff);
  (p+1)^ := byte(v >> 8);
  (p+2)^ := byte(v >> 16);
  (p+3)^ := byte(v >> 24);
end;

procedure put2(p : pbyte; v : word);
begin
  p^ := v and $ff;
  (p+1)^ := (v shr 8) and $ff;
end;

function WIFIDeviceDownloadFirmware(WIFI : PWIFIDevice) : Longword;

var
 FirmwareFile : file of byte;
 firmwarep : pbyte;
 comparebuf : pbyte;
 off : longword;
 fsize : longword;
 i : integer;
 lastramvalue : longword;
 chunksize : longword;
 bytesleft : longword;
 bytestransferred : longword;
 Found : boolean;
 ConfigFilename : string;
 FirmwareFilename : string;
 bytebuf : array[1..4] of byte;
begin
 try
  Result := WIFI_STATUS_INVALID_PARAMETER;

  // zero out an address of some sort which is at the top of the ram?
  lastramvalue := 0;
  WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.rambase + WIFI^.socramsize - 4);
  SDIOWIFIDeviceReadWriteExtended(WIFI, sdioWrite, BACKPLANE_FUNCTION, (WIFI^.rambase + WIFI^.socramsize - 4) and $7fff{ or $8000}, true, @lastramvalue, 0, 4);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Starting WIFI firmware load...');

  // locate firmware detils based on chip id and revision

  Found := false;

  for i := 1 to FIRWMARE_OPTIONS_COUNT do
  begin
    if (firmware[i].chipid = WIFI^.chipid) and (firmware[i].chipidrev = WIFI^.chipidrev) then
    begin
      FirmwareFilename := FIRMWARE_FILENAME_ROOT + firmware[i].firmwarefilename;
      ConfigFilename := FIRMWARE_FILENAME_ROOT + firmware[i].configfilename;
      Found := true;
      break;
    end;
  end;

  if (not Found) then
  begin
    WIFILogError(nil, 'Unable to find a suitable firmware file to load for chip id 0x' + inttohex(WIFI^.chipid, 4) + ' revision 0x' + inttohex(WIFI^.chipidrev, 4));
    exit;
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Using ' + FirmwareFilename + ' for firmware.');

  // open file and read entire block into memory. Perhaps ought to do this in
  // chunks really? If we do, then the verify stuff needs to be done a chunk
  // at a time as well.

  if (not FileExists(FirmwareFilename)) then
  begin
    Result := WIFI_STATUS_INVALID_DATA;
    exit;
  end;

  assignfile(FirmwareFile, FirmwareFilename);
  reset(FirmwareFile);
  fsize := filesize(FirmwareFile);
  getmem(firmwarep, fsize);
  blockread(FirmwareFile, firmwarep^, fsize);
  closefile(FirmwareFile);

  // transfer firmware over the bus to the chip.
  // first, grab the reset vector from the first 4 bytes of the firmware.
  // Not needed on a Pi Zero as the firmware loads at addres 0 by default - needs updating to suit.

  move(firmwarep^, WIFI^.resetvec, 4);
  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Reset vector of 0x' + inttohex(WIFI^.resetvec, 8) + ' copied out of firmware');
  {$endif}


  off := 0;
  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

  // dword align the buffer size.
  if (fsize mod 4 <> 0) then
    fsize := ((fsize div 4) + 1) * 4;

  getmem(comparebuf, fsize);

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Bytes to transfer: ' + inttostr(fsize));
  {$endif}

  bytestransferred := 0;
  dodumpregisters := false;

  while bytestransferred < fsize do
  begin
    sbmem(WIFI, true, firmwarep+off, chunksize, WIFI^.rambase + off);
    bytestransferred := bytestransferred + chunksize;

    off += chunksize;
    bytesleft := fsize - bytestransferred;
    if (bytesleft < chunksize) then
      chunksize := bytesleft;
  end;


  off := 0;
  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

  freemem(firmwarep);



  // now we need to upload the configuration to ram

  if (not FileExists(ConfigFilename)) then
  begin
    Result := WIFI_STATUS_INVALID_DATA;
    exit;
  end;

  AssignFile(FirmwareFile, ConfigFilename);
  Reset(FirmwareFile);
  FSize := FileSize(FirmwareFile);

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Size of firmware config file is ' + inttostr(fsize) + ' bytes');
  {$endif}

  // dword align to be sure
  if (fsize mod 4 <> 0) then
    fsize := ((fsize div 4) + 1) * 4;

  GetMem(firmwarep, fsize);
  BlockRead(FirmwareFile, FirmwareP^, FileSize(FirmwareFile));



  fsize := Condense(PChar(FirmwareP), Filesize(FirmwareFile)); // note we deliberately *don't* use fsize here!

  // Although what we've done here is correct, I noticed that ether4330.c only
  // reads the first 2048 bytes of the config which it then condenses, resulting
  // in a config string of 1720 bytes which misses off the last few items and
  // truncates on of the assigned values.
  // This just looks like a simple bug - on a Pi3B the file is about 2074 bytes
  // and perhaps in the past it was smaller so would fit in the 2048 byte read.

  off := WIFI^.rambase + WIFI^.socramsize - fsize - 4;
  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Tansferring config file to socram at offset 0x' + inttohex(off, 8));
  {$endif}

  bytestransferred := 0;

  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

  while bytestransferred < fsize do
  begin
    sbmem(WIFI, true, firmwarep+bytestransferred, chunksize, off);
    bytestransferred := bytestransferred + chunksize;

    off += chunksize;
    bytesleft := fsize - bytestransferred;
    if (bytesleft < chunksize) then
      chunksize := bytesleft;
  end;

  Freemem(firmwarep);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Finished transferring config file to socram');

  // I believe this is some sort of checksum
  fsize := fsize div 4;
  fsize := (fsize and $ffff) or ((not fsize) << 16);

  // write checksum thingy to ram.

  put4(bytebuf, fsize);
  sbmem(WIFI, true, @bytebuf[1], 4, WIFI^.rambase + WIFI^.socramsize - 4);

  // I think this brings the arm core back up after writing the firmware.
  if (WIFI^.armcore = ARMcr4) then
  begin
     WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.sdregs);
     cfgwritel(WIFI, WIFI^.sdregs + IntStatus, $ffffffff);
     // write reset vector to bottom of RAM
     if (WIFI^.resetvec <> 0) then
     begin
       {$ifdef CYW43455_DEBUG}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Firmware upload: Writing reset vector to address 0');
       {$endif}

       sbmem(WIFI, true, @WIFI^.resetvec, sizeof(WIFI^.resetvec), 0);
     end;

     // reactivate the core.
     sbreset(WIFI, WIFI^.armctl, Cr4Cpuhalt, 0);
  end
  else
     sbreset(WIFI, WIFI^.armctl, 0, 0);
 except
   on e : exception do
     WIFILogError(nil, 'exception : ' + e.message + ' at address ' + inttohex(longword(exceptaddr),8));
 end;
end;

procedure sbenable(WIFI : PWIFIDevice);
var
  i : integer;
  iobits : byte;
  mbox : longword;
  ints : longword;
begin
  WIFIDeviceSetBackplaneWindow(WIFI, BAK_BASE_ADDR);
  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Enabling high throughput clock...');
  cfgw(WIFI, BAK_CHIP_CLOCK_CSR_REG, 0);
  sleep(1);
  cfgw(WIFI, BAK_CHIP_CLOCK_CSR_REG, ReqHT);

  // wait for HT clock to become available. 100ms timeout approx
  i := 0;
  while ((cfgr(WIFI, BAK_CHIP_CLOCK_CSR_REG) and HTavail) = 0) do
  begin
    i += 1;
    if (i = 100) then
    begin
      WIFILogError(nil, 'Could not enable HT clock; csr=' + inttohex(cfgr(WIFI, BAK_CHIP_CLOCK_CSR_REG), 8));
      exit;
    end;

    Sleep(1);
  end;

  cfgw(WIFI, BAK_CHIP_CLOCK_CSR_REG, cfgr(WIFI, BAK_CHIP_CLOCK_CSR_REG) or ForceHT);
  sleep(10);

  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'After request for HT clock, CSR_REG=0x' + inttohex(cfgr(WIFI, BAK_CHIP_CLOCK_CSR_REG), 4));
  {$endif}

  WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.sdregs);

  cfgwritel(WIFI, WIFI^.sdregs + Sbmboxdata, 4 shl 16);   // set protocol version
  cfgwritel(WIFI, WIFI^.sdregs + Intmask, FrameInt or MailboxInt or Fcchange);

  // enable function 2
  SDIOWIFIDeviceReadWriteDirect(WIFI, sdioRead, BUS_FUNCTION, SDIO_CCCR_IOEx, 0, @iobits);
  SDIOWIFIDeviceReadWriteDirect(WIFI, sdioWrite, BUS_FUNCTION, SDIO_CCCR_IOEx, iobits or SDIO_FUNC_ENABLE_2, nil);

  // now wait for function 2 to be ready
  i := 0;
  iobits := 0;
  while ((iobits and SDIO_FUNC_ENABLE_2) = 0) do
  begin
    i += 1;
    if (i = 10) then
    begin
      WIFILogError(nil, 'Could not enable SDIO function 2; iobits=0x'+inttohex(iobits, 8));
      exit;
    end;

    SDIOWIFIDeviceReadWriteDirect(WIFI, sdioRead, BUS_FUNCTION, SDIO_CCCR_IORx, 0, @iobits);

    Sleep(100);
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Radio function (f2) successfully enabled');

  // enable interrupts.
  SDIOWIFIDeviceReadWriteDirect(WIFI,sdioWrite, BUS_FUNCTION, SDIO_CCCR_IENx, (INTR_CTL_MASTER_EN or INTR_CTL_FUNC1_EN or INTR_CTL_FUNC2_EN), nil );

  ints := 0;
  while (ints = 0) do
  begin
    ints := cfgreadl(WIFI, WIFI^.sdregs + Intstatus);
    cfgwritel(WIFI, WIFI^.sdregs + Intstatus, ints);

    if ((ints and mailboxint) > 0) then
    begin
      mbox := cfgreadl(WIFI, WIFI^.sdregs + Hostmboxdata);
      cfgwritel(WIFI, WIFI^.sdregs + Sbmbox, 2);	//ack
      if ((mbox and $8) = $8) then
      begin
         if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'The Broadcom firmware reports it is ready!')
      end
      else
        WIFILogError(nil, 'The firmware is not ready! mbox=0x'+inttohex(mbox, 8));
    end
    else
      WIFILogError(nil, 'Mailbox interrupt was not set as expected ints=0x'+inttohex(ints, 8));
  end;

  // It seems like we need to execute a read first to kick things off. If we don't do this the first
  // IOCTL command response will be an empty one rather than the one for the IOCTL we sent.
  if (SDIOWIFIDeviceReadWriteExtended(WIFI, sdioRead, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, @ioctl_rxmsg, 0, 64) <> WIFI_STATUS_SUCCESS) then
     WIFILogError(nil, 'Unsuccessful initial read from WIFI function 2 (packets)')
  else
  begin
   {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Successfully read function 2 first empty response.');
   {$endif}
  end;


  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Device Enabled');
end;




function WirelessIOCTLCommand(WIFI : PWIFIDevice; cmd : integer;
                                   InputP : Pointer;
                                   InputLen : Longword;
                                   write : boolean; ResponseDataP : Pointer;
                                   ResponseDataLen : integer;
                                   trace : string = '') : longword;

var
  msgp : PIOCTL_MSG = @ioctl_txmsg;
  responseP  : PIOCTL_MSG;
  cmdp : IOCTL_CMDP;
  TransmitDataLen : longword;
  HeaderLen : longword;
  TransmitLen : longword;
  Res : longword;
  WorkerRequestP : PWIFIRequestItem;

begin
  Result := WIFI_STATUS_INVALID_PARAMETER;

  if txglom then
    cmdp := @(msgp^.glom_cmd.cmd)
  else
    cmdp := @(msgp^.cmd);

  {$ifdef CYW43455_SDIO_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'wirelessioctlcmd write='+booltostr(write, true)
                   + ' cmd='+inttostr(cmd)
                   + ' InputLen='+inttostr(InputLen)
                   + ' datalen='+inttostr(responsedatalen));
  {$endif}

  if (write) then
    TransmitDataLen := InputLen + ResponseDataLen
  else
    TransmitDataLen := max(InputLen, ResponseDataLen);

  // works out header length by subtracting addresses
  // this might look wrong but cmdp is a pointer to msgp's cmd and msgp is
  // a pointer to ioctl_txmsg. therefore the address are from the same instance.
  HeaderLen := longword(@cmdp^.data) - longword(@ioctl_txmsg);
  TransmitLen := ((HeaderLen + TransmitDataLen + 3) div 4) * 4;

    // Prepare IOCTL command
  fillchar(msgp^, sizeof(IOCTL_MSG), 0);

  msgp^.len := HeaderLen + TransmitDataLen;
  msgp^.notlen := not msgp^.len;

  if (txglom) then
  begin
    msgp^.glom_cmd.glom_hdr.len := HeaderLen + TransmitDataLen - 4;
    msgp^.glom_cmd.glom_hdr.flags := 1;
  end;

  cmdp^.sdpcmheader.seq := txseq;
  if (txseq < 255) then
    txseq += 1
  else
    txseq := 0;

  if (txglom) then
    cmdp^.sdpcmheader.hdrlen := 20
  else
    cmdp^.sdpcmheader.hdrlen := 12;

  cmdp^.cmd := cmd;
  cmdp^.outlen := TransmitDataLen;

  // request id is a word, so need to stay within limits.
  if (ioctl_reqid > $fffe) then
    ioctl_reqid := 1
  else
    ioctl_reqid := ioctl_reqid + 1;

  if (write) then
    cmdp^.flags := (ioctl_reqid << 16) or 2
  else
    cmdp^.flags := (ioctl_reqid << 16);

  if (InputLen > 0) then
  begin
    move(InputP^, cmdp^.data[0], InputLen);
  end;

  if (write) then
    move(ResponseDataP^, PByte(@(cmdp^.data[0])+InputLen)^, ResponseDataLen);

  // Signal to the worker thread that we need a response for this request.
  WorkerRequestP := WIFIWorkerThread.AddRequest(ioctl_reqid, [], nil, nil);

  // Send IOCTL command.
  // Is it safe to submit multiple ioctl commands and then see the events come through
  // out of order? I think so but needs testing and investigating.
  // requests are safe because the sdio read write functions have a spinlock.

  {$ifdef CYW43455_SDIO_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'sending ' + inttostr(TransmitLen) + ' bytes to the wifi device');
  {$endif}

  Res := SDIOWIFIDeviceReadWriteExtended(WIFI, sdioWrite, WLAN_FUNCTION, BAK_BASE_ADDR and $1FFFF{ SB_32BIT_WIN}, false, msgp, 0, TransmitLen);
  Result := Res;
  if (Res <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'failed to send cmd53 for ioctl command ' + inttostr(res));

  // wait for the worker thread to process the response.
  SemaphoreWait(WorkerRequestP^.Signal);

  // use old variable for now so copy paste from old code works still
  ResponseP := WorkerRequestP^.MsgP;
  if (ResponseP = nil) then
  begin
    WIFILogError(nil, 'Nil response item in WirelessIOCTLCommand');
    exit;
  end;

  // Now we have the response we can validate it.
  if ((responseP^.cmd.sdpcmheader.chan and $f) <> 0) then
    WIFILogError(nil, 'IOCTL response received for a non-zero channel');

  if (((ResponseP^.cmd.flags >> 16) and $ffff) <> WorkerRequestP^.RequestID) then
    WIFILogError(nil, 'IOCTL response received for a different request id. We got one for '
                      + inttostr(responsep^.cmd.flags >> 16)
                      + ' whereas our request was for '
                      + inttostr(workerrequestp^.RequestID));

  // in cases where the response is smaller than the command parameters we have to move less data.
  // need to verify this as I had some weird if statement in there before.
  move(ResponseP^.cmd.Data[0], ResponseDataP^, ResponseDataLen);

  WIFIWorkerThread.DoneWithRequest(WorkerRequestP);
end;

function WirelessGetVar(WIFI : PWIFIDevice; varname : string; ValueP : PByte; len : integer) : longword;
begin
  // getvar name must have a null on the end of it.
  varname := varname + #0;

  Result := WirelessIOCTLCommand(WIFI, WLC_GET_VAR, @varname[1], length(varname), false, ValueP, len);
end;

function WirelessSetVar(WIFI : PWIFIDevice; varname : string; InputValueP : PByte; Inputlen : integer; trace : string = '') : longword;
begin
  varname := varname + #0;
  Result := WirelessIOCTLCommand(WIFI, WLC_SET_VAR, @varname[1], length(varname), true, InputValueP, Inputlen, trace);
end;

function WirelessSetInt(WIFI : PWIFIDevice; varname : string; Value : longword) : longword;
begin
  Result := WirelessSetVar(WIFI, varname, @Value, 4);
end;

function WirelessCommandInt(WIFI : PWIFIDevice; wlccmd : longword; Value : longword) : longword;
var
  response : byte4;
begin
  Result := WirelessIOCTLCommand(WIFI, wlccmd, @Value, 4, true, @response[1], 4);
end;

function WIFIDeviceUploadRegulatoryFile(WIFI : PWIFIDevice) : longword;
const
  Reguhdr = 2+2+4+4;
  Regusz = 400;
  Regutyp = 2;
  Flagclm = 1 shl 12;
  Firstpkt = 1 shl 1;
  Lastpkt = 1 shl 2;

var
 FirmwareFile : file of byte;
 firmwarep : pbyte;
 off : longword;
 fsize : longword;
 i : integer;
 chunksize : longword;
 Found : boolean;
 RegulatoryFilename : string;
 flag : word;

begin
  Result := WIFI_STATUS_SUCCESS;

  // locate regulatory detils based on chip id and revision

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Starting to upload regulatory file');
  Found := false;

  for i := 1 to FIRWMARE_OPTIONS_COUNT do
  begin
    if (firmware[i].chipid = WIFI^.chipid) and (firmware[i].chipidrev = WIFI^.chipidrev) then
    begin
      // If no regulatory file just succeed
      if Length(firmware[i].regufilename) = 0 then
      begin
       Result := WIFI_STATUS_SUCCESS;
       Exit;
      end;

      RegulatoryFilename := FIRMWARE_FILENAME_ROOT + firmware[i].regufilename;
      Found := true;
      break;
    end;
  end;

  if (not Found) then
  begin
    Result := WIFI_STATUS_INVALID_PARAMETER;
    WIFILogError(nil, 'Unable to find a suitable firmware file to load for chip id 0x' + inttohex(WIFI^.chipid, 4) + ' revision 0x' + inttohex(WIFI^.chipidrev, 4));
    exit;
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Using ' + RegulatoryFilename + ' for regulatory file.');

  // now regulatory file if there is one.

  if (not FileExists(RegulatoryFilename)) then
  begin
    Result := WIFI_STATUS_INVALID_DATA;
    exit;
  end;

  AssignFile(FirmwareFile, RegulatoryFilename);
  Reset(FirmwareFile);
  FSize := FileSize(FirmwareFile);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Size of regulatory file is ' + inttostr(fsize) + ' bytes');

  GetMem(firmwarep, Reguhdr+Regusz+1);

  put2(firmwarep+2, Regutyp);
  put2(firmwarep+8, 0);
  off := 0;
  flag := Flagclm or Firstpkt;
  chunksize := 0;

  try
    while ((flag and Lastpkt) = 0) do
    begin
      // read a block of data from the file
      BlockRead(FirmwareFile, (firmwarep+Reguhdr)^, Regusz, chunksize);
      if (chunksize <= 0) then
        break;

      if (chunksize <> Regusz) then
      begin
        // fill out end of the block with zeroes.
        while ((chunksize and 7) > 0) do
        begin
          (firmwarep+Reguhdr+chunksize)^ := 0;
          chunksize += 1;
        end;
        flag := flag or Lastpkt;
      end;

      put2(firmwarep+0, flag);
      put4_2(firmwarep+4, chunksize);

      Result := WirelessSetVar(WIFI, 'clmload', firmwarep, Reguhdr + chunksize);
      if (Result <> WIFI_STATUS_SUCCESS) then
        exit;

      off += chunksize;
      flag := flag and (not Firstpkt);
    end;


  finally
   freemem(firmwarep);
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Finished transferring regulatory file');
end;


function whd_tlv_find_tlv8(message : pbyte; message_length : longword; atype : byte) : pwhd_tlv8_data;
var
  current_tlv_type : byte;
  current_tlv_length : byte;
begin
  // scans list of TLV's for the one with tge specified atype.
  // and returns a pointer to it.

  Result := nil;

  while (message_length <> 0) do
  begin
    current_tlv_type := message^;
    current_tlv_length := pword(message+1)^ + 2;

    // Check if we've overrun the buffer
    if (current_tlv_length > message_length) then
      Exit;

    // Check if we've found the type we are looking for
    if (current_tlv_type = atype) then
    begin
      Result := pwhd_tlv8_data(message);
      exit;
    end;

    // Skip current TLV
    message += current_tlv_length;
    message_length -= current_tlv_length;
  end;
end;

procedure WirelessScanCallback(Event : TWIFIEvent;
            EventRecordP : pwhd_event;
            RequestItemP : PWIFIRequestItem;
            DataLength : longint);
var
  ScanResultP : pwl_escan_result;
  ssidstr, s : string;
begin
  if (Event = WLC_E_ESCAN_RESULT) then
  begin
    scanresultp := pwl_escan_result(pbyte(longword(@eventrecordp^.whd_event) + sizeof(whd_event_msg)));
    ssidstr := buftostr(@scanresultp^.bss_info[1].SSID[0], scanresultp^.bss_info[1].SSID_len, true);
    if (ssidstr = '') then
      ssidstr := '<hidden>';

    s := 'SSID='+ssidstr
                  + ' event status=' + inttostr(eventrecordp^.whd_event.status)
                  + ' buflen = '+inttostr(scanresultp^.buflen)
                  + ' channel = ' +inttostr(scanresultp^.bss_info[1].chanspec and $ff)
                  + ' chanspec = ' +inttohex(scanresultp^.bss_info[1].chanspec, 8)
                  + ' BSSID = ' +inttohex(scanresultp^.bss_info[1].BSSID.octet[0], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID.octet[1], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID.octet[2], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID.octet[3], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID.octet[4], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID.octet[5], 2);

    if (eventrecordp^.whd_event.status = 8) then
      s := s + ' Partial scan result';
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, s);

    if (RequestItemP^.UserDataP <> nil) then
    begin
      // the user data here is a callback function. See WirelessScan.
     TWIFIScanUserCallback(RequestItemP^.UserDataP)(ssidstr, ScanResultP);
    end;
  end
  else
    WIFILogError(nil, 'Wireless Scan: Unexpected event received');

end;

procedure WirelessScan(UserCallback : TWIFIScanUserCallback; WaitTime : Longint = 10000); cdecl;

var
  scanparams : wl_escan_params;
  i : integer;
  RequestItemP : PWIFIRequestItem;

begin

  // scan for wifi networks
  // passive scan - listens for beacons only.

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Starting wireless network scan');

  WirelessCommandInt(WIFI, $b9, $28); // scan channel time
  WirelessCommandInt(WIFI, $bb, $28); // scan unassoc time
  WirelessCommandInt(WIFI, $102, $82); // passive scan time

  WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.sdregs);
  cfgwritel(WIFI, WIFI^.sdregs + IntStatus, 0);

  WirelessCommandInt(WIFI, 49, 0);	// PASSIVE_SCAN */
  WirelessCommandInt(WIFI, 2, 0); // up (command has no parameters)


  // clear scan parameters.
  fillchar(scanparams, sizeof(scanparams), 0);

  // now setup what we need.
  scanparams.version := 1;
  scanparams.action := 1;   // start
  scanparams.sync_id:=NetSwapWord($1234);
  scanparams.params.scan_type := SCAN_TYPE_PASSIVE;
  scanparams.params.bss_type := BSS_TYPE_ANY;
  fillchar(scanparams.params.bssid.octet[0], sizeof(ether_addr), $ff);  // broadcast address
  scanparams.params.nprobes := $ffffffff;
  scanparams.params.active_time := $ffffffff;
  scanparams.params.passive_time := $ffffffff;
  scanparams.params.home_time := $ffffffff;
  for i := 1 to 14 do
  begin
    scanparams.params.channel_list[i].chan:=i;
    scanparams.params.channel_list[i].other:=$2b;
  end;
  scanparams.params.channel_num:=14;

  // add interest in the scan result event so we can get a callback. Add in the user callback too.
  RequestItemP := WIFIWorkerThread.AddRequest(0, [WLC_E_ESCAN_RESULT], @WirelessScanCallback, UserCallback);

  // it's lights out and away we go!
  WirelessSetVar(WIFI, 'escan', @scanparams, sizeof(scanparams));

  // wait 10 seconds (this semaphmore won't actually be signalled although it
  // could be in the future if we were scanning for a specific network name).
  SemaphoreWaitEx(RequestItemP^.Signal, WaitTime);

  WIFIWorkerThread.DoneWithRequest(RequestItemP);
end;

procedure JoinCallback(Event : TWIFIEvent; EventRecordP : pwhd_event; RequestItemP : PWIFIRequestItem; DataLength : Longint);
begin
  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'wireless join callback: removing an event ' + inttostr(ord(event)) + ' from the list (status='+inttostr(eventrecordp^.whd_event.status)+')');

  if (EventRecordP^.whd_event.event_type = Ord(WLC_E_LINK)) then
  begin
    // for the e_link event, the flags have to be tested because these convey the status of the link.
    // (it appears the 'reason' field also has 1 in it when connection lost - ignored here though)
    if (EventRecordP^.whd_event.flags and CYW43455_EVENT_FLAG_LINK = CYW43455_EVENT_FLAG_LINK) then
      RequestItemP^.RegisteredEvents := RequestItemP^.RegisteredEvents - [Event]
    else
      WIFILogError(nil, 'CYW43455: Link went down during network connect');
  end
  else
    RequestItemP^.RegisteredEvents := RequestItemP^.RegisteredEvents - [Event];

  // early termination of the wait.
  if RequestItemP^.RegisteredEvents = [] then
    SemaphoreSignal(RequestItemP^.Signal);
end;



{$ifdef supplicant}
function WirelessJoinNetwork(JoinType : TWIFIJoinType;
                             timeout : integer = 10000) : longword; cdecl;
begin
  Result := ERROR_SUCCESS;

  if (WPASupplicantThread <> nil) then
    WPASupplicantThread.Start;

  if (JoinType = WIFIJoinBlocking) then
    Result := SemaphoreWaitEx(PCYW43455Network(WIFI^.NetworkP)^.NetworkUpSignal, timeout);
end;

{$else}
function WirelessJoinNetwork(ssid : string; security_key : string;
                             countrycode : string;
                             JoinType : TWIFIJoinType;
                             ReconnectType : TWIFIReconnectionType;
                             bssid : ether_addr;
                             usebssid : boolean = false) : longword; cdecl;
var
  data : array[0..1] of longword;
  psk : wsec_pmk;
  responseval : longword;
  auth_mfp : longword;
  wpa_auth : longword;
  simplessid : wlc_ssid;
  RequestEntryP : PWIFIRequestItem;
  countrysettings : countryparams;
  clen : integer;
  extjoinparams : wl_extjoin_params;
  bandforchannel : word;


begin
  (*
   Assumptions:
     the access point exists
     it supports WPA2 security
  *)

  Result := WIFI_STATUS_INVALID_DATA;

  // pass connection details to the retry thread for handling loss of connection.
  BackgroundJoinThread.SetConnectionDetails(SSID, security_key, countrycode, BSSID, UseBSSID, ReconnectType);

  if (JoinType = WIFIJoinBackground) then
  begin
    // set network status down, schedule the join thread to do a background join
    // (it will call this function in blocking form to achieve that)
   {$ifdef CYW43455_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CY43455: Background join scheduled.');
   {$endif}

    // Set join not completed
    PCYW43455Network(WIFI^.NetworkP)^.JoinCompleted := False;

    {Set Status to Down}
    WIFI^.NetworkP^.NetworkStatus := NETWORK_STATUS_DOWN;
    
    SemaphoreSignal(BackgroundJoinThread.FConnectionLost);
    Result := WIFI_STATUS_SUCCESS;
    exit;
  end;

  // set country code
  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Setting country code');
  {$endif}

  clen := length(countrycode);
  fillchar(countrysettings, 0, sizeof(countrysettings));
  move(countrycode[1], countrysettings.country_ie[1], clen);
  move(countrycode[1], countrysettings.country_code[1], clen);
  countrysettings.revision := -1;

  Result := WirelessSetVar(WIFI, 'country', @countrysettings, sizeof(countrysettings));
  if Result <> WIFI_STATUS_SUCCESS then
    exit;

  // Set infrastructure mode
  // 1 = normal, 0 = access point
  Result := WirelessCommandInt(WIFI, WLC_SET_INFRA, 1);
  if (Result <> WIFI_STATUS_SUCCESS) then
    exit;

  Result := WirelessCommandInt(WIFI, WLC_SET_AUTH, WL_AUTH_OPEN_SYSTEM);
  if (Result <> WIFI_STATUS_SUCCESS) then
    exit;

  // whilst it is not recommended, the driver will connect to an open network
  // if you specify an empty security key. Currently the pi400 and Pi Zero 2 W
  // both require a software wpa_supplicant, and therefore until this is completed
  // they cannot connect to a wifi network unless it is unencrypted.

  if (security_key <> '') then
  begin
    // Set Wireless Security Type
    Result := WirelessCommandInt(WIFI, WLC_SET_WSEC, AES_ENABLED);
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    data[0] := 0;  // this is the primary interface
    data[1] := 1;  // wpa security on (enables the firmware supplicant)
    Result := WirelessSetVar(WIFI, 'bsscfg:sup_wpa', @data[0], sizeof(data));
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    // Set the EAPOL version to whatever the AP is using (-1) */
    data[0] := 0;
    data[1] := longword(-1);
    Result := WirelessSetVar(WIFI, 'bsscfg:sup_wpa2_eapver', @data[0], sizeof(data));
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    // Send WPA Key
    // Set the EAPOL key packet timeout value, otherwise unsuccessful supplicant
    // events aren't reported. If the IOVAR is unsupported then continue.
    data[0] := 0;
    data[1] := DEFAULT_EAPOL_KEY_PACKET_TIMEOUT;
    Result := WirelessSetVar(WIFI, 'bsscfg:sup_wpa_tmo', @data, sizeof(data));
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    // Set WPA authentication mode
    wpa_auth := WPA2_AUTH_PSK;
    Result := WirelessIOCTLCommand(WIFI, WLC_SET_WPA_AUTH, @wpa_auth, sizeof(wpa_auth), true, @responseval, 4);
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    fillchar(psk, sizeof(psk), 0);
    move(security_key[1], psk.key[0], length(security_key));
    psk.key_len := length(security_key);
    psk.flags := WSEC_PASSPHRASE;

    // Delay required to allow radio firmware to be ready to receive PMK and avoid intermittent failure
    sleep(1);

    Result := WirelessIOCTLCommand(WIFI, WLC_SET_WSEC_PMK, @psk, sizeof(psk), true, @responseval, 4);
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;

    auth_mfp := 0;
    Result := WirelessSetVar(WIFI, 'mfp', @auth_mfp, 4);
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;
  end;

  // if there is a BSSID we set it here. Setting the SSID (mandatory) is what initiates the join.
  if (usebssid) then
  begin
    Result := WirelessIOCTLCommand(WIFI, WLC_SET_BSSID, @bssid, sizeof(bssid), true, @responseval, 4);
    if (Result <> WIFI_STATUS_SUCCESS) then
      exit;
  end;

  // simple join (no joinparams).
  fillchar(simplessid, sizeof(simplessid), 0);
  move(ssid[1], simplessid.SSID[0], length(ssid));
  simplessid.SSID_len:=length(ssid);
  Result := WirelessIOCTLCommand(WIFI, WLC_SET_SSID, @simplessid, sizeof(simplessid), true, @responseval, 4);
  if (Result <> WIFI_STATUS_SUCCESS) then
    exit;

  // register for join events we are interested in.
  // for an open network we don't expect to see the PSK_SUP event.
  if (security_key <> '') then
    RequestEntryP := WIFIWorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK, WLC_E_PSK_SUP], @JoinCallback, nil)
  else
    RequestEntryP := WIFIWorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK], @JoinCallback, nil);

  // wait for 5 seconds or a completion signal.
  SemaphoreWaitEx(RequestEntryP^.Signal, 7000);

  if (RequestEntryP^.RegisteredEvents = []) then
  begin
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Successfully joined WIFI network ' + SSID);

    // Set join completed
    PCYW43455Network(WIFI^.NetworkP)^.JoinCompleted := True;

    {Set Status to Up}
    WIFI^.NetworkP^.NetworkStatus := NETWORK_STATUS_UP;

    {Notify the Status}
    NotifierNotify(@WIFI^.NetworkP^.Device, DEVICE_NOTIFICATION_UP);
  end
  else
  begin
    Result := WIFI_STATUS_INVALID_PARAMETER;
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'The join attempt to network ' + SSID + ' was unsuccessful');

    if (ReconnectType = WIFIReconnectAlways) then
    begin
      if (BackgroundJoinThread.Suspended) then
        BackgroundJoinThread.Start;

      // Set join not completed
      PCYW43455Network(WIFI^.NetworkP)^.JoinCompleted := False;
      
      {Set Status to Down}
      WIFI^.NetworkP^.NetworkStatus := NETWORK_STATUS_DOWN;

      // tell the join thread to make another join attempt.
      SemaphoreSignal(BackgroundJoinThread.FConnectionLost);
    end;

    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then
    begin
      if (WLC_E_SET_SSID in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'WLC_E_SET_SSID event was not returned from the join request');

      if (WLC_E_LINK in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'WLC_E_LINK event was not returned from the join request');

      if (WLC_E_PSK_SUP in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'WLC_E_PSK_SUP event was not returned from the join request');
    end;
    {$endif}
  end;

  WIFIWorkerThread.DoneWithRequest(RequestEntryP);
end;
{$endif}

function WirelessLeaveNetwork : longword;
var
 simplessid : wlc_ssid;
 responseval : longword;

begin
 Result := WIFI_STATUS_SUCCESS;

 {disable the automatic reconnection}
 BackgroundJoinThread.FReconnectionType:=WIFIReconnectNever;

 {blank out the SSID to leave the network?}
 fillchar(simplessid, sizeof(simplessid), 0);
 simplessid.SSID_len:=0;
 Result := WirelessIOCTLCommand(WIFI, WLC_SET_SSID, @simplessid, sizeof(simplessid), true, @responseval, 4);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;
end;

function WirelessInit(WIFI : PWIFIDevice) : longword;
const
  WLC_SET_PM = 86;

var
  version : array[1..250] of byte;
  eventmask : array[0..WL_EVENTING_MASK_LEN] of byte;

  procedure DisableEvent(id : integer);
  begin
     EventMask[id div 8] := EventMask[id div 8] and (not (1 << (id mod 8)));
  end;


begin
 Result := WIFI_STATUS_INVALID_PARAMETER;

 // set up the event mask
 fillchar(eventmask, sizeof(eventmask), 255);           // turn everything on
 DisableEvent(40);	// E_RADIO
 DisableEvent(44);	// E_PROBREQ_MSG
 DisableEvent(54);	// E_IF
 DisableEvent(71);	// E_PROBRESP_MSG
 DisableEvent(20);	// E_TXFAIL
 DisableEvent(124);	//?
 {$ifdef supplicant}
 DisableEvent(25);      // we are using event 25 (EAPOL) via packets, not via events, so we don't want the event generated.
                        // broadcom firmware generates these as well as sending packets; we only need one or the other
                        // and we've decided to use packets (channel 2).

 DisableEvent(51);	// WLC_E_MULTICAST_DECODE_ERROR. We get this during the authentication process for broadcast
                        // packets on the network (e.g. my home automation devices). Ignore.
 {$endif}

 Result := WirelessSetVar(WIFI, 'event_msgs', @eventmask, sizeof(eventmask));
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 // request the mac address of the wifi device
 fillchar(macaddress[0], sizeof(macaddress), 0);

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Requesting MAC address from the WIFI device...');
 Result := WirelessGetVar(WIFI, 'cur_etheraddr', @macaddress[0], MAC_ADDRESS_LEN);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;


 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'WIFI Current MAC address is '
         + IntToHex(macaddress[0], 2) + ':'
         + IntToHex(macaddress[1], 2) + ':'
         + IntToHex(macaddress[2], 2) + ':'
         + IntToHex(macaddress[3], 2) + ':'
         + IntToHex(macaddress[4], 2) + ':'
         + IntToHex(macaddress[5], 2));

 // upload regulatory file - can't set country and join a network without this.
 Result := WIFIDeviceUploadRegulatoryFile(WIFI);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 // do some further initialisation once the firmware is booted.
 Result := WirelessSetInt(WIFI, 'assoc_listen', 10);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 // powersave
 if (WIFI^.chipid = 43430) or (WIFI^.chipid=$4345) then
   Result := WirelessCommandInt(WIFI, WLC_SET_PM, 0)  // powersave off
 else
   Result := WirelessCommandInt(WIFI, WLC_SET_PM, 2); // powersave fast
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 {$ifndef notxglom}
 WIFILogInfo(nil, 'Enabling support for receiving glom packets');
 // this is NOT an error. txglom = receive, because the context is access point.
 Result := WirelessSetInt(WIFI, 'bus:txglom', 1);
// if Result <> WIFI_STATUS_SUCCESS then
//  exit;
 {$else}
 Result := WirelessSetInt(WIFI, 'bus:txglom', 0);
 {$endif}

 Result := WirelessSetInt(WIFI, 'bus:rxglom', 0);
 // Linux driver says this is allowed to fail
 //if Result <> WIFI_STATUS_SUCCESS then
 // exit;

 Result := WirelessSetInt(WIFI, 'bcn_timeout', 10);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 Result := WirelessSetInt(WIFI, 'assoc_retry_max', 3);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 // get first 50 chars of the firmware version string
 Result := WirelessGetVar(WIFI, 'ver', @version[1], 50);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Firmware version string (partial): ' + buftostr(@version[1], 50));

 Result := WirelessSetInt(WIFI, 'roam_off', 1);
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 Result := WirelessCommandInt(WIFI, $14, 1); // set infra 1
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 Result := WirelessCommandInt(WIFI, 10, 0);  // promiscuous
end;



constructor TWIFIWorkerThread.Create(CreateSuspended : boolean; AWIFI : PWIFIDevice);
begin
  inherited Create(CreateSuspended);
  FWIFI := AWIFI;

  FRequestQueueP := nil;
  FLastRequestQueueP := nil;
  FQueueProtect := CriticalSectionCreate;
end;

destructor TWIFIWorkerThread.Destroy;
begin
  // free memory allocated to the queue and release any semaphores.
  // do this later - most apps just get turned off on shutdown anyway!

  // release other resources.
  CriticalSectionDestroy(FQueueProtect);
  inherited Destroy;
end;

procedure TWIFIWorkerThread.dumpqueue;
var
  entryp : PWIFIRequestItem;
  count : integer;
begin
 CriticalSectionLock(FQueueProtect);
 try
   entryp := FRequestQueueP;
   count := 0;
   while (entryp <> nil) do
   begin
     WIFILogInfo(nil, inttostr(count) + ' 0x' + inttohex(longword(entryp), 8)
          + ' 0x' + inttohex(psemaphoreentry(entryp^.signal)^.Signature, 8)
          + ' handle ' + inttostr(entryp^.signal)
          + ' request id ' + inttostr(entryp^.RequestID)
          );
     entryp := entryp^.nextp;
   end;

 finally
   CriticalSectionUnlock(FQueueProtect);
 end;
end;

function TWIFIWorkerThread.AddRequest(ARequestID : word; InterestedEvents : TWIFIEventSet; Callback : TWirelessEventCallback; UserDataP : Pointer) : PWIFIRequestItem;
var
  ItemP : PWIFIRequestItem;
begin
  Result := nil;
  CriticalSectionLock(FQueueProtect);
  try
    //allocate structure
    getmem(ItemP, sizeof(TWIFIRequestItem));

    //store request id and create semaphore to signal when request is filled.
    ItemP^.RequestId := ARequestID;
    ItemP^.Signal:=SemaphoreCreate(0);
    ItemP^.NextP := nil;
    ItemP^.MsgP := nil;
    ItemP^.RegisteredEvents := InterestedEvents;
    ItemP^.Callback := Callback;
    ItemP^.UserDataP := UserDataP;

    //add to the end of the request list.
    if (FRequestQueueP = nil) then
      FRequestQueueP := ItemP
    else
      FLastRequestQueueP^.NextP := ItemP;

    FLastRequestQueueP := ItemP;
    Result := ItemP;
  finally
    CriticalSectionUnlock(FQueueProtect);
  end;
end;

procedure TWIFIWorkerThread.DoneWithRequest(ARequestItemP : PWIFIRequestItem);
var
  CurP, PrevP : PWIFIRequestItem;
begin
  CriticalSectionLock(FQueueProtect);

  try
    // find the request item in the queue
    CurP := FRequestQueueP;
    PrevP := nil;
    while (CurP <> ARequestItemP) and (CurP <> nil) do
    begin
       PrevP := CurP;
       CurP := CurP^.NextP;
    end;

    if (CurP <> nil) then
    begin
     // remove it from the queue
     if (PrevP <> nil) then
       PrevP^.NextP := CurP^.NextP
     else
       FRequestQueueP := CurP^.NextP;

     if (CurP = FLastRequestQueueP) then
       FLastRequestQueueP := PrevP;

     // dispose of the semaphore and free the memory the request item used
     SemaphoreDestroy(ARequestItemP^.Signal);

     if (ARequestItemP^.MsgP <> nil) then
       FreeMem(ARequestItemP^.MsgP);

     Freemem(ARequestItemP);

    end
    else
      WIFILogError(nil, 'Unable to locate item in the request queue');

  finally
    CriticalSectionUnLock(FQueueProtect);
  end;
end;

function TWIFIWorkerThread.FindRequest(ARequestId : word) : PWIFIRequestItem;
var
  CurP : PWIFIRequestItem;

begin
  Result := nil;

  CriticalSectionLock(FQueueProtect);
  try
    CurP := FRequestQueueP;
    while (CurP <> nil) and (CurP^.RequestID <> ARequestId) do
       CurP := CurP^.NextP;

  finally
    CriticalSectionUnLock(FQueueProtect);
  end;

  Result := CurP;
end;

function TWIFIWorkerThread.FindRequestByEvent(AEvent : longword) : PWIFIRequestItem;
var
  CurP : PWIFIRequestItem;
begin
  // find the first request item that has an interest in the specified event.
  // we really need to find a list of items but we can do that later. We currently
  // only have one consumer of this anyway.

  Result := nil;

  CriticalSectionLock(FQueueProtect);
  try
    CurP := FRequestQueueP;
    while (CurP <> nil) and (not (TWIFIEvent(AEvent) in CurP^.RegisteredEvents)) do
      CurP := CurP^.NextP;

  finally
    CriticalSectionUnLock(FQueueProtect);
  end;

  Result := CurP;
end;



procedure TWIFIWorkerThread.ProcessDevicePacket(var responseP : PIOCTL_MSG;
                             NetworkEntryP : PNetworkEntry;
                             var bytesleft : longword;
                             var isfinished : boolean);
var
 SequenceNumber : word;
 RequestEntryP : PWIFIRequestItem;
 EventRecordP : pwhd_event;
 Framelength : word;
 EtherHeaderP : pether_header;

begin
 // the channel has 4 bits flags in the upper nibble and 4 bits channel number in the lower nibble.
 case (responseP^.cmd.sdpcmheader.chan and $f) of
   // ioctl response
   0: begin
        // Now we have a message, we need to check the sequence id to see if there is a
        // thread waiting to be signaled for the response.

        SequenceNumber := (responseP^.cmd.flags >> 16) and $ffff;
        RequestEntryP := WIFIWorkerThread.FindRequest(SequenceNumber);

        if (RequestEntryP <> nil) then
        begin
          // this isn't very efficient and will need attention later.
         getmem(RequestEntryP^.MsgP, Sizeof(IOCTL_MSG));
         move(ResponseP^, RequestEntryP^.MsgP^, ResponseP^.Len);

         // tell the waiting thread the response is ready.
         SemaphoreSignal(RequestEntryP^.Signal);
        end
        else
          WIFILogError(nil, 'Failed to find a request for sequence number ' + inttostr(sequencenumber));
      end;

   // event
   1: begin
        if (ResponseP^.len <= responsep^.cmd.sdpcmheader.hdrlen) then
          exit;

        // for the Pi Zero, the header structure is a different size. Don't know what the
        // extra 4 bytes represent yet. Note the packet data offset for channel 2 has to be
        // adjusted as well but this is done during init.

        if (FWIFI^.ChipId = CHIP_ID_PI_ZEROW) and (WIFI^.ChipIdRev = 1) then
          EventRecordP := pwhd_event(pbyte(responsep)+responsep^.cmd.sdpcmheader.hdrlen + 8)
        else
          EventRecordP := pwhd_event(pbyte(responsep)+responsep^.cmd.sdpcmheader.hdrlen + 4);

        EventRecordP^.whd_event.status := NetSwapLong(EventRecordP^.whd_event.status);
        EventRecordP^.whd_event.event_type := NetSwapLong(EventRecordP^.whd_event.event_type);
        EventRecordP^.whd_event.reason:= NetSwapLong(EventRecordP^.whd_event.reason);
        EventRecordP^.whd_event.flags := NetSwapWord(EventRecordP^.whd_event.flags);

        if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'event message received ' + inttostr(eventrecordp^.whd_event.event_type)
            + ' status='+inttostr(eventrecordp^.whd_event.status)
            + ' reason='+inttostr(eventrecordp^.whd_event.reason)
            + ' reponsep len='+inttostr(responsep^.len) + ' hdrlen='+inttostr(responsep^.cmd.sdpcmheader.hdrlen));

(*      if (EventRecordP^.whd_event.event_type = ord(WLC_E_EAPOL_MSG)) then
        begin
          wifiloginfo(nil, 'eapol rx dump including headers');
          hexdump(pbyte(responsep), responsep^.len);
        end;
*)

        if (EventRecordP^.whd_event.event_type = ord(WLC_E_DEAUTH))
           or (EventRecordP^.whd_event.event_type = ord(WLC_E_DEAUTH_IND))
           or (EventRecordP^.whd_event.event_type = ord(WLC_E_DISASSOC_IND))
           or
              ((EventRecordP^.whd_event.event_type = ord(WLC_E_LINK))
                and (EventRecordP^.whd_event.flags and CYW43455_EVENT_FLAG_LINK = 0))
           then
        begin
          // the wifi link has gone down. Reset conection
          if (WIFI_LOG_ENABLED) then WIFILogInfo(nil, 'The WIFI link appears to have been lost (flags='+inttostr(EventRecordP^.whd_event.flags)+')');

          // Set join not completed
          PCYW43455Network(FWIFI^.NetworkP)^.JoinCompleted := False;

          {$ifdef supplicant}
          // Set EAPOL not completed
          PCYW43455Network(FWIFI^.NetworkP)^.EAPOLCompleted := False;
          SupplicantOperatingState:=0;
          {$endif}

          {Set Status to Down}
          FWIFI^.NetworkP^.NetworkStatus := NETWORK_STATUS_DOWN;

          {Notify the Status}
          NotifierNotify(@FWIFI^.NetworkP^.Device, DEVICE_NOTIFICATION_DOWN);

          SemaphoreSignal(BackgroundJoinThread.FConnectionLost);
        end;

        // see if there are any requests interested in this event, and if so trigger
        // the callbacks. We only do the first one at the moment; we need a list eventually.

        RequestEntryP := WIFIWorkerThread.FindRequestByEvent(EventRecordP^.whd_event.event_type);
        if (RequestEntryP <> nil) and (RequestEntryP^.Callback <> nil) then
        begin
          RequestEntryP^.Callback(TWIFIEvent(EventRecordP^.whd_event.event_type), EventRecordP, RequestEntryP, 0);
        end
        else
        begin
          {$ifdef CYW43455_DEBUG}
          if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'there was no interest in event ' + IntToStr(EventRecordP^.whd_event.event_type));
          {$endif}
        end;
   end;

   // network packet
   2: begin
      try
        // account for different header length in pi zero
        if (FWIFI^.ChipId = CHIP_ID_PI_ZEROW) and (WIFI^.ChipIdRev = 1) then
        begin
          FrameLength := responseP^.len - 8 - ETHERNET_HEADER_BYTES;
          EtherHeaderP := pether_header(PByte(ResponseP) + ETHERNET_CRC_SIZE + ETHERNET_HEADER_BYTES + 4);
        end
        else
        begin
          FrameLength := responseP^.len - 4 - ETHERNET_HEADER_BYTES;
          EtherHeaderP := pether_header(PByte(ResponseP) + ETHERNET_CRC_SIZE + ETHERNET_HEADER_BYTES);
        end;

{$ifdef supplicant}
        // detect supplicant status. Needs to be more resilient.
        if (WordSwap(EtherHeaderP^.ethertype) = EAP_OVER_LAN_PROTOCOL_ID)
           {and (PCYW43455Network(WIFI^.NetworkP)^.JoinCompleted)} then
        begin
         wifiloginfo(nil,'EAPOL network packet received (after join); length='+inttostr(FrameLength - ETHERNET_CRC_SIZE) + ' operatingstate='+inttostr(SupplicantOperatingState));
           RequestEntryP := WIFIWorkerThread.FindRequestByEvent(longword(WLC_E_EAPOL_MSG));
           if (RequestEntryP <> nil) and (RequestEntryP^.Callback <> nil) {and (SupplicantOperatingState=0)} then
           begin
             // this is kinda dirty but it works.
             // we pass the packet in the user data pointer instead as this isn't an event.
             RequestEntryP^.UserDataP := EtherHeaderP;
             RequestEntryP^.Callback(WLC_E_EAPOL_MSG, nil, RequestEntryP, FrameLength - ETHERNET_HEADER_BYTES);
           end;
        end
        else
{$endif}
        begin
          // add into this network entry's packet buffer.
          NetworkEntryP^.Count:=NetworkEntryP^.Count+1;

          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Buffer:=ResponseP;
          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Data:=Pointer(ResponseP) + NetworkEntryP^.Offset;
          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Length:=FrameLength - ETHERNET_CRC_SIZE;

          {Update Statistics}
          Inc(FWIFI^.NetworkP^.ReceiveCount);
          Inc(FWIFI^.NetworkP^.ReceiveBytes,NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Length);

          // update bytesleft from current packet read
          bytesleft := bytesleft - ResponseP^.len;

          // check enough bytes left for a worst case packet for the next read.
          if (bytesleft > sizeof(IOCTL_MSG)) then
          begin
            ResponseP := PIOCTL_MSG(PByte(ResponseP)+ResponseP^.Len);
          end
          else
          begin
            // not enough bytes for next worst case packet so drop out of loop to give time for processing.
            isfinished := true;
            exit;
          end;

        end;
      except
        on e : exception do
          WIFILogError(nil, 'network packet exception ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
      end;
      end;
   end;

end;

procedure TWIFIWorkerThread.Execute;
var
  istatus : longword;
  responseP  : PIOCTL_MSG = @ioctl_rxmsg;
  blockcount, remainder : longword;
  NetworkEntryP : PNetworkEntry;
  PacketP : PNetworkPacket;
  txlen : longword;
  i : integer;
  BytesLeft : longword;
  isfinished : boolean;
  {$ifdef supplicant}
  KeyP : PWPAKey;
  {$endif}
  nGlomPackets : integer;
  SubPacketLengthP : PWord;
  GlomDescriptor : TGlomDescriptor;
  p : integer;
  PrevResponseP : PIOCTL_MSG;
  LastCredit : byte;
  seqdiff : integer;

begin
  {$ifdef CYW43455_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'WIFI Worker thread started.');
  {$endif}

  ThreadSetName(ThreadGetCurrent, 'WIFI Worker Thread');

  txseq := 0;

  try

  while not terminated do
  begin
     // this polls for the interrupt status changing, which tells us there is some data to read.
     istatus := cfgreadl(FWIFI, FWIFI^.sdregs + IntStatus);
     while (istatus and $40 <> $40) and (FWIFI^.NetworkP^.TransmitQueue.Count = 0) do        // safe to check transmit queue here?
     begin
       // give up timeslice to other threads.
       Yield;
       istatus := cfgreadl(FWIFI, FWIFI^.sdregs + IntStatus);
     end;

     if (istatus and $40 = $40) then
     begin
       // clear interrupt status (seems like writing the value back does this based on other drivers)
       cfgwritel(FWIFI, FWIFI^.sdregs + IntStatus, istatus);
       cfgreadl(FWIFI, FWIFI^.sdregs + IntStatus);
     end;

     if MutexLock(FWIFI^.NetworkP^.Lock) = ERROR_SUCCESS then
     begin
      try
         NetworkEntryP:=BufferGet(FWIFI^.NetworkP^.ReceiveQueue.Buffer);
         if (NetworkEntryP <> nil) then
         begin
           if (NetworkEntryP^.Buffer <> nil) then
           begin
             // Point our structure that we read data into from SDIO interface to the receive buffer
             ResponseP := NetworkEntryP^.Buffer;
             NetworkEntryP^.Count := 0;
             BytesLeft := PCYW43455Network(FWIFI^.NetworkP)^.ReceiveRequestSize;
           end;
         end;

         // look at the network device to see if there is any data to receive,
         // only if we had an interrupt flag

         if (istatus and $40 = $40) then
         begin
           if SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioRead, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, ResponseP, 0, SDPCM_HEADER_SIZE) <> WIFI_STATUS_SUCCESS then
           begin
             WIFILogError(nil, 'Error trying to read SDPCM header');
             exit;
           end
           else
           begin

             // once we have a len, keep repeating the reads until no more data is left
             // this is signified by reading a length (at end of loop) and getting zero.
             // we do it this way because the interrupt flag may be set but the device might
             // receive more data in between us clearing the flag and attempting to read the
             // available data.

             isfinished := false;
             while (not isfinished) and (ResponseP^.Len <> 0) do
             begin
               if ((responsep^.len + responsep^.notlen) <> $ffff) then
                  WIFILogError(nil, 'IOCTL Header length failure: len='+inttohex(responsep^.len, 8) + ' notlen='+inttohex(responsep^.notlen, 8));

               if (ResponseP^.Len > SDPCM_HEADER_SIZE) then
               begin
                 blockcount := (ResponseP^.Len-SDPCM_HEADER_SIZE) div 512;
                 remainder := (ResponseP^.Len-SDPCM_HEADER_SIZE) mod 512;
               end
               else
               begin
                blockcount := 0;
                remainder := ResponseP^.Len-SDPCM_HEADER_SIZE;
               end;

               LastCredit := ResponseP^.Cmd.sdpcmheader.credit;

               if (ResponseP^.Len <= NetworkEntryP^.Size) and (ResponseP^.Len > 0) then
               begin
                 if (blockcount > 0) then
                 begin
                   if SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioRead, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, pbyte(responsep)+SDPCM_HEADER_SIZE, blockcount, 512) <> WIFI_STATUS_SUCCESS then
                     WIFILogError(nil, 'Error trying to read blocks for ioctl response');
                 end;

                 if (remainder > 0) then
                 begin
                   if SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioRead, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, pbyte(responsep)+SDPCM_HEADER_SIZE + blockcount*512, 0, remainder) <> WIFI_STATUS_SUCCESS then
                     WIFILogError(nil, 'Error trying to read blocks for ioctl response (len='+inttostr(responsep^.len)+')');
                 end;

                  if ((responseP^.cmd.sdpcmheader.chan and $f) <= 2) then
                    ProcessDevicePacket(responseP, NetworkEntryP, bytesleft, isfinished)
                  else
                  if ((responseP^.cmd.sdpcmheader.chan and $f) = 3) then
                  begin
                     {"glomming" packet (superframe) }

                     if ((responseP^.cmd.sdpcmheader.chan and $80) = $80) then
                     begin
                       {This is the glom descriptor. It contains a list of packet lengths which will be in the
                        superpacket that will follow in the next SDIO read. Each length is a 2 byte quantity}

                       nGlomPackets := (responseP^.len - responseP^.cmd.sdpcmheader.hdrlen) div 2;
                       {$ifdef CYW43455_DEBUG}
                       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Glom descriptor: ' + nGlomPackets.tostring + ' packets present');
                       {$endif}

                       SubPacketLengthP := PWord(pbyte(responseP) + responseP^.cmd.sdpcmheader.hdrlen);
                       for i := 0 to nGlomPackets-1 do
                       begin
                        {$ifdef CYW43455_DEBUG}
                         if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Sub packet length : ' + SubPacketLengthP^.ToString);
                        {$endif}

                         {update total length}
                         WIFI^.ReceiveGlomPacketSize += SubPacketLengthP^;

                         {next entry}
                         SubPacketLengthP += 1;       {1 word, not byte}
                       end;

                       {store glom descriptor}
                       move(ResponseP^, GlomDescriptor, ResponseP^.Len);

                       WIFI^.ReceiveGlomPacketCount += 1;

                       {stop looping if not enough room, so network stack can process data so far}
                       if (bytesleft < nGlomPackets * IOCTL_MAX_BLKLEN) then
                          isfinished := true;
                     end
                     else
                     begin
                       {This is a superpacket. Should have a valid glom descriptor loaded, ready for processing}

                       {move past the superpacket header}
                       PrevResponseP := ResponseP;
                       ResponseP := PIOCTL_MSG(PByte(ResponseP) + responseP^.cmd.sdpcmheader.hdrlen);

                       {process each subpacket}
                       for p := 0 to nGlomPackets-1 do
                       begin
                         ProcessDevicePacket(ResponseP, NetworkEntryP, bytesleft, isfinished);

                         {This scenario should never occur - it results in data loss if it does}
                         if (isfinished) and (p < nGlomPackets-1) then
                           WIFILogError(nil, 'Error: there was not enough buffer space '
                                             + 'available to process the superpacket (bytesleft='
                                             + bytesleft.tostring + ' this len=' + GlomDescriptor.packetlengths[p].ToString + ')');

                         {our buffer location must be updated after processdevicepacket because the individual packet
                          has a length that is 16 byte aliged, whereas the packet length in the individual packet is a data length}
                         ResponseP := PIOCTL_MSG(PByte(PrevResponseP) + GlomDescriptor.packetlengths[p]);
                         PrevResponseP := ResponseP;
                       end;

                       {In order to stop the buffers getting over-filled, we drop out of the loop to allow sending of the
                        data to the network layer, otherwise we might just keep looping until the buffer is full}
                       isfinished := true;
                     end;
                  end
                  else
                  begin
                    WIFILogError(nil,'WIFIWorker: Unrecognised channel message received channel='+inttohex(responseP^.cmd.sdpcmheader.chan and $f, 2) + ' msglen='+inttostr(responsep^.len));
                    hexdump(pbyte(responsep), responsep^.len, 'Unrecognised message on channel ' + inttohex(responseP^.cmd.sdpcmheader.chan and $f, 2));
                  end;
               end
               else
                 if (ResponseP^.Len > 0) then
                   WIFILogError(nil, 'WIFIWorkern: Could not read a large message into an undersized buffer (len='+inttostr(responsep^.len)+')');

               // read next sdpcm header (may not be one present in which case everything will be zero including length)
               if (not isFinished) and (SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioRead, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, ResponseP, 0, SDPCM_HEADER_SIZE, 'worker4') <> WIFI_STATUS_SUCCESS) then
               begin
                 WIFILogError(nil, 'Error trying to read SDPCM header (repeat)');
                 exit;
               end;
             end;
           end;

         end;

        // if we put something into the receive buffer, pass it on the network layer via the receive queue.
        if (NetworkEntryP <> nil) then
        begin
          if (NetworkEntryP^.Count > 0) then
          begin
            FWIFI^.NetworkP^.ReceiveQueue.Entries[(FWIFI^.NetworkP^.ReceiveQueue.Start
                                  + FWIFI^.NetworkP^.ReceiveQueue.Count)
                                  mod  PCYW43455Network(FWIFI^.NetworkP)^.ReceiveEntryCount]
                                    := NetworkEntryP;
            {Update Count}
            Inc(FWIFI^.NetworkP^.ReceiveQueue.Count);
            {Signal Packet Received}
            SemaphoreSignal(FWIFI^.NetworkP^.ReceiveQueue.Wait);
          end
          else
          begin
            // if the buffer wasn't used, return it
            BufferFree(NetworkEntryP);
            NetworkEntryP := nil;
          end;
        end;

        // now look at the transmit queue and see if there is anything to send.

        {Check Count}
        while FWIFI^.NetworkP^.TransmitQueue.Count > 0 do
        begin
          {Get Entry}
          NetworkEntryP := FWIFI^.NetworkP^.TransmitQueue.Entries[FWIFI^.NetworkP^.TransmitQueue.Start];
          if NetworkEntryP <> nil then
          begin
            {Get Packet}
            for i := 0 to NetworkEntryP^.Count - 1 do
            begin
              PacketP:=@NetworkEntryP^.Packets[i];

              //write an sdpcm header
              if (PacketP <> nil) then
              begin
                PSDPCM_HEADER(PacketP^.Buffer+4)^.chan:=2;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.nextlen := 0;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.hdrlen:=sizeof(SDPCM_HEADER)+4;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.credit := 0;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.flow := 0;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.seq:=txseq;
                PSDPCM_HEADER(PacketP^.Buffer+4)^.reserved := 0;

                // we still don't know what this part of the header is.
                // just having to replicate what is in circle at the moment.
                (PLongword(PacketP^.Buffer)+3)^ := NetSwapLong($20000000);

                // needs thread protection?
                if (txseq < 255) then
                  txseq := txseq + 1
                else
                  txseq := 0;

                // If we are beyond the sending limit then we need to wait a bit.
                // it may be more advantageous to drop out of the loop and send the rest
                // of the data next time, as we'd likely get a credit update from the reading
                // code, but that is a larger change.
                if (CYW43455_USE_FIRMWARE_CREDIT_VALUE) then
                begin
                  seqdiff := LastCredit - txseq;
                  if (seqdiff < 0) then
                    seqdiff := seqdiff + 256;

                  if (seqdiff > 127) then
                  begin
                    // the sleep here should allow the firmware to empty its send buffers a bit.
                    // perhaps the sleep should be a function of the difference value.
                    sleep(30);
                  end;
                end;

                PIOCTL_MSG(PacketP^.Buffer)^.len := PacketP^.length+sizeof(SDPCM_HEADER)+4+4;  // 4 for end of sdpcm header, 4 for length itself?
                PIOCTL_MSG(PacketP^.Buffer)^.notlen := not PIOCTL_MSG(PacketP^.Buffer)^.len;

                // calculate transmission sizes
                txlen := PIOCTL_MSG(PacketP^.Buffer)^.len;
                blockcount := txlen div 512;
                remainder := txlen mod 512;

                //Update Statistics
                Inc(FWIFI^.NetworkP^.TransmitCount);
                Inc(FWIFI^.NetworkP^.TransmitBytes, txlen);

                //send data
                if (blockcount > 0) then
                begin
                  if SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioWrite, WLAN_FUNCTION,
                        BAK_BASE_ADDR and $1ffff, false, PacketP^.Buffer, blockcount, 512) <> WIFI_STATUS_SUCCESS then
                          WIFILogError(nil, 'Failed to transmit packet data blocks txseq='+inttostr(txseq)+' lastcredit='+inttostr(LastCredit));
                end;

                if (remainder > 0) then
                begin
                  if SDIOWIFIDeviceReadWriteExtended(FWIFI, sdioWrite, WLAN_FUNCTION,
                        BAK_BASE_ADDR and $1ffff, false, PacketP^.Buffer + blockcount*512, 0, remainder) <> WIFI_STATUS_SUCCESS then
                          WIFILogError(nil, 'Failed to transmit packet data remainder txseq='+inttostr(txseq)+' lastcredit='+inttostr(LastCredit));
                end;

                {$ifdef supplicant}
                // there are two EAPOL messages to be sent by the supplicant - messages 2 & 4.
                // we want keys to be set after these messages are sent, so we detect the sitution here
                // and the keys are set in DoNetworkNotify just before the network is brought up.

                if (pether_header(pbyte(packetp^.buffer) + 8 + sizeof(SDPCM_HEADER))^.ethertype = WordSwap(EAP_OVER_LAN_PROTOCOL_ID)) then
                begin
                  WIFILogDebug(nil, 'Detected transmission of an eapol packet');

                  // if we are transmitting an EAPOL packet, then we must be sending message 2 or greater.
                  if (PCYW43455Network(FWIFI^.NetworkP)^.EAPOLCompleted) then
                  begin
                    WIFILogInfo(nil, 'EAPOL Completed - bringing network up');

                    if (WorkerSchedule(0, @DoNetworkNotify, nil, nil) <> ERROR_SUCCESS) then
                      WIFILogError(nil, 'Failed to schedule task to signify the network is up');
                  end;
                end;
                {$endif}

              end;
            end;
          end;

          {Update Start}
          FWIFI^.NetworkP^.TransmitQueue.Start:=(FWIFI^.NetworkP^.TransmitQueue.Start + 1) mod PCYW43455Network(FWIFI^.NetworkP)^.TransmitEntryCount;

          {Update Count}
          Dec(FWIFI^.NetworkP^.TransmitQueue.Count);

          {Signal Queue Free}
          SemaphoreSignal(FWIFI^.NetworkP^.TransmitQueue.Wait);

          BufferFree(NetworkEntryP);

        end;


      finally
        MutexUnlock(FWIFI^.NetworkP^.Lock);
      end
     end
     else
       WIFILogError(nil, 'WIFIWorkerThread: Failed to get a mutex lock');
  end;

  except
    on e : exception do
     WIFILogError(nil, 'workerthread execute exception ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
  end;
end;


constructor TWirelessReconnectionThread.Create;
begin
  inherited Create(true);

  FConnectionLost := SemaphoreCreate(0);
  FUseBSSID := false;
  FReconnectionType := WIFIReconnectAlways;
  Start;
end;

destructor TWirelessReconnectionThread.Destroy;
begin
  SemaphoreDestroy(FConnectionLost);

  inherited Destroy;
end;

procedure TWirelessReconnectionThread.SetConnectionDetails(aSSID, aKey, aCountry : string;
          BSSID : ether_addr; useBSSID : boolean; aReconnectionType : TWIFIReconnectionType);
begin
  FSSID := aSSID;
  FKey := aKey;
  FCountry := aCountry;
  FUseBSSID:=useBSSID;
  FBSSID := BSSID;
  FReconnectionType := aReconnectionType;
end;

procedure TWirelessReconnectionThread.Execute;
var
  Res : longword;
begin
  ThreadSetName(ThreadGetCurrent, 'WIFI Reconnection Thread');

  while not Terminated do
  begin
    // wifi worker is the source of this sempahore.

    if (SemaphoreWaitEx(FConnectionLost, 1000) = ERROR_SUCCESS) then
      if (FReconnectionType = WIFIReconnectAlways) then
      begin
        // the connection has been lost. Reconnect. Keep trying indefinitely until
        // the join function returns a success.

        WIFILogInfo(nil, 'Reconnection attempt started');

        {$ifdef supplicant}
        {request termination of the supplicant eloop. The supplicant thread will restart it to kick off a new
         connection attempt}
        UltiboEloopTerminate;
        {$else}
        Res := WIFI_STATUS_INVALID_PARAMETER;
        while (Res <> WIFI_STATUS_SUCCESS) do
        begin
          Res := WirelessJoinNetwork(FSSID, FKey, FCountry, WIFIJoinBlocking, WIFIReconnectNever, FBSSID, FUseBSSID); // do *not* call with WIFIReconnectAlways from this thread.
          if (Res <> 0) then
          begin
            {$ifdef CYW43455_DEBUG}
            if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'Wireless Join returned fail ' + inttostr(res));
            {$endif}
          end
          else
            WIFILogInfo(nil, 'Successfully reconnected to the WIFI network');
        end;
        {$endif}
      end
      else
        WIFILogInfo(nil, 'Not reconnecting as reconnections are disabled');
  end;
end;


procedure UltiboTimeProc(var epoch : longint; var millisecond : integer); cdecl;
var
  st : SYSTEMTIME;
  dt : TDateTime;
begin
  dt := Now;
  DateTimeToSystemTime(dt, st);
  epoch := DateTimeToUnixTime(dt);
  millisecond := st.Millisecond;
end;

procedure UltiboMonotonicTimeProc(var epoch : longint; var millisecond : integer); cdecl;
var
  c : QWord;
begin
  c := GetTickCount64;
  epoch := c div 1000;
  millisecond := c mod 1000;
end;


{$ifdef supplicant}
procedure EAPOLCallback(Event : TWIFIEvent;
            EventRecordP : pwhd_event;
            RequestItemP : PWIFIRequestItem;
            DataLength : longint);
var
  packetbufferp : pbyte;
  EtherHeaderP : pether_header;
begin
  if (Event = WLC_E_EAPOL_MSG) then
  begin
    {this callback is now a from a packet rather than an event but it still uses
     the event mechanism to reach this callback.}
    EtherHeaderP := pether_header(RequestItemP^.UserDataP);
    packetbufferp := PByte(EtherHeaderP) + sizeof(ether_header);

    ultibo_driver_new_packet_data(@EtherHeaderP^.source_address[0], packetbufferp, DataLength);

  end
  else
    WIFILogError(nil, 'Wireless Scan: Unexpected event received');

end;


function SupplicantWirelessJoinNetwork(ssid : PChar; authkey : PByte;
                             authkeylen : longword;
                             bssid : Pether_addr;
                             usebssid : boolean = false) : longword; cdecl;
var
  n : integer;
  chan : integer;
  countrysettings : countryparams;
  res : longword;
  data : array[0..1] of longword;
  responseval : longword;
  wpa_auth : longword;
  RequestEntryP : PWIFIRequestItem;
  extjoinparams : wl_extjoin_params;
  auth_mfp : longword;

begin
 fillchar(countrysettings, 0, sizeof(countrysettings));
 move(PCYW43455Network(WIFI^.NetworkP)^.CountryCode[0], countrysettings.country_ie[1], 2);
 move(PCYW43455Network(WIFI^.NetworkP)^.CountryCode[0], countrysettings.country_code[1], 2);
 countrysettings.revision := -1;

 Result := WirelessSetVar(WIFI, 'country', @countrysettings, sizeof(countrysettings));
 if Result <> WIFI_STATUS_SUCCESS then
   exit;

 {Set infrastructure mode, 1 = normal, 0 = access point}
 Result := WirelessCommandInt(WIFI, WLC_SET_INFRA, 1);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;

 {send auth key}
 if (WirelessSetVar(WIFI, 'wpaie', authkey, authkeylen) <> WIFI_STATUS_SUCCESS) then
   exit;


 // Set WPA authentication mode (string style)
 // this one is kinda replaced by the one later down but this is what circle does
 // need a constant for this if it works.
 wpa_auth := WPA2_AUTH_PSK;
 Result := WirelessSetInt(WIFI, 'wpa_auth', 192);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;


 Result := WirelessCommandInt(WIFI, WLC_SET_AUTH, WL_AUTH_OPEN_SYSTEM);
 if (Result <> WIFI_STATUS_SUCCESS) then
  exit;

 // set auth with string method
// result := WirelessSetInt(WIFI, 'auth', WL_AUTH_OPEN_SYSTEM);
// if (Result <> WIFI_STATUS_SUCCESS) then
//   exit;

 {Set Wireless Security Type}
 Result := WirelessCommandInt(WIFI, WLC_SET_WSEC, AES_ENABLED);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;

 // set wsec with string method
// Result := WirelessSetInt(WIFI, 'wsec', AES_ENABLED);
// if (Result <> WIFI_STATUS_SUCCESS) then
//   exit;

 {Set WPA authentication mode}
 wpa_auth := WPA2_AUTH_PSK;
 Result := WirelessIOCTLCommand(WIFI, WLC_SET_WPA_AUTH, @wpa_auth, sizeof(wpa_auth), true, @responseval, 4);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;

 // Set WPA authentication mode (string style)
// wpa_auth := WPA2_AUTH_PSK;
// Result := WirelessSetInt(WIFI, 'wpa_auth', WPA2_AUTH_PSK);
// if (Result <> WIFI_STATUS_SUCCESS) then
//   exit;


 auth_mfp := 0;
 Result := WirelessSetVar(WIFI, 'mfp', @auth_mfp, 4);
 if (Result <> WIFI_STATUS_SUCCESS) then
   exit;

 {use joinparams strucuture to join the network}
  {for now we override this and leave channel unselected (=0)}
  chan := 0;
  if (chan <> 0) then
    chan := chan or $2b00;  { 20Mhz channel width.}

  {configure ssid}
  fillchar(extjoinparams, sizeof(wl_extjoin_params), 0);
  n := min(length(ssid), 32);
  move(ssid[0], extjoinparams.ssid.SSID[0], n);
  extjoinparams.ssid.SSID_len := n;

  {configure scan info}
  extjoinparams.scan_params.scan_type:=$ff;
  if (chan <> 0) then
  begin
    extjoinparams.scan_params.nprobes:=2;
    extjoinparams.scan_params.active_time:=120;
    extjoinparams.scan_params.passive_time:=390;
  end
  else
  begin
   extjoinparams.scan_params.nprobes:=-1;
   extjoinparams.scan_params.active_time:=-1;
   extjoinparams.scan_params.passive_time:=-1;
  end;
  extjoinparams.scan_params.home_time:=-1;

  {configure bssid}
  if (usebssid) then
     move(bssid^.octet[0], extjoinparams.assoc_params.bssid.octet[0], 6) { bssid}
  else
     fillchar(extjoinparams.assoc_params.bssid.octet[0], 6, $ff); { empty}

  {configure channel info}
  if (chan <> 0) then
  begin
    extjoinparams.assoc_params.chanspec_num:=1;
    extjoinparams.assoc_params.chanspec_list[0]:=chan;
  end
  else
    extjoinparams.assoc_params.chanspec_num:=0;

  {execute extended join to network}
  if (chan <> 0) then
    res := WirelessSetVar(WIFI, 'join', @extjoinparams, SizeOf(wl_extjoin_params))
  else
    res := WirelessSetVar(WIFI, 'join', @extjoinparams, SizeOf(wl_extjoin_params) - 4);

  if (res <> 0) then
    WIFILogError(nil, 'Supplicant join: setvar returned fail code of ' + inttostr(res));

  RequestEntryP := WIFIWorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK], @JoinCallback, nil);

  {wait for 5 seconds or a completion signal.}
  SemaphoreWaitEx(RequestEntryP^.Signal, 5000);

  {Set EAPOL not completed}
  PCYW43455Network(WIFI^.NetworkP)^.EAPOLCompleted := False;

  if (RequestEntryP^.RegisteredEvents = []) then
  begin
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'Successfully associated ' + SSID);

    {Set join completed}
    PCYW43455Network(WIFI^.NetworkP)^.JoinCompleted := True;
  end
  else
  begin
    Result := WIFI_STATUS_INVALID_PARAMETER;
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'The join attempt to network ' + SSID + ' was unsuccessful');

    {$ifdef CYW43455_DEBUG}
    if WIFI_LOG_ENABLED then
    begin
      if (WLC_E_SET_SSID in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'WLC_E_SET_SSID event was not returned from the join request');

      if (WLC_E_LINK in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'WLC_E_LINK event was not returned from the join request');
    end;
    {$endif}
  end;

  WIFIWorkerThread.DoneWithRequest(RequestEntryP);
end;

procedure LockEAPOLPacketQueue(trace : pchar); cdecl;
var
 CriticalSectionHandle:TCriticalSectionHandle;
begin
  if (not tryentercriticalsection(EAPOLQueueLock)) then
  begin
    CriticalSectionHandle := TCriticalSectionHandle(EAPOLQueueLock.LockSemaphore);
    writeln(trace, ' failed to get critical section. ownner=',ThreadGetName(CriticalSectionOwner(CriticalSectionHandle)), ' count=',CriticalSectionCount(CriticalSectionHandle), ' current=',ThreadGetName(ThreadGetCurrent));
    EnterCriticalSection(EAPOLQueueLock);
    writeln(trace, 'got critical section');
  end;
end;

procedure UnlockEAPOLPacketQueue; cdecl;
begin
  LeaveCriticalSection(EAPOLQueueLock);
end;

procedure LockSendPacketQueue; cdecl;
begin
 EnterCriticalSection(EAPOLQueueLock);
//  EnterCriticalSection(L2SendQueueLock);
end;

procedure UnlockSendPacketQueue; cdecl;
begin
//  LeaveCriticalSection(L2SendQueueLock);
 LeaveCriticalSection(EAPOLQueueLock);
end;


function UltiboSetKey(const ifname : PChar; priv : pointer; alg : byte;
		      const addr : PByte; key_idx : integer; set_tx : integer;
		      const seq : PByte; seq_len : integer;
		      const key : PByte; key_len : integer) : integer; cdecl;
var
  KeyP : PWPAKey;
  pairwise : boolean;
begin
  Result := 0;

  {check which key is being set as this is our clue how far through the process we are}
  {key_idx=0 is first, so when we reach 1 we've got the other key from message 3}
  if (key_idx = 1) then
  begin
    {this is the second key, so we're nearing the end of the EAPOL negotiation}
    PCYW43455Network(WIFI^.NetworkP)^.EAPOLCompleted := true;
  end;

  {probably a request to clear the key - ignore for now.}
  if (addr = nil) then
    exit;

  {we don't support TKIP; only CCMP.}
  if (alg <> WPA_ALG_CCMP) then
    exit;

  {take a copy of the parameters and store in a record for later.}
  KeyP := @(PCYW43455Network(WIFI^.NetworkP)^.WPAKeys[key_idx]);

  fillchar(KeyP^, sizeof(TWPAKey), 0);

  pairwise := (set_tx = 1);
  if (pairwise) then
    KeyP^.groupkeyid := 0
  else
    KeyP^.groupkeyid := key_idx;


  KeyP^.keylen := key_len;
  move(Key^, KeyP^.Key[0], key_len);

  if (alg = WPA_ALG_TKIP) then
    KeyP^.algo := 2 {TKIP -- not formally supported by this device driver.}
  else
    KeyP^.algo := 4; {AES_CCM}

  if (pairwise) then
    KeyP^.flags := 0
  else
    KeyP^.flags := 2;	{primary key}

  {this copy of a C implementation does work but I don't like it, as the qword
   cast goes beyond the length of the data. Change for something better.}
  KeyP^.ivinitialised := 0;
  KeyP^.ivhigh := pqword(seq)^ >> 16;
  KeyP^.ivlow := pqword(seq)^ and $ffff;
  if (pairwise) then
    move(addr^, KeyP^.ethaddr[0], 6);
end;


procedure SendSupplicantL2Packet(PacketBufferP : PByte; Len : longword); cdecl;
var
  NetworkEntryP : PNetworkEntry;
  TransmitBufferP : Pointer;
begin
  if MutexLock(WIFI^.NetworkP^.Lock) = ERROR_SUCCESS then
  begin

    {get transmit buffer.}
    try
      NetworkEntryP:=BufferGet(WIFI^.NetworkP^.TransmitQueue.Buffer);

      if (NetworkEntryP <> nil) then
      begin
        if (NetworkEntryP^.Buffer <> nil) then
        begin
          {get packet list pointer}
          TransmitBufferP := NetworkEntryP^.Buffer;

          {increment packet count - should almost always be zero at this point?}
          NetworkEntryP^.Count:=1;

          {setup packet entry}
          NetworkEntryP^.Packets[NetworkEntryP^.Count-1].Buffer:=TransmitBufferP;
          NetworkEntryP^.Packets[NetworkEntryP^.Count-1].Data:=Pointer(TransmitBufferP) + NetworkEntryP^.Offset;
          NetworkEntryP^.Packets[NetworkEntryP^.Count-1].Length:=Len;

          {need to byte swap the protocol id apparently?}
          pether_header(PacketBufferP)^.ethertype := WordSwap(pether_header(PacketBufferP)^.ethertype);

          {not sure if this is right either!}
          {move data into packet}
          move(PacketBufferP^, NetworkEntryP^.Packets[NetworkEntryP^.Count-1].Data^, len);

          {populate entries for transmission???}
          WIFI^.NetworkP^.TransmitQueue.Entries[(WIFI^.NetworkP^.TransmitQueue.Start
                                + WIFI^.NetworkP^.TransmitQueue.Count)
                                mod  PCYW43455Network(WIFI^.NetworkP)^.TransmitEntryCount]
                                  := NetworkEntryP;
          {Update Count}
          Inc(WIFI^.NetworkP^.TransmitQueue.Count);
          {Signal Packet Received}
          SemaphoreSignal(WIFI^.NetworkP^.TransmitQueue.Wait);
        end;
      end;

    except
      on e : exception do
        WIFILogInfo(nil, 'Exception in sendsupplicantl2packet : ' + e.message + ' at address ' + inttohex(longword(exceptaddr), 8));
    end;

    MutexUnlock(WIFI^.NetworkP^.Lock);

  end
  else
    WIFILogError(nil, 'Failed to get network lock!!');
end;

procedure DoNetworkNotify(data : pointer);
var
  KeyP : PWPAKey;
begin
  {set both keys here before bringing up the network.}
  WIFILogInfo(nil, 'Setting key 0');

  KeyP := @(PCYW43455Network(WIFI^.NetworkP)^.WPAKeys[0]);
  if (WirelessSetVar(WIFI, 'wsec_key', PByte(KeyP), sizeof(TWPAKey), 'ultibosetkey') <> ERROR_SUCCESS) then
    WIFILogError(nil, 'Failed to update wsec_key in UltiboSetKey')
  else
    WIFILogInfo(nil, 'Successfully set the wsec_key var');

  WIFILogInfo(nil, 'Setting key 1');

  KeyP := @(PCYW43455Network(WIFI^.NetworkP)^.WPAKeys[1]);
  if (WirelessSetVar(WIFI, 'wsec_key', PByte(KeyP), sizeof(TWPAKey), 'ultibosetkey') <> ERROR_SUCCESS) then
    WIFILogError(nil, 'Failed to update wsec_key in UltiboSetKey')
  else
    WIFILogInfo(nil, 'Successfully set the wsec_key var');


  {Set Status to Up}
  WIFI^.NetworkP^.NetworkStatus := NETWORK_STATUS_UP;

  {Notify the Status}
  NotifierNotify(@WIFI^.NetworkP^.Device, DEVICE_NOTIFICATION_UP);

  {signal network joined}
  SemaphoreSignal(PCYW43455Network(WIFI^.NetworkP)^.NetworkUpSignal);

  WIFILogInfo(nil, 'The WIFI network status is up');
end;

procedure UltiboEAPOLComplete; cdecl;
begin
  {don't really need this function anymore - leave stub for now but expect to remove it.}
  WIFILogInfo(nil, 'EAPOL auth is complete.');
end;

function UltiboSetCountry(priv : pointer; ccode : PChar) : integer; cdecl; public name 'UltiboSetCountry';
begin
  Result := 0;
  PCYW43455Network(WIFI^.NetworkP)^.CountryCode[0] := ccode^;
  PCYW43455Network(WIFI^.NetworkP)^.CountryCode[1] := (ccode+1)^;
end;

constructor TWPASupplicantThread.create;
begin
  {the thread is created suspended. We'll let it run when the application asks for a connection.}
  inherited Create(True);
end;

procedure TWPASupplicantThread.Execute;
var
 retcode : integer;
 EAPOLRequestItemP : PWIFIRequestItem;
begin
  ThreadSetName(ThreadGetCurrent, 'WPA Supplicant Thread');
  WIFILogInfo(nil, 'WPA Supplicant Thread started execution');

  {set up the mac address pointer so the supplicant can see it}
  SetUltiboMacAddress(@macaddress[0]);

  {add callback request for EAPOL messages. Dunno if this will work.}
  {haven't actually seen any yet.}
  EAPOLRequestItemP := WIFIWorkerThread.AddRequest(0, [WLC_E_EAPOL_MSG], @EAPOLCallback, nil);

  {call main - this will read the configuration and start the event loop off.}
  {Each time this loop executes, it repeats because the connection was lost. We are asking
   the supplicant to reset itself each time in order to trigger a reconnection. That may not
   be very efficient but so far I have not found a function I can call to
   kick it into reconnecting.}

  try
    while (not Terminated) do
    begin
      retcode := wpa_supplicant_main('wpa_supplicant.conf');

      WIFILogInfo(nil, 'The supplicant main loop has terminated - return code ' + inttostr(retcode));
    end;

    WIFIWorkerThread.DoneWithRequest(EAPOLRequestItemP);

    WIFILogInfo(nil, 'The supplicant thread has terminated - return code ' + inttostr(retcode));
  except
    on e : exception do
      WIFILogError(nil, 'Exception generated in TWPASupplicantThread.Execute ' + e.Message + ' ' + inttohex(longword(exceptaddr), 8));
  end;

end;
{$endif}

initialization
  if WIFI_AUTO_INIT then
    WIFIInit;

end.
