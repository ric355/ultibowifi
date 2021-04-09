unit wifidevice;

{$mode objfpc}{$H+}


interface

uses
  mmc,
  Classes, SysUtils, Devices,
  GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,DMA,gpio;

const
  FIRMWARE_CHUNK_SIZE = 2048;
  FIRWMARE_OPTIONS_COUNT = 7;
  FIRMWARE_FILENAME_ROOT = 'c:\firmware\';

  {SDIO Bus Speeds (Hz)}
  SDIO_BUS_SPEED_DEFAULT   = 0;
  SDIO_BUS_SPEED_HS26      = 26000000;
  SDIO_BUS_SPEED_HS52      = 52000000;
  SDIO_BUS_SPEED_DDR       = 52000000;
  SDIO_BUS_SPEED_HS          = 50000000;
  SDIO_BUS_SPEED_HS200     = 200000000;

  WLAN_ON_PIN = GPIO_PIN_41;
  SD_32KHZ_PIN = GPIO_PIN_43;

  {MMC logging}
  SDHCI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SDHCI debugging messages}
  SDHCI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SDHCI informational messages, such as a device being attached or detached}
  SDHCI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {SDHCI warning messages}
  SDHCI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SDHCI error messages}
  SDHCI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SDHCI messages}

  {SDIO Device States}
  SDIO_STATE_EJECTED  = 0;
  SDIO_STATE_INSERTED = 1;

  SDIO_STATE_MAX      = 1;

  {Maximum block count for SDIO}
  SDIO_MAX_BLOCK_COUNT = 65535;

  {SDIO Modes} //To Do //These are really the capabilities flags for SDIO/SDHCI.Capabilities/PresetCapabilities  SDIO_CAP_
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

  {SDHCI Quirks/Bugs}
  {From: U-Boot sdhci.h}
  (*SDHCI_QUIRK_32BIT_DMA_ADDR	          = (1 shl 0); {See: SDHCI_QUIRK_32BIT_DMA_ADDR below}
  SDHCI_QUIRK_REG32_RW		          = (1 shl 1);
  SDHCI_QUIRK_BROKEN_R1B		          = (1 shl 2);
  SDHCI_QUIRK_NO_HISPD_BIT	          = (1 shl 3); {See: SDHCI_QUIRK_NO_HISPD_BIT below}
  SDHCI_QUIRK_BROKEN_VOLTAGE	          = (1 shl 4); {Use  SDHCI_QUIRK_MISSING_CAPS instead}
  SDHCI_QUIRK_NO_CD		              = (1 shl 5); {See: SDHCI_QUIRK_BROKEN_CARD_DETECTION below}
  SDHCI_QUIRK_WAIT_SEND_CMD	          = (1 shl 6);
  SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER  = (1 shl 7); {See: SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER below}
  SDHCI_QUIRK_USE_WIDE8		          = (1 shl 8);
  SDHCI_QUIRK_MISSING_CAPS             = (1 shl 9); {See: SDHCI_QUIRK_MISSING_CAPS below}*)

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
  BAK_SDIO_PULL_UP_REG = $1000f;


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
function WIFIDeviceCoreScan(WIFI : PWIFIDevice) : longint;
procedure WIFIDeviceRamScan(WIFI : PWIFIDevice);
function WIFIDeviceDownloadFirmware(WIFI : PWIFIDevice) : Longword;

procedure sbreset(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);
procedure sbdisable(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);



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

  dodumpregisters : boolean = false;

  chipid : word;
  chipidrev : word;
  firmwarefilename : string;
  configfilename : string;
  regufilename : string;

  firmware : array[1..FIRWMARE_OPTIONS_COUNT] of TFirmwareEntry =
    (
    	( chipid : $4330; chipidrev : 3; firmwarefilename: 'fw_bcm40183b1.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename : ''),
    	( chipid : $4330; chipidrev : 4; firmwarefilename: 'fw_bcm40183b2.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename : ''),
    	( chipid : 43362; chipidrev : 0; firmwarefilename: 'fw_bcm40181a0.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename : ''),
    	( chipid : 43362; chipidrev : 1; firmwarefilename: 'fw_bcm40181a2.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename : ''),
    	( chipid : 43430; chipidrev : 1; firmwarefilename: 'brcmfmac43430-sdio.bin'; configfilename: 'brcmfmac43430-sdio.txt'; regufilename : ''),
    	( chipid : $4345; chipidrev : 6; firmwarefilename: 'brcmfmac43455-sdio.bin'; configfilename: 'brcmfmac43455-sdio.txt'; regufilename : 'brcmfmac43455-sdio.clm_blob'),
    	( chipid : $4345; chipidrev : 9; firmwarefilename: 'brcmfmac43456-sdio.bin'; configfilename: 'brcmfmac43456-sdio.txt'; regufilename : 'brcmfmac43456-sdio.clm_blob')
    );


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


    wifiloginfo(nil, '32 bit block count 0x'+ inttohex(r^[0], 8));
    wifiloginfo(nil, 'blocksize and count 0x'+ inttohex(r^[1], 8));
    wifiloginfo(nil, 'arg 0x%x'+ inttohex(r^[2], 8));
    wifiloginfo(nil, 'transfermode and command 0x'+ inttohex(r^[3], 8));
    wifiloginfo(nil, 'response0 0x'+ inttohex(r^[4], 8));
    wifiloginfo(nil, 'response1 0x'+ inttohex(r^[5], 8));
    wifiloginfo(nil, 'response2 0x'+ inttohex(r^[6], 8));
    wifiloginfo(nil, 'response3 0x'+ inttohex(r^[7], 8));
    //wifiloginfo(nil, 'buffer data port 0x%x'+ inttohex(r^[8], 8));
    wifiloginfo(nil, 'present state 0x'+ inttohex(r^[9], 8));
    wifiloginfo(nil, 'host ctrl1, pwr ctrl, block gap ctrl, wakeup ctrl0x'+ inttohex(r^[10], 8));


    wifiloginfo(nil, 'host ctrl1 0x' +inttohex(rb^[SDHCI_HOST_CONTROL], 2));
    wifiloginfo(nil, 'pwr ctrl 0x' +inttohex(rb^[SDHCI_POWER_CONTROL], 2));
    wifiloginfo(nil, 'block gap ctrl 0x' +inttohex(rb^[SDHCI_BLOCK_GAP_CONTROL], 2));
    wifiloginfo(nil, 'wakeup ctrl 0x' +inttohex(rb^[SDHCI_WAKE_UP_CONTROL], 2));

    wifiloginfo(nil, 'clock ctrl, timeout ctrl, sw reset 0x'+ inttohex(r^[11], 8));

    wifiloginfo(nil, 'clock ctrl byte 1 0x' +inttohex(rb^[SDHCI_CLOCK_CONTROL], 2));
    wifiloginfo(nil, 'clock ctrl byte 2 0x' +inttohex(rb^[SDHCI_CLOCK_CONTROL+1], 2));
    wifiloginfo(nil, 'timeout ctrl 0x' +inttohex(rb^[SDHCI_TIMEOUT_CONTROL], 2));
    wifiloginfo(nil, 'sw reset 0x' +inttohex(rb^[SDHCI_SOFTWARE_RESET], 2));


    wifiloginfo(nil, 'normal interrupt status, error interrupt status 0x'+ inttohex(r^[12], 8));
    wifiloginfo(nil, 'normal interr enable, error interr enable 0x'+ inttohex(r^[13], 8));
    wifiloginfo(nil, 'auto cmd status, host ctrl 2 0x'+ inttohex(r^[14], 8));
    wifiloginfo(nil, 'capabilities part 1 0x'+ inttohex(r^[15], 8));
    wifiloginfo(nil, 'capabilities part 2 0x'+ inttohex(r^[16], 8));
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

 wifiloginfo(nil, 'clock='+inttostr(clock) + ' max freq='+inttostr(sdhci^.MaximumFrequency));
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
 WIFI_DEFAULT_LOG_LEVEL:=WIFI_LOG_LEVEL_INFO;
 MMC_DEFAULT_LOG_LEVEL:=MMC_LOG_LEVEL_NONE;

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
   wifiloginfo(nil, 'sdhci^.voltages is ' + inttostr(sdhci^.voltages) + 'firstbitsetof()='+inttostr(firstbitset(sdhci^.voltages)));
   Result:=SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI^.Voltages) - 1);
   if Result <> WIFI_STATUS_SUCCESS then
    begin
     WIFILogError(nil,'failed to Set initial power');

     Exit;
    end;

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
     wifilogerror(nil, 'go idle returned fail but lets plough on anyway...');
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

   {Set Clock to high speed}
   WIFILogInfo(nil,'Set device clock');
   Result:=WIFIDeviceSetClock(WIFI,SDIO_BUS_SPEED_HS);
   if Result <> WIFI_STATUS_SUCCESS then
     wifilogError(nil, 'failed to set the clock speed to default')
   else
     wifiloginfo(nil, 'Set device clock succeeded');

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

   wifiloginfo(nil,'Waiting until the backplane is ready');
   blocksize := 0;
   retries := 0;
   repeat
     // attempt to set and read back the fn0 block size.
     result1 := SDIOWIFIDeviceReadWriteDirect(WIFI, true, 0, SDIO_CCCR_BLKSIZE, WIFI_BAK_BLK_BYTES, nil);
     result2 := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 0, SDIO_CCCR_BLKSIZE, 1, @blocksize);
     retries += 1;
     sleep(1);
   until ((result1 = WIFI_STATUS_SUCCESS) and (result2 = WIFI_STATUS_SUCCESS) and (blocksize = WIFI_BAK_BLK_BYTES)) or (retries > 500);

   if (retries > 500) then
     wifilogerror(nil, 'the backplane was not ready');

   // if we get here we have successfully set the fn0 block size in CCCR and therefore the backplane is up.

   wifiloginfo(nil, 'setting backplane block size');

   // set block sizes for fn1 and fn2 in their respective function registers.
   // note these are still writes to the common IO area (function 0).
   updatevalue := WIFI_BAK_BLK_BYTES;
   wifiloginfo(nil, 'setting backplane fn1 block size to 64');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, BUS_BAK_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, BUS_BAK_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   wifiloginfo(nil, 'setting backplane fn2 (radio) block size to 64 - might change to 512 later?');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, BUS_RAD_BLKSIZE_REG, updatevalue and $ff,nil);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, BUS_RAD_BLKSIZE_REG+1,(updatevalue shr 8) and $ff,nil);

   // we only check the last result here. Needs changing really.
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated backplane block sizes')
   else
     wifilogerror(nil, 'Failed to update backplane block sizes');

   wifiloginfo(nil, 'io enable');
   ioreadyvalue := 0;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,0,SDIO_CCCR_IOEx,  1 shl 1,nil);   // yuck. 1 shl fd_func_bak (not defined yet).
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'io enable successfully set')
   else
     wifilogerror(nil, 'io enable could not be set');

   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,0, SDIO_CCCR_IORx,  0, @ioreadyvalue);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'ioready value read and is ' + inttohex(ioreadyvalue, 4))
   else
     wifilogerror(nil, 'ioready value could not be read');

   wifiloginfo(nil, 'Enabling interrupts for all functions');
   Result := SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, SDIO_CCCR_IENx, (INTR_CTL_MASTER_EN or INTR_CTL_FUNC1_EN or INTR_CTL_FUNC2_EN), nil );
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully enabled interrupts')
   else
     wifilogerror(nil, 'Failed  to enable interrupts');

   // Enable F1 and F2 */
   wifiloginfo(nil, 'Enabling F1 and F2');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 0, SDIO_CCCR_IOEx, SDIO_FUNC_ENABLE_1 or SDIO_FUNC_ENABLE_2, nil);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully enabled f1 and f2')
   else
     wifilogerror(nil, 'Failed to enable f1 and f2');

   // Setup host-wake signals
   wifiloginfo(nil, 'not setting up host wake signals');

   //Enable F2 interrupt only
   wifiloginfo(nil, 'enable f2 interrupt only - why is this?. What is function 2 anyway?');
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI, true, 0, SDIO_CCCR_IENx, INTR_CTL_MASTER_EN or INTR_CTL_FUNC2_EN, nil);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully enabled f2 interrupt only')
   else
     wifilogerror(nil, 'Failed to enable f2 interrupt only');

   wifiloginfo(nil, 'not checking for ioready here either');


   wifiloginfo(nil, 'Reading the Chip ID');
   chipid := 0;
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,0,  0, @chipid);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,1,  0, pbyte(@chipid)+1);
   Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,2,  0, @chipidrev);
   chipidrev := chipidrev and $f;
   if (Result = WIFI_STATUS_SUCCESS) then
   begin
     wifiloginfo(nil, 'WIFI Chip ID is 0x'+inttohex(chipid, 4) + ' rev ' + inttostr(chipidrev));
     WIFI^.chipid := chipid;
     WIFI^.chipidrev := chipidrev;
   end;


   // set backplane window
  wifiloginfo(nil, 'set backplane window ' + inttohex(bak_base_addr, 8));
  Result := WIFIDeviceSetBackplaneWindow(WIFI, BAK_BASE_ADDR);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully updated backplane window')
   else
     wifilogerror(nil, 'Failed to update backplane window');


  MMC_DEFAULT_LOG_LEVEL:=MMC_LOG_LEVEL_NONE;
  WIFI_LOG_ENABLED:=true;


  MMC_LOG_ENABLED := true;


  // scan the cores to establish various key addresses
//  MMC_DEFAULT_LOG_LEVEL:=MMC_LOG_LEVEL_INFO;
  WIFIDeviceCoreScan(WIFI);

  if (WIFI^.armctl = 0) or (WIFI^.dllctl = 0) or
    ((WIFI^.armcore = ARMcm3) and ((WIFI^.socramctl = 0) or (WIFI^.socramregs = 0))) then
  begin
    WIFILogError(nil, 'Corescan did not find essential cores!');
    exit;
  end;


  WIFI_DEFAULT_LOG_LEVEL:=WIFI_LOG_LEVEL_INFO;
  WIFILogInfo(nil, 'Disable core');
  if (WIFI^.armcore = ARMcr4) then
  begin
    wifiloginfo(nil, 'sbreset armcr4 core');
    sbreset(WIFI, WIFI^.armctl, Cr4Cpuhalt, Cr4CpuHalt)    // seems weird but that's what ether4330.c does.
  end
  else
  begin
    wifiloginfo(nil, 'sbdisable armctl core');
    sbdisable(WIFI, WIFI^.armctl, 0, 0);
  end;

  WIFILogInfo(nil, 'sbreset dllctl');

  sbreset(WIFI, WIFI^.dllctl, 8 or 4, 4);

  WIFILogInfo(nil, 'Device RAM scan');

  WIFIDeviceRamScan(WIFI);

  Result := SDIOWIFIDeviceReadWriteDirect(WIFI, true, 1, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
  if (Result <> WIFI_STATUS_SUCCESS) then
    WIFILogError(nil, 'Unable to update config at chip clock csr register');
  MicrosecondDelay(10);

  wifiloginfo(nil, 'requesting active low power clock');

  // I think this is supposed to be a check to see if ALP clock is available and error if not.
  // according to the other init function in the cypress driver.
  // first we zero the clock settings and request what we want.
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,1, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
  sleep(1);
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,1, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ReqALP, nil);

  // now we keep reading them until we have some availability
  bytevalue := 0;
  while (bytevalue and (HTavail or ALPavail) = 0) do
  begin
    Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,False,1, BAK_CHIP_CLOCK_CSR_REG, 0, @bytevalue);
    if (Result <> WIFI_STATUS_SUCCESS) then
      wifilogerror(nil, 'failed to read clock settings');
    sleep(1);
  end;

  // finally we can clear active low power request. Not sure if any of this is needed to be honest.
  wifiloginfo(nil, 'clearing active low power clock request');
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True,1, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ForceALP, nil);

  // Disable the extra SDIO pull-ups
  wifiloginfo(nil, 'Disabling backplane SDIO extra pullups????');
  Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 1, BAK_SDIO_PULL_UP_REG, 0, nil);
   if (Result = WIFI_STATUS_SUCCESS) then
     wifiloginfo(nil, 'Successfully disabled SDIO extra pullups???')
   else
     wifilogerror(nil, 'Failed to disable SDIO extra pullups???');

   if (WIFI^.chipid = $4330) or (WIFI^.chipid = 43362) then
   begin
    // there is other stuff from sbinit() to do here but we'll leave it for now as it is not for the pi3 or zero.
    // at least I think so.
   end;


   WIFILogInfo(nil, 'Download WIFI firmware');
   Result := WIFIDeviceDownloadFirmware(WIFI);





   Result:=WIFI_STATUS_SUCCESS;
  end;

 except
   on e : exception do
   wifilogerror(nil, 'Exception ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8) + ' during wifiinitialize');
 end;
end;

function WIFIDeviceSetBackplaneWindow(WIFI : PWIFIDevice; addr : longword) : longword;
begin
 addr := addr and (not $7fff);

 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 1, BAK_WIN_ADDR_REG, (addr shr 8) and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 1, BAK_WIN_ADDR_REG+1,(addr shr 16) and $ff,nil);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,True, 1, BAK_WIN_ADDR_REG+2,(addr shr 24) and $ff,nil);

 wifilogdebug(nil, 'setting backplane address to ' + inttohex((addr shr 8) and $ff, 8) + ' '
                  + inttohex((addr shr 16) and $ff, 8) + ' '
                  + inttohex((addr shr 24) and $ff, 8));

 if (Result = WIFI_STATUS_SUCCESS) then
   wifilogdebug(nil, 'function ' + inttostr(1) + ' backplanewindow updated to ' + inttohex(addr, 8))
 else
   wifilogerror(nil, 'something went wrong in setbackplanewindow');
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
       wifiloginfo(nil, 'turn off 8 bit bus');
       Value:=Value and not(SDHCI_CTRL_8BITBUS);
      end;

     if WIFI^.BusWidth = WIFI_BUS_WIDTH_4 then
      begin
       wifiloginfo(nil, 'set 4 bit bus');
       Value:=Value or SDHCI_CTRL_4BITBUS;
      end
     else
      begin
       wifiloginfo(nil, 'turn off 4 bit bus');
       Value:=Value and not(SDHCI_CTRL_4BITBUS);
      end;
    (*end;*)

   // block gap control
   SDHCIHostWriteByte(SDHCI, SDHCI_BLOCK_GAP_CONTROL, 0);
   SDHCIHostWriteByte(SDHCI, SDHCI_POWER_CONTROL, 0);

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

 WIFILogDebug(nil,'SDIOReadWriteExtended ' + booltostr(write, true) + ' ' + inttostr(operation)
   + ' address=0x' + inttohex(address, 8)
   + ' buf=0x'+inttohex(longword(buffer), 8)
   + ' blockcount='+inttostr(blockcount)
   + ' blocksize='+inttostr(blocksize));

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
 Command.ResponseType:=SDIO_RSP_R1;
 Command.Data:=nil;

 Command.Data := @SDIOData;
 SDIOData.Data := Buffer;

 SDIOData.Blocksize := BlockSize;
 SDIOData.BlockCount := blockcount;

 if (write) then
   SDIOData.Flags := WIFI_DATA_WRITE
 else
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
 TransferMode:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Timeout:LongWord;
 SDHCI:PSDHCIHost;
 blksizecnt : longword;
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
         if WIFI_LOG_ENABLED then WifiLogError(nil,'MMC Send Command Invalid Response Type');
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
         wifilogdebug(nil, 'adding sdhci_cmd_data flag to the flags for this command');
         Flags:=Flags or SDHCI_CMD_DATA;
        end;

       {Write Timeout Control}
       if (Command^.Data <> nil) or ((Command^.ResponseType and SDIO_RSP_BUSY) <> 0) then
        begin
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_TIMEOUT_CONTROL (Value=' + IntToHex(SDHCI_TIMEOUT_VALUE,8) + ')');
         {$ENDIF}
         SDHCIHostWriteByte(SDHCI,SDHCI_TIMEOUT_CONTROL,SDHCI_TIMEOUT_VALUE);
        end;

       {Check Data}
       if (command^.data = nil) then
        begin
         wifilogdebug(nil, 'writing a standard command; status='+inttostr(command^.status));

         {Setup Transfer Mode}
         TransferMode:=SDHCIHostReadWord(SDHCI,SDHCI_TRANSFER_MODE);

         {Clear Auto CMD settings for non data CMDs}
         TransferMode:=TransferMode and not(SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23);

         {Clear Block Count, Multi, Read and DMA for non data CMDs}
         TransferMode:=TransferMode and not(SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI or SDHCI_TRNS_READ or SDHCI_TRNS_DMA);

         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         {$ENDIF}

         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer TransferMode}
         {$IFDEF MMC_DEBUG}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(TransferMode,8) + ')');
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

         wifilogdebug(nil, 'blockcount='+inttostr(command^.data^.blockcount) + ' blocksize='+inttostr(command^.data^.blocksize));

         {Setup Transfer TransferMode}
         TransferMode := 0;
         if (Command^.Data^.BlockCount > 0) then
          begin
           wifilogdebug(nil, 'enabling block transfer TransferMode');
           TransferMode:=SDHCI_TRNS_BLK_CNT_EN;
           TransferMode:=TransferMode or SDHCI_TRNS_MULTI;

          // TransferMode:=TransferMode or SDHCI_TRNS_AUTO_CMD12; //To Do //Testing (This works, need to sort out properly where it fits, plus SDHCI_TRNS_AUTO_CMD23)

           //To Do //SDHCI_TRNS_AUTO_CMD12 //SDHCI_TRNS_AUTO_CMD23 //SDHCI_ARGUMENT2 //See: sdhci_set_transfer_mode
                   //See 1.15 Block Count in the SD Host Controller Simplified Specifications
          end;
         if (Command^.Data^.Flags and WIFI_DATA_READ) <> 0 then
          begin
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
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_SIZE (Value=' + IntToStr(Command^.Data^.BlockSize) + ') makeblocksize='+inttohex(SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize), 8));

//         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize));
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command^.Data^.BlockSize)); //Command^.Data^.BlockSize);
//         wifilogdebug(nil, 'actually wrote 0x' + inttohex(command^.data^.blocksize, 8) + ' to block size register');

         {Write Block Count}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_BLOCK_COUNT (Value=' + IntToStr(Command^.Data^.BlockCount) + ')');
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,Command^.Data^.BlockCount);

         blksizecnt := SDHCIHostReadLong(SDHCI, SDHCI_BLOCK_SIZE);
         wifilogdebug(nil, 'reading back blksizecnt to match circle=0x'+inttohex(blksizecnt, 8));

         {Write Argument}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command^.Argument,8) + ')');
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command^.Argument);

         {Write Transfer TransferMode}
         if WIFI_LOG_ENABLED then WIFILogDebug(nil,'WIFI Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(TransferMode,8) + ')');
         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,TransferMode);
        end;


       {Setup Command}
       SDHCI^.Command:=PMMCCommand(Command);
       try
        {Write Command}

        WIFILogDebug(nil,'WIFI Send Command SDHCI_COMMAND cmd=' + inttostr(command^.command) + '  value written to cmd register=' + IntToHex(SDHCIMakeCommand(Command^.Command,Flags),8) + ') status='+inttostr(command^.status));

        if (dodumpregisters) and (command^.Command = SDIO_CMD_RW_EXTENDED) then
        begin
          dumpregisters(WIFI);
          dodumpregisters := false;
        end;

        SDHCIHostWriteWord(SDHCI,SDHCI_COMMAND,SDHCIMakeCommand(Command^.Command,Flags));

        {Wait for Completion}   // short timeout for a read command.
        if SDHCI^.Command^.Data = nil then // need to go back to test for data=nil
         begin
          {Wait for Signal with Timeout (100ms)}
          Status:=SemaphoreWaitEx(SDHCI^.Wait,500);  // increased during debug
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
           wifilogdebug(nil, 'semaphore wait succeeded command=' + inttostr(command^.Command) + ' status=' + inttostr(command^.Status));
          end;
         end
        else
         begin
          {Wait for Signal with Timeout (5000ms)}
          wifilogdebug(nil, 'wait for semaphore 5000');
          Status:=SemaphoreWaitEx(SDHCI^.Wait,5000);
          wifilogdebug(nil, 'semaphore returned');
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
       if Command^.Status <> WIFI_STATUS_SUCCESS then //To Do //More see: sdhci_tasklet_finish //SDHCI_QUIRK_RESET_AFTER_REQUEST and SDHCI_QUIRK_CLOCK_BEFORE_RESET
        begin
         SDHCIHostReset(SDHCI,SDHCI_RESET_CMD);
         SDHCIHostReset(SDHCI,SDHCI_RESET_DATA);
        end;
      end;

     finally
      {Release the Lock}
      MutexUnlock(WIFI^.Lock);
     end;
    end;

   WIFILogDebug(nil,'WIFI Send Command completed: ' + MMCStatusToString(Command^.Status));
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
 str : string;
 chipidbuf : longword;
 chipid : word;
 chiprev : word;
 socitype : word;
begin
 wifiloginfo(nil, 'starting core scan');

 WIFI_DEFAULT_LOG_LEVEL:=WIFI_LOG_LEVEL_INFO;
 Result := WIFI_STATUS_INVALID_PARAMETER;

 // set backplane window
 fillchar(buf, sizeof(buf), 0);

 Result := WIFIDeviceSetBackplaneWindow(WIFI, BAK_BASE_ADDR);

 // read 32 bits containing chip id and other info
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,0,  0, pbyte(@chipidbuf));
 if (Result <> WIFI_STATUS_SUCCESS) then
    wifilogerror(nil, 'failed to read the first byte of the chip id');

 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,1,  0, pbyte(@chipidbuf)+1);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,2,  0, pbyte(@chipidbuf)+2);
 Result:=SDIOWIFIDeviceReadWriteDirect(WIFI,false,1,2,  0, pbyte(@chipidbuf)+3);

 wifiloginfo(nil, 'chipid 32 bit value = ' + inttohex(chipidbuf, 8));
 chipid := chipidbuf  and CID_ID_MASK;
 chiprev := (chipidbuf and CID_REV_MASK) shr CID_REV_SHIFT;
 socitype := (chipidbuf and CID_TYPE_MASK) shr CID_TYPE_SHIFT;
 wifiloginfo(nil, 'chipid ' + inttohex(chipid,4) + ' chiprev ' + inttohex(chiprev, 4) + ' socitype ' + inttohex(socitype,4));



 Result := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 1, 63*4, 0, @addressbytes[1]);
 if (Result <> WIFI_STATUS_SUCCESS) then
 begin
  wifilogerror(nil, 'Failed to read from card memory - exit');
  exit;
 end;

 Result := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 1, 63*4 + 1, 0, @addressbytes[2]);
 Result := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 1, 63*4 + 2, 0, @addressbytes[3]);
 Result := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 1, 63*4 + 3, 0, @addressbytes[4]);

 address := plongint(@addressbytes[1])^;

 wifiloginfo(nil, 'address is read as ' + inttohex(address, 8));

// Result := SDIOWIFIDeviceReadWriteExtended(WIFI, false, 1, 63*4, true, @addressbytes[1], 0, 4);

// address := plongint(@addressbytes[1])^;

 wifiloginfo(nil, 'using extended read, address is read as ' + inttohex(address, 8));

 // we must get the top 15 bits from the address and set the bakplane window to it
 wifiloginfo(nil, 'setting backplane window for ' + inttohex(address, 8));
 WIFIDeviceSetBackplaneWindow(WIFI, address);
 // is this right? do we split off the upper bits to give us a set of bits that go with the offset?

 address := (address and $7fff) or $8000;

 wifiloginfo(nil, 'address is now ' + inttohex(address, 8));

 try
 // read the core info from the device

//for i := 1 to 100 do
//begin
//repeat
  Result := SDIOWIFIDeviceReadWriteExtended(WIFI, false, 1, address, true, @buf[0], 8, 64);
 if (Result <> WIFI_STATUS_SUCCESS) then
   wifilogerror(nil, 'Failed to read using extended call')
 else
   wifiloginfo(nil, 'read block success ' + inttostr(i));

//until false;
//end;

 str := '';
 for i := 1 to 512 do
 begin
   str := str + ' ' + inttohex(buf[i-1], 2);
   if i mod 20 = 0 then
   begin
     wifiloginfo(nil, str);
     str := '';
   end;
 end;
 wifiloginfo(nil, str);


 coreid := 0;
 corerev := 0;

 i := 0;

  while i < Corescansz do
  begin
     case buf[i] and $0f of
 	  $0F: begin
                 wifiloginfo(nil, 'Reached end of descriptor');
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

  wifiloginfo(nil, 'Corescan completed.');
  wifiloginfo(nil,'chipcommon=0x' + inttohex(WIFI^.chipcommon,8));
  wifiloginfo(nil,'armcore=0x' + inttohex(WIFI^.armcore,8));
  wifiloginfo(nil,'armctl=0x' + inttohex(WIFI^.armctl,8));
  wifiloginfo(nil,'armregs=0x' + inttohex(WIFI^.armregs,8));
  wifiloginfo(nil,'socramctl=0x' + inttohex(WIFI^.socramctl,8));
  wifiloginfo(nil,'socramregs=0x' + inttohex(WIFI^.socramregs,8));
  wifiloginfo(nil,'socramrev=0x' + inttohex(WIFI^.socramrev,8));
  wifiloginfo(nil,'sdregs=0x' + inttohex(WIFI^.sdregs,8));
  wifiloginfo(nil,'sdiorev=0x' + inttohex(WIFI^.sdiorev,8));
  wifiloginfo(nil,'dllctl=0x' + inttohex(WIFI^.dllctl,8));

 except
   on e : exception do
     wifilogerror(nil, 'exception in corescan: ' + e.message);
 end;

end;

function cfgreadl(WIFI : PWIFIDevice; func : word; addr : longword) : longword;
var
  v : longword;
begin
  v := 0;
  Result := SDIOWIFIDeviceReadWriteExtended(WIFI, false, func, (addr and $7fff) or $8000, true, @v, 0, 4);
  if (Result <> WIFI_STATUS_SUCCESS) then
    wifilogerror(nil, 'Failed to read config item 0x'+inttohex(addr, 8));
  Result := v;
end;

procedure cfgwritel(WIFI : PWIFIDevice; func : word; addr : longword; v : longword);
var
  Result : longword;
begin
 Result := SDIOWIFIDeviceReadWriteExtended(WIFI, true, func, (addr and $7fff) or $8000, true, @v, 0, 4);
 if (Result <> WIFI_STATUS_SUCCESS) then
   wifilogerror(nil,'Failed to update config item 0x'+inttohex(addr, 8));
end;

procedure cfgw(WIFI : PWIFIDevice; offset : longword; value : byte);
var
  Result : longword;
begin
  Result := SDIOWIFIDeviceReadWriteDirect(WIFI, true, 1, offset, value, nil);
  if (Result <> WIFI_STATUS_SUCCESS) then
    wifilogerror(nil, 'Failed to write config item 0x'+inttohex(offset, 8));
end;

function cfgr(WIFI : PWIFIDevice; offset : longword) : byte;
var
  Res : longword;
  value : byte;
begin
  Res := SDIOWIFIDeviceReadWriteDirect(WIFI, false, 1, offset, 0, @value);
  if (Res <> WIFI_STATUS_SUCCESS) then
    wifilogerror(nil, 'Failed to read config item 0x'+inttohex(offset, 8));

  Result := value;
end;

procedure sbdisable(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);
begin
 try
  WIFIDeviceSetBackplaneWindow(WIFI,  regs);

  if ((cfgreadl(WIFI, 1, regs + Resetctrl) and 1) <> 0) then
  begin
    cfgwritel(WIFI, 1, regs + Ioctrl, 3 or ioctl);
    cfgreadl(WIFI, 1, regs + Ioctrl);
    exit;
  end;

  cfgwritel(WIFI, 1, regs + Ioctrl, 3 or pre);
  cfgreadl(WIFI, 1, regs + Ioctrl);
  cfgwritel(WIFI, 1, regs + Resetctrl, 1);

  MicrosecondDelay(10);

  while((cfgreadl(WIFI, 1, regs + Resetctrl) and 1) = 0) do
    begin
      MicrosecondDelay(10);
    end;

  cfgwritel(WIFI, 1, regs + Ioctrl, 3 or ioctl);
  cfgreadl(WIFI, 1, regs + Ioctrl);
 except
   on e : exception do
     wifilogerror(nil, 'exception in sbdisable 0x' + inttohex(longword(exceptaddr), 8));
 end;
end;

procedure sbmem(WIFI : PWIFIDevice; write : boolean; buf : pointer; len : longword; off : longword);
var
  n : longword;
  addr : longword;
begin
  n := (((off)+(Sbwsize)-1) div (Sbwsize) * (Sbwsize)) - off;
  if (n = 0) then
    n := Sbwsize;

  wifilogdebug(nil, 'sbmem len='+inttostr(len) + ' n=' + inttostr(n) + ' offset=0x'+inttohex(off,8) + ' off&(sbwsize-1)=0x' + inttohex(off and (Sbwsize-1), 8));
  while (len > 0) do
  begin
    if (n > len) then
      n := len;
    WIFIDeviceSetBackplaneWindow(WIFI, off);
    addr := off and (sbwsize-1);
    SDIOWIFIDeviceReadWriteExtended(WIFI, write, 1, addr, true, buf, n div WIFI_BAK_BLK_BYTES, WIFI_BAK_BLK_BYTES);
    off += n;
    buf += n;
    len -= n;
    n := Sbwsize;
  end;
end;

procedure sbreset(WIFI : PWIFIDevice; regs : longword; pre : word; ioctl : word);

begin
 wifilogdebug(nil, 'sbreset entry 0x' + inttohex(regs, 8) + ' 0x'
                   + inttohex(cfgreadl(WIFI, 1, regs + IOCtrl), 8)
                   + ' 0x ' + inttohex(cfgreadl(WIFI, 1, regs + Resetctrl), 8));
  sbdisable(WIFI, regs, pre, ioctl);
  WIFIDeviceSetBackplaneWindow(WIFI, regs);
  while ((cfgreadl(WIFI, 1, regs + Resetctrl) and 1) <> 0) do
  begin
    cfgwritel(WIFI, 1, regs + Resetctrl, 0);
    MicrosecondDelay(40);
  end;

  cfgwritel(WIFI, 1, regs + Ioctrl, 1 or ioctl);
  cfgreadl(WIFI, 1, regs + Ioctrl);

  wifilogdebug(nil, 'sbreset exit 0x' + inttohex(cfgreadl(WIFI, 1, regs + IOCtrl), 8)
                    + ' 0x ' + inttohex(cfgreadl(WIFI, 1, regs + Resetctrl), 8));
end;

procedure WIFIDeviceRamScan(WIFI : PWIFIDevice);
var
 n, size : longword;
 r : longword;
 banks, i : longword;
begin
 WIFI_DEFAULT_LOG_LEVEL:=WIFI_LOG_LEVEL_INFO;
  if (WIFI^.armcore = ARMcr4) then
  begin
    r := WIFI^.armregs;
    wifiloginfo(nil, 'ramscan armcr4 0x' + inttohex(r, 8));
    WIFIDeviceSetBackplaneWindow(WIFI, r);
    r := (r and $7fff) or $8000;
    n := cfgreadl(WIFI, 1, r + Cr4Cap);

    wifiloginfo(nil, 'cr4 banks 0x' + inttohex(n, 8));

    banks := ((n shr 4) and $F) + (n and $F);
    size := 0;

    wifiloginfo(nil, 'banks='+inttostr(banks));

    for i := 0 to banks - 1 do
    begin
       cfgwritel(WIFI, 1, r + Cr4Bankidx, i);
       n := cfgreadl(WIFI, 1, r + Cr4Bankinfo);
       wifiloginfo(nil, 'bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
       size += 8192 * ((n and $3F) + 1);
    end;
    WIFI^.socramsize := size;
    WIFI^.rambase := $198000;
    exit;
  end;

  sbreset(WIFI, WIFI^.socramctl, 0, 0);
  r := WIFI^.socramregs;
  WIFIDeviceSetBackplaneWindow(WIFI, r);
  n := cfgreadl(WIFI, 1, r + Coreinfo);

  wifiloginfo(nil ,'socramrev ' + inttostr(WIFI^.socramrev) + ' coreinfo 0x' + inttohex(n, 8));

  banks := (n>>4) and $F;
  size := 0;
  for i := 0 to banks-1 do
  begin
    cfgwritel(WIFI, 1, r + Bankidx, i);
    n := cfgreadl(WIFI, 1, r + Bankinfo);
    wifiloginfo(nil, 'bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
    size += 8192 * ((n and $3F) + 1);
  end;
  WIFI^.socramsize := size;
  WIFI^.rambase := 0;
  if(WIFI^.chipid = 43430) then
  begin
    wifiloginfo(nil, 'updating bankidx values for 43430');
    cfgwritel(WIFI, 1, r + Bankidx, 3);
    cfgwritel(WIFI, 1, r + Bankpda, 0);
  end;
end;


function WIFIDeviceDownloadFirmware(WIFI : PWIFIDevice) : Longword;

var
 rambase : longword;
 FirmwareFile : file of byte;
 firmwarep : pbyte;
 comparebuf : pbyte;
 off : longword;
 fsize : longword;
 i : integer;
 lastramvalue : longword;
 chunksize : longword;
 bytestransferred : longword;
 Found : boolean;
 ConfigFilename : string;
 FirmwareFilename : string;
 s : string;
begin
 try
 WIFI_DEFAULT_LOG_LEVEL:=WIFI_LOG_LEVEL_INFO;
  Result := WIFI_STATUS_INVALID_PARAMETER;

  // request active low power. Dunno why.
  wifiloginfo(nil, 'request active low power dunno why');
  cfgw(WIFI, BAK_CHIP_CLOCK_CSR_REG, ReqALP);

  // wait for active low power available
  wifiloginfo(nil, 'waiting for active low power available');
  while ((cfgr(WIFI, BAK_CHIP_CLOCK_CSR_REG) and ALPavail) = 0) do
    MicrosecondDelay(10);

  // zero out an address of some sort which is at the top of the ram?
  lastramvalue := 0;
  WIFIDeviceSetBackplaneWindow(WIFI, WIFI^.rambase + WIFI^.socramsize - 4);
  SDIOWIFIDeviceReadWriteExtended(WIFI, true, 1, (WIFI^.rambase + WIFI^.socramsize - 4) and $7fff{ or $8000}, true, @lastramvalue, 0, 4);

  WIFILogInfo(nil, 'Starting firmware load...');

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

  WIFILogInfo(nil, 'Using ' + FirmwareFilename + ' for firmware.');

  // open file and read entire block into memory. Perhaps ought to do this in
  // chunks really? If we do, then the verify stuff needs to be done a chunk
  // at a time as well.
  assignfile(FirmwareFile, FirmwareFilename);
  reset(FirmwareFile);
  fsize := filesize(FirmwareFile);
  getmem(firmwarep, fsize);
  blockread(FirmwareFile, firmwarep^, fsize);
  closefile(FirmwareFile);
  wifiloginfo(nil, 'firmware file read into memory buffer successfully');

  // transfer firmware over the bus to the chip.
  // first, grab the reset vector from the first 4 bytes of the firmware.

  move(firmwarep^, WIFI^.resetvec, 4);
  wifiloginfo(nil, 'Reset vector of 0x' + inttohex(WIFI^.resetvec, 8) + ' copied out of firmware');

  // we haven't done this yet but the reset vector is supposed to be written to the
  // bottom of RAM, depending on which chip it is (as some load the firmware into address
  // zero anyway).

  // we need to split the calls into multiple chunks so that the addressing does
  // not go beyond the limit of the lower part of the address.
  // We'll use 2k chunks like ether4330 does.

  off := 0;
  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

  // align the size to the backplane block size.
  // we should probably zero out the extra bytes in the memory buffer.
  // do that later...
  if (fsize mod 64 <> 0) then
    fsize := ((fsize div 64) + 1) * 64;

  getmem(comparebuf, fsize);

  wifiloginfo(nil, 'bytes to transfer are ' + inttostr(fsize));
  bytestransferred := 0;
  dodumpregisters := true;

  while bytestransferred < fsize do
  begin

    sbmem(WIFI, true, firmwarep+off, chunksize, WIFI^.rambase + off);
    bytestransferred := bytestransferred + chunksize;
    //wifiloginfo(nil, 'bytes transferred = ' + inttostr(bytestransferred) + ' bytes left = ' +inttostr(fsize-bytestransferred));

    if (bytestransferred < fsize) then
    begin
      off += FIRMWARE_CHUNK_SIZE;
      if (off + chunksize > fsize) then
        chunksize := fsize - off;
    end;
  end;


  off := 0;
  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

 (*
 We don't need this comparison to always run

  wifiloginfo(nil, 'bytes to read are ' + inttostr(fsize));
  bytestransferred := 0;
  while bytestransferred < fsize do
  begin

    sbmem(WIFI, false, comparebuf+off, chunksize, WIFI^.rambase + off);
    bytestransferred := bytestransferred + chunksize;
     wifiloginfo(nil, 'bytes transferred = ' + inttostr(bytestransferred) + ' bytes left = ' +inttostr(fsize-bytestransferred));

    if (bytestransferred < fsize) then
    begin
      off += FIRMWARE_CHUNK_SIZE;
      if (off + chunksize > fsize) then
        chunksize := fsize - off;
    end;
  end;


  wifilogdebug(nil, 'block comparison started');
  for i := 0 to fsize - 1 do
    if (pbyte(firmwarep+i)^ <> pbyte(comparebuf+i)^) then
    begin
      wifiloginfo(nil, 'compare failed at byte ' + inttostr(i));
      break;
    end;
  wifiloginfo(nil, 'block comparison completed');

  freemem(firmwarep);
  freemem(comparebuf);
  *)


 except
   on e : exception do
     wifiloginfo(nil, 'exception : ' + e.message + ' at address ' + inttohex(longword(exceptaddr),8));
 end;

(*
  upload(ctl, firmware[i].fwfile, 0);



  if(FWDEBUG) print("config load...");
  n = upload(ctl, firmware[i].cfgfile, 1);
  n /= 4;
  n = (n & 0xFFFF) | (~n << 16);
  put4(buf, n);
  sbmem(1, buf, 4, ctl->rambase + ctl->socramsize - 4);
  if(ctl->armcore == ARMcr4){
  	sbwindow(ctl->sdregs);
  	cfgwritel(Fn1, ctl->sdregs + Intstatus, ~0);
  	if(ctl->resetvec.i != 0){
  		if(SBDEBUG) print("%x\n", ctl->resetvec.i);
  		sbmem(1, ctl->resetvec.c, sizeof(ctl->resetvec.c), 0);
  	}
  	sbreset(ctl->armctl, Cr4Cpuhalt, 0);
  }else
  	sbreset(ctl->armctl, 0, 0);

*)

  // assume ram base address is zero, based on code in cypress driver
  // can't find equivalent in broadcom full mac driver.
  // looks like the RAM starts at address zero and that's why the config is higher up at 0x18000000 or whatever it is.

  rambase := 0;

(*
  arm core base address  =
   switch (wlan_chip_id)
    {
        case 0x4373:
            *addr = 0x18002000 + WRAPPER_REGISTER_OFFSET;
            break;
        case 43012:
        case 43430:
            *addr = 0x18003000 + WRAPPER_REGISTER_OFFSET;
            break;
        default:
            return WHD_BADARG;
    }
    return WHD_SUCCESS;
  this doesn't match our chip if so we don't know what to use. try the broadcom full mac driver.
*)
  // disable the cores, presumably so we can write to memory.
(*

      CHECK_RETURN(whd_disable_device_core(whd_driver, WLAN_ARM_CORE, WLAN_CORE_FLAG_NONE) );
      CHECK_RETURN(whd_disable_device_core(whd_driver, SOCRAM_CORE, WLAN_CORE_FLAG_NONE) );
      CHECK_RETURN(whd_reset_device_core(whd_driver, SOCRAM_CORE, WLAN_CORE_FLAG_NONE) );

      CHECK_RETURN(whd_chip_specific_socsram_init(whd_driver) );


  result = whd_bus_write_wifi_firmware_image(whd_driver);

  if (result == WHD_UNFINISHED)
  {
      WPRINT_WHD_INFO( ("User aborted fw download\n") );
      /* user aborted */
      return result;
  }
  else if (result != WHD_SUCCESS)
  {
      whd_assert("Failed to load wifi firmware\n", result == WHD_SUCCESS);
      return result;
  }

  CHECK_RETURN(whd_bus_write_wifi_nvram_image(whd_driver) );

  /* Take the ARM core out of reset */
  if (ram_start_address != 0)
  {
      CHECK_RETURN(whd_reset_core(whd_driver, WLAN_ARM_CORE, 0, 0) );
  }
  else
  {
      CHECK_RETURN(whd_reset_device_core(whd_driver, WLAN_ARM_CORE, WLAN_CORE_FLAG_NONE) );

      result = whd_device_core_is_up(whd_driver, WLAN_ARM_CORE);
      if (result != WHD_SUCCESS)
      {
          WPRINT_WHD_ERROR( ("Could not bring ARM core up\n") );
          /* Reachable after hitting assert */
          return result;
      }
  }

  /* Wait until the High Throughput clock is available */
  loop_count = 0;
  while ( ( (result = whd_bus_read_register_value(whd_driver, BACKPLANE_FUNCTION, SDIO_CHIP_CLOCK_CSR, (uint8_t)1,
                                                  &csr_val) ) == WHD_SUCCESS ) &&
          ( (csr_val & SBSDIO_HT_AVAIL) == 0 ) &&
          (loop_count < (uint32_t)HT_AVAIL_TIMEOUT_MS) )
  {
      (void)cy_rtos_delay_milliseconds( (uint32_t)1 );   /* Ignore return - nothing can be done if it fails */
      loop_count++;
  }
  if (loop_count >= (uint32_t)HT_AVAIL_TIMEOUT_MS)
  {
      /* If your system times out here, it means that the WLAN firmware is not booting.
       * Check that your WLAN chip matches the 'wifi_image.c' being built - in GNU toolchain, $(CHIP)
       * makefile variable must be correct.
       */
      WPRINT_WHD_ERROR( ("Timeout while waiting for high throughput clock\n") );
      /* Reachable after hitting assert */
      return WHD_TIMEOUT;
  }
  if (result != WHD_SUCCESS)
  {
      WPRINT_WHD_ERROR( ("Error while waiting for high throughput clock\n") );
      /* Reachable after hitting assert */
      return result;
  }

  /* Set up the interrupt mask and enable interrupts */
  CHECK_RETURN(whd_bus_write_backplane_value(whd_driver, SDIO_INT_HOST_MASK(whd_driver), (uint8_t)4, HOSTINTMASK) );

  /* Enable F2 interrupts. This wasn't required for 4319 but is for the 43362 */
  CHECK_RETURN(whd_bus_write_backplane_value(whd_driver, SDIO_FUNCTION_INT_MASK(whd_driver), (uint8_t)1,
                                             SDIO_FUNC_MASK_F1 | SDIO_FUNC_MASK_F2) );

  /* Lower F2 Watermark to avoid DMA Hang in F2 when SD Clock is stopped. */
  CHECK_RETURN(whd_bus_write_register_value(whd_driver, BACKPLANE_FUNCTION, SDIO_FUNCTION2_WATERMARK, (uint8_t)1,
                                            (uint32_t)SDIO_F2_WATERMARK) );

  return WHD_SUCCESS;
  *)
end;


initialization
  WIFIInit;

end.

