unit sdhcihost;

{$H+}
{$mode delphi} {Default to Delphi compatible syntax}

interface

uses
  Classes, SysUtils, Devices, globaltypes, platform, threads, globalconst, globalconfig, gpio;

const
  {SDIO Bus Speeds (Hz)}
  SDIO_BUS_SPEED_DEFAULT   = 0;
  SDIO_BUS_SPEED_HS26      = 26000000;
  SDIO_BUS_SPEED_HS52      = 52000000;
  SDIO_BUS_SPEED_DDR       = 52000000;
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


type
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


 PSDIODevice = ^TSDIODevice;


 {SDHCI Device Methods}
 TSDIODeviceInitialize = function(SDHCI:PSDIODevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODeviceDeinitialize = function(SDHCI:PSDIODevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODeviceGetCardDetect = function(SDHCI:PSDIODevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODeviceGetWriteProtect = function(SDHCI:PSDIODevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODeviceSendCommand = function(SDHCI:PSDIODevice;Command:PSDIOCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODeviceSetIOS = function(SDHCI:PSDIODevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSDIODevice = record
  {Device Properties}
  Device:TDevice;                                  {The Device entry for this SDHCI}
  {SDHCI Properties}
  SDHCIId:LongWord;                                  {Unique Id of this SDHCI in the SDHCI table}
  SDIOState:LongWord;                               {SDHCI state (eg SDHCI_STATE_INSERTED)}
  DeviceInitialize:TSDIODeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
  DeviceDeinitialize:TSDIODeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
  DeviceGetCardDetect:TSDIODeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
  DeviceGetWriteProtect:TSDIODeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
  DeviceSendCommand:TSDIODeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
  DeviceSetIOS:TSDIODeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard SDHCI device interface (Or nil if the default method is suitable)}
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
//  CardSpecificData:TSDHCICardSpecificData;
//  CardIdentificationData:TSDHCICardIdentificationData;
//  SDStatusData:TSDStatusData;
//  SDSwitchData:TSDSwitchData;
//  SDConfigurationData:TSDConfigurationData;
  {Storage Properties}
//  Storage:PStorageDevice;                          {The Storage entry for this SDHCI (Where Applicable)}
  {Internal Properties}
  Prev:PSDIODevice;                                 {Previous entry in SDHCI device table}
  Next:PSDIODevice;                                 {Next entry in SDHCI device table}
 end;

 {SDHCI Host}
 PSDHCIHost = ^TSDHCIHost;

 {SDHCI Enumeration Callback}
 TSDHCIEnumerate = function(SDHCI:PSDHCIHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {SDHCI Notification Callback}
 TSDHCINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {SDHCI Host Methods}
 TSDHCIHostStart = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostStop = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadByte = function(SDHCI:PSDHCIHost;Reg:LongWord):Byte;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadWord = function(SDHCI:PSDHCIHost;Reg:LongWord):Word;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadLong = function(SDHCI:PSDHCIHost;Reg:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteByte = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteWord = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteLong = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetClockDivider = function(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetControlRegister = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSDHCIHost = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this SDHCI}
  {SDHCI Properties}
  SDHCIId:LongWord;                    {Unique Id of this SDHCI in the SDHCI table}
  SDHCIState:LongWord;                 {SDHCI state (eg SDHCI_STATE_ENABLED)}
  HostStart:TSDHCIHostStart;           {A Host specific HostStart method implementing a standard SDHCI host interface}
  HostStop:TSDHCIHostStop;             {A Host specific HostStop method implementing a standard SDHCI host interface}
  HostReadByte:TSDHCIHostReadByte;     {A Host specific HostReadByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadWord:TSDHCIHostReadWord;     {A Host specific HostReadWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadLong:TSDHCIHostReadLong;     {A Host specific HostReadLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteByte:TSDHCIHostWriteByte;   {A Host specific HostWriteByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteWord:TSDHCIHostWriteWord;   {A Host specific HostWriteWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteLong:TSDHCIHostWriteLong;   {A Host specific HostWriteLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetClockDivider:TSDHCIHostSetClockDivider;
  HostSetControlRegister:TSDHCIHostSetControlRegister;
  DeviceInitialize:TSDIODeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceDeinitialize:TSDIODeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetCardDetect:TSDIODeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetWriteProtect:TSDIODeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSendCommand:TSDIODeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSetIOS:TSDIODeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                   {Host lock}
  Address:Pointer;                     {Host register base address}
  Version:LongWord;                    {Host version information}
  Quirks:LongWord;                     {Host quirks/bugs flags}
  Quirks2:LongWord;                    {Host additional quirks/bugs flags}
  Clock:LongWord;                      {Host current clock}
  BusWidth:LongWord;                   {Host current bus width}
  Interrupts:LongWord;                 {Host interrupts to be handled}
  Voltages:LongWord;                   {Host configured voltage flags}
  Capabilities:LongWord;               {Host configured capabilities flags}
  MinimumFrequency:LongWord;           {Host configured minimum frequency}
  MaximumFrequency:LongWord;           {Host configured maximum frequency}
  MaximumBlockCount:LongWord;          {Host configured maximum block count}
  //To Do
  //PowerGPIO
  //CardDetectGPIO
  Command:PSDIOCommand;                 {Currently processing command}
  Wait:TSemaphoreHandle;               {Command completed semaphore}
  {Configuration Properties}
  PresetVoltages:LongWord;             {Host predefined voltage flags}
  PresetCapabilities:LongWord;         {Host predefined capabilities flags}
  ClockMinimum:LongWord;               {Host predefined minimum clock frequency}
  ClockMaximum:LongWord;               {Host predefined maximum clock frequency}
  DriverStageRegister:LongWord;        {Host predefined driver stage register (DSR)}
  //To Do
  //PartitionType
  {Statistics Properties}
  InterruptCount:LongWord;             {Number of interrupt requests received by the host}
  //To Do
  {Internal Properties}
  Prev:PSDHCIHost;                     {Previous entry in SDHCI table}
  Next:PSDHCIHost;                     {Next entry in SDHCI table}
 end;

 {SDHCI Functions}
 function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
 function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
 function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;

 function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord;
 function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord;

 function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord;
 function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord;

 function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
 function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;

 function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
 function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

 function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
 function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
 function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
 procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
 procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
 procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;

 function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
 function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;

 function SDHCIHostCreate:PSDHCIHost;
 function SDHCIHostCreateEx(Size:LongWord):PSDHCIHost;
 function SDHCIHostDestroy(SDHCI:PSDHCIHost):LongWord;

 function SDHCINewHostRegister(SDHCI:PSDHCIHost):LongWord;
 function SDHCIHostDeregister(SDHCI:PSDHCIHost):LongWord;

 function SDHCIHostFind(SDHCIId:LongWord):PSDHCIHost;
 function SDHCIHostEnumerate(Callback:TSDHCIEnumerate;Data:Pointer):LongWord;

 function SDHCIHostNotification(SDHCI:PSDHCIHost;Callback:TSDHCINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;


 procedure SDHCILog(Level:LongWord;SDIO:PSDIODevice;const AText:String);
 procedure SDHCILogInfo(SDIO:PSDIODevice;const AText:String); inline;
 procedure SDHCILogWarn(SDIO:PSDIODevice;const AText:String); inline;
 procedure SDHCILogError(SDIO:PSDIODevice;const AText:String); inline;
 procedure SDHCILogDebug(SDIO:PSDIODevice;const AText:String); inline;


 {SDHCI Helper Functions}
 function SDHCIGetCount:LongWord; inline;

 function SDHCIHostCheck(SDHCI:PSDHCIHost):PSDHCIHost;

 function SDHCIGetVersion(SDHCI:PSDHCIHost):Word;

 function SDHCIGetCommand(Command:Word):Word;
 function SDHCIMakeCommand(Command,Flags:Word):Word;
 function SDHCIMakeBlockSize(DMA,BlockSize:Word):Word;

 function SDHCIDeviceTypeToString(SDHCIType:LongWord):String;
 function SDHCIDeviceStateToString(SDHCIState:LongWord):String;
 function SDHCIStatusToString(Status:LongWord):String;



var
  {SDHCI logging}
  SDHCI_LOG_ENABLED:Boolean;
  SDHCI_DEFAULT_LOG_LEVEL:LongWord = SDHCI_LOG_LEVEL_DEBUG; {Minimum level for SDHCI messages.  Only messages with level greater than or equal to this will be printed}



implementation

var
 {SDHCI specific variables}
 SDHCIInitialized : boolean = false;
 SDHCIHostTable:PSDHCIHost;
 SDHCIHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SDHCIHostTableCount:LongWord;


{==============================================================================}
{==============================================================================}
{SDHCI Functions}
function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
{Reference: Section ?.? of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 Timeout:LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Reset (Mask=' + IntToHex(Mask,2) + ')');
 {$ENDIF}

 {Setup Timeout (100ms)}
 Timeout:=100;

 {Send Reset}
 SDHCIHostWriteByte(SDHCI,SDHCI_SOFTWARE_RESET,Mask);

 {Reset Clock}
 if (Mask and SDHCI_RESET_ALL) <> 0 then
  begin
   SDHCI.Clock:=0;
  end;

 {Wait for Completion}
 while (SDHCIHostReadByte(SDHCI,SDHCI_SOFTWARE_RESET) and Mask) <> 0 do
  begin
   if Timeout = 0 then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Reset timeout (Mask=' + IntToHex(Mask,8) + ')');
     Exit;
    end;
   Dec(Timeout);
   MicrosecondDelay(1000);
  end;

 {Reset Interrupts}
 if (Mask and SDHCI_RESET_ALL) <> 0 then
  begin
   SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
   SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_reset in sdhci.c
end;

{==============================================================================}

function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
{Reference: Section 3.3 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
//To Do //Caller needs to pass result of fls (Find Last Set) as Power
        //FirstBitSet in Platform should provide the correct behaviour ? (- 1 to produce $FFFF on nothing ?)
var
 Value:Byte;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Set Power (Power=' + IntToStr(Power) + ')');
 {$ENDIF}

 Value:=0;
 if Power <> $FFFF then
  begin
   case (1 shl Power) of
    SDHCI_VDD_165_195:Value:=SDHCI_POWER_180;
    SDHCI_VDD_29_30,SDHCI_VDD_30_31:Value:=SDHCI_POWER_300;
    SDHCI_VDD_32_33,SDHCI_VDD_33_34:Value:=SDHCI_POWER_330;
   end;
  end;

 if Value = 0 then
  begin
   {Power Off}
   SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,0);
  end
 else
  begin
   {Power On}
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER) <> 0 then
    begin
     SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);
    end;
   Value:=Value or SDHCI_POWER_ON;
   SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);

   //To Do //More quirks, see: sdhci_set_power in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_set_power in sdhci.c
end;

{==============================================================================}

function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;
{Reference: Section 3.2 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 Value:Word;
 Timeout:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Set Clock (Clock=' + IntToStr(Clock) + ')');
 {$ENDIF}

 {Clock Off}
 SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,0);

 {Check Clock}
 Result:=SDHCI_STATUS_SUCCESS;
 if Clock = 0 then Exit;

 {Check Version}
 if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
  begin
   {Version 3.00 divisors must be a multiple of 2}
   if SDHCI.MaximumFrequency <= Clock then
    begin
     Divider:=1;
    end
   else
    begin
     Divider:=2;
     while Divider < SDHCI_MAX_CLOCK_DIV_SPEC_300 do
      begin
       if (SDHCI.MaximumFrequency div Divider) <= Clock then Break;
       Divider:=Divider + 2;
      end;
    end;
  end
 else
  begin
   {Version 2.00 divisors must be a power of 2}
   Divider:=1;
   while Divider < SDHCI_MAX_CLOCK_DIV_SPEC_200 do
    begin
     if (SDHCI.MaximumFrequency div Divider) <= Clock then Break;
     Divider:=Divider * 2;
    end;
  end;
 Divider:=Divider shr 1;

 {Set Clock Divider}
 SDHCIHostSetClockDivider(SDHCI,0,Divider);  //To Do Index ? //What is this function for ?

 {Set Clock}
 Value:=(Divider and SDHCI_DIV_MASK) shl SDHCI_DIVIDER_SHIFT;
 Value:=Value or (((Divider and SDHCI_DIV_HI_MASK) shr SDHCI_DIV_MASK_LEN) shl SDHCI_DIVIDER_HI_SHIFT);
 Value:=Value or SDHCI_CLOCK_INT_EN;
 SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Value);

 {Wait 20ms for Clock Stable}
 Timeout:=20;
 Value:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
 while (Value and SDHCI_CLOCK_INT_STABLE) = 0 do
  begin
   if Timeout = 0 then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Clock stable timeout');
     Exit;
    end;
   Dec(Timeout);
   MicrosecondDelay(1000);
   Value:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
  end;

 {Clock On}
 Value:=Value or SDHCI_CLOCK_CARD_EN;
 SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Value);

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_set_clock in sdhci.c
 //See also: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord;
var
 Mask:LongWord;
 BlockChunk:LongWord;
 BlockBuffer:LongWord;
 BlockOffset:LongWord;
 BlockRemain:LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Transfer PIO');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Transfer PIO when no current command');
   {$ENDIF}
   Exit;
  end;

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Transfer PIO when no current data');
   {$ENDIF}
   Exit;
  end;

 {Check Data Buffer}
 if SDHCI.Command.Data.Data = nil then Exit;

 {Check Blocks Remaining}
 if SDHCI.Command.Data.BlocksRemaining = 0 then Exit;

 {Check Direction}
 if (SDHCI.Command.Data.Flags and SDIO_DATA_READ) <> 0 then
  begin
   Mask:=SDHCI_DATA_AVAILABLE;
  end
 else
  begin
   Mask:=SDHCI_SPACE_AVAILABLE;
  end;
 {Some controllers (JMicron JMB38x) mess up the buffer bits for transfers < 4 bytes. As long as it is just one block, we can ignore the bits}
 if ((SDHCI.Quirks and SDHCI_QUIRK_BROKEN_SMALL_PIO) <> 0) and (SDHCI.Command.Data.BlockCount = 1) then
  begin
   Mask:=LongWord(-1);
  end;

 {Check Data / Space Available}
 while (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and Mask) <> 0 do
  begin
   if (SDHCI.Quirks and SDHCI_QUIRK_PIO_NEEDS_DELAY) <> 0 then
    begin
     MicrosecondDelay(100);
    end;

   if (SDHCI.Command.Data.Flags and SDIO_DATA_READ) <> 0 then
    begin
     {Read Block}
     BlockChunk:=0;
     BlockOffset:=0;
     BlockRemain:=SDHCI.Command.Data.BlockSize;
     while BlockRemain > 0 do
      begin
       {Read Chunk} //To Do //Rework to read in LongWord blocks with LongWordBEToN etc ? (What about devices that read in Bytes ? SPI ? - Use MMCSPI instead)
       if BlockChunk = 0 then
        begin
         BlockBuffer:=SDHCIHostReadLong(SDHCI,SDHCI_BUFFER);
         BlockChunk:=4;
        end;

       {Copy Chunk}
       PByte(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^:=(BlockBuffer and $FF);

       {Update Block}
       Inc(BlockOffset);
       BlockBuffer:=BlockBuffer shr 8;
       Dec(BlockChunk);
       Dec(BlockRemain);
      end;
    end
   else
    begin
     {Write Block}
     BlockChunk:=0;
     BlockBuffer:=0;
     BlockOffset:=0;
     BlockRemain:=SDHCI.Command.Data.BlockSize;
     while BlockRemain > 0 do
      begin
       {Copy Chunk}
       BlockBuffer:=BlockBuffer or (PLongWord(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^ shl (BlockChunk shl 3));

       {Update Block}
       Inc(BlockOffset);
       Inc(BlockChunk);
       Dec(BlockRemain);

       {Write Chunk} //To Do //Rework to write in LongWord blocks with LongWordNToBE etc ? (What about devices that write in Bytes ? SPI ? - Use MMCSPI instead)
       if (BlockChunk = 4) or (BlockRemain = 0) then
        begin
         SDHCIHostWriteLong(SDHCI,SDHCI_BUFFER,BlockBuffer);
         BlockChunk:=0;
         BlockBuffer:=0;
        end;
      end;
    end;

   Inc(SDHCI.Command.Data.BlockOffset,SDHCI.Command.Data.BlockSize);
   Dec(SDHCI.Command.Data.BlocksRemaining);
   if SDHCI.Command.Data.BlocksRemaining = 0 then Break;
  end;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Transfer PIO completed (BlocksRemaining=' + IntToStr(SDHCI.Command.Data.BlocksRemaining) + ')');
 {$ENDIF}

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_transfer_pio in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_transfer_pio in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_read_block_pio / sdhci_write_block_pio
end;

{==============================================================================}

function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Transfer DMA');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Transfer DMA when no current command');
   {$ENDIF}
   Exit;
  end;

 {Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Transfer DMA when no current data');
   {$ENDIF}
   Exit;
  end;

 //To Do

  Result:=sDHCI_STATUS_SUCCESS;

 //See: bcm2835_mmc_transfer_dma in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord;
{Called by Interrupt Command handler when an SDHCI_INT_RESPONSE is received}
var
 Count:Integer;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'arse SDHCI Finish Command');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Finish Command when no current command');
   {$ENDIF}
   Exit;
  end;

 {Check for Response}
 if (SDHCI.Command.ResponseType and SDIO_RSP_PRESENT) <> 0 then
  begin
   {Check for 136 bit Response}
   if (SDHCI.Command.ResponseType and SDIO_RSP_136) <> 0 then
    begin
     {CRC is stripped so we need to do some shifting}
     for Count:=0 to 3 do
      begin
       SDHCI.Command.Response[Count]:=SDHCIHostReadLong(SDHCI,SDHCI_RESPONSE + ((3 - Count) * 4)) shl 8;
       if Count <> 3 then
        begin
         SDHCI.Command.Response[Count]:=SDHCI.Command.Response[Count] or SDHCIHostReadByte(SDHCI,SDHCI_RESPONSE + (((3 - Count) * 4) - 1));
        end;
      end;
    end
   else
    begin
     SDHCI.Command.Response[0]:=SDHCIHostReadLong(SDHCI,SDHCI_RESPONSE);
    end;
  end;

 SDHCI.Command.CommandCompleted:=True;

 {Check for CMD23}
 //To Do

 {Finished CMD23, now send actual command}
 //To Do


 if SDHCI.Command.DataCompleted then
  begin
   SDHCIHostFinishData(SDHCI);
  end;

 if SDHCI.Command.Data = nil then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_SUCCESS;
   SemaphoreSignal(SDHCI.Wait);
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_finish_command in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_command in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord;
{Called by Interrupt Data handler when data is received}
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Finish Data');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Finish Data when no current command');
   {$ENDIF}
   Exit;
  end;

 {Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Finish Data when no current data');
   {$ENDIF}
   Exit;
  end;

 {Check for DMA request}
 //To Do

 {Check for Error}
 if SDHCI.Command.Status <> SDHCI_STATUS_NOT_PROCESSED then
  begin
   {The specification states that the block count register must be updated, but it does not specify at what point in the data flow.
    That makes the register entirely useless to read back so we have to assume that nothing made it to the card in the event of an error}
   SDHCI.Command.Data.BytesTransfered:=0;
  end
 else
  begin
   SDHCI.Command.Data.BytesTransfered:=(SDHCI.Command.Data.BlockSize * SDHCI.Command.Data.BlockCount);
  end;

 {Check for CMD12}
 {Need to send CMD12 if a) open-ended multiblock transfer (no CMD23) or b) error in multiblock transfer}
 if SDHCI.Command.Status <> SDHCI_STATUS_NOT_PROCESSED then
  begin
   //To Do //Check for No CMD23/Check for Stop needed/Check for Error
                         //If Error do reset (CMD and DATA) before SendCommand

   Exit;
  end;

 SDHCI.Command.DataCompleted:=True;

 if SDHCI.Command.CommandCompleted then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_SUCCESS;
   SemaphoreSignal(SDHCI.Wait);
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_finish_data in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_data in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(SDHCI_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Command Interrupt');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Command Interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit;
  end;

 {Update Return Mask}
 {ReturnMask:=InterruptMask;} {Do not update from InterruptMask}

 {Check Timeout}
 if (InterruptMask and SDHCI_INT_TIMEOUT) <> 0 then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_TIMEOUT;
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;

 {Check Invalid Sequence}
 if (InterruptMask and (SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX)) <> 0 then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_INVALID_SEQUENCE;
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;

 {The host can send an interrupt when the busy state has ended, allowing us to wait without wasting CPU cycles.
  Unfortunately this is overloaded on the "data complete" interrupt, so we need to take some care when handling it.
  Note: The 1.0 specification is a bit ambiguous about this feature so there might be some problems with older controllers}
 if (SDHCI.Command.ResponseType and SDIO_RSP_BUSY) <> 0 then
  begin
   if SDHCI.Command.Data <> nil then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Command Interrupt cannot wait for busy end when data transfer current');
     {$ENDIF}
    end
   else if ((SDHCI.Quirks and SDHCI_QUIRK_NO_BUSY_IRQ) = 0) and (not(SDHCI.Command.BusyCompleted)) then
    begin
     {Mark that command completed before busy is ended}
     SDHCI.Command.BusyCompleted:=True;
     Exit;
    end;
   {The controller does not support the end-of-busy IRQ fall through and take the SDHCI_INT_RESPONSE}
  end
 else if ((SDHCI.Quirks2 and SDHCI_QUIRK2_STOP_WITH_TC) <> 0) and (SDHCI.Command.Command = SDIO_CMD_STOP_TRANSMISSION) and (SDHCI.Command.Data = nil) then
  begin
   ReturnMask:=ReturnMask and not(SDHCI_INT_DATA_END);
  end;

 {Check Response}
 if (InterruptMask and SDHCI_INT_RESPONSE) <> 0 then
  begin
   SDHCIHostFinishCommand(SDHCI);
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_cmd_irq in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_cmd_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
var
 Command:Word;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(SDHCI_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Data Interrupt');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Data Interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit;
  end;

 {CMD19 generates only Buffer Read Ready interrupt}
 if (InterruptMask and SDHCI_INT_DATA_AVAIL) <> 0 then
  begin
   Command:=SDHCIGetCommand(SDHCIHostReadWord(SDHCI,SDHCI_COMMAND));
   if (Command = SDIO_CMD_SEND_TUNING_BLOCK) or (Command = SDIO_CMD_SEND_TUNING_BLOCK_HS200) then
    begin
     SDHCI.Command.TuningCompleted:=True;
     //To Do
     Exit;
    end;
  end;

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {The "data complete" interrupt is also used to indicate that a busy state has ended. See comment above in SDHCIHostCommandInterrupt}
   if (SDHCI.Command.ResponseType and SDIO_RSP_BUSY) <> 0 then
    begin
     {Check Timeout}
     if (InterruptMask and SDHCI_INT_DATA_TIMEOUT) <> 0 then
      begin
       SDHCI.Command.Status:=SDHCI_STATUS_TIMEOUT;
       SemaphoreSignal(SDHCI.Wait);
       Exit;
      end;

     {Check Data End}
     if (InterruptMask and SDHCI_INT_DATA_END) <> 0 then
      begin
       {Some cards handle busy-end interrupt before the command completed, so make sure we do things in the proper order}
       if SDHCI.Command.BusyCompleted then
        begin
         SDHCIHostFinishCommand(SDHCI);
        end
       else
        begin
         SDHCI.Command.BusyCompleted:=True;
        end;
       Exit;
      end;
    end;

   {$IFDEF INTERRUPT_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Data Interrupt when no current data');
   {$ENDIF}
   Exit;
  end;

 {Check Timeout}
 if (InterruptMask and SDHCI_INT_DATA_TIMEOUT) <> 0 then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_TIMEOUT;
   SDHCIHostFinishData(SDHCI);
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;

 {Check Invalid Sequence}
 if (InterruptMask and SDHCI_INT_DATA_END_BIT) <> 0 then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_INVALID_SEQUENCE;
   SDHCIHostFinishData(SDHCI);
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;
 if ((InterruptMask and SDHCI_INT_DATA_CRC) <> 0) and (SDHCIGetCommand(SDHCIHostReadWord(SDHCI,SDHCI_COMMAND)) <> SDIO_CMD_BUS_TEST_R) then
  begin
   SDHCI.Command.Status:=SDHCI_STATUS_INVALID_SEQUENCE;
   SDHCIHostFinishData(SDHCI);
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;

 {Check Data Available / Space Available}
 if (InterruptMask and (SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL)) <> 0 then
  begin
   SDHCIHostTransferPIO(SDHCI);
  end;

 {Check for DMA End}
 if (InterruptMask and SDHCI_INT_DMA_END) <> 0 then
  begin
   //SDHCIHostTransferDMA(SDHCI);
   //To Do //Setup next DMA boundary address
   //StartAddress/CurrentAddress/BytesTransfered
  end;

 {Check for Data End}
 if (InterruptMask and SDHCI_INT_DATA_END) <> 0 then
  begin
   if not(SDHCI.Command.CommandCompleted) then
    begin
     {Data finished before the command completed}
     SDHCI.Command.DataCompleted:=True;
    end
   else
    begin
     SDHCIHostFinishData(SDHCI);
    end;
  end;

 Result:=SDHCI_STATUS_SUCCESS;

 //See: sdhci_data_irq in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_data_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
 Capabilities:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host Start');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if SDHCI.SDHCIState = SDHCI_STATE_ENABLED then Exit;

 {Check Start/Stop}
 Result:=ERROR_INVALID_PARAMETER;
 if not(Assigned(SDHCI.HostStart)) then Exit;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check Host Flags}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_NON_STANDARD) <> 0 then
  begin
   {Call Host Start}
   Result:=SDHCI.HostStart(SDHCI);
  end
 else
  begin
   {Call Host Start}
   if SDHCI.HostStart(SDHCI) <> ERROR_SUCCESS then Exit;

   {Get Capabilities}
   Capabilities:=SDHCIHostReadLong(SDHCI,SDHCI_CAPABILITIES);
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Capabilities = ' + IntToHex(Capabilities,8));
   {$ENDIF}

   {Check DMA Support}
   if ((Capabilities and SDHCI_CAN_DO_SDMA) = 0) and ((SDHCI.Quirks and SDHCI_QUIRK_MISSING_CAPS) = 0) then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Host does not support SDMA');
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Check Clock Maximum}
   if SDHCI.ClockMaximum <> 0 then
    begin
     SDHCI.MaximumFrequency:=SDHCI.ClockMaximum;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       SDHCI.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_V3_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
      end
     else
      begin
       SDHCI.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
      end;
     SDHCI.MaximumFrequency:=(SDHCI.MaximumFrequency * SDHCI_CLOCK_BASE_MULTIPLIER);
    end;
   if SDHCI.MaximumFrequency = 0 then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Host does not specify a maximum clock frequency');
     SDHCI.HostStop(SDHCI);
     Exit;
    end;
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host maximum frequency = ' + IntToStr(SDHCI.MaximumFrequency));
   {$ENDIF}

   {Check Clock Minimum}
   if SDHCI.ClockMinimum <> 0 then
    begin
     SDHCI.MinimumFrequency:=SDHCI.ClockMinimum;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       SDHCI.MinimumFrequency:=SDHCI.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
      end
     else
      begin
       SDHCI.MinimumFrequency:=SDHCI.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
      end;
    end;
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host minimum frequency = ' + IntToStr(SDHCI.MinimumFrequency));
   {$ENDIF}

   {Determine Voltages}
   SDHCI.Voltages:=0;
   if (Capabilities and SDHCI_CAN_VDD_330) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or SDHCI_VDD_32_33 or SDHCI_VDD_33_34;
    end;
   if (Capabilities and SDHCI_CAN_VDD_300) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or SDHCI_VDD_29_30 or SDHCI_VDD_30_31;
    end;
   if (Capabilities and SDHCI_CAN_VDD_180) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or SDHCI_VDD_165_195;
    end;
   {Check Presets}
   if SDHCI.PresetVoltages <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or SDHCI.PresetVoltages;
    end;
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host voltages = ' + IntToHex(SDHCI.Voltages,8));
   {$ENDIF}

   {Determine Capabilities}
   SDHCI.Capabilities:=SDIO_MODE_HS or SDIO_MODE_HS_52MHz or SDIO_MODE_4BIT;
   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     if (Capabilities and SDHCI_CAN_DO_8BIT) <> 0 then
      begin
       SDHCI.Capabilities:=SDHCI.Capabilities or SDIO_MODE_8BIT;
      end;
    end;
   {Check Presets}
   if SDHCI.PresetCapabilities <> 0 then
    begin
     SDHCI.Capabilities:=SDHCI.Capabilities or SDHCI.PresetCapabilities;
    end;
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host capabilities = ' + IntToHex(SDHCI.Capabilities,8));
   {$ENDIF}

   {Determine Maximum Blocks}
   SDHCI.MaximumBlockCount:=SDIO_MAX_BLOCK_COUNT;
   {$IFDEF MMC_DEBUG}
   if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host maximum blocks = ' + IntToStr(SDHCI.MaximumBlockCount));
   {$ENDIF}

   {Host reset done by host start}

   {Create MMC}
//   MMC:=SDIODeviceCreate;
(*   if MMC = nil then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogError(nil,'SDHCI Failed to create new MMC device');
     SDHCI.HostStop(SDHCI);
     Exit;
    end;*)

   {Update MMC}
   {Device}
(*   MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Device.DeviceType:=SDHCI_TYPE_MMC;
   MMC.Device.DeviceFlags:=SDHCI_FLAG_NONE;
   MMC.Device.DeviceData:=SDHCI;*)
//   MMC.Device.DeviceDescription:=_DEVICE_DESCRIPTION;
   {MMC}
(*   MMC.SDIOState:=SDIO_STATE_EJECTED;
   MMC.DeviceInitialize:=SDHCI.DeviceInitialize;
   MMC.DeviceDeinitialize:=SDHCI.DeviceDeinitialize;
   MMC.DeviceGetCardDetect:=SDHCI.DeviceGetCardDetect;
   MMC.DeviceGetWriteProtect:=SDHCI.DeviceGetWriteProtect;
   MMC.DeviceSendCommand:=SDHCI.DeviceSendCommand;
   MMC.DeviceSetIOS:=SDHCI.DeviceSetIOS;*)

(*   {Create Storage}
   MMC.Storage:=StorageDeviceCreate;
   if MMC.Storage = nil then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Failed to create new Storage device');
     SDIODeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Update Storage}
   {Device}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Storage.Device.DeviceType:=STORAGE_TYPE_REMOVABLE;
   MMC.Storage.Device.DeviceFlags:=STORAGE_FLAG_REMOVABLE or STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA;
   MMC.Storage.Device.DeviceData:=MMC;
   MMC.Storage.Device.DeviceDescription:=MMC_STORAGE_DESCRIPTION;
   {Storage}
   MMC.Storage.StorageState:=STORAGE_STATE_EJECTED;
   MMC.Storage.DeviceRead:=MMCStorageDeviceRead;
   MMC.Storage.DeviceWrite:=MMCStorageDeviceWrite;
   MMC.Storage.DeviceErase:=MMCStorageDeviceErase;
   MMC.Storage.DeviceControl:=MMCStorageDeviceControl;
*)

   {Initialize MMC}
(*   Status:=SDIODeviceInitialize(MMC);
   if (Status <> MMC_STATUS_SUCCESS) and (Status <> MMC_STATUS_NO_MEDIA) then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Failed to initialize new MMC device');
     StorageDeviceDestroy(MMC.Storage);
     SDIODeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Check MMC Type}
   if MMC.Device.DeviceType = MMC_TYPE_SDIO then
    begin
     {Destroy Storage}
     StorageDeviceDestroy(MMC.Storage);
     MMC.Storage:=nil;
    end;

   {Register MMC}
   if SDIODeviceRegister(MMC) <> MMC_STATUS_SUCCESS then
    begin
     if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Failed to register new MMC device');
     StorageDeviceDestroy(MMC.Storage);
     SDIODeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;
*)
   {Enable Host}
   SDHCI.SDHCIState:=SDHCI_STATE_ENABLED;

   {Notify Enable}
   NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_ENABLE);

   {Notify Insert MMC}
(*   if MMC.SDIOState = SDIO_STATE_INSERTED then
    begin
     NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_INSERT);
    end;
*)
   {Check Storage}
(*   if MMC.Storage <> nil then
    begin
     {Set Storage State to Inserted}
     if MMC.MMCState = MMC_STATE_INSERTED then
      begin
       StorageDeviceSetState(MMC.Storage,STORAGE_STATE_INSERTED);
      end;

     {Start Storage Status Checking}
     if StorageDeviceStartStatus(MMC.Storage,MMC_STATUS_TIMER_INTERVAL) <> ERROR_SUCCESS then
      begin
       if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Failed start status for new MMC device');
      end;
    end;
*)
   Result:=ERROR_SUCCESS;
  end;

 //See: add_sdhci in sdhci.c
end;

{==============================================================================}

function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if SDHCI_LOG_ENABLED then SDHCILogDebug(nil,'SDHCI Host Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if SDHCI.SDHCIState <> SDHCI_STATE_ENABLED then Exit;

 {Check Stop}
 Result:=ERROR_INVALID_PARAMETER;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check Host Flags}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_NON_STANDARD) <> 0 then
  begin
   {Call Host Stop}
   Result:=SDHCI.HostStop(SDHCI);
  end
 else
  begin
   //To Do //

   {Call Host Stop}
   if SDHCI.HostStop(SDHCI) <> ERROR_SUCCESS then Exit;

   {Disable Host}
   SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;

   {Notify Disable}
   NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_DISABLE);

   //To Do //Eject/Deregister/Destroy (MMC/Storage) etc //See: Storage/Mouse etc

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
begin
 {}
 if Assigned(SDHCI.HostReadByte) then
  begin
   Result:=SDHCI.HostReadByte(SDHCI,Reg);
  end
 else
  begin
   Result:=PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
begin
 {}
 if Assigned(SDHCI.HostReadWord) then
  begin
   Result:=SDHCI.HostReadWord(SDHCI,Reg);
  end
 else
  begin
   Result:=PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
begin
 {}
 if Assigned(SDHCI.HostReadLong) then
  begin
   Result:=SDHCI.HostReadLong(SDHCI,Reg);
  end
 else
  begin
   Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteByte) then
  begin
   SDHCI.HostWriteByte(SDHCI,Reg,Value);
  end
 else
  begin
   PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteWord) then
  begin
   SDHCI.HostWriteWord(SDHCI,Reg,Value);
  end
 else
  begin
   PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteLong) then
  begin
   SDHCI.HostWriteLong(SDHCI,Reg,Value);
  end
 else
  begin
   PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
begin
 {}
 Result:=SDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 if Assigned(SDHCI.HostSetClockDivider) then
  begin
   Result:=SDHCI.HostSetClockDivider(SDHCI,Index,Divider);
  end
 else
  begin
   Result:=SDHCI_STATUS_SUCCESS;
  end;

 //See: sdhci_host->set_clock in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=sDHCI_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 if Assigned(SDHCI.HostSetControlRegister) then
  begin
   Result:=SDHCI.HostSetControlRegister(SDHCI);
  end
 else
  begin
   Result:=SDHCI_STATUS_SUCCESS;
  end;

 //See: sdhci_host->set_control_reg in sdhci.h
end;

{==============================================================================}

function SDHCIHostCreate:PSDHCIHost;
{Create a new SDHCI entry}
{Return: Pointer to new SDHCI entry or nil if SDHCI could not be created}
begin
 {}
 Result:=SDHCIHostCreateEx(SizeOf(TSDHCIHost));
end;

{==============================================================================}

function SDHCIHostCreateEx(Size:LongWord):PSDHCIHost;
{Create a new SDHCI entry}
{Size: Size in bytes to allocate for new SDHCI (Including the SDHCI entry)}
{Return: Pointer to new SDHCI entry or nil if SDHCI could not be created}
begin
 SDHCILogDebug(nil, 'creating SDHCI host');
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSDHCIHost) then Exit;

 {Create SDHCI}
 Result:=PSDHCIHost(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=SDHCI_TYPE_NONE;
 Result.Device.DeviceFlags:=SDHCI_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update SDHCI}
 Result.SDHCIId:=DEVICE_ID_ANY;
 Result.SDHCIState:=SDHCI_STATE_DISABLED;
 Result.HostStart:=nil;
 Result.HostStop:=nil;
 Result.HostReadByte:=nil;
 Result.HostReadWord:=nil;
 Result.HostReadLong:=nil;
 Result.HostWriteByte:=nil;
 Result.HostWriteWord:=nil;
 Result.HostWriteLong:=nil;
 Result.HostSetClockDivider:=nil;
 Result.HostSetControlRegister:=nil;
 Result.DeviceInitialize:=nil;
 Result.DeviceDeinitialize:=nil;
 Result.DeviceGetCardDetect:=nil;
 Result.DeviceGetWriteProtect:=nil;
 Result.DeviceSendCommand:=nil;
 Result.DeviceSetIOS:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Wait:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'Failed to create lock for SDHCI host');
   SDHCIHostDestroy(Result);
   Result:=nil;
   Exit;
  end;

 SDHCILogDebug(nil, 'SDHCI host successfully created');

end;

{==============================================================================}

function SDHCIHostDestroy(SDHCI:PSDHCIHost):LongWord;
{Destroy an existing SDHCI entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SDHCI}
 Result:=ERROR_IN_USE;
 if SDHCIHostCheck(SDHCI) = SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if SDHCI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SDHCI.Lock);
  end;

 {Destroy SDHCI}
 Result:=DeviceDestroy(@SDHCI.Device);
end;

{==============================================================================}

function SDHCINewHostRegister(SDHCI:PSDHCIHost):LongWord;
{Register a new SDHCI in the SDHCI table}
var
 SDHCIId:LongWord;
begin
 {}
 SDHCILogDebug(nil, 'Registering new SDHCI Host');
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.SDHCIId <> DEVICE_ID_ANY then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(SDHCI.HostStart)) then Exit;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check SDHCI}
 Result:=ERROR_ALREADY_EXISTS;
 if SDHCIHostCheck(SDHCI) = SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert SDHCI}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update SDHCI}
    SDHCIId:=0;
    while SDHCIHostFind(SDHCIId) <> nil do
     begin
      Inc(SDHCIId);
     end;
    SDHCI.SDHCIId:=SDHCIId;

    {Update Device}
    SDHCI.Device.DeviceName:=SDHCI_NAME_PREFIX + IntToStr(SDHCI.SDHCIId);
    SDHCI.Device.DeviceClass:=DEVICE_CLASS_SDHCI;

    {Register Device}
    Result:=DeviceRegister(@SDHCI.Device);
    if Result <> ERROR_SUCCESS then
     begin
      SDHCI.SDHCIId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link SDHCI}
    if SDHCIHostTable = nil then
     begin
      SDHCIHostTable:=SDHCI;
     end
    else
     begin
      SDHCI.Next:=SDHCIHostTable;
      SDHCIHostTable.Prev:=SDHCI;
      SDHCIHostTable:=SDHCI;
     end;

    {Increment Count}
    Inc(SDHCIHostTableCount);

(*    {Check Started}
    if MMCStarted then
     begin
      if not MMC_ASYNCSTART then
       begin
        {Start Host}*)
        SDHCIHostStart(SDHCI);

        {Return Result}
        Result:=ERROR_SUCCESS;
(*       end
      else
       begin
        {Schedule Worker}
        Result:=WorkerSchedule(0,TWorkerTask(MMCAsyncStart),SDHCI,nil)
       end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_SUCCESS;
     end;*)
   finally
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostDeregister(SDHCI:PSDHCIHost):LongWord;
{Deregister a SDHCI from the SDHCI table}
var
 Prev:PSDHCIHost;
 Next:PSDHCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.SDHCIId = DEVICE_ID_ANY then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SDHCI}
 Result:=ERROR_NOT_FOUND;
 if SDHCIHostCheck(SDHCI) <> SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove SDHCI}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    //To Do //Stop host if started, see USB

    {Deregister Device}
    Result:=DeviceDeregister(@SDHCI.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink SDHCI}
    Prev:=SDHCI.Prev;
    Next:=SDHCI.Next;
    if Prev = nil then
     begin
      SDHCIHostTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SDHCIHostTableCount);

    {Update SDHCI}
    SDHCI.SDHCIId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostFind(SDHCIId:LongWord):PSDHCIHost;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=nil;

 {Check Id}
 if SDHCIId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SDHCI}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Check State}
      if SDHCI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if SDHCI.SDHCIId = SDHCIId then
         begin
          Result:=SDHCI;
          Exit;
         end;
       end;

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function SDHCIHostEnumerate(Callback:TSDHCIEnumerate;Data:Pointer):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SDHCI}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Check State}
      if SDHCI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(SDHCI,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostNotification(SDHCI:PSDHCIHost;Callback:TSDHCINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SDHCI,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check SDHCI}
   if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@SDHCI.Device,DEVICE_CLASS_SDHCI,Callback,Data,Notification,Flags);
  end;
end;


{==============================================================================}
{==============================================================================}
{SDHCI Helper Functions}
function SDHCIGetCount:LongWord; inline;
{Get the current SDHCI count}
begin
 {}
 Result:=SDHCIHostTableCount;
end;

{==============================================================================}

function SDHCIHostCheck(SDHCI:PSDHCIHost):PSDHCIHost;
{Check if the supplied SDHCI is in the SDHCI table}
var
 Current:PSDHCIHost;
begin
 {}
 Result:=nil;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SDHCI}
    Current:=SDHCIHostTable;
    while Current <> nil do
     begin
      {Check SDHCI}
      if Current = SDHCI then
       begin
        Result:=SDHCI;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

{==============================================================================}

function SDHCIGetVersion(SDHCI:PSDHCIHost):Word;
begin
 {}
 Result:=SDHCI_SPEC_100; {Default to Version 1.0}

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Version and SDHCI_SPEC_VER_MASK);
end;

{==============================================================================}

function SDHCIGetCommand(Command:Word):Word;
begin
 {}
 Result:=((Command shr 8) and $3F);
end;

{==============================================================================}

function SDHCIMakeCommand(Command,Flags:Word):Word;
begin
 {}
 Result:=((Command and $FF) shl 8) or (Flags and $FF);
end;

{==============================================================================}

function SDHCIMakeBlockSize(DMA,BlockSize:Word):Word;
begin
 {}
 Result:=((DMA and $07) shl 12)  or (BlockSize and $0FFF);
end;

{==============================================================================}

function SDHCIDeviceTypeToString(SDHCIType:LongWord):String;
begin
 {}
 Result:='SDHCI_TYPE_UNKNOWN';

 if SDHCIType <= SDHCI_TYPE_MAX then
  begin
   Result:=SDHCI_TYPE_NAMES[SDHCIType];
  end;
end;

{==============================================================================}

function SDHCIDeviceStateToString(SDHCIState:LongWord):String;
 begin
 {}
 Result:='SDHCI_STATE_UNKNOWN';

 if SDHCIState <= SDHCI_STATE_MAX then
  begin
   Result:=SDHCI_STATE_NAMES[SDHCIState];
  end;
end;


procedure SDHCILog(Level:LongWord;SDIO:PSDIODevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SDHCI_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = SDHCI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SDHCI_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SDHCI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SDHCI: ';

 {Check SDHCI}
 if SDIO <> nil then
  begin
   WorkBuffer:=WorkBuffer + SDHCI_NAME_PREFIX + IntToStr(SDIO.SDHCIId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_DEVICES,LogLevelToLoggingSeverity(Level),'SDHCI',WorkBuffer + AText);
end;

{==============================================================================}

procedure SDHCILogInfo(SDIO:PSDIODevice;const AText:String); inline;
begin
 {}
 SDHCILog(SDHCI_LOG_LEVEL_INFO,SDIO,AText);
end;

{==============================================================================}

procedure SDHCILogWarn(SDIO:PSDIODevice;const AText:String); inline;
begin
 {}
 SDHCILog(SDHCI_LOG_LEVEL_WARN,SDIO,AText);
end;

{==============================================================================}

procedure SDHCILogError(SDIO:PSDIODevice;const AText:String); inline;
begin
 {}
 SDHCILog(SDHCI_LOG_LEVEL_ERROR,SDIO,AText);
end;

{==============================================================================}

procedure SDHCILogDebug(SDIO:PSDIODevice;const AText:String); inline;
begin
 {}
 SDHCILog(SDHCI_LOG_LEVEL_DEBUG,SDIO,AText);
end;



function SDHCIStatusToString(Status:LongWord):String;
{Translates an MMC status code into a string describing it}
begin
 {}
 Result:='unknown error';

 case Status of
  SDHCI_STATUS_SUCCESS:Result:='success';
  SDHCI_STATUS_TIMEOUT:Result:='request timed out';
  SDHCI_STATUS_NO_MEDIA:Result:='no media present';
  SDHCI_STATUS_HARDWARE_ERROR:Result:='hardware error';
  SDHCI_STATUS_INVALID_DATA:Result:='invalid data';
  SDHCI_STATUS_INVALID_PARAMETER:Result:='invalid parameter';
  SDHCI_STATUS_INVALID_SEQUENCE:Result:='invalid sequence';
  SDHCI_STATUS_OUT_OF_MEMORY:Result:='out of memory';
  SDHCI_STATUS_UNSUPPORTED_REQUEST:Result:='unsupported request';
  SDHCI_STATUS_NOT_PROCESSED:Result:='request not processed yet';
 end;
end;

{Initialization Functions}
procedure SDHCIInit;
var
  i : integer;
begin
 {Check Initialized}
 if SDHCIInitialized then Exit;

 {disconnect emmc from SD card (connect sdhost instead) }
  for i := 48 to 53 do
   GPIOFunctionSelect(i,GPIO_FUNCTION_ALT0);

 {connect emmc to wifi }
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



 {Initialize Logging}
 SDHCI_LOG_ENABLED:=(SDHCI_DEFAULT_LOG_LEVEL <> SDHCI_LOG_LEVEL_NONE);

 {Initialize SDHCI Host Table}
 SDHCIHostTable:=nil;
 SDHCIHostTableLock:=CriticalSectionCreate;
 SDHCIHostTableCount:=0;
 if SDHCIHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if SDHCI_LOG_ENABLED then SDHCILogError(nil,'Failed to create SDHCI host table lock');
  end;

 SDHCIInitialized :=True;
end;

initialization
   SDHCIInit;

end.

