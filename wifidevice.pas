unit wifidevice;

{$mode objfpc}{$H+}
{$ifdef RPI}
{$linklib libwpa_supplicant_pizero.a}
{$endif}
{$ifdef RPI3}
{$linklib libwpa_supplicant_pi3.a}
{$endif}
{$ifdef RPI4}
{$linklib libwpa_supplicant_pi4.a}
{$endif}


//{$DEFINE CYW43455_DEBUG}

interface

uses
  mmc,
  Classes,
  SysUtils,
  Devices,
  Ultibo,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  DMA,
  GPIO,
  Network,
  {$IFDEF RPI4}BCM2711{$ENDIF} {$IFDEF RPI3}BCM2710{$ENDIF} {$IFDEF RPI}BCM2708{$ENDIF};

const
  {WIFI logging}
  WIFI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {WIFI debugging messages}
  WIFI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {WIFI informational messages, such as a device being attached or detached}
  WIFI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {WIFI warning messages}
  WIFI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {WIFI error messages}
  WIFI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No WIFI messages}

  WPA_KEY_MAX = 4;

  {wpa supplicant logging}
  MSG_EXCESSIVE = 0;
  MSG_MSGDUMP = 1;
  MSG_DEBUG = 2;
  MSG_INFO = 3;
  MSG_WARNING = 4;
  MSG_ERROR = 5;

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

  CYW43455_SDIO_DRIVER_NAME = 'Cypress CYW43455 Wireless LAN SDIO Driver';

  CYW43455_NETWORK_DESCRIPTION = 'Cypress 43455 SDIO Wireless Network Adapter';

  //cyw43455 event flags bits (see whd_event_msg record)
  CYW43455_EVENT_FLAG_LINK        = $01;  // bit 0
  CYW43455_EVENT_FLAG_FLUSHTXQ    = $02;  // bit 1
  CYW43455_EVENT_FLAG_GROUP       = $04;  // but 2

  IOCTL_MAX_BLKLEN = 2048;
  SDPCM_HEADER_SIZE = 8;
  IOCTL_LEN_BYTES = 4;
  BCDC_HEADER_SIZE = 4;

  RECEIVE_REQUEST_PACKET_COUNT = 16;

  // BCDC Header Flags
  BCDC_PROTOCOL_VERSION = 2;  // Protocol version
  BCDC_VERSION_MASK = $f0;    // Protocol version mask
  BCDC_VERSION_SHIFT = 4;     // Protocol version shift

  // wifi control commands
  WLC_GET_VAR = 262;
  WLC_SET_VAR = 263;

  WHD_MSG_IFNAME_MAX = 16;

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

  CYW43455_FIRMWARE_TIMEOUT = 2000;

  {$IFDEF RPI}
  WLAN_ON_PIN = GPIO_PIN_41;
  {$ENDIF}
  {$IF DEFINED(RPI3) or DEFINED(RPI4)}
  WLAN_ON_PIN = VIRTUAL_GPIO_PIN_1;
  {$ENDIF}
  SD_32KHZ_PIN = GPIO_PIN_43;

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

  MAX_GLOM_PACKETS = 20; // maximum number of packets allowed in glomming (superpackets).

type
  PWPAKey = ^TWPAKey;
  TWPAKey = packed record
    groupkeyid: longword;
    keylen: longword;
    key: array[0..103] of byte;
    algo: longword;
    flags: longword;
    pad1: array[1..12] of byte;
    ivinitialised: longword;
    unused: longword;
    ivhigh: longword;
    ivlow: word;
    pad2: array[1..10] of byte;
    ethaddr: THardwareAddress;
    pad3: array[1..2] of byte;
  end;

  TWIFIJoinType = (WIFIJoinBlocking, WIFIJoinBackground);
  TWIFIReconnectionType = (WIFIReconnectNever, WIFIReconnectAlways);

  countryparams = record
    country_ie: array[1..4] of char;
    revision: longint;
    country_code: array[1..4] of char;
  end;

  wlc_ssid = record
    SSID_len: longword;
    SSID: array[0..31] of byte;
  end;

  chanrec = record
    chan: byte;
    other: byte;
  end;

  wl_scan_params = record
    ssid: wlc_ssid;
    bssid: THardwareAddress;
    bss_type: byte;
    scan_type: byte;
    nprobes: longword;
    active_time: longword;
    passive_time: longword;
    home_time: longword;
    channel_num: longword;
    channel_list: array[1..14] of chanrec;
    ssids: array[0..SSID_MAX_LEN-1] of word;
  end;

  wl_escan_params = record
    version: longword;
    action: word;
    sync_id: word;
    params: wl_scan_params;
  end;

  pwhd_tlv8_header = ^whd_tlv8_header;
  whd_tlv8_header = record
    atype: byte;
    length: byte;
  end;

  pwhd_tlv8_data = ^whd_tlv8_data;
  whd_tlv8_data = record
    atype: byte;
    length: byte;
    data: array[1..1] of byte;    // used as a pointer to a list.
  end;

  prsn_ie_fixed_portion = ^rsn_ie_fixed_portion;
  rsn_ie_fixed_portion = record
    tlv_header: whd_tlv8_header; //id, length
    version: word;
    group_key_suite: longword; // See whd_80211_cipher_t for values
    pairwise_suite_count: word;
    pairwise_suite_list: array[1..1] of longword;
  end;

  wsec_pmk = record
    key_len: word;
    flags: word;
    key: array[0..WSEC_MAX_PSK_LEN-1] of byte;
  end;

  wl_join_assoc_params = record
    bssid: THardwareAddress;
    bssid_cnt: word;
    chanspec_num: longword;
    chanspec_list: array[0..1] of word;
  end;

  wl_join_scan_params = record
    scan_type: byte;          // 0 use default, active or passive scan */
    nprobes: longint;         // -1 use default, number of probes per channel */
    active_time: longint;     // -1 use default, dwell time per channel for active scanning
    passive_time: longint;    // -1 use default, dwell time per channel for passive scanning
    home_time: longint;       // -1 use default, dwell time for the home channel between channel scans
  end;

  wl_extjoin_params = record
     ssid: wlc_ssid;                          // {0, ""}: wildcard scan */
     scan_params: wl_join_scan_params;
     assoc_params: wl_join_assoc_params;     // optional field, but it must include the fixed portion
                                              // of the wl_join_assoc_params_t struct when it does
                                              // present.
  end;

  PGlomDescriptor = ^TGlomDescriptor;
  TGlomDescriptor = record
    len: word;
    notlen: word;
    seq: byte;
    chan: byte;
    nextlen: byte; // unsure
    hdrlen: byte;
    flow: byte;
    credit: byte;
    reserved: word;
    packetlengths: array[0..MAX_GLOM_PACKETS-1] of word;     // puts an upper limit on number of coalesced packets but should be ok.
  end;

  PSDPCM_HEADER = ^SDPCM_HEADER;
  SDPCM_HEADER = record
    // sdpcm_sw_header     (hardware extension header)
     seq,                  // rx/tx sequence number
     chan,                 // 4 MSB channel number, 4 LSB aritrary flag
     nextlen,              // length of next data frame, reserved for Tx
     hdrlen,               // data offset
     flow,                 // flow control bits, reserved for tx
     credit: byte;        // maximum sequence number allowed by firmware for Tx
     reserved: word;      // reserved
  end;

  PBCDC_HEADER = ^BCDC_HEADER;
  BCDC_HEADER = record
    flags,                // flags contain protocol and checksum info
    priority,             // 802.1d priority and USB flow control info (bit 4:7)
    flags2,               // additional flags containing dongle interface index
    data_offset: byte;   // start of packet data. header is following by firmware signals
  end;

  IOCTL_CMDP = ^IOCTL_CMD;
  IOCTL_CMD = record
    sdpcmheader: SDPCM_HEADER;
    cmd: longword;
    outlen,
    inlen: word;
    flags,
    status: longword;
    data: array[0..IOCTL_MAX_BLKLEN-1] of byte;
  end;

  IOCTL_GLOM_HDR = record
    len: word;
    reserved1,
    flags: byte;
    reserved2: word;
    pad: word;
  end;

  IOCTL_GLOM_CMD = record
    glom_hdr: IOCTL_GLOM_HDR;
    cmd: IOCTL_CMD
  end;

  PIOCTL_MSG = ^IOCTL_MSG;
  IOCTL_MSG = record
    len: word;           // length in bytes to follow
    notlen: word;        // bitwise inverse of length (len + notlen = $ffff)
    case byte of
      1: (cmd: IOCTL_CMD);
      2: (glom_cmd: IOCTL_GLOM_CMD);
  end;

  byte4 = array[1..4] of byte;

  TFirmwareEntry = record
    chipid: word;
    chipidrev: word;
    firmwarefilename: string;
    configfilename: string;
    regufilename: string;
  end;

  pether_header = ^ether_header;
  ether_header = record
      destination_address: THardwareAddress;
      source_address: THardwareAddress;
      ethertype: word;
  end;

  whd_event_eth_hdr = record
      subtype: word;                 // Vendor specific..32769
      length: word;
      version: byte;                 // Version is 0
      oui: array[0..2] of byte;      //  OUI
      usr_subtype: word;             // user specific Data
  end;

  whd_event_msg = record
      version: word;
      flags: word;                                    // see flags below
      event_type: longword;                           // Message (see below)
      status: longword;                               // Status code (see below)
      reason: longword;                               // Reason code (if applicable)
      auth_type: longword;                            // WLC_E_AUTH
      datalen: longword;                              // data buf
      addr: THardwareAddress;                         // Station address (if applicable)
      ifname: array[0..WHD_MSG_IFNAME_MAX-1] of char; // name of the packet incoming interface
      ifidx: byte;                                    // destination OS i/f index
      bsscfgidx: byte;                                // source bsscfg index
  end;

  // used by driver msgs
  pwhd_event = ^whd_event;
  whd_event = record
      eth: ether_header;                  // 12 bytes
      eth_evt_hdr: whd_event_eth_hdr ;    // 10 bytes
      whd_event: whd_event_msg;           // 48 bytes
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
      version: longword;                       // version field
      length: longword;                        // byte length of data in this record, starting at version and including IEs
      BSSID: THardwareAddress;                 // Unique 6-byte MAC address
      beacon_period: word;                     // Interval between two consecutive beacon frames. Units are Kusec
      capability: word;                        // Capability information
      SSID_len: byte;                          // SSID length
      SSID: array[0..31] of char;                // Array to store SSID

      // this is a sub struct in cypress driver.
          ratecount: longword;                 // Count of rates in this set
          rates: array[1..15] of byte;         // rates in 500kbps units, higher bit set if basic

      chanspec: wl_chanspec ;                   // Channel specification for basic service set
      atim_window: word;                       // Announcement traffic indication message window size. Units are Kusec
      dtim_period: byte;                       // Delivery traffic indication message period
      RSSI: integer;                           // receive signal strength (in dBm)
      phy_noise: shortint;                     // noise (in dBm)

      n_cap: byte;                             // BSS is 802.11N Capable
      nbss_cap: longword;                      // 802.11N BSS Capabilities (based on HT_CAP_*)
      ctl_ch: byte;                            // 802.11N BSS control channel number
      reserved32: array[1..1] of longword;       // Reserved for expansion of BSS properties
      flags: byte;                             // flags
      reserved: array[1..3] of byte;           // Reserved for expansion of BSS properties
      basic_mcs: array[0..MCSSET_LEN-1] of byte; // 802.11N BSS required MCS set

      ie_offset: word;                         // offset at which IEs start, from beginning
      ie_length: longword;                     // byte length of Information Elements
      SNR: integer;                            // Average SNR(signal to noise ratio) during frame reception
  end;

  pwl_escan_result = ^wl_escan_result;
  wl_escan_result = record
    buflen: longword;
    version: longword;
    sync_id: word;
    bss_count: word;
    bss_info: array [1..1] of wl_bss_info; // used as pointer to a list.
  end;

  TWIFIScanUserCallback = procedure(ssid: string; ScanResultP: pwl_escan_result); cdecl;
  TWirelessEventCallback = procedure(Event: TWIFIEvent; EventRecordP: pwhd_event; RequestItemP: PWIFIRequestItem; datalength: longint);

  TWIFIRequestItem = record
    RequestID: Word;
    RegisteredEvents: TWIFIEventSet;
    MsgP: PIOCTL_MSG;
    Signal: TSemaphoreHandle;
    Callback: TWirelessEventCallback;
    UserDataP: Pointer;
    NextP: PWIFIRequestItem;
  end;

  PUltiboPacketXFer = ^ TUltiboPacketXFer;
  TUltiboPacketXFer = record
    source_address: THardwareAddress;
    packetbufferp: PByte;
    DataLength: longint;
  end;

  PCYW43455Network = ^TCYW43455Network;

  TCYW43455WorkerThread = class(TThread)
  private
    FNetwork: PCYW43455Network;

    FRequestQueueP: PWIFIRequestItem;
    FLastRequestQueueP: PWIFIRequestItem;
    FQueueProtect: TCriticalSectionHandle;
    procedure ProcessDevicePacket(var responseP: PIOCTL_MSG;
                             NetworkEntryP: PNetworkEntry;
                             var bytesleft: longword;
                             var isfinished: boolean);
  public
    constructor Create(CreateSuspended: Boolean; ANetwork: PCYW43455Network);
    destructor Destroy; override;
    function AddRequest(ARequestID: word; InterestedEvents: TWIFIEventSet;
                                   Callback: TWirelessEventCallback;
                                   UserDataP: Pointer): PWIFIRequestItem;
    procedure DoneWithRequest(ARequestItemP: PWIFIRequestItem);
    function FindRequest(ARequestId: word): PWIFIRequestItem;
    function FindRequestByEvent(AEvent: longword): PWIFIRequestItem;
    procedure dumpqueue;
    procedure Execute; override;
  end;

  TCYW43455ReconnectionThread = class(TThread)
  private
    FNetwork: PCYW43455Network;

    FConnectionLost: TSemaphoreHandle;
    FSSID: string;
    FBSSID: THardwareAddress;
    FUseBSSID: boolean;
    FKey: string;
    FCountry: string;
    FReconnectionType: TWIFIReconnectionType;
  public
    constructor Create(ANetwork: PCYW43455Network);
    procedure SetConnectionDetails(aSSID, aKey, aCountry: string; BSSID: THardwareAddress; useBSSID: boolean; aReconnectionType: TWIFIReconnectionType);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TWPASupplicantThread = class(TThread)
  private
    FNetwork: PCYW43455Network;
  public
    constructor Create(ANetwork: PCYW43455Network);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TCYW43455Network = record
   {Network Properties}
   Network: TNetworkDevice;
   {Driver Properties}
   ReceiveRequestSize: LongWord;                 {Size of each receive request buffer}
   TransmitRequestSize: LongWord;                {Size of each transmit request buffer}
   ReceiveEntryCount: LongWord;                  {Number of entries in the receive queue}
   TransmitEntryCount: LongWord;                 {Number of entries in the transmit queue}
   ReceivePacketCount: LongWord;                 {Maximum number of packets per receive entry}
   TransmitPacketCount: LongWord;                {Maximum number of packets per transmit entry}
   {SDIO Properties}
   Func1: PSDIOFunction;                         {Backplane function}
   Func2: PSDIOFunction;                         {Wireless LAN function (Function 3 on some chips is a standard Bluetooth interface)}

   {CYW43455 Properties}
   chipid: Word;
   chipidrev: Word;
   armcore: LongWord;
   chipcommon: LongWord;
   armctl: LongWord;
   armregs: LongWord;
   d11ctl: LongWord;
   socramregs: LongWord;
   socramctl: LongWord;
   socramrev: LongWord;
   sdregs: LongWord;
   sdiorev: LongWord;
   socramsize: LongWord;
   rambase: LongWord;
   dllctl: LongWord;
   resetvec: LongWord;

   txglom: Boolean;
   txseq: byte;            // ioctl tx sequence number.
   ioctl_reqid: LongWord;  // ioctl request id used to match request to response. starts at 1 because 0 is reserved for an event entry.

   macaddress: THardwareAddress;

   TXBuffer: PIOCTL_MSG;
   RXBuffer: PIOCTL_MSG;
   DMABuffer: Pointer;
   DMAAlignment: LongWord;

   JoinCompleted: Boolean;

   EAPOLCompleted: boolean;
   WPAKeys: array[0..WPA_KEY_MAX] of TWPAKey;
   ValidKeys: array[0..WPA_KEY_MAX] of Boolean;
   NetworkUpSignal: TSemaphoreHandle;
   CountryCode: array[0..1] of char;

   WorkerThread: TCYW43455WorkerThread;
   BackgroundJoinThread: TCYW43455ReconnectionThread;
   WPASupplicantThread: TWPASupplicantThread;

   ReceiveGlomPacketCount: LongWord;              {number of Glom packets received}
   ReceiveGlomPacketSize: LongWord;               {total bytes received via Glom packets}
end;

function WIFIDeviceInitialize(Network: PCYW43455Network): LongWord;

function WIFIDeviceSetBackplaneWindow(Network: PCYW43455Network; addr: longword): longword;
function WIFIDeviceCoreScan(Network: PCYW43455Network): longint;
procedure WIFIDeviceRamScan(Network: PCYW43455Network);
function WIFIDeviceDownloadFirmware(Network: PCYW43455Network): Longword;

procedure sbreset(Network: PCYW43455Network; regs: longword; pre: word; ioctl: word);
procedure sbdisable(Network: PCYW43455Network; regs: longword; pre: word; ioctl: word);

procedure WIFILogError(Network: PCYW43455Network; const AText: String); inline;

{primary function for the user to call to join a wireless network}
function WirelessJoinNetwork(JoinType: TWIFIJoinType;
                             timeout: integer = 10000): longword; cdecl;

{functions the supplicant needs to be able to call that are defined in this device driver}
procedure SendSupplicantL2Packet(PacketBufferP: PByte; Len: longword); cdecl; public name 'SendSupplicantL2Packet';
procedure UltiboTimeProc(var epoch: longint; var millisecond: integer); cdecl; public name 'UltiboTimeProc';
procedure UltiboMonotonicTimeProc(var epoch: longint; var millisecond: integer); cdecl; public name 'UltiboMonotonicTimeProc';
procedure WirelessScan(UserCallback: TWIFIScanUserCallback; WaitTime: Longint = 10000); cdecl; public name 'WirelessScan';
function SupplicantWirelessJoinNetwork(ssid: PChar; authkey: PByte;
                             authkeylen: longword;
                             bssid: PHardwareAddress;
                             usebssid: boolean = false): longword; cdecl; public name 'SupplicantWirelessJoinNetwork';
procedure LockEAPOLPacketQueue(trace: pchar); cdecl; public name 'LockEAPOLPacketQueue';
procedure UnlockEAPOLPacketQueue; cdecl; public name 'UnlockEAPOLPacketQueue';
procedure LockSendPacketQueue; cdecl; public name 'LockSendPacketQueue';
procedure UnlockSendPacketQueue; cdecl; public name 'UnlockSendPacketQueue';
function UltiboSetKey(const ifname: PChar; priv: pointer; alg: byte;
		      const addr: PByte; key_idx: integer; set_tx: integer;
		      const seq: PByte; seq_len: integer;
		      const key: PByte; key_len: integer): integer; cdecl; public name 'UltiboSetKey';
procedure UltiboEAPOLComplete; cdecl; public name 'UltiboEAPOLComplete';
function UltiboSetCountry(priv: pointer; ccode: PChar): integer; cdecl; public name 'UltiboSetCountry';

//procedure WirelessScan(UserCallback: TWIFIScanUserCallback; WaitTime: Longint = 10000); cdecl;
function FirmwareWirelessJoinNetwork(ssid: string; security_key: string;
                             countrycode: string;
                             JoinType: TWIFIJoinType;
                             ReconnectType: TWIFIReconnectionType;
                             bssid: THardwareAddress;
                             usebssid: boolean = false): longword; cdecl;

function WirelessLeaveNetwork: longword;

var
  WIFI_LOG_ENABLED: boolean = false;

var
  // defines whether to inject waits into packet transmission when the firmware
  // credit value indicates internal buffers are filling up.
  CYW43455_USE_FIRMWARE_CREDIT_VALUE: Boolean = FALSE;

  // Log level as used by the supplicant. Enables us to control it from the device
  // driver source instead of having to change the supplicant source.
  WPASupplicantLogLevel: integer = MSG_INFO; cvar;

  WIFI_DEFAULT_LOG_LEVEL:LongWord = WIFI_LOG_LEVEL_ERROR; {Minimum level for WIFI messages.  Only messages with level greater than or equal to this will be printed}

implementation

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

var
  CYW43455Initialized: Boolean;
  CYW43455SDIODriver: PSDIODriver;  {CYW43455 SDIO Driver interface (Set by WIFIDeviceInit)}

  WIFI_USE_SUPPLICANT: boolean = true;

  firmware: array[1..FIRWMARE_OPTIONS_COUNT] of TFirmwareEntry =
    (
      	    ( chipid: $4330; chipidrev: 3; firmwarefilename: 'fw_bcm40183b1.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename: ''),
    	    ( chipid: $4330; chipidrev: 4; firmwarefilename: 'fw_bcm40183b2.bin'; configfilename: 'bcmdhd.cal.40183.26MHz'; regufilename: ''),
    	    ( chipid: 43362; chipidrev: 0; firmwarefilename: 'fw_bcm40181a0.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename: ''),
    	    ( chipid: 43362; chipidrev: 1; firmwarefilename: 'fw_bcm40181a2.bin'; configfilename: 'bcmdhd.cal.40181'; regufilename: ''),
    	    ( chipid: 43430; chipidrev: 1; firmwarefilename: 'brcmfmac43430-sdio.bin'; configfilename: 'brcmfmac43430-sdio.txt'; regufilename: 'brcmfmac43430-sdio.clm_blob'),
{Pi Zero2W} ( chipid: 43430; chipidrev: 2; firmwarefilename: 'brcmfmac43436-sdio.bin'; configfilename: 'brcmfmac43436-sdio.txt'; regufilename: 'brcmfmac43436-sdio.clm_blob'),
    	    ( chipid: $4345; chipidrev: 6; firmwarefilename: 'brcmfmac43455-sdio.bin'; configfilename: 'brcmfmac43455-sdio.txt'; regufilename: 'brcmfmac43455-sdio.clm_blob'),
    	    ( chipid: $4345; chipidrev: 9; firmwarefilename: 'brcmfmac43456-sdio.bin'; configfilename: 'brcmfmac43456-sdio.txt'; regufilename: 'brcmfmac43456-sdio.clm_blob')
    );

  EAPOLQueueLock: TRTLCriticalSection;
  SupplicantOperatingState: integer; cvar; external;

{functions we need to call that are defined by wpa_supplicant}
procedure SetUltiboMacAddress(AMacAddress: PChar); cdecl; external;
function wpa_supplicant_main(confname: PChar): integer; cdecl external;
procedure ultibo_driver_new_packet_data(srcaddr: pbyte; packetbuf: pbyte; len: word); cdecl; external;
procedure DoNetworkNotify(data: pointer); forward;
procedure UltiboEloopTerminate; cdecl; external;

procedure sbenable(Network: PCYW43455Network); forward;
function WirelessInit(Network: PCYW43455Network): longword; forward;
procedure WIFILogInfo(Network: PCYW43455Network;const AText: String); forward;

function WIFIDeviceSDIODriverBind(MMC: PMMCDevice; Func: PSDIOFunction): LongWord; forward;
function WIFIDeviceSDIODriverUnbind(MMC: PMMCDevice; Func: PSDIOFunction): LongWord; forward;

function WIFIDeviceCheckFunction(Func: PSDIOFunction): LongWord; forward;

procedure WIFIDeviceInit;
var
 Status: LongWord;
begin
 {}
 {Check Initialized}
 if CYW43455Initialized then
   Exit;

 {Initialize Logging}
 WIFI_LOG_ENABLED := (WIFI_DEFAULT_LOG_LEVEL <> WIFI_LOG_LEVEL_NONE);

 {Initialize EAPOL queue lock for use with supplicant}
 InitializeCriticalSection(EAPOLQueueLock);

 {Create SDIO Driver}
 CYW43455SDIODriver := SDIODriverCreate;
 if CYW43455SDIODriver <> nil then
  begin
   {Update SDIO Driver}
   {Driver}
   CYW43455SDIODriver^.Driver.DriverName := CYW43455_SDIO_DRIVER_NAME;
   {SDIO}
   CYW43455SDIODriver^.DriverBind := @WIFIDeviceSDIODriverBind;
   CYW43455SDIODriver^.DriverUnbind := @WIFIDeviceSDIODriverUnbind;

   {Register SDIO Driver}
   Status := SDIODriverRegister(CYW43455SDIODriver);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil, 'CYW43455: Failed to register driver: ' + MMCStatusToString(Status));

     {Destroy Driver}
     SDIODriverDestroy(CYW43455SDIODriver);
    end;
  end
 else
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil, 'CYW43455: Failed to create driver');
  end;

 CYW43455Initialized := True;
end;

procedure hexdump(p: pbyte; len: word; title: string = '');
var
  rows: integer;
  remainder: integer;
  i: integer;

  function line(bytecount: integer): string;
  var
    s: string;
    asc: string;
    j: integer;
    b: byte;
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
    WIFILogInfo(nil, title + ' @ address 0x'+ PtrToHex(p) + ' for ' + inttostr(len) + ' bytes')
  else
    WIFILogInfo(nil, 'hexdump @ address 0x'+ PtrToHex(p) + ' for ' + inttostr(len) + ' bytes');
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

function NetSwapLong(v: longword): longword; inline;
begin
  Result:= ((v and $ff) << 24) or ((v and $ff00) << 8) or ((v and $ff0000) >> 8) or ((v and $ff000000) >> 24);
end;

function NetSwapWord(v: word): word; inline;
begin
 Result := ((v and $ff) << 8) or ((v and $ff00) >> 8);
end;

function buftostr(bufferp: pbyte; messagelen: word; nullterminated: boolean = false): string;
var
 i: integer;
 b: byte;
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
   on e: exception do
     WIFILogError(nil, 'CYW43455: exception in buftostr ' + e.message + ' i='+inttostr(i));
 end;
end;

procedure WIFILog(Level: LongWord; Network: PCYW43455Network; const AText: String);
var
 WorkBuffer: String;
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

 {Check Network}
 if Network <> nil then
  begin
   WorkBuffer:=WorkBuffer + NETWORK_NAME_PREFIX + IntToStr(Network^.Network.NetworkId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_DEVICES,LogLevelToLoggingSeverity(Level),'WIFIdevice',WorkBuffer + AText);
end;

procedure WIFILogError(Network: PCYW43455Network; const AText: String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_ERROR,Network,AText);
end;

{==============================================================================}

procedure WIFILogDebug(Network: PCYW43455Network; const AText: String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_DEBUG,Network,AText);
end;

procedure WIFILogInfo(Network: PCYW43455Network; const AText: String); inline;
begin
 {}
 WIFILog(WIFI_LOG_LEVEL_INFO,Network,AText);
end;

function CYW43455DeviceOpen(Network:PNetworkDevice):LongWord;
var
 i: Integer;
 MMC:PMMCDevice;
 Status:longword;
 Entry:PNetworkEntry;
 DMAProperties:TDMAProperties;
begin
  Result := ERROR_INVALID_PARAMETER;

  {Check Network}
  if Network = nil then Exit;
  if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(Network, 'CYW43455: Network Open');
  {$ENDIF}

  {Get MMC}
  MMC := PMMCDevice(Network^.Device.DeviceData);
  if MMC = nil then Exit;

  {Acquire the Lock}
  if MutexLock(Network^.Lock) = ERROR_SUCCESS then
  begin
    try
      {Check State}
      Result := ERROR_ALREADY_OPEN;
      if Network^.NetworkState <> NETWORK_STATE_CLOSED then Exit;

      {Set Result}
      Result := ERROR_OPERATION_FAILED;

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

      // Setup request id and tx sequence
      PCYW43455Network(Network)^.ioctl_reqid := 1;

      // Create Up Semaphore
      PCYW43455Network(Network)^.EAPOLCompleted := false;
      PCYW43455Network(Network)^.NetworkUpSignal := SemaphoreCreate(0);
      for i := 0 to WPA_KEY_MAX do
        PCYW43455Network(Network)^.ValidKeys[i] := false;

      // Allocate buffers
      PCYW43455Network(Network)^.TXBuffer := DMABufferAllocate(DMAHostGetDefault, SizeOf(IOCTL_MSG));
      PCYW43455Network(Network)^.RXBuffer := DMABufferAllocate(DMAHostGetDefault, SizeOf(IOCTL_MSG));
      PCYW43455Network(Network)^.DMABuffer := DMABufferAllocate(DMAHostGetDefault,IOCTL_MAX_BLKLEN);

      if not(DMA_CACHE_COHERENT) then
      begin
        {Clean Cache (Dest)}
        CleanDataCacheRange(PtrUInt(PCYW43455Network(Network)^.TXBuffer), SizeOf(IOCTL_MSG));
        CleanDataCacheRange(PtrUInt(PCYW43455Network(Network)^.RXBuffer), SizeOf(IOCTL_MSG));
        CleanDataCacheRange(PtrUInt(PCYW43455Network(Network)^.DMABuffer), IOCTL_MAX_BLKLEN);
      end;

      // Get DMA properties
      if DMAHostProperties(DMAHostGetDefault, @DMAProperties) = ERROR_SUCCESS then
      begin
        // Save DMA alignment
        PCYW43455Network(Network)^.DMAAlignment := DMAProperties.Alignment;
      end;

      {Initialize WIFI}
      Status := WIFIDeviceInitialize(PCYW43455Network(Network));
      if Status <> MMC_STATUS_SUCCESS then
       begin
        WIFILogError(nil, 'CYW43455: Failed to initialize new device ' + inttostr(status));

        Exit;
       end;

      // set buffering sizes
      PCYW43455Network(Network)^.ReceiveRequestSize := RECEIVE_REQUEST_PACKET_COUNT * sizeof(IOCTL_MSG); // space for 16 reads; actually more than that as a packet can't be as large as the IOCTL msg allocates
      PCYW43455Network(Network)^.TransmitRequestSize := SizeOf(IOCTL_MSG);
      PCYW43455Network(Network)^.ReceiveEntryCount := 40;
      PCYW43455Network(Network)^.TransmitEntryCount := 40;
      PCYW43455Network(Network)^.ReceivePacketCount := RECEIVE_REQUEST_PACKET_COUNT * IOCTL_MAX_BLKLEN div (ETHERNET_MIN_PACKET_SIZE);
      PCYW43455Network(Network)^.TransmitPacketCount := 1;

      {Allocate Receive Queue Buffer}
      Network^.ReceiveQueue.Buffer := BufferCreate(SizeOf(TNetworkEntry), PCYW43455Network(Network)^.ReceiveEntryCount);
      if Network^.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
       begin
        if NETWORK_LOG_ENABLED then NetworkLogError(Network, 'CYW43455: Failed to create receive queue buffer');

        Exit;
       end;

      {Allocate Receive Queue Semaphore}
      Network^.ReceiveQueue.Wait := SemaphoreCreate(0);
      if Network^.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
       begin
        if NETWORK_LOG_ENABLED then NetworkLogError(Network, 'CYW43455: Failed to create receive queue semaphore');

        Exit;
       end;

      {Allocate Receive Queue Buffers}
      Entry := BufferIterate(Network^.ReceiveQueue.Buffer, nil);
      while Entry <> nil do
       begin
        {Initialize Entry}
        Entry^.Size := PCYW43455Network(Network)^.ReceiveRequestSize;
        Entry^.Offset := 0; // The actual offset is variable and is accounted for when receiving a packet
        Entry^.Count := 0;

        {Allocate Request Buffer}
        Entry^.Buffer := DMABufferAllocate(DMAHostGetDefault, Entry^.Size);
        if Entry^.Buffer = nil then
         begin
          if WIFI_LOG_ENABLED then WIFILogError(nil, 'CYW43455: Failed to allocate receive buffer');

          Exit;
         end;

        if not(DMA_CACHE_COHERENT) then
         begin
          {Clean Cache (Dest)}
          CleanDataCacheRange(PtrUInt(Entry^.Buffer), Entry^.Size);
         end;

        {Initialize Packets}
        SetLength(Entry^.Packets, PCYW43455Network(Network)^.ReceivePacketCount);

        {Initialize First Packet}
        Entry^.Packets[0].Buffer := Entry^.Buffer;
        Entry^.Packets[0].Data := Entry^.Buffer + Entry^.Offset;
        Entry^.Packets[0].Length := Entry^.Size - Entry^.Offset;

        Entry := BufferIterate(Network^.ReceiveQueue.Buffer,Entry);
       end;

      {Allocate Receive Queue Entries}
      SetLength(Network^.ReceiveQueue.Entries, PCYW43455Network(Network)^.ReceiveEntryCount);

      {Allocate Transmit Queue Buffer}
      Network^.TransmitQueue.Buffer := BufferCreate(SizeOf(TNetworkEntry), PCYW43455Network(Network)^.TransmitEntryCount);
      if Network^.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
       begin
        if WIFI_LOG_ENABLED then WIFILogError(nil, 'CYW43455: Failed to create transmit queue buffer');

        Exit;
       end;

      {Allocate Transmit Queue Semaphore}
      Network^.TransmitQueue.Wait := SemaphoreCreate(PCYW43455Network(Network)^.TransmitEntryCount);
      if Network^.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
       begin
        if WIFI_LOG_ENABLED then WIFILogError(nil, 'CYW43455: Failed to create transmit queue semaphore');

        Exit;
       end;

      {Allocate Transmit Queue Buffers}
      Entry:=BufferIterate(Network^.TransmitQueue.Buffer, nil);
      while Entry <> nil do
       begin
        {Initialize Entry}
        Entry^.Size := PCYW43455Network(Network)^.TransmitRequestSize;
        Entry^.Offset := IOCTL_LEN_BYTES + SDPCM_HEADER_SIZE + BCDC_HEADER_SIZE;
        Entry^.Count := PCYW43455Network(Network)^.TransmitPacketCount;

        {Allocate Request Buffer}
        Entry^.Buffer := DMABufferAllocate(DMAHostGetDefault, Entry^.Size);
        if Entry^.Buffer = nil then
         begin
          if WIFI_LOG_ENABLED then WIFILogError(nil, 'CYW43455: Failed to allocate wifi transmit buffer');

          Exit;
         end;

        if not(DMA_CACHE_COHERENT) then
         begin
          {Clean Cache (Dest)}
          CleanDataCacheRange(PtrUInt(Entry^.Buffer), Entry^.Size);
         end;

        {Initialize Packets}
        SetLength(Entry^.Packets, PCYW43455Network(Network)^.TransmitPacketCount);

        {Initialize First Packet}
        Entry^.Packets[0].Buffer := Entry^.Buffer;
        Entry^.Packets[0].Data := Entry^.Buffer + Entry^.Offset;
        Entry^.Packets[0].Length := Entry^.Size - Entry^.Offset;

        Entry := BufferIterate(Network^.TransmitQueue.Buffer,Entry);
       end;

      {Allocate Transmit Queue Entries}
      SetLength(Network^.TransmitQueue.Entries, PCYW43455Network(Network)^.TransmitEntryCount);

      // create the receive thread
      if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Creating Worker Thread');

      PCYW43455Network(Network)^.WorkerThread := TCYW43455WorkerThread.Create(true, PCYW43455Network(Network));
      PCYW43455Network(Network)^.WorkerThread.Start;

      // create the reconnect thread
      PCYW43455Network(Network)^.BackgroundJoinThread := TCYW43455ReconnectionThread.Create(PCYW43455Network(Network));

      // call initialisation (loads country code etc)
      // note this code requires the worker thread to be running otherwise
      // responses won't be received.
      WirelessInit(PCYW43455Network(Network));

      {Set State to Open}
      Network^.NetworkState := NETWORK_STATE_OPEN;

      {Notify the State}
      NotifierNotify(@Network^.Device, DEVICE_NOTIFICATION_OPEN);

      Result := ERROR_SUCCESS;
    finally
      {Release the Lock}
      MutexUnlock(Network^.Lock);
    end;
  end
  else
  begin
    Result := ERROR_CAN_NOT_COMPLETE;
  end;
end;

function CYW43455DeviceClose(Network:PNetworkDevice):LongWord;
var
 MMC: PMMCDevice;
begin
  Result:=ERROR_INVALID_PARAMETER;

  {Check Network}
  if Network = nil then Exit;
  if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(Network, 'CYW43455: Network Close');
  {$ENDIF}

  {Get MMC}
  MMC := PMMCDevice(Network^.Device.DeviceData);
  if MMC = nil then Exit;

  {Leave Network}
  if PCYW43455Network(Network)^.JoinCompleted then
    WirelessLeaveNetwork;

  {Check State}
  Result:=ERROR_NOT_OPEN;
  if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

  {Set State to Closing}
  Result := ERROR_OPERATION_FAILED;
  if NetworkDeviceSetState(Network, NETWORK_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

  {Acquire the Lock}
  if MutexLock(Network^.Lock) = ERROR_SUCCESS then
  begin
    try

      //To Do //TestingSDIO // More to add here

      {Set State to Closed}
      Network^.NetworkState := NETWORK_STATE_CLOSED;

      {Notify the State}
      NotifierNotify(@Network^.Device, DEVICE_NOTIFICATION_CLOSE);

      {Free Buffers}
      DMABufferRelease(PCYW43455Network(Network)^.TXBuffer);
      DMABufferRelease(PCYW43455Network(Network)^.RXBuffer);
      DMABufferRelease(PCYW43455Network(Network)^.DMABuffer);

      {Destroy Up Semaphore}
      SemaphoreDestroy(PCYW43455Network(Network)^.NetworkUpSignal);

      Result := ERROR_SUCCESS;
    finally
      {Release the Lock}
      MutexUnlock(Network^.Lock);
    end;
 end
 else
 begin
   Result := ERROR_CAN_NOT_COMPLETE;
 end;
end;

function CYW43455DeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
 var
  MMC:PMMCDevice;
  Status:LongWord;
begin
  {}
  Result:=ERROR_INVALID_PARAMETER;

  {Check Network}
  if Network = nil then Exit;
  if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(Network, 'CYW43455: Network Control');
  {$ENDIF}

  {Get MMC}
  MMC:=PMMCDevice(Network^.Device.DeviceData);
  if MMC = nil then Exit;

  {Acquire the Lock}
  if MutexLock(Network^.Lock) = ERROR_SUCCESS then
   begin
    try
     {Set Result}
     Result:=ERROR_OPERATION_FAILED;
     Status:=MMC_STATUS_SUCCESS;

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
        {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: set mac address to '
                         + inttohex(phardwareaddress(argument1)^[0], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[1], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[2], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[3], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[4], 2) + ':'
                         + inttohex(phardwareaddress(argument1)^[5], 2)
                         );
        {$ENDIF}
        Status := MMC_STATUS_SUCCESS;
       end;
      NETWORK_CONTROL_GET_MAC:begin
        {Get the MAC for this device}
        move(PCYW43455Network(Network)^.macaddress[0], PHardwareAddress(Argument1)^, sizeof(THardwareAddress));
        Status := MMC_STATUS_SUCCESS;
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
       move(PCYW43455Network(Network)^.macaddress[0], PHardwareAddress(Argument1)^, sizeof(THardwareAddress));
       Status := MMC_STATUS_SUCCESS;
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
     if Status <> MMC_STATUS_SUCCESS then Exit;

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

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CYW43455: Buffer Allocate');
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
   Entry^.Offset:=IOCTL_LEN_BYTES + SDPCM_HEADER_SIZE + BCDC_HEADER_SIZE;
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
   WIFILogError(nil, 'CYW43455: Failed to get a transmit buffer!');

end;

function CYW43455BufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network^.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil,'CYW43455: Buffer Release');
 {$ENDIF}

 {Check Entry}
 if Entry = nil then Exit;

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network^.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Free Entry (Receive Buffer)}
 Result:=BufferFree(Entry);
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

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
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
     {Remove Entry}
     Entry:=Network^.ReceiveQueue.Entries[Network^.ReceiveQueue.Start];

     {Update Start}
     Network^.ReceiveQueue.Start:=(Network^.ReceiveQueue.Start + 1) mod PCYW43455Network(Network)^.ReceiveEntryCount;

     {Update Count}
     Dec(Network^.ReceiveQueue.Count);

     {Return Result}
     Result:=ERROR_SUCCESS;

     {Release the Lock}
     MutexUnlock(Network^.Lock);
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

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
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
        //CYW43455TransmitStart(PCYW43455Network(Network));
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;

      {Release the Lock}
      MutexUnlock(Network^.Lock);
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

function WIFIDeviceInitialize(Network: PCYW43455Network): LongWord;
var
 chipid: word;
 chipidrev: byte;
 bytevalue: byte;
begin
 {}
 try
   if WIFI_LOG_ENABLED then WIFILogInfo(nil,'CYW43455: WIFIDeviceInitialize');

   Result:=MMC_STATUS_INVALID_PARAMETER;

   {Check Network}
   if Network = nil then Exit;

   // if we get here we have successfully set the fn0 block size in CCCR and therefore the backplane is up.

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: setting backplane block size');

   // set block sizes for fn1 and fn2 in their respective function registers.
   // note these are still writes to the common IO area (function 0).
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: setting backplane fn1 block size to ' + IntToStr(Network^.Func1^.MaxBlockSize));
   Result := SDIOFunctionSetBlockSize(Network^.Func1, Network^.Func1^.MaxBlockSize);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: setting backplane fn2 (radio) block size to ' + IntToStr(Network^.Func2^.MaxBlockSize));
   Result := SDIOFunctionSetBlockSize(Network^.Func2, Network^.Func2^.MaxBlockSize);
   if (Result = MMC_STATUS_SUCCESS) then
   begin
     {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Successfully updated backplane block sizes');
     {$ENDIF}
   end
   else
     WIFILogError(nil, 'CYW43455: Failed to update backplane block sizes');

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: IO Enable backplane function 1');
   Result:=SDIOFunctionEnable(Network^.Func1);
   if (Result = MMC_STATUS_SUCCESS) then
   begin
     {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: io enable successfully set for function 1');
     {$ENDIF}
   end
   else
     WIFILogError(nil, 'CYW43455: io enable could not be set for function 1');

   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Reading the Chip ID');
   {$ENDIF}

   chipid := 0;
   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,0,  0, @chipid);
   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,1,  0, pbyte(@chipid)+1);
   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,2,  0, @chipidrev);
   chipidrev := chipidrev and $f;
   if (Result = MMC_STATUS_SUCCESS) then
   begin
     if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: WIFI Chip ID is 0x'+inttohex(chipid, 4) + ' rev ' + inttostr(chipidrev));
     Network^.chipid := chipid;
     Network^.chipidrev := chipidrev;
   end;

   // scan the cores to establish various key addresses
   WIFIDeviceCoreScan(Network);

   if (Network^.armctl = 0) or (Network^.dllctl = 0) or
     ((Network^.armcore = ARMcm3) and ((Network^.socramctl = 0) or (Network^.socramregs = 0))) then
   begin
     WIFILogError(nil, 'CYW43455: Corescan did not find essential cores!');
     exit;
   end;

   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Disable core');
   {$ENDIF}

   if (Network^.armcore = ARMcr4) then
   begin
     {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sbreset armcr4 core');
     {$ENDIF}

     sbreset(Network, Network^.armctl, Cr4Cpuhalt, Cr4CpuHalt)
   end
   else
   begin
     {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sbdisable armctl core');
     {$ENDIF}

     sbdisable(Network, Network^.armctl, 0, 0);
   end;

   sbreset(Network, Network^.dllctl, 8 or 4, 4);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: WIFI Device RAM scan');

   WIFIDeviceRamScan(Network);

   // Set clock on function 1
   Result := SDIODeviceReadWriteDirect(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
   if (Result <> MMC_STATUS_SUCCESS) then
     WIFILogError(nil, 'CYW43455: Unable to update config at chip clock csr register');
   MicrosecondDelay(10);

   // check active low power clock availability

   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, nil);
   sleep(1);
   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ReqALP, nil);

   // now we keep reading them until we have some availability
   bytevalue := 0;
   while (bytevalue and (HTavail or ALPavail) = 0) do
   begin
     Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, 0, @bytevalue);
     if (Result <> MMC_STATUS_SUCCESS) then
       WIFILogError(nil, 'CYW43455: failed to read clock settings');
     MicrosecondDelay(10);
   end;

   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Clock availability is 0x' + inttohex(bytevalue, 2));
   {$ENDIF}

   // finally we can clear active low power request. Not sure if any of this is needed to be honest.
   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: clearing active low power clock request');
   Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_CHIP_CLOCK_CSR_REG, Nohwreq or ForceALP, nil);

   MicrosecondDelay(65);

  WIFIDeviceSetBackplaneWindow(Network, Network^.chipcommon);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Disable pullups');
  Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, gpiopullup, 0, nil);
  if (Result = MMC_STATUS_SUCCESS) then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Successfully disabled SDIO extra pullups');
   {$ENDIF}
  end
  else
    WIFILogError(nil, 'CYW43455: Failed to disable SDIO extra pullups');

  Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, Gpiopulldown, 0, nil);
  if (Result = MMC_STATUS_SUCCESS) then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Successfully disabled SDIO extra pulldowns');
    {$ENDIF}
  end
  else
    WIFILogError(nil, 'CYW43455: Failed to disable SDIO extra pulldowns');

   if (Network^.chipid = $4330) or (Network^.chipid = 43362) then
   begin
    // there is other stuff from sbinit() to do here
    // however the chipids are not either 3b or zero as far as I can tell so
    // we won't do them until we find a device that needs them.
    // relates to power management by the look of it. PMU, drive strength.
   end;

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Download WIFI firmware to Cypress SOC');
   Result := WIFIDeviceDownloadFirmware(Network);

   // Enable the device. This should boot the firmware we just loaded to the chip
   sbenable(Network);

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Enabling interrupts for all functions');
   Result := SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BUS_FUNCTION, SDIO_CCCR_IENx, (INTR_CTL_MASTER_EN or INTR_CTL_FUNC1_EN or INTR_CTL_FUNC2_EN), nil );
   if (Result = MMC_STATUS_SUCCESS) then
   begin
     {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Successfully enabled interrupts');
     {$ENDIF}
   end
   else
     WIFILogError(nil, 'CYW43455: Failed to enable interrupts');

   if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: End of WIFIDeviceInitialize');
   Result:=MMC_STATUS_SUCCESS;

 except
   on e: exception do
   WIFILogError(nil, 'CYW43455: Exception ' + e.message + ' at ' + PtrToHex(exceptaddr) + ' in WIFIDeviceInitialize');
 end;
end;

function WIFIDeviceSetBackplaneWindow(Network: PCYW43455Network; addr: longword): longword;
begin
 addr := addr and (not $7fff);

 {$ifdef CYW43455_SDIO_DEBUG}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: setting backplane address to ' + inttohex((addr shr 8) and $ff, 8) + ' '
                  + inttohex((addr shr 16) and $ff, 8) + ' '
                  + inttohex((addr shr 24) and $ff, 8));
 {$endif}

 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG, (addr shr 8) and $ff,nil);
 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG+1,(addr shr 16) and $ff,nil);
 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BACKPLANE_FUNCTION, BAK_WIN_ADDR_REG+2,(addr shr 24) and $ff,nil);

 if (Result = MMC_STATUS_SUCCESS) then
 begin
  {$ifdef CYW43455_SDIO_DEBUG}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: function ' + inttostr(1) + ' backplanewindow updated to ' + inttohex(addr, 8));
  {$endif}
 end
 else
   WIFILogError(nil, 'CYW43455: something went wrong in setbackplanewindow');
end;

function WIFIDeviceCoreScan(Network: PCYW43455Network): longint;
const
  corescansz = 512;
  CID_ID_MASK  =   $0000ffff;
  CID_REV_MASK  =  $000f0000;
  CID_REV_SHIFT  = 16;
  CID_TYPE_MASK  = $f0000000;
  CID_TYPE_SHIFT = 28;

var
 buf: PByte;
 i, coreid, corerev: integer;
 addr: longint;
 addressbytes: array[1..4] of byte;
 address: longword;
 chipidbuf: longword;
 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 chipid: word;
 chiprev: word;
 socitype: word;
 str: string;
 {$ENDIF}
begin
 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Starting WIFI core scan');

 Result := MMC_STATUS_INVALID_PARAMETER;

 buf := DMABufferAllocate(DMAHostGetDefault, Corescansz);

 if not(DMA_CACHE_COHERENT) then
  begin
   // Clean Cache (Dest)
   CleanDataCacheRange(PtrUInt(buf), Corescansz);
  end;

 // set backplane window
 Result := WIFIDeviceSetBackplaneWindow(Network, BAK_BASE_ADDR);

 // read 32 bits containing chip id and other info
 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,0,  0, pbyte(@chipidbuf));
 if (Result <> MMC_STATUS_SUCCESS) then
    WIFILogError(nil, 'CYW43455: failed to read the first byte of the chip id');

 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,1,  0, pbyte(@chipidbuf)+1);
 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,2,  0, pbyte(@chipidbuf)+2);
 Result:=SDIODeviceReadWriteDirect(Network^.Func1^.MMC,False,BACKPLANE_FUNCTION,2,  0, pbyte(@chipidbuf)+3);

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 chipid := chipidbuf  and CID_ID_MASK;
 chiprev := (chipidbuf and CID_REV_MASK) shr CID_REV_SHIFT;
 socitype := (chipidbuf and CID_TYPE_MASK) shr CID_TYPE_SHIFT;
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: chipid ' + inttohex(chipid,4) + ' chiprev ' + inttohex(chiprev, 4) + ' socitype ' + inttohex(socitype,4));
 {$ENDIF}

 // read pointer to core info structure.
 // 63*4 is yucky. Could do with a proper address definition for it.
 Result := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, False, BACKPLANE_FUNCTION, 63*4, true, @addressbytes[1], 0, 4);
 address := plongint(@addressbytes[1])^;

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Core info pointer is read as ' + inttohex(address, 8));
 {$ENDIF}

 // we must get the top 15 bits from the address and set the bakplane window to it
 WIFIDeviceSetBackplaneWindow(Network, address);

 address := (address and $7fff) or $8000;

 try
   // read the core info from the device
   Result := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, False, BACKPLANE_FUNCTION, address, true, buf, 8, 64);
   if (Result <> MMC_STATUS_SUCCESS) then
   begin
     WIFILogError(nil, 'CYW43455: Failed to read Core information from the SDIO device.');
     exit;
   end;

   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
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
   {$ENDIF}

   coreid := 0;
   corerev := 0;

   i := 0;

    while i < Corescansz do
    begin
       case buf[i] and $0f of
 	    $0F: begin
                   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
                   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Reached end of descriptor');
                   {$ENDIF}

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
                         Network^.chipcommon := addr;
                      end;
                      ARMcm3,
  		      ARM7tdmi,
  		      ARMcr4:
                      begin
                         Network^.armcore := coreid;
                         if ((buf[i] and $c0) > 0) then
                         begin
                           if (Network^.armctl = 0) then
                             Network^.armctl := addr;
                         end
                         else
                         if (Network^.armregs = 0) then
                            Network^.armregs := addr;
                      end;

  		      $80E:
                      begin
                         if ((buf[i] and $c0) > 0) then
                           Network^.socramctl := addr
                         else
                         if (Network^.socramregs = 0) then
                           Network^.socramregs := addr;
                         Network^.socramrev := corerev;
                      end;

                      $829:
                      begin
                         if ((buf[i] and $c0) = 0) then
                           Network^.sdregs := addr;
                         Network^.sdiorev := corerev;
                      end;

                      $812:
                      begin
                         if ((buf[i] and $c0) > 0) then
                           Network^.dllctl := addr;
                      end;
                    end;
                 end;
       end;
      i := i + 4;
    end;

    DMABufferRelease(buf);

    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Corescan completed.');

    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then
    begin
      WIFILogDebug(nil,'CYW43455: chipcommon=0x' + inttohex(Network^.chipcommon,8));
      WIFILogDebug(nil,'CYW43455: armcore=0x' + inttohex(Network^.armcore,8));
      WIFILogDebug(nil,'CYW43455: armctl=0x' + inttohex(Network^.armctl,8));
      WIFILogDebug(nil,'CYW43455: armregs=0x' + inttohex(Network^.armregs,8));
      WIFILogDebug(nil,'CYW43455: socramctl=0x' + inttohex(Network^.socramctl,8));
      WIFILogDebug(nil,'CYW43455: socramregs=0x' + inttohex(Network^.socramregs,8));
      WIFILogDebug(nil,'CYW43455: socramrev=0x' + inttohex(Network^.socramrev,8));
      WIFILogDebug(nil,'CYW43455: sdregs=0x' + inttohex(Network^.sdregs,8));
      WIFILogDebug(nil,'CYW43455: sdiorev=0x' + inttohex(Network^.sdiorev,8));
      WIFILogDebug(nil,'CYW43455: dllctl=0x' + inttohex(Network^.dllctl,8));
    end;
    {$ENDIF}

    Result := MMC_STATUS_SUCCESS;

 except
   on e: exception do
     WIFILogError(nil, 'CYW43455: exception in corescan: ' + e.message);
 end;

end;

function cfgreadl(Network: PCYW43455Network; addr: longword; trace: string = ''): longword;
begin
  PLongWord(Network^.DMABuffer)^ := 0;

  Result := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, False, BACKPLANE_FUNCTION, (addr and $1ffff) or $8000, true, Network^.DMABuffer, 0, 4);
  if (Result <> MMC_STATUS_SUCCESS) then
    WIFILogError(nil, 'CYW43455: Failed to read config item 0x'+inttohex(addr, 8) + ' result='+inttostr(Result));

  Result := PLongWord(Network^.DMABuffer)^;
end;

procedure cfgwritel(Network: PCYW43455Network; addr: longword; v: longword; trace: string = '');
var
  Result: longword;
begin
 PLongWord(Network^.DMABuffer)^ := v;

 Result := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, (addr and $1ffff) or $8000, true, Network^.DMABuffer, 0, 4);
 if (Result <> MMC_STATUS_SUCCESS) then
   WIFILogError(nil,'CYW43455: Failed to update config item 0x'+inttohex(addr, 8));
end;

procedure cfgw(Network: PCYW43455Network; offset: longword; value: byte);
var
  Result: longword;
begin
  Result := SDIODeviceReadWriteDirect(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, offset, value, nil);
  if (Result <> MMC_STATUS_SUCCESS) then
    WIFILogError(nil, 'CYW43455: Failed to write config item 0x'+inttohex(offset, 8));
end;

function cfgr(Network: PCYW43455Network; offset: longword): byte;
var
  Res: longword;
  value: byte;
begin
  Res := SDIODeviceReadWriteDirect(Network^.Func1^.MMC, False, BACKPLANE_FUNCTION, offset, 0, @value);
  if (Res <> MMC_STATUS_SUCCESS) then
    WIFILogError(nil, 'CYW43455: Failed to read config item 0x'+inttohex(offset, 8));

  Result := value;
end;

procedure sbdisable(Network: PCYW43455Network; regs: longword; pre: word; ioctl: word);
begin
 try
  WIFIDeviceSetBackplaneWindow(Network,  regs);

  if ((cfgreadl(Network, regs + Resetctrl) and 1) <> 0) then
  begin
    cfgwritel(Network, regs + Ioctrl, 3 or ioctl);
    cfgreadl(Network, regs + Ioctrl);
    exit;
  end;

  cfgwritel(Network, regs + Ioctrl, 3 or pre);
  cfgreadl(Network, regs + Ioctrl);
  cfgwritel(Network, regs + Resetctrl, 1);

  MicrosecondDelay(10);

  while((cfgreadl(Network, regs + Resetctrl) and 1) = 0) do
    begin
      MicrosecondDelay(10);
    end;

  cfgwritel(Network, regs + Ioctrl, 3 or ioctl);
  cfgreadl(Network, regs + Ioctrl);
 except
   on e: exception do
     WIFILogError(nil, 'CYW43455: exception in sbdisable 0x' + PtrToHex(exceptaddr));
 end;
end;

procedure sbmem(Network: PCYW43455Network; write: boolean; buf: pointer; len: longword; off: longword);
var
  n: longword;
  addr: longword;
  Res: longword;
begin
  n := (((off)+(Sbwsize)-1) div (Sbwsize) * (Sbwsize)) - off;
  if (n = 0) then
    n := Sbwsize;

  {$ifdef CYW43455_SDIO_DEBUG}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sbmem len='+inttostr(len) + ' n=' + inttostr(n) + ' offset=0x'+inttohex(off,8) + ' off&(sbwsize-1)=0x' + inttohex(off and (Sbwsize-1), 8));
  {$endif}

  while (len > 0) do
  begin
    if (n > len) then
      n := len;

    WIFIDeviceSetBackplaneWindow(Network, off);
    addr := off and (sbwsize-1);

    if (len >= 4) then
      addr := addr or $8000;

    if (n < Network^.Func1^.MaxBlockSize) then
      Res := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, addr, true, buf, 0, n)
    else
    begin
      Res := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, addr, true, buf, n div Network^.Func1^.MaxBlockSize, Network^.Func1^.MaxBlockSize);
      n := (n div Network^.Func1^.MaxBlockSize) * Network^.Func1^.MaxBlockSize;
    end;

    if (Res <> MMC_STATUS_SUCCESS) then
    begin
      WIFILogError(nil, 'CYW43455: Error transferring to/from backplane 0x' + inttohex(addr,8) + ' ' + inttostr(n) + 'bytes (write='+booltostr(write, true)+')');
    end;

    off += n;
    buf += n;
    len -= n;
    n := Sbwsize;
  end;
end;

procedure sbreset(Network: PCYW43455Network; regs: longword; pre: word; ioctl: word);

begin
 sbdisable(Network, regs, pre, ioctl);
 WIFIDeviceSetBackplaneWindow(Network, regs);
 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sbreset entry regs 0x' + inttohex(regs, 8) + ' regs+ioctrl val 0x'
                   + inttohex(cfgreadl(Network, regs + IOCtrl), 8)
                   + ' regs+resetctrl val 0x ' + inttohex(cfgreadl(Network, regs + Resetctrl), 8));
 {$ENDIF}

  while ((cfgreadl(Network, regs + Resetctrl) and 1) <> 0) do
  begin
    cfgwritel(Network, regs + Resetctrl, 0);
    MicrosecondDelay(40);
  end;

  cfgwritel(Network, regs + Ioctrl, 1 or ioctl);
  cfgreadl(Network, regs + Ioctrl);

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sbreset exit regs+ioctrl val 0x' + inttohex(cfgreadl(Network, regs + IOCtrl), 8)
                    + ' regs+resetctrl val 0x ' + inttohex(cfgreadl(Network, regs + Resetctrl), 8));
  {$ENDIF}
end;

procedure WIFIDeviceRamScan(Network: PCYW43455Network);
var
 n, size: longword;
 r: longword;
 banks, i: longword;
begin
  if (Network^.armcore = ARMcr4) then
  begin
    r := Network^.armregs;
    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: ramscan armcr4 0x' + inttohex(r, 8));
    {$ENDIF}

    WIFIDeviceSetBackplaneWindow(Network, r);
    r := (r and $7fff) or $8000;
    n := cfgreadl(Network, r + Cr4Cap);

    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: cr4 banks 0x' + inttohex(n, 8));
    {$ENDIF}

    banks := ((n shr 4) and $F) + (n and $F);
    size := 0;

    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: banks='+inttostr(banks));
    {$ENDIF}

    for i := 0 to banks - 1 do
    begin
       cfgwritel(Network, r + Cr4Bankidx, i);
       n := cfgreadl(Network, r + Cr4Bankinfo);
       {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
       {$ENDIF}

       size += 8192 * ((n and $3F) + 1);
    end;
    Network^.socramsize := size;
    Network^.rambase := $198000;
    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Socram size=0x'+inttohex(size, 8) + ' rambase=0x'+inttohex(Network^.rambase, 8));
    {$ENDIF}

    exit;
  end;

  sbreset(Network, Network^.socramctl, 0, 0);
  r := Network^.socramregs;
  WIFIDeviceSetBackplaneWindow(Network, r);
  n := cfgreadl(Network, r + Coreinfo);

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil ,'CYW43455: socramrev ' + inttostr(Network^.socramrev) + ' coreinfo 0x' + inttohex(n, 8));
  {$ENDIF}

  banks := (n>>4) and $F;
  size := 0;
  for i := 0 to banks-1 do
  begin
    cfgwritel(Network, r + Bankidx, i);
    n := cfgreadl(Network, r + Bankinfo);
    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: bank ' + inttostr(i) + ' reg 0x' + inttohex(n, 2) + ' size 0x' + inttohex(8192 * ((n and $3F) + 1), 8));
    {$ENDIF}

    size += 8192 * ((n and $3F) + 1);
  end;
  Network^.socramsize := size;
  Network^.rambase := 0;
  if(Network^.chipid = 43430) then
  begin
    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: updating bankidx values for 43430');
    {$ENDIF}

    cfgwritel(Network, r + Bankidx, 3);
    cfgwritel(Network, r + Bankpda, 0);
  end;

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Socram size=0x'+inttohex(size, 8) + ' rambase=0x'+inttohex(Network^.rambase, 8));
  {$ENDIF}
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

function condense(buf: pchar; n: integer): integer;
var
 p, ep, lp, op: pchar;
 c: char;
 skipping: boolean;
 // i: integer;
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
      '#': skipping := true;
      #0,
      #10: begin
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

procedure put4(var p: byte4; v: longword);
begin
  p[1] := byte(v and $ff);
  p[2] := byte(v >> 8);
  p[3] := byte(v >> 16);
  p[4] := byte(v >> 24);
end;

procedure put4_2(p: pbyte; v: longword);
begin
  p^ := byte(v and $ff);
  (p+1)^ := byte(v >> 8);
  (p+2)^ := byte(v >> 16);
  (p+3)^ := byte(v >> 24);
end;

procedure put2(p: pbyte; v: word);
begin
  p^ := v and $ff;
  (p+1)^ := (v shr 8) and $ff;
end;

function WIFIDeviceDownloadFirmware(Network: PCYW43455Network): Longword;

var
 firmwarep: pbyte;
 off: longword;
 fsize: LongInt;
 i: integer;
 lastramvalue: longword;
 chunksize: LongInt;
 bytesleft: longword;
 bytestransferred: LongInt;
 Found: boolean;
 ConfigFilename: string;
 FirmwareFilename: string;
 bytebuf: array[1..4] of byte;

 Handle: THandle;
 Status: LongWord;
begin
 try
  Result := MMC_STATUS_INVALID_PARAMETER;

  // zero out an address of some sort which is at the top of the ram?
  lastramvalue := 0;
  WIFIDeviceSetBackplaneWindow(Network, Network^.rambase + Network^.socramsize - 4);
  SDIODeviceReadWriteExtended(Network^.Func1^.MMC, True, BACKPLANE_FUNCTION, (Network^.rambase + Network^.socramsize - 4) and $7fff{ or $8000}, true, @lastramvalue, 0, 4);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Starting WIFI firmware load...');

  // locate firmware detils based on chip id and revision

  Found := false;

  for i := 1 to FIRWMARE_OPTIONS_COUNT do
  begin
    if (firmware[i].chipid = Network^.chipid) and (firmware[i].chipidrev = Network^.chipidrev) then
    begin
      FirmwareFilename := firmware[i].firmwarefilename;
      ConfigFilename := firmware[i].configfilename;
      Found := true;
      break;
    end;
  end;

  if (not Found) then
  begin
    WIFILogError(nil, 'CYW43455: Unable to find a suitable firmware file to load for chip id 0x' + inttohex(Network^.chipid, 4) + ' revision 0x' + inttohex(Network^.chipidrev, 4));
    exit;
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Using ' + FirmwareFilename + ' for firmware.');

  // Open firmware file
  Status := DeviceFirmwareOpen(DEVICE_CLASS_ANY, FirmwareFilename, CYW43455_FIRMWARE_TIMEOUT, Handle);
  if Status <> ERROR_SUCCESS then
  begin
    if Status <> ERROR_NOT_READY then
    begin
      Result := MMC_STATUS_DEVICE_UNSUPPORTED;
      Exit;
    end
    else
    begin
      Result := MMC_STATUS_NOT_READY;
      Exit;
    end;
  end;

  fsize := DeviceFirmwareSize(Handle);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Size of firmware file is ' + inttostr(fsize) + ' bytes');

  firmwarep := DMABufferAllocate(DMAHostGetDefault, FIRMWARE_CHUNK_SIZE);

  if not(DMA_CACHE_COHERENT) then
   begin
    // Clean Cache (Dest)
    CleanDataCacheRange(PtrUInt(firmwarep), FIRMWARE_CHUNK_SIZE);
   end;

  // Upload File
  off := 0;
  bytestransferred := 0;

  while bytestransferred < fsize do
  begin
    // Read Next Block
    chunksize := DeviceFirmwareRead(Handle, firmwarep, FIRMWARE_CHUNK_SIZE);
    if chunksize <= 0 then
      Break;

    if off = 0 then
    begin
      // Copy the reset vector from the first 4 bytes of the firmware.
      // Not needed on a Pi Zero as the firmware loads at addres 0 by default - needs updating to suit.
      move(firmwarep^, Network^.resetvec, 4);

      {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Reset vector of 0x' + inttohex(Network^.resetvec, 8) + ' copied out of firmware');
      {$ENDIF}
    end;

    // Zero Pad Unaligned Bytes
    while (chunksize and 3) <> 0 do
     begin
      firmwarep[chunksize] := 0;
      Inc(chunksize);
     end;

    // Upload Block
    sbmem(Network, true, firmwarep, chunksize, Network^.rambase + off);
    Inc(bytestransferred, chunksize);

    Inc(off, chunksize);
  end;

  DMABufferRelease(firmwarep);

  // Close firmware file
  DeviceFirmwareClose(Handle);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Finished transferring firmware file to socram');

  // now we need to upload the configuration to ram
  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Using ' + ConfigFilename + ' for config.');

  // Open configuration file
  Status := DeviceFirmwareOpen(DEVICE_CLASS_ANY, ConfigFilename, CYW43455_FIRMWARE_TIMEOUT, Handle);
  if Status <> ERROR_SUCCESS then
  begin
    if Status <> ERROR_NOT_READY then
    begin
      Result := MMC_STATUS_DEVICE_UNSUPPORTED;
      Exit;
    end
    else
    begin
      Result := MMC_STATUS_NOT_READY;
      Exit;
    end;
  end;

  FSize := DeviceFirmwareSize(Handle);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Size of config file is ' + inttostr(fsize) + ' bytes');

  // dword align to be sure
  if (fsize mod 4 <> 0) then
    fsize := ((fsize div 4) + 1) * 4;

  firmwarep := DMABufferAllocate(DMAHostGetDefault, fsize);

  if not(DMA_CACHE_COHERENT) then
   begin
    // Clean Cache (Dest)
    CleanDataCacheRange(PtrUInt(firmwarep), fsize);
   end;

  // Read configuration file
  DeviceFirmwareRead(Handle, firmwarep, DeviceFirmwareSize(Handle));

  fsize := Condense(PChar(FirmwareP), DeviceFirmwareSize(Handle)); // note we deliberately *don't* use fsize here!

  // Close configuration file
  DeviceFirmwareClose(Handle);

  // Although what we've done here is correct, I noticed that ether4330.c only
  // reads the first 2048 bytes of the config which it then condenses, resulting
  // in a config string of 1720 bytes which misses off the last few items and
  // truncates one of the assigned values.
  // This just looks like a simple bug - on a Pi3B the file is about 2074 bytes
  // and perhaps in the past it was smaller so would fit in the 2048 byte read.

  off := Network^.rambase + Network^.socramsize - fsize - 4;

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Tansferring config file to socram at offset 0x' + inttohex(off, 8));
  {$ENDIF}

  bytestransferred := 0;

  if (fsize > FIRMWARE_CHUNK_SIZE) then
    chunksize := FIRMWARE_CHUNK_SIZE
  else
    chunksize := fsize;

  while bytestransferred < fsize do
  begin
    sbmem(Network, true, firmwarep+bytestransferred, chunksize, off);
    bytestransferred := bytestransferred + chunksize;

    off += chunksize;
    bytesleft := fsize - bytestransferred;
    if (bytesleft < chunksize) then
      chunksize := bytesleft;
  end;

  DMABufferRelease(firmwarep);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Finished transferring config file to socram');

  // I believe this is some sort of checksum
  fsize := fsize div 4;
  fsize := (fsize and $ffff) or ((not fsize) << 16);

  // write checksum thingy to ram.

  put4(bytebuf, fsize);
  sbmem(Network, true, @bytebuf[1], 4, Network^.rambase + Network^.socramsize - 4);

  // I think this brings the arm core back up after writing the firmware.
  if (Network^.armcore = ARMcr4) then
  begin
     WIFIDeviceSetBackplaneWindow(Network, Network^.sdregs);
     cfgwritel(Network, Network^.sdregs + IntStatus, $ffffffff);
     // write reset vector to bottom of RAM
     if (Network^.resetvec <> 0) then
     begin
       {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Firmware upload: Writing reset vector to address 0');
       {$ENDIF}

       sbmem(Network, true, @Network^.resetvec, sizeof(Network^.resetvec), 0);
     end;

     // reactivate the core.
     sbreset(Network, Network^.armctl, Cr4Cpuhalt, 0);
  end
  else
     sbreset(Network, Network^.armctl, 0, 0);
 except
   on e: exception do
     WIFILogError(nil, 'CYW43455: exception: ' + e.message + ' at address ' + PtrToHex(exceptaddr));
 end;
end;

procedure sbenable(Network: PCYW43455Network);
var
  i: integer;
  mbox: longword;
  ints: longword;
  Status: LongWord;
begin
  WIFIDeviceSetBackplaneWindow(Network, BAK_BASE_ADDR);
  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Enabling high throughput clock...');
  cfgw(Network, BAK_CHIP_CLOCK_CSR_REG, 0);
  sleep(1);
  cfgw(Network, BAK_CHIP_CLOCK_CSR_REG, ReqHT);

  // wait for HT clock to become available. 100ms timeout approx
  i := 0;
  while ((cfgr(Network, BAK_CHIP_CLOCK_CSR_REG) and HTavail) = 0) do
  begin
    i += 1;
    if (i = 100) then
    begin
      WIFILogError(nil, 'CYW43455: Could not enable HT clock; csr=' + inttohex(cfgr(Network, BAK_CHIP_CLOCK_CSR_REG), 8));
      exit;
    end;

    Sleep(1);
  end;

  cfgw(Network, BAK_CHIP_CLOCK_CSR_REG, cfgr(Network, BAK_CHIP_CLOCK_CSR_REG) or ForceHT);
  sleep(10);

  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: After request for HT clock, CSR_REG=0x' + inttohex(cfgr(Network, BAK_CHIP_CLOCK_CSR_REG), 4));
  {$ENDIF}

  WIFIDeviceSetBackplaneWindow(Network, Network^.sdregs);

  cfgwritel(Network, Network^.sdregs + Sbmboxdata, 4 shl 16);   // set protocol version
  cfgwritel(Network, Network^.sdregs + Intmask, FrameInt or MailboxInt or Fcchange);

  // enable function 2
  Status := SDIOFunctionEnable(Network^.Func2);
  if (Status <> MMC_STATUS_SUCCESS) then
  begin
    WIFILogError(nil, 'CYW43455: io enable could not be set for function 2');

    Exit;
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Radio function (f2) successfully enabled');

  // enable interrupts.
  SDIODeviceReadWriteDirect(Network^.Func1^.MMC,True, BUS_FUNCTION, SDIO_CCCR_IENx, (INTR_CTL_MASTER_EN or INTR_CTL_FUNC1_EN or INTR_CTL_FUNC2_EN), nil );

  ints := 0;
  while (ints = 0) do
  begin
    ints := cfgreadl(Network, Network^.sdregs + Intstatus);
    cfgwritel(Network, Network^.sdregs + Intstatus, ints);

    if ((ints and mailboxint) > 0) then
    begin
      mbox := cfgreadl(Network, Network^.sdregs + Hostmboxdata);
      cfgwritel(Network, Network^.sdregs + Sbmbox, 2);	//ack
      if ((mbox and $8) = $8) then
      begin
         if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: The Broadcom firmware reports it is ready!')
      end
      else
        WIFILogError(nil, 'CYW43455: The firmware is not ready! mbox=0x'+inttohex(mbox, 8));
    end
    else
      WIFILogError(nil, 'CYW43455: Mailbox interrupt was not set as expected ints=0x'+inttohex(ints, 8));
  end;

  // It seems like we need to execute a read first to kick things off. If we don't do this the first
  // IOCTL command response will be an empty one rather than the one for the IOCTL we sent.
  if (SDIODeviceReadWriteExtended(Network^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, Network^.RXBuffer, 0, 64) <> MMC_STATUS_SUCCESS) then
     WIFILogError(nil, 'CYW43455: Unsuccessful initial read from WIFI function 2 (packets)')
  else
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Successfully read function 2 first empty response.');
   {$ENDIF}
  end;


  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: WIFI Device Enabled');
end;

function WirelessIOCTLCommand(Network: PCYW43455Network; cmd: integer;
                                   InputP: Pointer;
                                   InputLen: Longword;
                                   write: boolean; ResponseDataP: Pointer;
                                   ResponseDataLen: integer;
                                   trace: string = ''): longword;

var
  msgp: PIOCTL_MSG;
  responseP: PIOCTL_MSG;
  cmdp: IOCTL_CMDP;
  TransmitDataLen: longword;
  HeaderLen: longword;
  TransmitLen: longword;
  Res: longword;
  WorkerRequestP: PWIFIRequestItem;
  status: longword;

  Lock: Boolean;
begin
  Result := MMC_STATUS_INVALID_PARAMETER;

  // Check lock already owned
  Lock := MutexOwner(Network^.Network.Lock) <> ThreadGetCurrent;

  // Acquire lock (Conditional)
  if Lock then
    status := MutexLock(Network^.Network.Lock)
  else
    status := ERROR_SUCCESS;
  if status = ERROR_SUCCESS then
  begin
    try

      msgp := Network^.TXBuffer;

      if Network^.txglom then
        cmdp := @(msgp^.glom_cmd.cmd)
      else
        cmdp := @(msgp^.cmd);

      {$ifdef CYW43455_SDIO_DEBUG}
      if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: wirelessioctlcmd write='+booltostr(write, true)
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
      // a pointer to TXBuffer. therefore the address are from the same instance.
      HeaderLen := PtrUInt(@cmdp^.data) - PtrUInt(msgp);
      TransmitLen := ((HeaderLen + TransmitDataLen + 3) div 4) * 4;

        // Prepare IOCTL command
      fillchar(msgp^, sizeof(IOCTL_MSG), 0);

      msgp^.len := HeaderLen + TransmitDataLen;
      msgp^.notlen := not msgp^.len;

      if (Network^.txglom) then
      begin
        msgp^.glom_cmd.glom_hdr.len := HeaderLen + TransmitDataLen - 4;
        msgp^.glom_cmd.glom_hdr.flags := 1;
      end;

      cmdp^.sdpcmheader.seq := Network^.txseq;
      if (Network^.txseq < 255) then
        Network^.txseq += 1
      else
        Network^.txseq := 0;

      if (Network^.txglom) then
        cmdp^.sdpcmheader.hdrlen := 20
      else
        cmdp^.sdpcmheader.hdrlen := 12;

      cmdp^.cmd := cmd;
      cmdp^.outlen := TransmitDataLen;

      // request id is a word, so need to stay within limits.
      if (Network^.ioctl_reqid > $fffe) then
        Network^.ioctl_reqid := 1
      else
        Network^.ioctl_reqid := Network^.ioctl_reqid + 1;

      if (write) then
        cmdp^.flags := (Network^.ioctl_reqid << 16) or 2
      else
        cmdp^.flags := (Network^.ioctl_reqid << 16);

      if (InputLen > 0) then
      begin
        move(InputP^, cmdp^.data[0], InputLen);
      end;

      if (write) then
        move(ResponseDataP^, PByte(@(cmdp^.data[0])+InputLen)^, ResponseDataLen);

      // Signal to the worker thread that we need a response for this request.
      WorkerRequestP := Network^.WorkerThread.AddRequest(Network^.ioctl_reqid, [], nil, nil);

      // Send IOCTL command.
      // Is it safe to submit multiple ioctl commands and then see the events come through
      // out of order? I think so but needs testing and investigating.
      // requests are safe because the sdio read write functions have a spinlock.

      {$ifdef CYW43455_SDIO_DEBUG}
      if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: sending ' + inttostr(TransmitLen) + ' bytes to the wifi device');
      {$endif}

      Res := SDIODeviceReadWriteExtended(Network^.Func1^.MMC, True, WLAN_FUNCTION, BAK_BASE_ADDR and $1FFFF{ SB_32BIT_WIN}, false, msgp, 0, TransmitLen);
      if (Res <> MMC_STATUS_SUCCESS) then
        WIFILogError(nil, 'CYW43455: failed to send cmd53 for ioctl command ' + inttostr(res));

      // Release lock (Always)
      MutexUnlock(Network^.Network.Lock);

      // wait for the worker thread to process the response.
      status := SemaphoreWaitEx(WorkerRequestP^.Signal, 10000);
      if (status = ERROR_WAIT_TIMEOUT) then
      begin
        Network^.WorkerThread.DoneWithRequest(WorkerRequestP);

        Lock := False;
        exit;
      end;

      // Reacquire lock (Always)
      status := MutexLock(Network^.Network.Lock);
      if status <> ERROR_SUCCESS then
      begin
        Network^.WorkerThread.DoneWithRequest(WorkerRequestP);

        Lock := False;
        exit;
      end;

      // use old variable for now so copy paste from old code works still
      ResponseP := WorkerRequestP^.MsgP;
      if (ResponseP = nil) then
      begin
        WIFILogError(nil, 'CYW43455: Nil response item in WirelessIOCTLCommand');
        exit;
      end;

      // Now we have the response we can validate it.
      if ((responseP^.cmd.sdpcmheader.chan and $f) <> 0) then
        WIFILogError(nil, 'CYW43455: IOCTL response received for a non-zero channel');

      if (((ResponseP^.cmd.flags >> 16) and $ffff) <> WorkerRequestP^.RequestID) then
        WIFILogError(nil, 'CYW43455: IOCTL response received for a different request id. We got one for '
                          + inttostr(responsep^.cmd.flags >> 16)
                          + ' whereas our request was for '
                          + inttostr(workerrequestp^.RequestID));

      // in cases where the response is smaller than the command parameters we have to move less data.
      // need to verify this as I had some weird if statement in there before.
      move(ResponseP^.cmd.Data[0], ResponseDataP^, ResponseDataLen);

      Network^.WorkerThread.DoneWithRequest(WorkerRequestP);

      Result := Res;
    finally
      // Release lock (Conditional)
      if Lock then MutexUnlock(Network^.Network.Lock);
    end
  end
end;

function WirelessGetVar(Network: PCYW43455Network; varname: string; ValueP: PByte; len: integer): longword;
begin
  // getvar name must have a null on the end of it.
  varname := varname + #0;

  Result := WirelessIOCTLCommand(Network, WLC_GET_VAR, @varname[1], length(varname), false, ValueP, len);
end;

function WirelessSetVar(Network: PCYW43455Network; varname: string; InputValueP: PByte; Inputlen: integer; trace: string = ''): longword;
begin
  varname := varname + #0;
  Result := WirelessIOCTLCommand(Network, WLC_SET_VAR, @varname[1], length(varname), true, InputValueP, Inputlen, trace);
end;

function WirelessSetInt(Network: PCYW43455Network; varname: string; Value: longword): longword;
begin
  Result := WirelessSetVar(Network, varname, @Value, 4);
end;

function WirelessCommandInt(Network: PCYW43455Network; wlccmd: longword; Value: longword): longword;
var
  response: byte4;
begin
  Result := WirelessIOCTLCommand(Network, wlccmd, @Value, 4, true, @response[1], 4);
end;

function WIFIDeviceUploadRegulatoryFile(Network: PCYW43455Network): longword;
const
  Reguhdr = 2+2+4+4;
  Regusz = 400; //To Do //TestingSDIO // Linux driver uses 1400, need to modify WirelessIOCTLCommand to check for blocks/bytes ? // And modify WirelessIOCTLCommand to use a DMA buffer
  Regutyp = 2;
  Flagclm = 1 shl 12;
  Firstpkt = 1 shl 1;
  Lastpkt = 1 shl 2;

var
 //FirmwareFile: file of byte;
 firmwarep: pbyte;
 off: longword;
 fsize: LongInt;
 i: integer;
 chunksize: LongInt;
 Found: boolean;
 RegulatoryFilename: string;
 flag: word;

 Handle: THandle;
 Status: LongWord;

begin
  Result := MMC_STATUS_SUCCESS;

  // locate regulatory detils based on chip id and revision

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Starting to upload regulatory file');
  Found := false;

  for i := 1 to FIRWMARE_OPTIONS_COUNT do
  begin
    if (firmware[i].chipid = Network^.chipid) and (firmware[i].chipidrev = Network^.chipidrev) then
    begin
      // If no regulatory file just succeed
      if Length(firmware[i].regufilename) = 0 then
      begin
       Result := MMC_STATUS_SUCCESS;
       Exit;
      end;

      RegulatoryFilename := firmware[i].regufilename;
      Found := true;
      break;
    end;
  end;

  if (not Found) then
  begin
    Result := MMC_STATUS_INVALID_PARAMETER;
    WIFILogError(nil, 'CYW43455: Unable to find a suitable firmware file to load for chip id 0x' + inttohex(Network^.chipid, 4) + ' revision 0x' + inttohex(Network^.chipidrev, 4));
    exit;
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Using ' + RegulatoryFilename + ' for regulatory file.');

  // Open regulatory file
  Status := DeviceFirmwareOpen(DEVICE_CLASS_ANY, RegulatoryFilename, CYW43455_FIRMWARE_TIMEOUT, Handle);
  if Status <> ERROR_SUCCESS then
  begin
    if Status <> ERROR_NOT_READY then
    begin
      Result := MMC_STATUS_DEVICE_UNSUPPORTED;
      Exit;
    end
    else
    begin
      Result := MMC_STATUS_NOT_READY;
      Exit;
    end;
  end;

  FSize := DeviceFirmwareSize(Handle);

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Size of regulatory file is ' + inttostr(fsize) + ' bytes');

  firmwarep := DMABufferAllocate(DMAHostGetDefault, Reguhdr + Regusz + 1);

  if not(DMA_CACHE_COHERENT) then
   begin
    // Clean Cache (Dest)
    CleanDataCacheRange(PtrUInt(firmwarep), Reguhdr + Regusz + 1);
   end;

  // Setup header
  put2(firmwarep + 2, Regutyp);
  put2(firmwarep + 8, 0);
  off := 0;
  flag := Flagclm or Firstpkt;
  chunksize := 0;

  try
    while ((flag and Lastpkt) = 0) do
    begin
      // Read Next Block
      chunksize := DeviceFirmwareRead(Handle, firmwarep + Reguhdr, Regusz);
      if chunksize <= 0 then
        Break;

      if (chunksize <> Regusz) then
      begin
        // fill out end of the block with zeroes.
        while ((chunksize and 7) > 0) do
        begin
          (firmwarep + Reguhdr + chunksize)^ := 0;
          chunksize += 1;
        end;
        flag := flag or Lastpkt;
      end;

      // Update header
      put2(firmwarep + 0, flag);
      put4_2(firmwarep + 4, chunksize);

      Result := WirelessSetVar(Network, 'clmload', firmwarep, Reguhdr + chunksize);
      if (Result <> MMC_STATUS_SUCCESS) then
        exit;

      off += chunksize;
      flag := flag and (not Firstpkt);
    end;

  finally
    DMABufferRelease(firmwarep);

    // Close regulatory file
    DeviceFirmwareClose(Handle);
  end;

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Finished transferring regulatory file');
end;

function whd_tlv_find_tlv8(message: pbyte; message_length: longword; atype: byte): pwhd_tlv8_data;
var
  current_tlv_type: byte;
  current_tlv_length: byte;
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

procedure WirelessScanCallback(Event: TWIFIEvent;
            EventRecordP: pwhd_event;
            RequestItemP: PWIFIRequestItem;
            DataLength: longint);
var
  ScanResultP: pwl_escan_result;
  ssidstr, s: string;
begin
  if (Event = WLC_E_ESCAN_RESULT) then
  begin
    scanresultp := pwl_escan_result(pbyte(PtrUInt(@eventrecordp^.whd_event) + sizeof(whd_event_msg)));
    ssidstr := buftostr(@scanresultp^.bss_info[1].SSID[0], scanresultp^.bss_info[1].SSID_len, true);
    if (ssidstr = '') then
      ssidstr := '<hidden>';

    s := 'SSID='+ssidstr
                  + ' event status=' + inttostr(eventrecordp^.whd_event.status)
                  + ' buflen = '+inttostr(scanresultp^.buflen)
                  + ' channel = ' +inttostr(scanresultp^.bss_info[1].chanspec and $ff)
                  + ' chanspec = ' +inttohex(scanresultp^.bss_info[1].chanspec, 8)
                  + ' BSSID = ' +inttohex(scanresultp^.bss_info[1].BSSID[0], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID[1], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID[2], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID[3], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID[4], 2) + ':'
                  + inttohex(scanresultp^.bss_info[1].BSSID[5], 2);

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
    WIFILogError(nil, 'CYW43455: Wireless Scan: Unexpected event received');

end;

procedure WirelessScan(UserCallback: TWIFIScanUserCallback; WaitTime: Longint = 10000); cdecl;

var
  Network: PCYW43455Network;
  scanparams: wl_escan_params;
  i: integer;
  RequestItemP: PWIFIRequestItem;

begin
  // scan for wifi networks
  // passive scan - listens for beacons only.

  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Starting wireless network scan');

  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
  if Network = nil then Exit;

  WirelessCommandInt(Network, $b9, $28); // scan channel time
  WirelessCommandInt(Network, $bb, $28); // scan unassoc time
  WirelessCommandInt(Network, $102, $82); // passive scan time

  WIFIDeviceSetBackplaneWindow(Network, Network^.sdregs);
  cfgwritel(Network, Network^.sdregs + IntStatus, 0);

  WirelessCommandInt(Network, 49, 0);	// PASSIVE_SCAN */
  WirelessCommandInt(Network, 2, 0); // up (command has no parameters)

  // clear scan parameters.
  fillchar(scanparams, sizeof(scanparams), 0);

  // now setup what we need.
  scanparams.version := 1;
  scanparams.action := 1;   // start
  scanparams.sync_id:=NetSwapWord($1234);
  scanparams.params.scan_type := SCAN_TYPE_PASSIVE;
  scanparams.params.bss_type := BSS_TYPE_ANY;
  fillchar(scanparams.params.bssid[0], sizeof(THardwareAddress), $ff);  // broadcast address
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
  RequestItemP := Network^.WorkerThread.AddRequest(0, [WLC_E_ESCAN_RESULT], @WirelessScanCallback, UserCallback);

  // it's lights out and away we go!
  WirelessSetVar(Network, 'escan', @scanparams, sizeof(scanparams));

  // wait 10 seconds (this semaphmore won't actually be signalled although it
  // could be in the future if we were scanning for a specific network name).
  SemaphoreWaitEx(RequestItemP^.Signal, WaitTime);

  Network^.WorkerThread.DoneWithRequest(RequestItemP);
end;

procedure JoinCallback(Event: TWIFIEvent; EventRecordP: pwhd_event; RequestItemP: PWIFIRequestItem; DataLength: Longint);
begin
  if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: wireless join callback: removing an event ' + inttostr(ord(event)) + ' from the list (status='+inttostr(eventrecordp^.whd_event.status)+')');

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

function WirelessJoinNetwork(JoinType: TWIFIJoinType;
                             timeout: integer = 10000): longword; cdecl;
var
  Network: PCYW43455Network;
begin
  Result := ERROR_INVALID_DATA;

  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
  if Network = nil then Exit;

  Result := ERROR_SUCCESS;

  WIFI_USE_SUPPLICANT := true;

  //the wpa_supplicant must now be initialized, once we have a mac address (which is done in wirelessinit)
  //the supplicant runs its own event loop, so it runs in a thread to allow this.
  if (Network^.WPASupplicantThread = nil) then
    Network^.WPASupplicantThread := TWPASupplicantThread.Create(Network);

  if (Network^.WPASupplicantThread <> nil) then
    Network^.WPASupplicantThread.Start;

  if (JoinType = WIFIJoinBlocking) then
    Result := SemaphoreWaitEx(PCYW43455Network(Network)^.NetworkUpSignal, timeout);
end;

function FirmwareWirelessJoinNetwork(ssid: string; security_key: string;
                             countrycode: string;
                             JoinType: TWIFIJoinType;
                             ReconnectType: TWIFIReconnectionType;
                             bssid: THardwareAddress;
                             usebssid: boolean = false): longword; cdecl;
var
  Network: PCYW43455Network;
  data: array[0..1] of longword;
  psk: wsec_pmk;
  responseval: longword;
  auth_mfp: longword;
  wpa_auth: longword;
  simplessid: wlc_ssid;
  RequestEntryP: PWIFIRequestItem;
  countrysettings: countryparams;
  clen: integer;

begin
  (*
   Assumptions:
     the access point exists
     it supports WPA2 security
  *)

  Result := MMC_STATUS_INVALID_DATA;

  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
  if Network = nil then Exit;

  WIFI_USE_SUPPLICANT := false;

  // pass connection details to the retry thread for handling loss of connection.
  Network^.BackgroundJoinThread.SetConnectionDetails(SSID, security_key, countrycode, BSSID, UseBSSID, ReconnectType);

  if (JoinType = WIFIJoinBackground) then
  begin
    // set network status down, schedule the join thread to do a background join
    // (it will call this function in blocking form to achieve that)
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Background join scheduled.');
   {$ENDIF}

    // Set join not completed
    Network^.JoinCompleted := False;

    {Set Status to Down}
    Network^.Network.NetworkStatus := NETWORK_STATUS_DOWN;

    SemaphoreSignal(Network^.BackgroundJoinThread.FConnectionLost);
    Result := MMC_STATUS_SUCCESS;
    exit;
  end;

  // set country code
  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Setting country code');
  {$ENDIF}

  clen := length(countrycode);
  fillchar(countrysettings, 0, sizeof(countrysettings));
  move(countrycode[1], countrysettings.country_ie[1], clen);
  move(countrycode[1], countrysettings.country_code[1], clen);
  countrysettings.revision := -1;

  Result := WirelessSetVar(Network, 'country', @countrysettings, sizeof(countrysettings));
  if Result <> MMC_STATUS_SUCCESS then
    exit;

  // Set infrastructure mode
  // 1 = normal, 0 = access point
  Result := WirelessCommandInt(Network, WLC_SET_INFRA, 1);
  if (Result <> MMC_STATUS_SUCCESS) then
    exit;

  Result := WirelessCommandInt(Network, WLC_SET_AUTH, WL_AUTH_OPEN_SYSTEM);
  if (Result <> MMC_STATUS_SUCCESS) then
    exit;

  // whilst it is not recommended, the driver will connect to an open network
  // if you specify an empty security key. Currently the pi400 and Pi Zero 2 W
  // both require a software wpa_supplicant, and therefore until this is completed
  // they cannot connect to a wifi network unless it is unencrypted.

  if (security_key <> '') then
  begin
    // Set Wireless Security Type
    Result := WirelessCommandInt(Network, WLC_SET_WSEC, AES_ENABLED);
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    data[0] := 0;  // this is the primary interface
    data[1] := 1;  // wpa security on (enables the firmware supplicant)
    Result := WirelessSetVar(Network, 'bsscfg:sup_wpa', @data[0], sizeof(data));
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    // Set the EAPOL version to whatever the AP is using (-1) */
    data[0] := 0;
    data[1] := longword(-1);
    Result := WirelessSetVar(Network, 'bsscfg:sup_wpa2_eapver', @data[0], sizeof(data));
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    // Send WPA Key
    // Set the EAPOL key packet timeout value, otherwise unsuccessful supplicant
    // events aren't reported. If the IOVAR is unsupported then continue.
    data[0] := 0;
    data[1] := DEFAULT_EAPOL_KEY_PACKET_TIMEOUT;
    Result := WirelessSetVar(Network, 'bsscfg:sup_wpa_tmo', @data, sizeof(data));
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    // Set WPA authentication mode
    wpa_auth := WPA2_AUTH_PSK;
    Result := WirelessIOCTLCommand(Network, WLC_SET_WPA_AUTH, @wpa_auth, sizeof(wpa_auth), true, @responseval, 4);
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    fillchar(psk, sizeof(psk), 0);
    move(security_key[1], psk.key[0], length(security_key));
    psk.key_len := length(security_key);
    psk.flags := WSEC_PASSPHRASE;

    // Delay required to allow radio firmware to be ready to receive PMK and avoid intermittent failure
    sleep(10);

    Result := WirelessIOCTLCommand(Network, WLC_SET_WSEC_PMK, @psk, sizeof(psk), true, @responseval, 4);
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;

    auth_mfp := 0;
    Result := WirelessSetVar(Network, 'mfp', @auth_mfp, 4);
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;
  end;

  // if there is a BSSID we set it here. Setting the SSID (mandatory) is what initiates the join.
  if (usebssid) then
  begin
    Result := WirelessIOCTLCommand(Network, WLC_SET_BSSID, @bssid, sizeof(bssid), true, @responseval, 4);
    if (Result <> MMC_STATUS_SUCCESS) then
      exit;
  end;

  // simple join (no joinparams).
  fillchar(simplessid, sizeof(simplessid), 0);
  move(ssid[1], simplessid.SSID[0], length(ssid));
  simplessid.SSID_len:=length(ssid);
  Result := WirelessIOCTLCommand(Network, WLC_SET_SSID, @simplessid, sizeof(simplessid), true, @responseval, 4);
  if (Result <> MMC_STATUS_SUCCESS) then
    exit;

  // register for join events we are interested in.
  // for an open network we don't expect to see the PSK_SUP event.
  if (security_key <> '') then
    RequestEntryP := Network^.WorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK, WLC_E_PSK_SUP], @JoinCallback, nil)
  else
    RequestEntryP := Network^.WorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK], @JoinCallback, nil);

  // wait for 5 seconds or a completion signal.
  SemaphoreWaitEx(RequestEntryP^.Signal, 15000); //7000

  if (RequestEntryP^.RegisteredEvents = []) then
  begin
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Successfully joined WIFI network ' + SSID);

    // Set join completed
    Network^.JoinCompleted := True;

    {Set Status to Up}
    Network^.Network.NetworkStatus := NETWORK_STATUS_UP;

    {Notify the Status}
    NotifierNotify(@Network^.Network.Device, DEVICE_NOTIFICATION_UP);
  end
  else
  begin
    Result := MMC_STATUS_INVALID_PARAMETER;
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: The join attempt to network ' + SSID + ' was unsuccessful');

    if (ReconnectType = WIFIReconnectAlways) then
    begin
      if (Network^.BackgroundJoinThread.Suspended) then
        Network^.BackgroundJoinThread.Start;

      // Set join not completed
      Network^.JoinCompleted := False;

      {Set Status to Down}
      Network^.Network.NetworkStatus := NETWORK_STATUS_DOWN;

      // tell the join thread to make another join attempt.
      SemaphoreSignal(Network^.BackgroundJoinThread.FConnectionLost);
    end;

    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then
    begin
      if (WLC_E_SET_SSID in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'CYW43455: WLC_E_SET_SSID event was not returned from the join request');

      if (WLC_E_LINK in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'CYW43455: WLC_E_LINK event was not returned from the join request');

      if (WLC_E_PSK_SUP in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'CYW43455: WLC_E_PSK_SUP event was not returned from the join request');
    end;
    {$ENDIF}
  end;

  Network^.WorkerThread.DoneWithRequest(RequestEntryP);
end;

function WirelessLeaveNetwork: longword;
var
 Network: PCYW43455Network;
 simplessid: wlc_ssid;
 responseval: longword;

begin
 Result := MMC_STATUS_INVALID_DATA;

 // Get Wireless Device
 Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
 if Network = nil then Exit;

 Result := MMC_STATUS_SUCCESS;

 {disable the automatic reconnection}
// Network^.BackgroundJoinThread.FReconnectionType:=WIFIReconnectNever;

 {blank out the SSID to leave the network?}
 fillchar(simplessid, sizeof(simplessid), 0);
 simplessid.SSID_len:=0;
 Result := WirelessIOCTLCommand(Network, WLC_SET_SSID, @simplessid, sizeof(simplessid), true, @responseval, 4);
(* if (Result <> MMC_STATUS_SUCCESS) then
   exit;*)

 // terminate the supplicant loop if the supplicant is active.
 if (WIFI_USE_SUPPLICANT) then
 begin
    UltiboEloopTerminate;
    if (Network^.WPASupplicantThread <> nil) then
      Network^.WPASupplicantThread.Terminate;

    Network^.WPASupplicantThread := nil;
 end;
end;

function WirelessInit(Network: PCYW43455Network): longword;
const
  WLC_SET_PM = 86;

var
  version: array[1..250] of byte;
  eventmask: array[0..WL_EVENTING_MASK_LEN] of byte;

  procedure DisableEvent(id: integer);
  begin
     EventMask[id div 8] := EventMask[id div 8] and (not (1 << (id mod 8)));
  end;

begin
 Result := MMC_STATUS_INVALID_PARAMETER;

 // set up the event mask
 fillchar(eventmask, sizeof(eventmask), 255);           // turn everything on
 DisableEvent(40);	// E_RADIO
 DisableEvent(44);	// E_PROBREQ_MSG
 DisableEvent(54);	// E_IF
 DisableEvent(71);	// E_PROBRESP_MSG
 DisableEvent(20);	// E_TXFAIL
 DisableEvent(124);	//?

 DisableEvent(25);      // we are using event 25 (EAPOL) via packets, not via events, so we don't want the event generated.
                        // broadcom firmware generates these as well as sending packets; we only need one or the other
                        // and we've decided to use packets (channel 2).

 DisableEvent(51);	// WLC_E_MULTICAST_DECODE_ERROR. We get this during the authentication process for broadcast
                        // packets on the network (e.g. my home automation devices). Ignore.

 Result := WirelessSetVar(Network, 'event_msgs', @eventmask, sizeof(eventmask));
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 // request the mac address of the wifi device
 fillchar(Network^.macaddress[0], HARDWARE_ADDRESS_SIZE, 0);

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Requesting MAC address from the WIFI device...');
 Result := WirelessGetVar(Network, 'cur_etheraddr', @Network^.macaddress[0], HARDWARE_ADDRESS_SIZE);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Current MAC address is ' + HardwareAddressToString(Network^.macaddress,':'));

 // upload regulatory file - can't set country and join a network without this.
 Result := WIFIDeviceUploadRegulatoryFile(Network);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 // do some further initialisation once the firmware is booted.
 Result := WirelessSetInt(Network, 'assoc_listen', 10);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 // powersave
 if (Network^.chipid = 43430) or (Network^.chipid=$4345) then
   Result := WirelessCommandInt(Network, WLC_SET_PM, 0)  // powersave off
 else
   Result := WirelessCommandInt(Network, WLC_SET_PM, 2); // powersave fast
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 {$ifndef notxglom}
 WIFILogInfo(nil, 'CYW43455: Enabling support for receiving glom packets');
 // this is NOT an error. txglom = receive, because the context is access point.
 Result := WirelessSetInt(Network, 'bus:txglom', 1);
// if Result <> MMC_STATUS_SUCCESS then
//  exit;
 {$else}
 Result := WirelessSetInt(Network, 'bus:txglom', 0);
 {$endif}

 Result := WirelessSetInt(Network, 'bus:rxglom', 0);
 // Linux driver says this is allowed to fail
 //if Result <> MMC_STATUS_SUCCESS then
 // exit;

 Result := WirelessSetInt(Network, 'bcn_timeout', 10);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 Result := WirelessSetInt(Network, 'assoc_retry_max', 3);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 // get first 50 chars of the firmware version string
 Result := WirelessGetVar(Network, 'ver', @version[1], 50);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Firmware version string (partial): ' + buftostr(@version[1], 50));

 Result := WirelessSetInt(Network, 'roam_off', 1);
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 Result := WirelessCommandInt(Network, $14, 1); // set infra 1
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 Result := WirelessCommandInt(Network, 10, 0);  // promiscuous
end;

constructor TCYW43455WorkerThread.Create(CreateSuspended: boolean; ANetwork: PCYW43455Network);
begin
  inherited Create(CreateSuspended,THREAD_STACK_DEFAULT_SIZE);
  FNetwork := ANetwork;

  FRequestQueueP := nil;
  FLastRequestQueueP := nil;
  FQueueProtect := CriticalSectionCreate;
end;

destructor TCYW43455WorkerThread.Destroy;
begin
  // free memory allocated to the queue and release any semaphores.
  // do this later - most apps just get turned off on shutdown anyway!

  // release other resources.
  CriticalSectionDestroy(FQueueProtect);
  inherited Destroy;
end;

procedure TCYW43455WorkerThread.dumpqueue;
var
  entryp: PWIFIRequestItem;
  count: integer;
begin
 CriticalSectionLock(FQueueProtect);
 try
   entryp := FRequestQueueP;
   count := 0;
   while (entryp <> nil) do
   begin
     WIFILogInfo(nil, inttostr(count) + ' 0x' + PtrToHex(entryp)
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

function TCYW43455WorkerThread.AddRequest(ARequestID: word; InterestedEvents: TWIFIEventSet; Callback: TWirelessEventCallback; UserDataP: Pointer): PWIFIRequestItem;
var
  ItemP: PWIFIRequestItem;
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

procedure TCYW43455WorkerThread.DoneWithRequest(ARequestItemP: PWIFIRequestItem);
var
  CurP, PrevP: PWIFIRequestItem;
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
      WIFILogError(nil, 'CYW43455: Unable to locate item in the request queue');

  finally
    CriticalSectionUnLock(FQueueProtect);
  end;
end;

function TCYW43455WorkerThread.FindRequest(ARequestId: word): PWIFIRequestItem;
var
  CurP: PWIFIRequestItem;

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

function TCYW43455WorkerThread.FindRequestByEvent(AEvent: longword): PWIFIRequestItem;
var
  CurP: PWIFIRequestItem;
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

procedure TCYW43455WorkerThread.ProcessDevicePacket(var responseP: PIOCTL_MSG;
                             NetworkEntryP: PNetworkEntry;
                             var bytesleft: longword;
                             var isfinished: boolean);
var
 SequenceNumber: word;
 RequestEntryP: PWIFIRequestItem;
 EventRecordP: pwhd_event;
 Framelength: word;
 EtherHeaderP: pether_header;
 k: integer;
 BCDCHeaderP: PBCDC_HEADER;
 Offset: LongWord;

begin
 // the channel has 4 bits flags in the upper nibble and 4 bits channel number in the lower nibble.
 case (responseP^.cmd.sdpcmheader.chan and $f) of
   // ioctl response
   0: begin
        // Now we have a message, we need to check the sequence id to see if there is a
        // thread waiting to be signaled for the response.

        SequenceNumber := (responseP^.cmd.flags >> 16) and $ffff;
        RequestEntryP := FindRequest(SequenceNumber);

        if (RequestEntryP <> nil) then
        begin
          // this isn't very efficient and will need attention later.
         getmem(RequestEntryP^.MsgP, ResponseP^.Len);
         move(ResponseP^, RequestEntryP^.MsgP^, ResponseP^.Len);

         // tell the waiting thread the response is ready.
         SemaphoreSignal(RequestEntryP^.Signal);
        end
        else
          WIFILogError(nil, 'CYW43455: Failed to find a request for sequence number ' + inttostr(sequencenumber));
      end;

   // event
   1: begin
        if (ResponseP^.len <= responsep^.cmd.sdpcmheader.hdrlen) then
          exit;

        // Get BCDC Header (which is after the SDPCM_HEADER)
        BCDCHeaderP := PBCDC_HEADER(PByte(responseP) + responseP^.cmd.sdpcmheader.hdrlen);

        // Get Data Offset
        Offset := responseP^.cmd.sdpcmheader.hdrlen + BCDC_HEADER_SIZE + (BCDCHeaderP^.data_offset shl 2);

        // Get the Event Record
        EventRecordP := pwhd_event(pbyte(responsep) + Offset);

        EventRecordP^.whd_event.status := NetSwapLong(EventRecordP^.whd_event.status);
        EventRecordP^.whd_event.event_type := NetSwapLong(EventRecordP^.whd_event.event_type);
        EventRecordP^.whd_event.reason:= NetSwapLong(EventRecordP^.whd_event.reason);
        EventRecordP^.whd_event.flags := NetSwapWord(EventRecordP^.whd_event.flags);

        if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: event message received ' + inttostr(eventrecordp^.whd_event.event_type)
            + ' status='+inttostr(eventrecordp^.whd_event.status)
            + ' reason='+inttostr(eventrecordp^.whd_event.reason)
            + ' reponsep len='+inttostr(responsep^.len) + ' hdrlen='+inttostr(responsep^.cmd.sdpcmheader.hdrlen));

(*      if (EventRecordP^.whd_event.event_type = ord(WLC_E_EAPOL_MSG)) then
        begin
          wifiloginfo(nil, 'CYW43455: eapol rx dump including headers');
          hexdump(pbyte(responsep), responsep^.len);
        end;
*)

        if (
           (EventRecordP^.whd_event.event_type = ord(WLC_E_DEAUTH))
           or (EventRecordP^.whd_event.event_type = ord(WLC_E_DEAUTH_IND))
           or (EventRecordP^.whd_event.event_type = ord(WLC_E_DISASSOC_IND))
           or
              ((EventRecordP^.whd_event.event_type = ord(WLC_E_LINK))
                and (EventRecordP^.whd_event.flags and CYW43455_EVENT_FLAG_LINK = 0))
           )
           and
           (FNetwork^.Network.NetworkStatus = NETWORK_STATUS_UP)
           then
        begin
          // the wifi link has gone down. Reset conection
          if (WIFI_LOG_ENABLED) then WIFILogInfo(nil, 'CYW43455: The WIFI link appears to have been lost (flags='+inttostr(EventRecordP^.whd_event.flags)+')');

          // Set join not completed
          FNetwork^.JoinCompleted := False;

          if (WIFI_USE_SUPPLICANT) then
          begin
            // Set EAPOL not completed
            FNetwork^.EAPOLCompleted := False;
            for k := 0 to WPA_KEY_MAX do
              FNetwork^.ValidKeys[k] := false;
            SupplicantOperatingState:=0;
          end;

          {Set Status to Down}
          FNetwork^.Network.NetworkStatus := NETWORK_STATUS_DOWN;

          {Notify the Status}
          NotifierNotify(@FNetwork^.Network.Device, DEVICE_NOTIFICATION_DOWN);

          SemaphoreSignal(FNetwork^.BackgroundJoinThread.FConnectionLost);
        end;

        // see if there are any requests interested in this event, and if so trigger
        // the callbacks. We only do the first one at the moment; we need a list eventually.

        RequestEntryP := FindRequestByEvent(EventRecordP^.whd_event.event_type);
        if (RequestEntryP <> nil) and (RequestEntryP^.Callback <> nil) then
        begin
          RequestEntryP^.Callback(TWIFIEvent(EventRecordP^.whd_event.event_type), EventRecordP, RequestEntryP, 0);
        end
        else
        begin
          {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: there was no interest in event ' + IntToStr(EventRecordP^.whd_event.event_type));
          {$ENDIF}
        end;
   end;

   // network packet
   2: begin
      try
        // Get BCDC Header (which is after the SDPCM_HEADER)
        BCDCHeaderP := PBCDC_HEADER(PByte(responseP) + responseP^.cmd.sdpcmheader.hdrlen);

        // Get Data Offset
        Offset := responseP^.cmd.sdpcmheader.hdrlen + BCDC_HEADER_SIZE + (BCDCHeaderP^.data_offset shl 2);

        // Get the Ethernet packet and frame length
        FrameLength := responseP^.len - Offset;
        EtherHeaderP := pether_header(PByte(ResponseP) + Offset);

        // detect supplicant status. Needs to be more resilient.
        if WIFI_USE_SUPPLICANT and (WordBEtoN(EtherHeaderP^.ethertype) = PACKET_TYPE_EAPOL) then
        begin
          wifiloginfo(nil,'CYW43455: EAPOL network packet received (after join); length='+inttostr(FrameLength) + ' operatingstate='+inttostr(SupplicantOperatingState));
          RequestEntryP := FindRequestByEvent(longword(WLC_E_EAPOL_MSG));
          if (RequestEntryP <> nil) and (RequestEntryP^.Callback <> nil) {and (SupplicantOperatingState=0)} then
          begin
            // this is kinda dirty but it works.
            // we pass the packet in the user data pointer instead as this isn't an event.
            RequestEntryP^.UserDataP := EtherHeaderP;
            RequestEntryP^.Callback(WLC_E_EAPOL_MSG, nil, RequestEntryP, FrameLength);
          end;
        end
        else
        begin
          // add into this network entry's packet buffer.
          NetworkEntryP^.Count:=NetworkEntryP^.Count+1;

          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Buffer:=ResponseP;
          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Data:=EtherHeaderP;
          NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Length:=FrameLength;

          {Update Statistics}
          Inc(FNetwork^.Network.ReceiveCount);
          Inc(FNetwork^.Network.ReceiveBytes, NetworkEntryP^.Packets[NetworkEntryP^.Count - 1].Length);

          // update bytesleft from current packet read
          bytesleft := bytesleft - RoundUp(ResponseP^.len, FNetwork^.DMAAlignment);

          // check enough bytes left for a worst case packet for the next read.
          if (bytesleft > sizeof(IOCTL_MSG)) then
          begin
            ResponseP := PIOCTL_MSG(PByte(ResponseP) + RoundUp(ResponseP^.Len, FNetwork^.DMAAlignment));
          end
          else
          begin
            // not enough bytes for next worst case packet so drop out of loop to give time for processing.
            isfinished := true;
            exit;
          end;

        end;
      except
        on e: exception do
          WIFILogError(nil, 'CYW43455: network packet exception ' + e.message + ' at ' + PtrToHex(exceptaddr));
      end;
      end;
   end;

end;

procedure TCYW43455WorkerThread.Execute;
var
  istatus: longword;
  responseP: PIOCTL_MSG;
  blockcount, remainder, offset: longword;
  NetworkEntryP: PNetworkEntry;
  PacketP: PNetworkPacket;
  txlen: longword;
  i: integer;
  BytesLeft: longword;
  isfinished: boolean;
  nGlomPackets: integer;
  SubPacketLengthP: PWord;
  GlomDescriptor: TGlomDescriptor;
  p: integer;
  PrevResponseP: PIOCTL_MSG;
  LastCredit: byte;
  seqdiff: integer;
  SDPCMHeaderP: PSDPCM_HEADER;
  BCDCHeaderP: PBCDC_HEADER;

begin
  {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: WIFI Worker thread started.');
  {$ENDIF}

  ThreadSetName(ThreadGetCurrent, 'CYW43455 Worker Thread');

  FNetwork^.txseq := 0;

  try

  while not terminated do
  begin
     // this polls for the interrupt status changing, which tells us there is some data to read.
     istatus := cfgreadl(FNetwork, FNetwork^.sdregs + IntStatus);
     while (istatus and $40 <> $40) and (FNetwork^.Network.TransmitQueue.Count = 0) do        // safe to check transmit queue here?
     begin
       // give up timeslice to other threads.
       Yield;
       istatus := cfgreadl(FNetwork, FNetwork^.sdregs + IntStatus);
     end;

     if (istatus and $40 = $40) then
     begin
       // clear interrupt status (seems like writing the value back does this based on other drivers)
       cfgwritel(FNetwork, FNetwork^.sdregs + IntStatus, istatus);
       cfgreadl(FNetwork, FNetwork^.sdregs + IntStatus);
     end;

     if MutexLock(FNetwork^.Network.Lock) = ERROR_SUCCESS then
     begin
      try
         ResponseP := nil;
         NetworkEntryP:=BufferGet(FNetwork^.Network.ReceiveQueue.Buffer);
         if (NetworkEntryP <> nil) then
         begin
           if (NetworkEntryP^.Buffer <> nil) then
           begin
             // Point our structure that we read data into from SDIO interface to the receive buffer
             ResponseP := NetworkEntryP^.Buffer;
             NetworkEntryP^.Count := 0;
             BytesLeft := FNetwork^.ReceiveRequestSize;
           end;
         end;

         // look at the network device to see if there is any data to receive,
         // only if we had an interrupt flag

         if (ResponseP <> nil) and (NetworkEntryP <> nil) and (istatus and $40 = $40) then
         begin
           if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, ResponseP, 0, IOCTL_LEN_BYTES) <> MMC_STATUS_SUCCESS then
           begin
             WIFILogError(nil, 'CYW43455: Error trying to read SDPCM header');
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
                  WIFILogError(nil, 'CYW43455: IOCTL Header length failure: len='+inttohex(responsep^.len, 8) + ' notlen='+inttohex(responsep^.notlen, 8));

               if (ResponseP^.Len > IOCTL_LEN_BYTES) then
               begin
                 blockcount := ResponseP^.Len div 512;
                 remainder := ResponseP^.Len mod 512;

                 if blockcount = 0 then
                   Dec(remainder, IOCTL_LEN_BYTES);
               end
               else
               begin
                blockcount := 0;
                remainder := 0;
               end;

               LastCredit := ResponseP^.Cmd.sdpcmheader.credit;

               if (ResponseP^.Len <= NetworkEntryP^.Size) and (ResponseP^.Len > 0) then
               begin
                 if (blockcount > 0) then
                 begin
                   // Read the rest of first block to maintain alignment for DMA (PIO)
                   if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, pbyte(responsep) + IOCTL_LEN_BYTES, 0, 512 - IOCTL_LEN_BYTES) <> MMC_STATUS_SUCCESS then
                     WIFILogError(nil, 'CYW43455: Error trying to read first block for ioctl response');

                   if (blockcount > 1) then
                   begin
                     // Read the full blocks as a single request (DMA)
                     if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, pbyte(responsep) + 512, blockcount - 1, 512) <> MMC_STATUS_SUCCESS then
                       WIFILogError(nil, 'CYW43455: Error trying to read blocks for ioctl response');
                   end;
                 end;

                 if (remainder > 0) then
                 begin
                   if blockcount = 0 then
                     offset := IOCTL_LEN_BYTES
                   else
                     offset := 0;

                   // Read the partial last block (PIO)
                   if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, pbyte(responsep) + offset + blockcount * 512, 0, remainder) <> MMC_STATUS_SUCCESS then
                     WIFILogError(nil, 'CYW43455: Error trying to read remainder for ioctl response (len='+inttostr(responsep^.len)+')');
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
                       {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
                       if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Glom descriptor: ' + nGlomPackets.tostring + ' packets present');
                       {$ENDIF}

                       SubPacketLengthP := PWord(pbyte(responseP) + responseP^.cmd.sdpcmheader.hdrlen);
                       for i := 0 to nGlomPackets-1 do
                       begin
                        {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
                         if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Sub packet length: ' + SubPacketLengthP^.ToString);
                        {$ENDIF}

                         {update total length}
                         FNetwork^.ReceiveGlomPacketSize += SubPacketLengthP^;

                         {next entry}
                         SubPacketLengthP += 1;       {1 word, not byte}
                       end;

                       {store glom descriptor}
                       move(ResponseP^, GlomDescriptor, ResponseP^.Len);

                       FNetwork^.ReceiveGlomPacketCount += 1;

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
                           WIFILogError(nil, 'CYW43455: Error: there was not enough buffer space '
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
                    WIFILogError(nil,'CYW43455: Unrecognised channel message received channel='+inttohex(responseP^.cmd.sdpcmheader.chan and $f, 2) + ' msglen='+inttostr(responsep^.len));
                    hexdump(pbyte(responsep), responsep^.len, 'Unrecognised message on channel ' + inttohex(responseP^.cmd.sdpcmheader.chan and $f, 2));
                  end;
               end
               else
                 if (ResponseP^.Len > 0) then
                   WIFILogError(nil, 'CYW43455: Could not read a large message into an undersized buffer (len='+inttostr(responsep^.len)+')');

               // read next sdpcm header (may not be one present in which case everything will be zero including length)
               if (not isFinished) and (SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, False, WLAN_FUNCTION, BAK_BASE_ADDR and $1ffff, false, ResponseP, 0, IOCTL_LEN_BYTES) <> MMC_STATUS_SUCCESS) then
               begin
                 WIFILogError(nil, 'CYW43455: Error trying to read SDPCM header (repeat)');
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
            FNetwork^.Network.ReceiveQueue.Entries[(FNetwork^.Network.ReceiveQueue.Start
                                  + FNetwork^.Network.ReceiveQueue.Count)
                                  mod  FNetwork^.ReceiveEntryCount]
                                    := NetworkEntryP;
            {Update Count}
            Inc(FNetwork^.Network.ReceiveQueue.Count);

            {Signal Packet Received}
            SemaphoreSignal(FNetwork^.Network.ReceiveQueue.Wait);
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
        while FNetwork^.Network.TransmitQueue.Count > 0 do
        begin
          {Get Entry}
          NetworkEntryP := FNetwork^.Network.TransmitQueue.Entries[FNetwork^.Network.TransmitQueue.Start];
          if NetworkEntryP <> nil then
          begin
            {Get Packet}
            for i := 0 to NetworkEntryP^.Count - 1 do
            begin
              PacketP:=@NetworkEntryP^.Packets[i];

              if (PacketP <> nil) then
              begin
                // Get SDPCM Header
                SDPCMHeaderP := PSDPCM_HEADER(PacketP^.Buffer + IOCTL_LEN_BYTES);

                //write an sdpcm header
                SDPCMHeaderP^.chan:=2;
                SDPCMHeaderP^.nextlen := 0;
                SDPCMHeaderP^.hdrlen:=IOCTL_LEN_BYTES + SDPCM_HEADER_SIZE;
                SDPCMHeaderP^.credit := 0;
                SDPCMHeaderP^.flow := 0;
                SDPCMHeaderP^.seq:=FNetwork^.txseq;
                SDPCMHeaderP^.reserved := 0;

                // Get BCDC Header
                BCDCHeaderP := PBCDC_HEADER(PacketP^.Buffer + SDPCMHeaderP^.hdrlen);

                // Write BCDC Header
                BCDCHeaderP^.flags := BCDC_PROTOCOL_VERSION shl BCDC_VERSION_SHIFT;
                BCDCHeaderP^.priority := 0;
                BCDCHeaderP^.flags2 := 0;
                BCDCHeaderP^.data_offset := 0;

                // needs thread protection?
                if (FNetwork^.txseq < 255) then
                  FNetwork^.txseq := FNetwork^.txseq + 1
                else
                  FNetwork^.txseq := 0;

                // If we are beyond the sending limit then we need to wait a bit.
                // it may be more advantageous to drop out of the loop and send the rest
                // of the data next time, as we'd likely get a credit update from the reading
                // code, but that is a larger change.
                if (CYW43455_USE_FIRMWARE_CREDIT_VALUE) then
                begin
                  seqdiff := LastCredit - FNetwork^.txseq;
                  if (seqdiff < 0) then
                    seqdiff := seqdiff + 256;

                  if (seqdiff > 127) then
                  begin
                    // the sleep here should allow the firmware to empty its send buffers a bit.
                    // perhaps the sleep should be a function of the difference value.
                    sleep(30);
                  end;
                end;

                PIOCTL_MSG(PacketP^.Buffer)^.len := PacketP^.length + IOCTL_LEN_BYTES + SDPCM_HEADER_SIZE + BCDC_HEADER_SIZE;
                PIOCTL_MSG(PacketP^.Buffer)^.notlen := not PIOCTL_MSG(PacketP^.Buffer)^.len;

                // calculate transmission sizes
                txlen := PIOCTL_MSG(PacketP^.Buffer)^.len;
                blockcount := txlen div 512;
                remainder := txlen mod 512;

                //Update Statistics
                Inc(FNetwork^.Network.TransmitCount);
                Inc(FNetwork^.Network.TransmitBytes, txlen);

                //send data
                if (blockcount > 0) then
                begin
                  if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, True, WLAN_FUNCTION,
                        BAK_BASE_ADDR and $1ffff, false, PacketP^.Buffer, blockcount, 512) <> MMC_STATUS_SUCCESS then
                          WIFILogError(nil, 'CYW43455: Failed to transmit packet data blocks txseq='+inttostr(FNetwork^.txseq)+' lastcredit='+inttostr(LastCredit));
                end;

                if (remainder > 0) then
                begin
                  if SDIODeviceReadWriteExtended(FNetwork^.Func1^.MMC, True, WLAN_FUNCTION,
                        BAK_BASE_ADDR and $1ffff, false, PacketP^.Buffer + blockcount*512, 0, remainder) <> MMC_STATUS_SUCCESS then
                          WIFILogError(nil, 'CYW43455: Failed to transmit packet data remainder txseq='+inttostr(FNetwork^.txseq)+' lastcredit='+inttostr(LastCredit));
                end;

                if (WIFI_USE_SUPPLICANT) then
                begin
                  // there are two EAPOL messages to be sent by the supplicant - messages 2 & 4.
                  // we want keys to be set after these messages are sent, so we detect the sitution here
                  // and the keys are set in DoNetworkNotify just before the network is brought up.

                  if (pether_header(pbyte(packetp^.buffer) + IOCTL_LEN_BYTES + SDPCM_HEADER_SIZE + BCDC_HEADER_SIZE)^.ethertype = WordNtoBE(PACKET_TYPE_EAPOL)) then
                  begin
                    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
                    if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Detected transmission of an eapol packet');
                    {$ENDIF}

                    // if we are transmitting an EAPOL packet, then we must be sending message 2 or greater.
                    if (FNetwork^.EAPOLCompleted) then
                    begin
                      if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: EAPOL Completed - bringing network up');

                      if (WorkerSchedule(0, @DoNetworkNotify, FNetwork, nil) <> ERROR_SUCCESS) then
                        if WIFI_LOG_ENABLED then WIFILogError(nil, 'CYW43455: Failed to schedule task to signify the network is up');
                    end;
                  end;
                end;

              end;
            end;
          end;

          {Update Start}
          FNetwork^.Network.TransmitQueue.Start:=(FNetwork^.Network.TransmitQueue.Start + 1) mod FNetwork^.TransmitEntryCount;

          {Update Count}
          Dec(FNetwork^.Network.TransmitQueue.Count);

          {Signal Queue Free}
          SemaphoreSignal(FNetwork^.Network.TransmitQueue.Wait);

          BufferFree(NetworkEntryP);
        end;

      finally
        MutexUnlock(FNetwork^.Network.Lock);
      end
     end
     else
       WIFILogError(nil, 'CYW43455: Failed to get a mutex lock');
  end;

  except
    on e: exception do
     WIFILogError(nil, 'CYW43455: workerthread execute exception ' + e.message + ' at ' + PtrToHex(exceptaddr));
  end;
end;

constructor TCYW43455ReconnectionThread.Create(ANetwork: PCYW43455Network);
begin
  inherited Create(true,THREAD_STACK_DEFAULT_SIZE);
  FNetwork := ANetwork;

  FConnectionLost := SemaphoreCreate(0);
  FUseBSSID := false;
  FReconnectionType := WIFIReconnectAlways;
  Start;
end;

destructor TCYW43455ReconnectionThread.Destroy;
begin
  SemaphoreDestroy(FConnectionLost);

  inherited Destroy;
end;

procedure TCYW43455ReconnectionThread.SetConnectionDetails(aSSID, aKey, aCountry: string;
          BSSID: THardwareAddress; useBSSID: boolean; aReconnectionType: TWIFIReconnectionType);
begin
  FSSID := aSSID;
  FKey := aKey;
  FCountry := aCountry;
  FUseBSSID:=useBSSID;
  FBSSID := BSSID;
  FReconnectionType := aReconnectionType;
end;

procedure TCYW43455ReconnectionThread.Execute;
var
  Res: longword;
begin
  ThreadSetName(ThreadGetCurrent, 'CYW43455 Reconnection Thread');

  while not Terminated do
  begin
    // wifi worker is the source of this sempahore.

    if (SemaphoreWaitEx(FConnectionLost, 1000) = ERROR_SUCCESS) then
    begin
      if (FReconnectionType = WIFIReconnectAlways) then
      begin
        // the connection has been lost. Reconnect. Keep trying indefinitely until
        // the join function returns a success.

        WIFILogInfo(nil, 'CYW43455: Reconnection attempt started');

        if (WIFI_USE_SUPPLICANT) then
          UltiboEloopTerminate   // Request termination of the supplicant eloop.
                                 // The supplicant thread will restart it to kick off a new
                                 // connection attempt.
        else
        begin
          Res := MMC_STATUS_INVALID_PARAMETER;
          while (Res <> MMC_STATUS_SUCCESS) do
          begin
            Res := FirmwareWirelessJoinNetwork(FSSID, FKey, FCountry, WIFIJoinBlocking, WIFIReconnectNever, FBSSID, FUseBSSID); // do *not* call with WIFIReconnectAlways from this thread.
            if (Res <> 0) then
            begin
              {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
              if WIFI_LOG_ENABLED then WIFILogDebug(nil, 'CYW43455: Wireless Join returned fail ' + inttostr(res));
              {$ENDIF}
            end
            else
              WIFILogInfo(nil, 'CYW43455: Successfully reconnected to the WIFI network');
          end;
        end;
      end
      else
      begin
        WIFILogInfo(nil, 'CYW43455: Not reconnecting as reconnections are disabled');
      end;
    end;
  end;
end;

procedure UltiboTimeProc(var epoch: longint; var millisecond: integer); cdecl;
var
  st: SYSTEMTIME;
  dt: TDateTime;
begin
  dt := Now;
  DateTimeToSystemTime(dt, st);
  epoch := DateTimeToUnixTime(dt);
  millisecond := st.Millisecond;
end;

procedure UltiboMonotonicTimeProc(var epoch: longint; var millisecond: integer); cdecl;
var
  c: QWord;
begin
  c := GetTickCount64;
  epoch := c div 1000;
  millisecond := c mod 1000;
end;

procedure DoSendDriverNewPacketData(data: pointer);
var
  PacketXFerP: PUltiboPacketXFer;
begin
 try
   PacketXFerP := PUltiboPacketXFer(data);
   try
    ultibo_driver_new_packet_data(@PacketXFerP^.source_address[0], PacketXFerP^.packetbufferp, PacketXFerP^.DataLength);

   except
     WIFILogError(nil, 'CYW43455: An exception occurred in the ultibo_driver_new_packet_data' + PtrToHex(exceptaddr));
   end;

   FreeMem(PacketXFerP^.packetbufferp);
   FreeMem(PacketXFerP);

 except
   WIFILogError(nil, 'CYW43455: An exception occurred in dosenddrivernewpacketdata at ' + PtrToHex(exceptaddr));
 end;
end;

procedure EAPOLCallback(Event: TWIFIEvent;
            EventRecordP: pwhd_event;
            RequestItemP: PWIFIRequestItem;
            DataLength: longint);
var
  packetbufferp: pbyte;
  EtherHeaderP: pether_header;
  PacketXFerP: PUltiboPacketXFer;
begin
  if (Event = WLC_E_EAPOL_MSG) then
  begin
    {this callback is now a from a packet rather than an event but it still uses
     the event mechanism to reach this callback.}
    EtherHeaderP := pether_header(RequestItemP^.UserDataP);
    packetbufferp := PByte(EtherHeaderP) + sizeof(ether_header);

    // Instead of sending the packet data directly to the supplicant, we instead
    // copy it into another structure and then use an ultibo worker task to actually
    // make the call to the supplicant. The reason for this is that it avoids a
    // deadlock scenario where the supplicant thread and the wifi worker thread are
    // are holding the locks which each other need to proceed (the network buffer
    // lock and the packet queue lock).
    // This isn't all that efficient but the EAPOL negotiation only happens at
    // connection time and not at all thereafter.

    getmem(PacketXferP, sizeof(TUltiboPacketXFer));
    getmem(PacketXFerP^.packetbufferp, DataLength);
    move(EtherHeaderP^.source_address[0], PacketXFerP^.source_address[0], 6);
    move(packetbufferp^, PacketXFerP^.packetbufferp^, DataLength);
    PacketXFerP^.DataLength := DataLength;

    // the task will release the allocated memory.
    WorkerSchedule(0, @DoSendDriverNewPacketData, PacketXFerP, nil)

    // no longer called directly.
    // ultibo_driver_new_packet_data(@EtherHeaderP^.source_address[0], packetbufferp, DataLength);

  end
  else
    WIFILogError(nil, 'CYW43455: Wireless Scan: Unexpected event received');

end;

function SupplicantWirelessJoinNetwork(ssid: PChar; authkey: PByte;
                             authkeylen: longword;
                             bssid: PHardwareAddress;
                             usebssid: boolean = false): longword; cdecl;
var
  Network: PCYW43455Network;

  n: integer;
  chan: integer;
  countrysettings: countryparams;
  res: longword;
  responseval: longword;
  wpa_auth: longword;
  RequestEntryP: PWIFIRequestItem;
  extjoinparams: wl_extjoin_params;
  auth_mfp: longword;
  k: integer;

begin
 Result := MMC_STATUS_INVALID_DATA;

 // Get Wireless Device
 Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION)); //To Do //TestingSDIO // Pass priv pointer from supplicant
 if Network = nil then Exit;

 fillchar(countrysettings, 0, sizeof(countrysettings));
 move(Network^.CountryCode[0], countrysettings.country_ie[1], 2);
 move(Network^.CountryCode[0], countrysettings.country_code[1], 2);
 countrysettings.revision := -1;

 Result := WirelessSetVar(Network, 'country', @countrysettings, sizeof(countrysettings));
 if Result <> MMC_STATUS_SUCCESS then
   exit;

 {Set infrastructure mode, 1 = normal, 0 = access point}
 Result := WirelessCommandInt(Network, WLC_SET_INFRA, 1);
 if (Result <> MMC_STATUS_SUCCESS) then
   exit;

 {send auth key}
 if (WirelessSetVar(Network, 'wpaie', authkey, authkeylen) <> MMC_STATUS_SUCCESS) then
   exit;

 // Set WPA authentication mode (string style)
 // this one is kinda replaced by the one later down but this is what circle does
 // need a constant for this if it works.
 wpa_auth := WPA2_AUTH_PSK;
 Result := WirelessSetInt(Network, 'wpa_auth', 192);
 if (Result <> MMC_STATUS_SUCCESS) then
   exit;

 Result := WirelessCommandInt(Network, WLC_SET_AUTH, WL_AUTH_OPEN_SYSTEM);
 if (Result <> MMC_STATUS_SUCCESS) then
  exit;

 // set auth with string method
// result := WirelessSetInt(Network, 'auth', WL_AUTH_OPEN_SYSTEM);
// if (Result <> MMC_STATUS_SUCCESS) then
//   exit;

 {Set Wireless Security Type}
 Result := WirelessCommandInt(Network, WLC_SET_WSEC, AES_ENABLED);
 if (Result <> MMC_STATUS_SUCCESS) then
   exit;

 // set wsec with string method
// Result := WirelessSetInt(Network, 'wsec', AES_ENABLED);
// if (Result <> MMC_STATUS_SUCCESS) then
//   exit;

 {Set WPA authentication mode}
 wpa_auth := WPA2_AUTH_PSK;
 Result := WirelessIOCTLCommand(Network, WLC_SET_WPA_AUTH, @wpa_auth, sizeof(wpa_auth), true, @responseval, 4);
 if (Result <> MMC_STATUS_SUCCESS) then
   exit;

 // Set WPA authentication mode (string style)
// wpa_auth := WPA2_AUTH_PSK;
// Result := WirelessSetInt(Network, 'wpa_auth', WPA2_AUTH_PSK);
// if (Result <> MMC_STATUS_SUCCESS) then
//   exit;

 auth_mfp := 0;
 Result := WirelessSetVar(Network, 'mfp', @auth_mfp, 4);
 if (Result <> MMC_STATUS_SUCCESS) then
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
     move(bssid^[0], extjoinparams.assoc_params.bssid[0], HARDWARE_ADDRESS_SIZE) { bssid}
  else
     fillchar(extjoinparams.assoc_params.bssid[0], HARDWARE_ADDRESS_SIZE, $ff); { empty}

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
    res := WirelessSetVar(Network, 'join', @extjoinparams, SizeOf(wl_extjoin_params))
  else
    res := WirelessSetVar(Network, 'join', @extjoinparams, SizeOf(wl_extjoin_params) - 4);

  if (res <> 0) then
    WIFILogError(nil, 'CYW43455: Supplicant join: setvar returned fail code of ' + inttostr(res));

  RequestEntryP := Network^.WorkerThread.AddRequest(0, [WLC_E_SET_SSID, WLC_E_LINK], @JoinCallback, nil);

  {Set EAPOL not completed}
  Network^.EAPOLCompleted := False;
  for k := 0 to WPA_KEY_MAX do
    Network^.ValidKeys[k] := false;

  {wait for a completion signal or timeout.}
  SemaphoreWaitEx(RequestEntryP^.Signal, 15000); //10000

  if (RequestEntryP^.RegisteredEvents = []) then
  begin
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: Successfully associated ' + SSID);

    {Set join completed}
    Network^.JoinCompleted := True;
  end
  else
  begin
    Result := MMC_STATUS_INVALID_PARAMETER;
    if WIFI_LOG_ENABLED then WIFILogInfo(nil, 'CYW43455: The join attempt to network ' + SSID + ' was unsuccessful');

    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if WIFI_LOG_ENABLED then
    begin
      if (WLC_E_SET_SSID in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'CYW43455: WLC_E_SET_SSID event was not returned from the join request');

      if (WLC_E_LINK in RequestEntryP^.RegisteredEvents) then
        WIFILogDebug(nil, 'CYW43455: WLC_E_LINK event was not returned from the join request');
    end;
    {$ENDIF}
  end;

  Network^.WorkerThread.DoneWithRequest(RequestEntryP);
end;

procedure LockEAPOLPacketQueue(trace: pchar); cdecl;
var
 CriticalSectionHandle:TCriticalSectionHandle;
begin
  if (not tryentercriticalsection(EAPOLQueueLock)) then
  begin
    CriticalSectionHandle := TCriticalSectionHandle(EAPOLQueueLock.LockSemaphore);
    WIFILogError(nil, trace + ' LockEAPOLPacketQueue failed to get critical section. ownner='+
                      ThreadGetName(CriticalSectionOwner(CriticalSectionHandle))+
                      ' count=' + inttostr(CriticalSectionCount(CriticalSectionHandle))+
                      ' current=' + ThreadGetName(ThreadGetCurrent));
    EnterCriticalSection(EAPOLQueueLock);
    WIFILogError(nil, 'LockEAPOLPacketQueue lock finally acquired');
  end;
end;

procedure UnlockEAPOLPacketQueue; cdecl;
begin
  LeaveCriticalSection(EAPOLQueueLock);
end;

procedure LockSendPacketQueue; cdecl;
var
 CriticalSectionHandle:TCriticalSectionHandle;
begin
  if (not tryentercriticalsection(EAPOLQueueLock)) then
  begin
    CriticalSectionHandle := TCriticalSectionHandle(EAPOLQueueLock.LockSemaphore);
    WIFILogError(nil, 'LockSendPacketQueue failed to get critical section. ownner=' +
                      ThreadGetName(CriticalSectionOwner(CriticalSectionHandle)) +
                      ' count=' + inttostr(CriticalSectionCount(CriticalSectionHandle)) +
                      ' current=' + ThreadGetName(ThreadGetCurrent));
    EnterCriticalSection(EAPOLQueueLock);
    WIFILogError(nil, 'LockSendPacketQueue lock finally acquired');
  end;

end;

procedure UnlockSendPacketQueue; cdecl;
begin
  LeaveCriticalSection(EAPOLQueueLock);
end;

function UltiboSetKey(const ifname: PChar; priv: pointer; alg: byte;
		      const addr: PByte; key_idx: integer; set_tx: integer;
		      const seq: PByte; seq_len: integer;
		      const key: PByte; key_len: integer): integer; cdecl;
var
  Network: PCYW43455Network;

  KeyP: PWPAKey;
  pairwise: boolean;
begin
 try
  Result := 0;

  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION)); //To Do //TestingSDIO // Pass priv pointer from supplicant
  if Network = nil then Exit;

  Network^.ValidKeys[key_idx] := true;

  {probably a request to clear the key - ignore for now.}
  if (addr = nil) then
    exit;

  {we don't support TKIP; only CCMP.}
  if (alg <> WPA_ALG_CCMP) then
    exit;

  {take a copy of the parameters and store in a record for later.}
  KeyP := @(Network^.WPAKeys[key_idx]);

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

 except
   WIFILogError(nil, 'CYW43455: Exception in SetKey at ' + PtrToHex(exceptaddr));
 end;
end;

procedure SendSupplicantL2Packet(PacketBufferP: PByte; Len: longword); cdecl;
var
  Network: PCYW43455Network;
  NetworkEntryP: PNetworkEntry;
  TransmitBufferP: Pointer;
begin
  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION)); //To Do //TestingSDIO // Pass priv pointer from supplicant ?
  if Network = nil then Exit;

  try
    {get transmit buffer.}
    NetworkEntryP:=BufferGet(Network^.Network.TransmitQueue.Buffer);
    if (NetworkEntryP <> nil) and (NetworkEntryP^.Buffer <> nil) then
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
      pether_header(PacketBufferP)^.ethertype := WordNtoBE(pether_header(PacketBufferP)^.ethertype);

      {not sure if this is right either!}
      {move data into packet}
      move(PacketBufferP^, NetworkEntryP^.Packets[NetworkEntryP^.Count-1].Data^, len);

      {Wait for Entry}
      if SemaphoreWait(Network^.Network.TransmitQueue.Wait) = ERROR_SUCCESS then
      begin
        {Acquire the Lock}
        if MutexLock(Network^.Network.Lock) = ERROR_SUCCESS then
        begin
          {Add Entry}
          Network^.Network.TransmitQueue.Entries[(Network^.Network.TransmitQueue.Start + Network^.Network.TransmitQueue.Count)
                                                mod Network^.TransmitEntryCount] := NetworkEntryP;
          {Update Count}
          Inc(Network^.Network.TransmitQueue.Count);

          {Release the Lock}
          MutexUnlock(Network^.Network.Lock);
        end
        else
          WIFILogError(nil, 'CYW43455: Failed to get network lock!!');
      end
      else
        WIFILogError(nil, 'CYW43455: Failed to wait for transmit semaphore');
    end
    else
      WIFILogError(nil, 'CYW43455: Failed to get a transmit buffer!');
  except
    on e: exception do
      WIFILogInfo(nil, 'CYW43455: Exception in sendsupplicantl2packet: ' + e.message + ' at address ' + PtrToHex(exceptaddr));
  end;
end;

procedure DoNetworkNotify(data: pointer);
var
  Network: PCYW43455Network;
  KeyP: PWPAKey;
  k: integer;
begin
  // Get Wireless Device
  Network := PCYW43455Network(data);
  if Network = nil then Exit;

  {set both keys here before bringing up the network.}
  for k := 0 to WPA_KEY_MAX do
    if (Network^.ValidKeys[k]) then
    begin
      WIFILogInfo(nil, 'CYW43455: Setting key ' + k.ToString);

      KeyP := @(Network^.WPAKeys[k]);
      if (WirelessSetVar(Network, 'wsec_key', PByte(KeyP), sizeof(TWPAKey), 'ultibosetkey') <> ERROR_SUCCESS) then
        WIFILogError(nil, 'CYW43455: Failed to update wsec_key in UltiboSetKey key=' + k.tostring)
      else
        WIFILogInfo(nil, 'CYW43455: Successfully set the wsec_key var key=' + k.tostring);
    end;

  {Set Status to Up}
  Network^.Network.NetworkStatus := NETWORK_STATUS_UP;

  {Notify the Status}
  NotifierNotify(@Network^.Network.Device, DEVICE_NOTIFICATION_UP);

  {signal network joined}
  SemaphoreSignal(Network^.NetworkUpSignal);

  WIFILogInfo(nil, 'CYW43455: The WIFI network status is up');
end;

procedure UltiboEAPOLComplete; cdecl;
var
  Network: PCYW43455Network;
begin
  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION)); //To Do //TestingSDIO // Pass priv pointer from supplicant
  if Network = nil then Exit;

  Network^.EAPOLCompleted := true;
  WIFILogInfo(nil, 'CYW43455: EAPOL auth is complete.');
end;

function UltiboSetCountry(priv: pointer; ccode: PChar): integer; cdecl;
var
  Network: PCYW43455Network;
begin
  Result := 0;

  // Get Wireless Device
  Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION)); //To Do //TestingSDIO // Pass priv pointer from supplicant
  if Network = nil then Exit;

  Network^.CountryCode[0] := ccode^;
  Network^.CountryCode[1] := (ccode+1)^;
end;

constructor TWPASupplicantThread.create(ANetwork: PCYW43455Network);
begin
  {the thread is created suspended. We'll let it run when the application asks for a connection.}
  inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
  FreeOnTerminate := true;

  FNetwork := ANetwork;
end;

destructor TWPASupplicantThread.Destroy;
begin
  inherited Destroy;
end;

procedure TWPASupplicantThread.Execute;
var
 retcode: integer;
 EAPOLRequestItemP: PWIFIRequestItem;
begin
  ThreadSetName(ThreadGetCurrent, 'WPA Supplicant Thread');
  WIFILogInfo(nil, 'CYW43455: WPA Supplicant Thread started execution');

  {set up the mac address pointer so the supplicant can see it}
  SetUltiboMacAddress(@FNetwork^.macaddress[0]);

  {add callback request for EAPOL messages. Dunno if this will work.}
  {haven't actually seen any yet.}
  EAPOLRequestItemP := FNetwork^.WorkerThread.AddRequest(0, [WLC_E_EAPOL_MSG], @EAPOLCallback, nil);

  {call main - this will read the configuration and start the event loop off.}
  {Each time this loop executes, it repeats because the connection was lost. We are asking
   the supplicant to reset itself each time in order to trigger a reconnection. That may not
   be very efficient but so far I have not found a function I can call to
   kick it into reconnecting.}

  try
    while (not Terminated) do
    begin
      retcode := wpa_supplicant_main('wpa_supplicant.conf');

      WIFILogInfo(nil, 'CYW43455: The supplicant main loop has terminated - return code ' + inttostr(retcode));
    end;

    FNetwork^.WorkerThread.DoneWithRequest(EAPOLRequestItemP);

    WIFILogInfo(nil, 'CYW43455: The supplicant thread has terminated - return code ' + inttostr(retcode));
  except
    on e: exception do
      WIFILogError(nil, 'CYW43455: Exception generated in TWPASupplicantThread.Execute ' + e.Message + ' ' + PtrToHex(exceptaddr));
  end;

end;

function WIFIDeviceSDIODriverBind(MMC: PMMCDevice; Func: PSDIOFunction): LongWord;
var
 Func1: PSDIOFunction;
 Func2: PSDIOFunction;
 Network: PCYW43455Network;
begin
 {}
 Result := MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then
   Exit;

 {Check Function}
 if Func = nil then
   Exit;

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Attempting to bind SDIO function (Number=' + IntToStr(Func^.Number) + ' VendorId=' + IntToHex(Func^.VendorId, 4) + ' DeviceId=' + IntToHex(Func^.DeviceId, 4) + ')');
 {$ENDIF}

 {Check Function}
 if WIFIDeviceCheckFunction(Func) <> MMC_STATUS_SUCCESS then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Function not found in supported device list');
   {$ENDIF}

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Function Number}
 case Func^.Number of
  1: begin
    {Function 1 (Backplane)}
    Func1 := Func;

    {Find Function 2}
    Func2 := SDIOFunctionFind(MMC, 2);
   end;
  2: begin
    {Function 2 (Wireless LAN)}
    Func2:=Func;

    {Find Function 1}
    Func1 := SDIOFunctionFind(MMC, 1);
   end;
  else
   begin
    {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Function number not in supported list');
    {$ENDIF}

    {Return Result}
    Result := MMC_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
 end;

 {Check Functions}
 if (Func1 = nil) or (Func2 = nil) then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Unable to locate all required functions');
   {$ENDIF}

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Bind Function 1}
 if SDIOFunctionBind(Func1, CYW43455SDIODriver) <> MMC_STATUS_SUCCESS then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Failed to bind driver to function 1');
   {$ENDIF}

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Bind Function 2}
 if SDIOFunctionBind(Func2, CYW43455SDIODriver) <> MMC_STATUS_SUCCESS then
  begin
   {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Failed to bind driver to function 2');
   {$ENDIF}

   {Unbind Function 1}
   SDIOFunctionUnbind(Func1, CYW43455SDIODriver);

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Create Network}
 Network := PCYW43455Network(NetworkDeviceCreateEx(SizeOf(TCYW43455Network)));
 if Network = nil then
  begin
   if MMC_LOG_ENABLED then MMCLogError(MMC, 'CYW43455: Failed to create new network device');

   {Unbind Function 2}
   SDIOFunctionUnbind(Func2, CYW43455SDIODriver);

   {Unbind Function 1}
   SDIOFunctionUnbind(Func1, CYW43455SDIODriver);

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Network}
 {Device}
 Network^.Network.Device.DeviceBus := DEVICE_BUS_SD;
 Network^.Network.Device.DeviceType := NETWORK_TYPE_ETHERNET;
 Network^.Network.Device.DeviceFlags := NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER or NETWORK_FLAG_RX_MULTIPACKET;
 Network^.Network.Device.DeviceData := MMC;
 Network^.Network.Device.DeviceDescription := CYW43455_NETWORK_DESCRIPTION;
 {Network}
 Network^.Network.NetworkState := NETWORK_STATE_CLOSED;
 Network^.Network.NetworkStatus := NETWORK_STATUS_DOWN;
 Network^.Network.DeviceOpen := @CYW43455DeviceOpen;
 Network^.Network.DeviceClose := @CYW43455DeviceClose;
 Network^.Network.DeviceControl := @CYW43455DeviceControl;
 Network^.Network.BufferAllocate := @CYW43455BufferAllocate;
 Network^.Network.BufferRelease := @CYW43455BufferRelease;
 Network^.Network.BufferReceive := @CYW43455BufferReceive;
 Network^.Network.BufferTransmit := @CYW43455BufferTransmit;
 {Driver}
 //To Do //TestingSDIO // Anything else ?
 {SDIO}
 Network^.Func1 := Func1;
 Network^.Func2 := Func2;
 {CYW43455}
 Network^.TXBuffer := nil;
 Network^.RXBuffer := nil;
 Network^.DMABuffer := nil;
 Network^.DMAAlignment := 0;

 {Register Network}
 if NetworkDeviceRegister(@Network^.Network) <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(MMC, 'CYW43455: Failed to register new network device');

   {Unbind Function 2}
   SDIOFunctionUnbind(Func2, CYW43455SDIODriver);

   {Unbind Function 1}
   SDIOFunctionUnbind(Func1, CYW43455SDIODriver);

   {Destroy Network}
   NetworkDeviceDestroy(@Network^.Network);

   {Return Result}
   Result := MMC_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Functions}
 Network^.Func1^.DriverData := Network;
 Network^.Func2^.DriverData := Network;

 {Return Result}
 Result := MMC_STATUS_SUCCESS;
end;

function WIFIDeviceSDIODriverUnbind(MMC: PMMCDevice; Func: PSDIOFunction): LongWord;
var
 Network: PCYW43455Network;
begin
 Result := MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then
   Exit;

 {Check Function}
 if Func = nil then
   Exit;

 {Check Driver}
 if Func^.Driver <> CYW43455SDIODriver then
   Exit;

 {$IF DEFINED(CYW43455_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC, 'CYW43455: Unbinding SDIO function (Number=' + IntToStr(Func^.Number) + ' VendorId=' + IntToHex(Func^.VendorId, 4) + ' DeviceId=' + IntToHex(Func^.DeviceId, 4) + ')');
 {$ENDIF}

 {Get Network}
 Network := PCYW43455Network(Func^.DriverData);
 if Network = nil then
   Exit;

 {Close Network}
 CYW43455DeviceClose(@Network^.Network);

 {Update Functions}
 Network^.Func1^.DriverData := nil;
 Network^.Func2^.DriverData := nil;

 {Unbind Function 1}
 SDIOFunctionUnbind(Network^.Func1, CYW43455SDIODriver);

 {Unbind Function 2}
 SDIOFunctionUnbind(Network^.Func2, CYW43455SDIODriver);

 {Deregister Network}
 if NetworkDeviceDeregister(@Network^.Network) <> ERROR_SUCCESS then
   Exit;

 {Destroy Network}
 NetworkDeviceDestroy(@Network^.Network);

 Result := MMC_STATUS_SUCCESS;
end;

function WIFIDeviceCheckFunction(Func: PSDIOFunction): LongWord;
begin
 {}
 Result := MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then
   Exit;

 {Check Vendor}
 if Func^.VendorId = SDIO_VENDOR_ID_BROADCOM then
  begin
   case Func^.DeviceId of
    SDIO_DEVICE_ID_BROADCOM_4330,
    SDIO_DEVICE_ID_BROADCOM_43362,
    SDIO_DEVICE_ID_BROADCOM_43430,
    SDIO_DEVICE_ID_BROADCOM_4345: begin
      Result := MMC_STATUS_SUCCESS;
      Exit;
     end;
   end;
  end;

 Result := MMC_STATUS_DEVICE_UNSUPPORTED;
end;

initialization
  WIFIDeviceInit;

end.
