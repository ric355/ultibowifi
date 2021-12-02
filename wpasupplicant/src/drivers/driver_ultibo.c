/*
 * Driver interface for RADIUS server or WPS ER only (no driver)
 * Copyright (c) 2008, Atheros Communications
 *
 * This software may be distributed under the terms of the BSD license.
 * See README for more details.
 */

#include "includes.h"

#include "common.h"
#include "driver.h"
#include "eloop.h"
#include "driver_ultibo.h"
#include <assert.h>
#include <string.h>
#include "eloop_ultibo.h"

#define SCAN_DURATION_SECS 3
#define MAX_SCAN_RESULTS 128

typedef u16 __le16;
typedef u32 __le32;

struct ultibo_driver_data {
	struct hostapd_data *hapd;
	void *ctx;
	u8 bssid[ETH_ALEN];
	u8 ssid_len;
	u8 ssid[32];
};

struct brcmf_bss_info_le
{
	__le32 version;		/* version field */
#define	BRCMF_BSS_INFO_VERSION	109 /* curr ver of brcmf_bss_info_le struct */
	__le32 length;		/* byte length of data in this record,
				 * starting at version and including IEs
				 */
	u8 BSSID[ETH_ALEN];
	__le16 beacon_period;	/* units are Kusec */
	__le16 capability;	/* Capability information */
	u8 SSID_len;
	u8 SSID[32];
	struct {
		__le32 count;	/* # rates in this set */
		u8 rates[16];	/* rates in 500kbps units w/hi bit set if basic */
	} rateset;		/* supported rates */
	__le16 chanspec;	/* chanspec for bss */
	__le16 atim_window;	/* units are Kusec */
	u8 dtim_period;		/* DTIM period */
	__le16 RSSI;		/* receive signal strength (in dBm) */
	s8 phy_noise;		/* noise (in dBm) */

	u8 n_cap;		/* BSS is 802.11N Capable */
	/* 802.11N BSS Capabilities (based on HT_CAP_*): */
	__le32 nbss_cap;
	u8 ctl_ch;		/* 802.11N BSS control channel number */
	__le32 reserved32[1];	/* Reserved for expansion of BSS properties */
	u8 flags;		/* flags */
	u8 reserved[3];		/* Reserved for expansion of BSS properties */
#define BRCMF_MCSSET_LEN		16
	u8 basic_mcs[BRCMF_MCSSET_LEN];	/* 802.11N BSS required MCS set */

	__le16 ie_offset;	/* offset at which IEs start, from beginning */
	__le32 ie_length;	/* byte length of Information Elements */
	__le16 SNR;		/* average SNR of during frame reception */
	/* Add new fields here */
	/* variable length Information Elements */
};


struct brcmf_escan_result_le
{
	__le32 buflen;
	__le32 version;
	__le16 sync_id;
	__le16 bss_count;
	struct brcmf_bss_info_le bss_info_le;
};

struct scanres_list_item {
	struct dl_list list;
	struct brcmf_escan_result_le scanres;
};

struct scanres_list_item scanreslist;

struct ultiborawpacket ultiborawpacketreceivequeue;
struct ultiborawpacket ultiborawpacketsendqueue;


// 1 for fully connected, 0 for not connected.
int SupplicantOperatingState;

// pointer to mac address - the memory for this is owned by the ultibo device driver.
const u8 *MACAddress;
extern struct eloop_data eloop;


// called by ultibo to get the event loop to terminate
void UltiboEloopTerminate()
{
	eloop.terminate = 1;
}


// called by ultibo to set the mac address
void SetUltiboMacAddress(const u8* UltiboMacAddress)
{
   MACAddress = UltiboMacAddress;
}

static int chanspec2freq (u16 chanspec)
{
	u8 chan = chanspec & 0xFF;

	if (1 <= chan && chan <= 14)
	{
		static const int low_freqs[] =
		{
			2412, 2417, 2422, 2427, 2432, 2437, 2442,
			2447, 2452, 2457, 2462, 2467, 2472, 2484
		};

		return low_freqs[chan-1];
	}

	if (32 <= chan && chan <= 173)
	{
		return 5160 + (chan-32) * 5;
	}

	return -1;
}

static void * ultibo_driver_hapd_init(struct hostapd_data *hapd,
				    struct wpa_init_params *params)
{
	struct ultibo_driver_data *drv;

	drv = os_zalloc(sizeof(struct ultibo_driver_data));
	if (drv == NULL) {
		wpa_printf(MSG_ERROR, "Could not allocate memory for none "
			   "driver data");
		return NULL;
	}
	drv->hapd = hapd;

	return drv;
}


static void ultibo_driver_hapd_deinit(void *priv)
{
	struct ultibo_driver_data *drv = priv;

	os_free(drv);
}


static int ultibo_driver_send_ether(void *priv, const u8 *dst, const u8 *src,
				  u16 proto, const u8 *data, size_t data_len)
{
	printf("ultibo driver send ether\n");
	return 0;
}


static void * ultibo_driver_init(void *ctx, const char *ifname)
{
	struct ultibo_driver_data *drv;

	drv = os_zalloc(sizeof(struct ultibo_driver_data));
	if (drv == NULL) {
		wpa_printf(MSG_ERROR, "Could not allocate memory for none "
			   "driver data");
		return NULL;
	}
	drv->ctx = ctx;

	dl_list_init(&scanreslist.list);
	dl_list_init(&ultiborawpacketreceivequeue.list);
	dl_list_init(&ultiborawpacketsendqueue.list);

	return drv;
}


static void ultibo_driver_deinit(void *priv)
{
	struct ultibo_driver_data *drv = priv;

	os_free(drv);
}

const u8 *ultibo_driver_getmac(void *priv)
{
   // this pointer is set by the ultibo driver using the SetUltiboMacAddress() function
   // as soon as it is available.
   return MACAddress;
}

void ultibo_driver_new_packet_data(const u8 *src_addr, void *packetbuf, u16 len)
// signal to the driver that new packet data is available.
// the data in the packet buffer must be copied as the original buffer 
// will be freed by the ultibo device driver.
{
	struct ultiborawpacket* rawpacket;

	//if we are finished, throw away any packets being added.
	if (SupplicantOperatingState) 
	  return;

	wpa_printf(MSG_DEBUG, "Ultibodriver: raw packet data received into supplicant queue; len=%d\n", len);
	rawpacket = os_zalloc(sizeof(struct ultiborawpacket));

	os_memmove(rawpacket->packetbuf, packetbuf, len);
	os_memmove(rawpacket->srcaddr, src_addr, ETH_ALEN);
	rawpacket->packetlen = len;

	LockEAPOLPacketQueue("supplicant: new_packet_data");
	dl_list_add_tail(&ultiborawpacketreceivequeue.list, &rawpacket->list);
	UnlockEAPOLPacketQueue();
}

static void wpa_driver_ultibo_scan_timeout (void *eloop_ctx, void *timeout_ctx)
{
	wpa_printf(MSG_INFO, "Ultibodriver: Scan timeout; signalling scan results are ready");

	wpa_supplicant_event (timeout_ctx, EVENT_SCAN_RESULTS, 0);
}


void UltiboScanResultCallback(char *ssid, void *scanres)
// Scan result callback. Comes from ultibo.
// add the scan result to a list ready for processing later.
{
	struct brcmf_escan_result_le *escanres;
	struct scanres_list_item *scanlistitem;

	escanres = (struct brcmf_escan_result_le*)scanres;

	//now we need to add this scan result to a list.
	scanlistitem = os_zalloc(sizeof(struct scanres_list_item) + escanres->bss_info_le.ie_length);
	os_memmove(&scanlistitem->scanres, escanres, sizeof(struct brcmf_escan_result_le) + escanres->bss_info_le.ie_length);
	dl_list_add_tail(&scanreslist.list, &scanlistitem->list);

	wpa_hexdump(MSG_EXCESSIVE, "ultibo data from scan",  ((u8 *) &escanres->bss_info_le + escanres->bss_info_le.ie_offset), escanres->bss_info_le.ie_length);
}

int ultibo_driver_initiate_scan(void *priv, struct wpa_driver_scan_params *params)
{
	struct ultibo_driver_data *drv = priv;
	struct scanres_list_item *scanresitem, *tmp;

	// clear out any existing list
	dl_list_for_each_safe(scanresitem, tmp, &(scanreslist.list), struct scanres_list_item, list)
	{
		dl_list_del(&scanresitem->list);
	}

	wpa_printf(MSG_DEBUG, "Ultibodriver: initiate_scan setting up timeout callback for scan completion");

	// set up timeout for scan completion (calls back the timeout handler function)
	eloop_cancel_timeout (wpa_driver_ultibo_scan_timeout, priv, NULL);
	eloop_register_timeout (SCAN_DURATION_SECS, 0, wpa_driver_ultibo_scan_timeout,
				drv, drv->ctx);

	WirelessScan(&UltiboScanResultCallback, SCAN_DURATION_SECS * 1000);

	return(0);
}

static struct wpa_scan_results *wpa_driver_ultibo_get_scan_results2 (void *priv)
{

	wpa_printf(MSG_DEBUG, "Ultino: get_scan_results2 processing the list of scan results");

	struct scanres_list_item *tmp;
	struct wpa_scan_res **res_vector =
		(struct wpa_scan_res **) os_zalloc (MAX_SCAN_RESULTS * sizeof (struct wpa_scan_res *));
	if (res_vector == 0)
	{
		return 0;
	}

	struct wpa_scan_results *results = (struct wpa_scan_results *) os_zalloc (sizeof (struct wpa_scan_results));
	if (results == 0)
	{
		os_free (res_vector);

		return 0;
	}
	results->res = res_vector;
	results->num = 0;

	unsigned len;
	u8 buf[FRAME_BUFFER_SIZE];
	const u8* ssid;

	// now we have to dequeue the list of scan results we created.
	wpa_printf(MSG_MSGDUMP, "Ultibodriver: scan results dump");
	dl_list_for_each(tmp, &(scanreslist.list), struct scanres_list_item, list) 
	{
		struct brcmf_escan_result_le *scan_res = &tmp->scanres;

		struct brcmf_bss_info_le *bss = &scan_res->bss_info_le;
		for (unsigned i = 0; i < scan_res->bss_count; i++)
		{
			assert (bss->version == BRCMF_BSS_INFO_VERSION);

			int freq = chanspec2freq (bss->chanspec);
			if (freq <= 0)
			{
				continue;
			}

			struct wpa_scan_res *res = (struct wpa_scan_res *) os_malloc (sizeof (struct wpa_scan_res) + bss->ie_length);
			if (res == 0)
			{
				break;
			}

			os_memset (res, 0, sizeof *res);

			res->flags = WPA_SCAN_LEVEL_DBM | WPA_SCAN_QUAL_INVALID;
			os_memcpy (res->bssid, bss->BSSID, ETH_ALEN);
			res->freq = freq;
			res->beacon_int = bss->beacon_period;
			res->caps = bss->capability;
			res->noise = bss->phy_noise;
			res->level = bss->RSSI;
			// TODO: set res->tsf
			// TODO: set res->age

			// append IEs
			res->ie_len = bss->ie_length;
			os_memcpy ((u8 *) res + sizeof *res, (u8 *) bss + bss->ie_offset,
				   bss->ie_length);

			wpa_hexdump(MSG_MSGDUMP, "Ultibodriver: Result from scan",  (u8 *) res, bss->ie_length);

			*res_vector++ = res;
			results->num++;

			bss = (struct brcmf_bss_info_le *) ((u8 *) bss + bss->length);
		}
	}

	return results;
}

int wpa_driver_ultibo_associate(void *priv,
			 struct wpa_driver_associate_params *params)
{
	struct ultibo_driver_data *drv = priv;

	wpa_printf(MSG_DEBUG, "Ultibodriver: Associate with station ssid=%s\n",params->ssid);

	SupplicantWirelessJoinNetwork(params->ssid, params->wpa_ie, params->wpa_ie_len, params->bssid, 1);

	os_memmove(drv->bssid, params->bssid, 6);
	drv->ssid_len = params->ssid_len;
	os_memmove(drv->ssid, params->ssid, 32);

	wpa_supplicant_event (drv->ctx, EVENT_ASSOC, 0);

	return 0;
}

static int wpa_driver_ultibo_get_bssid (void *priv, u8 *bssid)
{
	struct ultibo_driver_data *drv = (struct ultibo_driver_data *)priv;

	os_memmove(bssid, &drv->bssid[0], 6);

	return 0;
}

static int wpa_driver_ultibo_get_ssid (void *priv, u8 *ssid)
{
	struct ultibo_driver_data *drv = (struct ultibo_driver_data *) priv;

	if (drv->ssid_len > 0)
	{
		os_memcpy(ssid, drv->ssid, drv->ssid_len);
	}

	return drv->ssid_len;
}


static int ultibo_driver_set_key(const char *ifname, void *priv, enum wpa_alg alg,
								const u8 *addr, int key_idx, int set_tx,
								const u8 *seq, size_t seq_len,
								const u8 *key, size_t key_len)
	
{
	return UltiboSetKey(ifname, priv, alg, addr, key_idx, set_tx, seq, seq_len, key, key_len);
}

int ultibo_set_operstate(void *priv, int state)
{
	wpa_printf(MSG_DEBUG, "Ultibodriver: Set operating state value=%d", state);

	SupplicantOperatingState = state;
	if (SupplicantOperatingState)
	{
		struct ultiborawpacket *rawpacket;

		// delete any remaing L2 packets from the input queue
		LockEAPOLPacketQueue("supplicant: set_operstate");

		while ((rawpacket = dl_list_first(&ultiborawpacketreceivequeue.list, struct ultiborawpacket, list)))
		{
			dl_list_del(&rawpacket->list);

			if (rawpacket != &ultiborawpacketreceivequeue)		
				os_free(rawpacket);
		}

		UnlockEAPOLPacketQueue();

		UltiboEAPOLComplete();
	}
	return 0;
}

extern int UltiboSetCountry(void *priv, const char *alpha2);



const struct wpa_driver_ops wpa_driver_ultibo_ops = {
	.name = "Ultibo",
	.desc = "Ultibo Driver",
	.hapd_init = ultibo_driver_hapd_init,
	.hapd_deinit = ultibo_driver_hapd_deinit,
	.send_ether = ultibo_driver_send_ether,
	.init = ultibo_driver_init,
	.deinit = ultibo_driver_deinit,
	.get_mac_addr = ultibo_driver_getmac,
	.scan2 = ultibo_driver_initiate_scan,
	.get_scan_results2 = wpa_driver_ultibo_get_scan_results2,
	.associate = wpa_driver_ultibo_associate,
	.get_bssid = wpa_driver_ultibo_get_bssid,
	.get_ssid = wpa_driver_ultibo_get_ssid,
	.set_key = ultibo_driver_set_key,
	.set_operstate = ultibo_set_operstate,
	.set_country = UltiboSetCountry,

};
