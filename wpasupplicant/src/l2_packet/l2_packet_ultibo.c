/*
 * WPA Supplicant - Layer2 packet handling example with dummy functions
 * Copyright (c) 2003-2005, Jouni Malinen <j@w1.fi>
 *
 * This software may be distributed under the terms of the BSD license.
 * See README for more details.
 *
 * This file can be used as a starting point for layer2 packet implementation.
 */

#include "includes.h"

#include "common.h"
#include "eloop.h"
#include "l2_packet.h"
#include "drivers/driver_ultibo.h"
#include "list.h"

extern struct ultiborawpacket ultiborawpacketreceivequeue;
extern struct ultiborawpacket ultiborawpacketsendqueue;
extern int SupplicantOperatingState;

#define L2_HEADER_LEN 14


struct l2_packet_data {
	char ifname[17];
	u8 own_addr[ETH_ALEN];
	void (*rx_callback)(void *ctx, const u8 *src_addr,
			    const u8 *buf, size_t len);
	void *rx_callback_ctx;
	int l2_hdr; /* whether to include layer 2 (Ethernet) header data
		     * buffers */
	int fd;
};


int l2_packet_get_own_addr(struct l2_packet_data *l2, u8 *addr)
{
	os_memcpy(addr, l2->own_addr, ETH_ALEN);
	return 0;
}


int l2_packet_send(struct l2_packet_data *l2, const u8 *dst_addr, u16 proto,
		   const u8 *buf, size_t len)
{
	if (l2 == NULL)
		return -1;

	// instead of calling this direct, we add the packet to a linked list and
	// pull it off later, to avoid issues with deadlock.

	struct ultiborawpacket* rawpacket;

	rawpacket = os_zalloc(sizeof(struct ultiborawpacket));
	os_memmove(rawpacket->srcaddr, dst_addr, ETH_ALEN);

    //add the l2 header where needed
	if (! l2->l2_hdr)
	{
		rawpacket->packetlen = len + L2_HEADER_LEN;
		os_memmove(rawpacket->packetbuf, rawpacket->srcaddr, 6); //srcaddr here is actuall the dst_addr
		os_memmove(rawpacket->packetbuf+6, l2->own_addr, 6);
		os_memmove(rawpacket->packetbuf+12, &proto, sizeof(u16));
   		os_memmove(rawpacket->packetbuf+L2_HEADER_LEN, buf, len);
	}
	else
	{
		os_memmove(rawpacket->packetbuf, buf, len);
  		rawpacket->packetlen = len;
	}

	LockSendPacketQueue();
	dl_list_add_tail(&ultiborawpacketsendqueue.list, &rawpacket->list);
	UnlockSendPacketQueue();

	wpa_printf(MSG_DEBUG, "Ultibodriver: Raw packet received from device driver");

	return 0;
}


static void l2_packet_receive(int sock, void *eloop_ctx, void *sock_ctx)
{
	struct l2_packet_data *l2 = eloop_ctx;

	/* dequeue our raw packets from the list.*/
	
	struct ultiborawpacket *rawpacket;

	LockEAPOLPacketQueue();

	while ((rawpacket = dl_list_first(&ultiborawpacketreceivequeue.list, struct ultiborawpacket, list)))
	{
		dl_list_del(&rawpacket->list);

		// packet is only passed in if the connection is no finalized
		// otherwise we simply dispose of it.
		if (! SupplicantOperatingState)
			l2->rx_callback(l2->rx_callback_ctx, rawpacket->srcaddr,
				rawpacket->packetbuf, rawpacket->packetlen);

		if (rawpacket != &ultiborawpacketreceivequeue)		
		  os_free(rawpacket);
		else
		  wpa_printf(MSG_ERROR, "Ultibodriver: ERROR - can't free packet otherwise we'd dispose of the queue");
	}

	UnlockEAPOLPacketQueue();
}

static void l2_send_packet_to_ultibo(int sock, void *eloop_ctx, void *sock_ctx)
{
	/* dequeue any send packets from the list and pass them to the device driver in ultibo */
	
	struct ultiborawpacket *rawpacket;

	LockSendPacketQueue();

	while ((rawpacket = dl_list_first(&ultiborawpacketsendqueue.list, struct ultiborawpacket, list)))
	{
		dl_list_del(&rawpacket->list);

		SendSupplicantL2Packet(rawpacket->packetbuf, rawpacket->packetlen);

		if (rawpacket != &ultiborawpacketsendqueue)		
		  os_free(rawpacket);
		else
		  wpa_printf(MSG_ERROR, "Ultibodriver: ERROR - can't free packet otherwise we'd dispose of the queue");
	}

	UnlockSendPacketQueue();	


}



struct l2_packet_data * l2_packet_init(
	const char *ifname, const u8 *own_addr, unsigned short protocol,
	void (*rx_callback)(void *ctx, const u8 *src_addr,
			    const u8 *buf, size_t len),
	void *rx_callback_ctx, int l2_hdr)
{
	struct l2_packet_data *l2;

	wpa_printf(MSG_DEBUG, "Ultibodriver: L2 Packet Init");

	l2 = os_zalloc(sizeof(struct l2_packet_data));
	if (l2 == NULL)
		return NULL;
	os_strlcpy(l2->ifname, ifname, sizeof(l2->ifname));
	l2->rx_callback = rx_callback;
	l2->rx_callback_ctx = rx_callback_ctx;
	l2->l2_hdr = l2_hdr;

	os_memmove(l2->own_addr, own_addr, 6);

	eloop_register_read_sock(l2->fd, l2_packet_receive, l2, NULL);
	eloop_register_write_sock(l2->fd, l2_send_packet_to_ultibo, l2, NULL);

	return l2;
}


struct l2_packet_data * l2_packet_init_bridge(
	const char *br_ifname, const char *ifname, const u8 *own_addr,
	unsigned short protocol,
	void (*rx_callback)(void *ctx, const u8 *src_addr,
			    const u8 *buf, size_t len),
	void *rx_callback_ctx, int l2_hdr)
{
	return l2_packet_init(br_ifname, own_addr, protocol, rx_callback,
			      rx_callback_ctx, l2_hdr);
}


void l2_packet_deinit(struct l2_packet_data *l2)
{
	if (l2 == NULL)
		return;

	if (l2->fd >= 0) {
		eloop_unregister_read_sock(l2->fd);
		/* TODO: close connection */
	}
		
	os_free(l2);
}


int l2_packet_get_ip_addr(struct l2_packet_data *l2, char *buf, size_t len)
{
	/* TODO: get interface IP address */
	return -1;
}


void l2_packet_notify_auth_start(struct l2_packet_data *l2)
{
	/* This function can be left empty */
}


int l2_packet_set_packet_filter(struct l2_packet_data *l2,
				enum l2_packet_filter_type type)
{
	return -1;
}

int l2_packet_auth_active (void)
{
	return -1;
}