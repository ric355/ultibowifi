/* driver_ultibo.h */

#define FRAME_BUFFER_SIZE 1600

#include <list.h>

struct ultiborawpacket {
	struct dl_list list;
	u8 packetbuf[FRAME_BUFFER_SIZE];
	u16 packetlen;
	u8 srcaddr[ETH_ALEN];
};
