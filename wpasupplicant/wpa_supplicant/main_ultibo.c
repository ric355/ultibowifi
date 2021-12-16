#include "includes.h"

#include "common.h"
#include "wpa_supplicant_i.h"

extern int WPASupplicantLogLevel;


int wpa_supplicant_main (const char *confname)
{
	struct wpa_interface iface;
	int exitcode = 0;
	struct wpa_params params;
	struct wpa_global *global;

	memset(&params, 0, sizeof(params));
	params.wpa_debug_level = WPASupplicantLogLevel;
	wpa_debug_level = WPASupplicantLogLevel;

    wpa_printf(MSG_INFO, "Ultibodriver: wpa_supplicant_main started");
	global = wpa_supplicant_init(&params);

	if (global == NULL)
		return -9;

	memset(&iface, 0, sizeof(iface));
	iface.driver = "Ultibo";
	iface.ifname = "wlan0";
	iface.confname = confname;

	if (wpa_supplicant_add_iface(global, &iface, NULL) == NULL)
		exitcode = -8;

	if (exitcode == 0)
		exitcode = wpa_supplicant_run(global);

	wpa_supplicant_deinit(global);

	return exitcode;
}
