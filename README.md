Ultibo WIFI Deivice Driver for onboard Cypress WIFI chips
----------------------------------------------------------

Prerequisites for building and running this app:
1. You must boot from a USB drive
2. Pi3b+ or Pi zero i.e. Must be a device with onboard wifi support (*not* a USB wifi device)
3. USB drive must contain c:\firmware with the correct firmware file in it
4. You must have the custom mmc.pas file that is in this repo built in to your RTL

This is a very rough and ready Ultibo application which creates a wifi device and attempts
to get the Arasan controller to talk to the Cypress WFI chip.

Latest capabilities (in the approximate order they occur):
- initialise Arasan controller for wifi (diconnects eMMC)
- initialise cypress chip via SDIO
- scan cores and ram to establish key addresses
- Load firmware to cypress chip
- Load configuration to cypress chip
- Load regulatory blob to cypress chip
- Set the country code and other basic settings
- Scan for wireless networks (optional)
- Join a network
- Get an IP address via DHCP
- Full IP traffic is working (although it will fail from time to time)

At the moment I'm debugging the work done to integrate the Ultibo buffer API and
looking out for all of the complications I haven't taken into proper consideration
but as a concept it basically works.

I may changed the architecture a little but not sure yet. At present it uses the
same thread for both sending and receiving traffic. This is not optimal but it
may not actually make much difference having a thread for each.


Running
-------
I have added some pre-built kernels to the repo. There is a kernel7.img (Pi3) and a
kernel.img (Pi Zero). They will run through the initialisation above, scan for networks
displaying some information on screen about what is found, and ultimately join the
network you specify in the cmdline.txt file. You should see an IP address in the
top console window once you are connected.

Put the kernel on a USB drive in the usual manner and use it to boot from.
You *must* use a USB drve; it won't work via the SD card slot at the moment
because we haven't yet migrated the SD support to work via the other SD Host controller
(since the Arasan, normally used for SD, is now being used for WIFI).

You will need to place the firmware folder into c:\ so it shows as c:\firmware
on the usb drive as well.

Finally, if you want to actually fully log on to a network then
you must alter cmdline.txt to add the following:

SSID=<ssid name> KEY=<passphrase> COUNTRY=<country code>

You must specify the country as this determines what frequencies the WIFI chip
will use to communicate with the router.

There is a possibility it won't work on a given pi if the firmware needed is
not present. The chip id will show what it is looking for. Let me know if there
is an id that is not supported.

Compiling
---------
In the overrides file, there are several overrides:

    MMC_AUTO_DEVICE_CREATE := False;
    MMC_AUTOSTART := False;
    CONSOLE_REGISTER_LOGGING := True;
    CONSOLE_LOGGING_DEFAULT := True;
    CONSOLE_LOGGING_POSITION := CONSOLE_POSITION_BOTTOM;

    WIFI_AUTO_INIT := False;

To support these, there are two files in this repo that need to be compiled into
the RTL;

    mmc.pas
    globalconfig.pas

Without these, the softare won't build. The reason they exist is temporary and is
just because we don't have the necessary changes in the core yet. Final solutions
may be different to those used here.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

Note previous pushes to this repo changed the kernel path to a place other than
the root folder. This is not the case anymore; the kernel is back in the root folder.
