Ultibo WIFI Deivice Driver for onboard Cypress WIFI chips
----------------------------------------------------------

Prerequisites for building and running this app:
1. You must boot from a USB drive
2. Pi3b+ or Pi zero i.e. Must be a device with onboard wifi support (*not* a USB wifi device)
3. USB drive must contain c:\firmware with the correct firmware file in it
4. You must have the custom mmc.pas file that is in this repo built in to your RTL

This is a very rough and ready Ultibo application which creates a wifi device and attempts
to get the Arasan controller to talk to the Cypress WFI chip.

Latest capabilities:
- initialise Arasan controller for wifi (diconnects eMMC)
- initialise cypress chip via SDIO
- scan cores and ram to establish key addresses
- Load firmware to cypress chip
- Load configuration to cypress chip
- Load regulatory blob to cypress chip
- Set the country code and other basic settings
- Scan for wireless networks
- Join a network

Last change was to introduce a worker thread which will handle all of the network
traffic. It probably needs a bit of work yet but it supports the means to signal
when an IOCTL response has been received, and for consumers to register interest
in events and receive callbacks when they occur.

There is no IP layer implemented yet so no means to pass any traffic. Getting
close to that point though. A lot of that is part of Ultibo so it's going to
be about getting the relevant glue built to tie it all together.

Running
-------
I have added some pre-built kernels to the repo. There is a kernel7.img and a
kernel.img. They will run through the initialisation above and scan for networks
displaying some information on screen about what is found.
Put the kernel on a USB drive in the usual manner and use it to boot from.
You *must* use a USB drve; it won't work via the SD card slot at the moment
because we haven't yet migrated the SD support to work via the other SD Host controller
(since the Arasan is now being used for WIFI).

You will need to place the firmware folder into c:\ so it shows as c:\firmware
on the usb drive as well.

Finally, if you want to 'join' a network (not that it means much at present) then
you must alter cmdline.txt to add SSID=<ssid name> and KEY=<passphrase>
The software will execute commands to join the network but as no traffic can
pass yet there is no way of knowing it was successful. That is kinda next on the
list but there's a bunch of things to be built before we'll be able to test it
properly.

There is a possibility it won't work on a given pi if the firmware needed is
not present. The chip id will show what it is looking for. Let me know if there
is an id that is not supported.

Compiling
---------
In the overrides file, there are three overrides:

MMC_AUTO_DEVICE_CREATE := False;
MMC_AUTOSTART := False;
BCM2710_REGISTER_SDHCI := False;

The first of these is a custom setting and is something that will need to be resolved
in due course. It prevents the mmc.pas unit from creating the MMC device automatically
as this prevents the WIFI device from working. This is needed because this code
uses the SDHCI device code in mmc.pas, and that code automatically creates the MMC
device by default when SDHCIHostStart is called.
That means the mmc.pas file in this repo needs to be compiled into the RTL instead
of the standard one.

You must also compile the RTL with the define IRQ_DEBUG (set in globaldefines.inc)
otherwise the logging system will not work and the app will crash at random points
(unless you do not compile with any of the debug logging capabilities in mmc.pas).

Within the project file you will find code that creates the wifi device, and code that
registers and starts the SDHCI device. It won't look like this eventually; it's
just stuff that has to be done until it is properly integrated into the core.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy (and not well understood if I'm honest!) initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

Note that this application changes the update path for the kernel. This is
specific to my own development platform where I run a dual boot system sometimes
(I just didn't want to restructure the existing files on the drive so used the same
location).
You will need to change this if you want to do remote kernel updates to the root folder.
Just delete this line;
SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
From the main program.
Alternatively instead of using 'update get kernel' use 'update get file kernel7.img /c'
when in the root directory.
