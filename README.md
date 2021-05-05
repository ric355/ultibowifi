Ultibo WIFI Device Driver for onboard Cypress WIFI chips
----------------------------------------------------------

Prerequisites for building and running this app:
1. You must boot from a USB drive
2. Pi3b+ or Pi zero i.e. Must be a device with onboard wifi support (*not* a USB wifi device)
3. USB drive must contain c:\firmware with the correct firmware file in it

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
- Full IP traffic is working although not guaranteed totally stable.

Once up and running you should be able to ping the device and you will also be
able to telnet to it and use Ultibo's standard set of shell commands. This includes
the ability to upgrade the kernel if you set up a local web server and point
your cmdline.txt to it in the usual way (see Ultibo website for details on how
to do this).

At the moment I'm verifying the work done to integrate the Ultibo buffer API is
correct, and looking out for all of the complications I haven't properly considered
but as a concept it basically works. The next items to look at are how to handle loss of
connection and subsequent reconnection.


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

and if you want to see a scan for base stations, add
    WIFISCAN=1

Note that the SSID is most likely case sensitive so copy your router's ID exactly.

The code does not attempt to connect to open networks and is only enabled for
WPA2/PSK.

There is a possibility it won't work on a given pi if the firmware needed is
not present. The chip id will show what it is looking for. Let me know if there
is an id that is not supported.

Compiling
---------

Previous versions of this repo had some custom RTL files. These can now be removed.
If you previously compiled the RTL with the custom mmc.pas and globalconfig.pas
files that were in this repo (now deleted) then put the RTL back to standard and
recompile it to remove those customisations.

The application should compile in Ultibo Lazarus if you open the wifi project.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

Note previous pushes to this repo changed the kernel path to a place other than
the root folder. This is not the case anymore; the kernel is back in the root folder.
