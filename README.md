Ultibo WIFI Device Driver for onboard Cypress WIFI chips
----------------------------------------------------------

This is an Ultibo application and device driver which enables support for the internal WIFI
device (a Cypress 43445 chip). It supports WPA2/PSK only at the moment, and does not
allow connection to open networks.

Prerequisites for building and running the demo kernel:
1. You must boot from a USB drive
2. Pi3b+ or Pi zero i.e. Must be a device with onboard wifi support (*not* a USB wifi device)
3. USB drive must contain c:\firmware with the correct firmware file in it (see repo folder)

With the demo kernel up and running you should be able to ping the device and you
will also be able to telnet to it and use Ultibo's standard set of shell commands.
This includes the ability to upgrade the kernel if you set up a local web server and point
your cmdline.txt to it in the usual way (see Ultibo website for details on how
to do this).

Latest changes cover the ability to automatically reconnect to the network if the
connection is lost, plus the ability to have either blocking or non-blocking
connection. The reconnect is always background (non-blocking).

Kernel images are compiled with a blocking initial connect but will work fine if
you change the relveant parameter to non-blocking in the connect call and recompile.

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
The initialisation process is derived from various sources including the broadcom
full mac driver, the plan9 driver, and the cypress wifi host driver.

Using the Device Driver in your applications
--------------------------------------------

See the contents of wifi.lpr for how this is currently done. It could change in
the future as presently the devices are being created outside of the unit
initialization process in order to manage the dependency on the C: drive
for firmware loading.
