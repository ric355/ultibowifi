Ultibo WIFI Device Driver for onboard Cypress WIFI chips
----------------------------------------------------------

This is an Ultibo application and device driver which enables support for the internal WIFI
device (a Cypress 43445 chip). It supports WPA2/PSK only at the moment. Connection to open
networks is now allowed but not recommended. See device support later for why this is.
The device driver currently supports Pi Zero, Pi3, Pi4, and Pi400.

### Pi Zero, Pi Zero2W, Pi3b, Pi4 and Pi400 - prerequisites for building and running the demo kernel
1. Must be a device with *onboard* wifi support (*not* a USB wifi device)
2. boot device must contain c:\firmware with the correct firmware file in it (see repo folder)

### Demo kernel
With the demo kernel up and running you should be able to ping the device and you
will also be able to telnet to it and use Ultibo's standard set of shell commands.
This includes the ability to upgrade the kernel if you set up a local web server and point
your cmdline.txt to it in the usual way (see Ultibo website for details on how
to do this).  I have also enabled the webstatus unit so that you can connect a browser
to the IP address and view system information over the wifi network. See Ultibo website
for details on this also.

I have now added some support for selecting a specific network via a combination of the
SSID (the usual router name) and the BSSID (a MAC address). This enables a specific network
address to be targetted in situations where a router broadcasts the same SSID for
two different frequency bands (e.g. 5Ghz and 2.4Ghz). The two bands will likely have separate
BSSIDs to support that.

Kernel images are compiled with a blocking initial connect but will work fine if
you change the relevant parameter to non-blocking in the connect call and recompile.

Device Support
--------------
As already mentioned this device driver supports all Pi devices with an onboard wifi chip.
However, in the case of the Pi400 and the Pi Zero2W only connections to an open
network are supported. This is because earlier Wifi firmware contains a WPA_SUPPLICANT
directly within the firmware but this is not included in the pi400 and zero2w firmware
and therefore a driver based supplicant needs to be added. Use open networks at your own risk!

Running
-------
I have added some pre-built kernels to the repo. There is a kernel7l.img (Pi4),
kernel7.img (Pi3 and zero2w) and a kernel.img (Pi Zero).
They will run through the initialisation above, scan for networks
displaying some information on screen about what is found, and ultimately join the
network you specify in the cmdline.txt file. You should see an IP address in the
top console window once you are connected.

Installing on Pi Zero, Zero2W, Pi3, and Pi4
-------------------------------------------
Put the kernel on your boot device in the usual manner, alongside all the other
standard boot files.

You will need to place the firmware folder into c:\ so it shows as c:\firmware
on the boot media as well.

Finally, if you want to actually fully log on to a network then
you must alter cmdline.txt to add the following:

    SSID=<ssid name> KEY=<passphrase> COUNTRY=<country code>

For an open network, KEY= can be left empty (or not included). Use of open networks
is obviously not recommended.

If you want to see a scan for base stations, add

    WIFISCAN=1

if you want to select a specific network interface, then in addition to the SSID you
can specify the BSSID viz;

    BSSID=a1:23:e5:c6:f3:e0

You must use the exact format given above although case of the characters doesn't matter.

Note that the SSID is most likely case sensitive so copy your router's ID exactly.

There is a possibility it won't work on a given pi if the firmware needed is
not present. The chip id will show what it is looking for. Let me know if there
is an id that is not supported.

Compiling
---------

You must update to the latest ultibo core to successfully compile this software.
At the time of writing this is Ultibo Core  2.1.079.

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

