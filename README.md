Ultibo/Raspberry Pi WIFI Device Driver for onboard Cypress WIFI chips
----------------------------------------------------------

This is an Ultibo application and device driver which enables support for the internal WIFI
device (a Cypress 43445 chip). It supports WPA2/PSK only at the moment. Connection to open
networks is now allowed but not recommended. See device support later for why this is.
The device driver currently supports Pi ZeroW, Pi3, Pi4, and Pi400.

### Pi ZeroW, Pi Zero2W, Pi3b, Pi4 and Pi400 - prerequisites for running the demo kernel
1. Must be a device with *onboard* wifi support (*not* a USB wifi device)
2. boot device must contain c:\firmware with the correct firmware files in it (see repo folder)

### Demo kernel
With the demo kernel up and running you should be able to ping the device and you
will also be able to telnet to it and use Ultibo's standard set of shell commands.
This includes the ability to upgrade the kernel if you set up a local web server and point
your cmdline.txt to it in the usual way (see Ultibo website for details on how
to do this).  I have also enabled the webstatus unit so that you can connect a browser
to the IP address and view system information over the wifi network. See Ultibo website
for details on this also.

I have added some support for selecting a specific network via a combination of the
SSID (the usual router name) and the BSSID (a MAC address). This enables a specific network
address to be targetted in situations where a router broadcasts the same SSID for
two different frequency bands (e.g. 5Ghz and 2.4Ghz). The two bands will likely have separate
BSSIDs to support that.

Kernel images are compiled with a blocking initial connect but will work fine if
you change the relevant parameter to non-blocking in the connect call and recompile.

New for the latest submission (Nov 2021) is a zip file containing all of the files
required to run the demo binary. To install it, extract all of the files to a blank 
SD card and then configure the cmdline.txt file as described below.

Binaries that were previously part of the main repo will be deleted shortly.

Device Support
--------------
As already mentioned this device driver supports all Pi devices with an onboard wifi chip.
However, in the case of the Pi400 and the Pi Zero2W only connections to an open
network are supported. This is because earlier Wifi firmware contains a WPA_SUPPLICANT
directly within the firmware but this is not included in the pi400 and zero2w firmware
and therefore a driver based supplicant needs to be added. This supplicant has been
developed and will be merged into the master repo in due course.
Use open networks at your own risk!

Installing on Pi Zero, Zero2W, Pi3, and Pi4
-------------------------------------------
1. Download the ZIP file from the releases area (links on the right of this page when
displayed on github.com). 

2. Extract the contents of the zip file to an empty SD card. Be sure to maintain the 
directory structure i.e. retain the firmware folder and its contents.

3. Edit the cmdline.txt file as below, then boot the device.

To log on to a network you need to specify your router settings in the cmdline.txt file
that is part of the demo image. Add the name of your router, the password, and your 
ISO country code.

    SSID=<ssid name> KEY=<passphrase> COUNTRY=<2 char ISO country code>

For an open network, KEY= can be left empty (or not included). Use of open networks
is obviously not recommended but is currently necessary for Pi400 and PiZero2W

If you want to see a scan for base stations, add

    WIFISCAN=1

if you want to select a specific network interface, then in addition to the SSID you
can specify the BSSID viz;

    BSSID=xx:xx:xx:xx:xx:xx

Obviously, replace the xx's with your router interface's MAC address. This setting
is not normally required. You must use the exact format given above although case 
of the characters doesn't matter.

Note that the SSID is most likely case sensitive so copy your router's ID exactly.

Don't forget that cmdline.txt must have all entries on the same line.

Compiling
---------

You must update to the latest ultibo core to successfully compile this software.
At the time of writing this is Ultibo Core  2.1.079.

The application will compile in Ultibo Lazarus if you open the wifi project and have
met the necessary Core version requirements.

Using the Device Driver in your applications
--------------------------------------------

See the contents of wifi.lpr for how this is currently done. It will change in
the future as presently the devices are being created outside of the Ultibo
initialization process in order to manage the dependency on the C: drive
for firmware loading. This part is important - the C: drive must be registered
before initiaising the device otherwise the firmware upload to the WIFI chip will
fail.
