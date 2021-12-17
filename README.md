Ultibo/Raspberry Pi WIFI Device Driver for onboard Cypress WIFI chips
----------------------------------------------------------

This is an Ultibo application and device driver which enables support for the *internal* WIFI
device (a Cypress 43445 based chip). It supports WPA2/PSK encryption and additionally allows
connection to an open network (although this is not generally recommended for security reasons).

The device driver currently supports Pi ZeroW, PiZero2W, Pi3, Pi4, and Pi400. It also supports compute
module devices with onboard WIFI although these have only been tested by others as I do not have any
on my test bench.

USB WIFI devices are *not* supported.

This readme refers to two versions of the WIFI component used for negotiating encyption
keys with a WPA2 secured router:
* Firmware supplicant: This is provided by Broadcom as part of the WIFI subsystem.
* Software supplicant: This is the more traditional supplicant that is present on
most Linux platforms (and others e.g. windows).

## Device Support:
* Pi Zero - both the firmware supplicant and the software supplicant are supported.
* Pi Zero2W - the software supplicant is required to connect to an encrypted network.
If not using the software supplicant, only an open network can be used because the Broadcom WIFI
firmware for this device does not include a firmware supplicant.
* Pi3b and 3b+ - both the firmware supplicant and the software supplicant are supported.
* Pi4 - both the firmware supplicant and the software supplicant are supported.
* Pi400 - the software supplicant is required to connect to an encrypted network.
If not using the software supplicant, only an open network can be used because the Broadcom WIFI
firmware for this device does not include a firmware supplicant.


### Demo kernel
The demo kernel will connect to the WIFI network you have configured and it
contains telnet access, so you can use Ultibo's standard set of shell commands if you
telnet to it (port 23). This includes the ability to upgrade the kernel if you set up
a local web server and point your cmdline.txt to it in the usual way (see Ultibo website
for details on how to do this).
I have also enabled the webstatus unit so that you can connect a browser to the IP address
and view system information over the wifi network. See Ultibo website for details on this also.

If you are using the firmware supplicant, I have added some support for selecting a
specific network via a combination of the SSID (the usual router name) and the BSSID
(a MAC address). This enables a specific network address to be targetted in situations
where a router broadcasts the same SSID for two different frequency bands (e.g. 5Ghz and 2.4Ghz).
The two bands will likely have separate BSSIDs to support that.

For the software supplicant, all configuration is done via c:\wpa_supplicant.conf.
A blank conf file is included in the demo kernel download. The software supplicant
version used is version 2.9, so you may refer to the Linux supplicant documentation
for details on additional configuration options. I have not tested anything
but the most basic configuration of router id and password.

*Please note that this device driver has no support for enabling the device as
an access point at this time (regardless of which supplicant is being used) and therefore
any configuration that is valid for Linux in this regard definitely will not work*

Kernel images are compiled with a blocking initial connect but will work fine if
you change the relevant parameter to non-blocking in the connect call and recompile.

To get access to the demo binaries without needing to recompile anything, download
the demo zip file asset from the release section. Note there is no release for the develop
branch, so people requiring early access will need to build from source and use the
blank wpa_supplicant.conf file found in the repo for configuration.

Note that the demo kernels are now all compiled with the software supplicant because
this keeps the configuration consistent across all devices.



Installation of the Demo Kernels
--------------------------------
1. Download the ZIP file from the releases area (links on the right of this page when
displayed on github.com). 

2. Extract the contents of the zip file to an empty SD card. Be sure to maintain the 
directory structure i.e. retain the firmware folder and its contents.

3. Edit the c:\wpa_supplicant.conf file, fill in the router name and password into the
spaces provided, then boot the device. Your router name is likely to be case sensitive.



Using the demo kernel with the firmware supplicant
---------------------------------------------------
You must recompile the source to switch the software supplicant off and hence enable the
firmware supplicant. To do this, ensure you build *without* the "supplicant" compiler
directive. You will find this in the project settings.

To log on to a network with the firmware supplicant you need to specify your router
settings in the cmdline.txt file that is part of the demo image. Add the name of your router,
the password, and your ISO country code.

    SSID=<ssid name> KEY=<passphrase> COUNTRY=<2 char ISO country code>

For an open network, KEY= can be left empty (or not included). Use of open networks
is obviously not recommended but is necessary for Pi400 and PiZero2W when the firmware
supplicant is being used.

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
At the time of writing this is Ultibo Core  2.1.079 but anything newer than this is fine.

The application will compile in Ultibo Lazarus if you open the wifi project and have
met the necessary Core version requirements.

Compiling with support for the supplicant
-----------------------------------------
By default, supplicant support is not included unless you define the compiler directive
'supplicant'. In order to build with the supplicant the repo contains pre-built
supplicant libraries for each of the target processors, hence you won't need to compile
any of the C code to get started. There is a condtional define at the top of the wifidevice.pas
file that deals with including the correct library version.

If for any reason you want to actually build the supplicant library from source,
there are makefiles in the wpasupplicant subdirectory of the repo. Building the supplicant
requires issuing one or more of the following commands, depending upon which
device you are targeting:

```
make -f Makefile.Ultibo libwpa_supplicant_pizero.a
make -f Makefile.Ultibo libwpa_supplicant_pi3.a
make -f Makefile.Ultibo libwpa_supplicant_pi4.a
```

Use Pi4 for a Pi400 as well, and use pi3 for a PiZero2w.
The resultant libraries are placed in the relevant device related subfolder ready for
compiling into an application via the wifidevice unit. You won't need to copy them
to any other location when compiling your Ultibo application.

Using the Device Driver in your applications
--------------------------------------------

See the contents of wifi.lpr for how this is currently done. It will change in
the future as presently the devices are being created outside of the Ultibo
initialization process in order to manage the dependency on the C: drive
for firmware loading. This part is important - the C: drive must be registered
before initiaising the device otherwise the firmware upload to the WIFI chip will
fail.

wifi.lpr currently contains code to activate both the firmware and software based
supplicants. If you are building your own application you won't need both capabilities
so just pick the code out for the relevant version.

Note that firmware or software based supplicant is a compile time choice; there is
no capability to switch between them at runtime.
