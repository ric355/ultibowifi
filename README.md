Ultibo WIFI Deivice Driver for onboard Cypress WIFI chips
----------------------------------------------------------

Prerequisites for running this app:
1. You must boot from a USB drive
2. Pi3b+  (unless you change the hard coded filename to the right one for your device)
3. Must be a device with onboard wifi support (*not* a USB wifi device)
4. USB drive must contain c:\firmware with the correct firmware file in it
4. You must have the custom mmc.pas file built in to your RTL

This is a very rough and ready Ultibo application which creates a wifi device and attempts
to get the Arasan controller to talk to the Cypress WFI chip.
There are a lot of issues with it at the moment but it goes through the motions of
creating the necessary devices, initialising the Cypress WIFI chip (that stuff
does work) and attempting to upload the firmware to it.
It may work on a pi zero to some extent, but the firmware it loads is a specific
pi3b+ version (id 0x4345 rev 6) at the moment so that would need to be changed
for other devices (not that it matters at this point because the firmware doesn't
actually load properly yet!). See the hard coded filename in WIFIDeviceDownloadFirmware.

In the overrides file, there are three overrides:

MMC_AUTO_DEVICE_CREATE := False;
MMC_AUTOSTART := False;
BCM2710_REGISTER_SDHCI := False;

The first of these is a custom setting and is something that will need to be resolved
in due course. It prevents the mmc.pas unit from creating the MMC device automatically
as this prevents the WIFI device from working. This is needed because this code
uses the SDHCI device code in mmc.pas, and that code automatically creates the MMC
device by default when SDHCIHostStart is called.
That means the mmc.pas file in this repo need to be compiled into the RTL instead
of the standard one.

Within this file you will find code that creates the wifi device, and code that
registers and starts an SDHCI device. The SDHCI device is created here because
it gives us better control over Arasan controller; it requires various GPIO's to be
set differently to connect it to the Cypress chip instead of to the MMC device.
This is another thing that needs a proper solution in due course.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy (and not well understood if I'm honest!) initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

At present the system can identify and select the wifi chip, read the chip id,
scan the cores to establish key addresses, scan the ram for similar purpose,
upload firmware to the device, and re-download the firmware for verification
purposes.

Unfortunately there are some rather nasty problems to address which appear to
be around the interrupt handler for cmd53. It will frequently stop working at
random points once cmd53 starts being used. So far it has resisted all attempts
to find the root cause of the problem as any attempt at logging tends to make
the crashing worse. This is possibly down to non-threadsafe logging functions
and their incompatibility with the interrupt handler, but I have tried
to fix this issue using what I think is the the correct splinlock calls, to no avail,
so not sure what the exact problem is.

I am fairly sure there is some sort of memory scribble going on as occasionally
the system will generate an unhandled exception at 0x00000000 from the platform
unit, although this is pretty rare. It usually simply stops, to the extent that
wired network access is lost. Therefore it is likely that either the interrupt
handler has crashed or has deadlocked in some way. Threads not using
interrupts do keep on running when it fails - I had a thread flashing the onboard
LED and that does not die.

Note that this application changes the update path for the kernel. This is
specific to my own development platform where I run a dual boot system sometimes
(I just didn't want to restructure the existing files on the drive so used the same
location).
You will need to change this if you want to do remote kernel updates to the root folder.
Just delete this line;
SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
From the main program below.

