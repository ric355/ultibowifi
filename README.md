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
It doesn't work fully yet but it goes through the motions of creating the necessary devices,
initialising the Cypress WIFI chip, uploading the firmware and configuration to it, and
restarting the arm core, and sending an IOCTL command to get the device's MAC address.

It may work on a pi zero but I haven't tested it there recently. It does try to select
the right firmware for the device, using the chip id and revision.

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
registers and starts the SDHCI device. Some changes will be needed around this
stuff for the proper Ultibo Core solution.

The guts of what goes on in this application is in the wifidevice source file.
It contains various functions for talking to the Cypress device including the
rather lengthy (and not well understood if I'm honest!) initialisation process.
Some of it is similar to what is being done in the mmc unit but a lot of it is
wifi specific. The initialisation process is derived from various sources including
the broadcom full mac driver, the plan9 driver, and the cypress wifi host driver.

At present the system can identify and select the wifi chip, read the chip id,
scan the cores to establish key addresses, scan the ram for similar purpose,
upload firmware to the device, re-download the firmware for verification
purposes, abd upload the configuration.

The next step it takes is to re-enable the arm core after uploading the
configuration data. At this point the arm core boots the firmware. This enables
use to convince the chip to give up it mak address via a poke in the IOCTL !

We are reaching the point where we need to consider introducing a secondary
thread to handle the function 2 (radio) traffic. At the moment the IOCTL response
is being polled for in the calling thread but this cannot be final solution.


Note that this application changes the update path for the kernel. This is
specific to my own development platform where I run a dual boot system sometimes
(I just didn't want to restructure the existing files on the drive so used the same
location).
You will need to change this if you want to do remote kernel updates to the root folder.
Just delete this line;
SHELL_UPDATE_LOCAL_PATH:= 'c:\clusterkernel\';
From the main program.

