# game-dragonsky
Send touch and drags to android device or android emulator (such as bluestacks) playing the game DragonSky. Does not auto-play watch ads nor Legion.
* Autoplays merge dragons (avoid merging shinies with regular blacks), pvp, daily bosses+journey+legendary, DimensionGate, search essenses and buy gold+free, search dragons (stops when hitting
elite)
* Enable auto-play pvp in the late evening, though it messes with return, so only if return is not near in time.
* Auto returns, after predined X+Y amount of time. Rushes when done playing for X minutes, and then plays 'final stages' for Y minutes, then returns.
* Control chosen stage in PlayerConfig. See estimated stages to complete before return in gui.

# Notes

This is not intended for public consumption, but it may be useful as a to program with a fun project such as hacking a game. Please don't take the coding standard in this project to heart, I'm not proud of it. The code would benefit from migrating to full build file I think and separate into much more files. You can try running it, and play around with it.

# Usage - How to run
Very brief pointers

Run with http://ammonite.io/

1) Download amm: https://github.com/lihaoyi/Ammonite/releases/download/2.0.4/2.12-2.0.4

2a) On windows rename to amm.cmd, otherwise rename to 'amm'. 

2b) Run with '/src> amm gameDragonSkyAutoPlay.sc gui'. Primarily developed on ammonite 2.13-2.0.1 and seems to run on 2.13-2.0.4

2c) Be in /src to run it, or localdata storage directory cannot be found

It uses ADB for connecting to device. Check `adb devices` to see if your device is visible. Maybe use `adb connect localhost:5555` to connect. Bluestacks emulator use port 5555 +x*10 for device no x (it seems). See `object Adb` in code.

# Usage - How to edit to your needs

- See DeviceSerial.playerAndName for filtering of devices, and mapping deviceId's to PlayerConfig. See 'val ENABLE_KNOWN_DEVICE_FILTER'. 

- Local storage on close is kept in: `val jsonFile: Path = pwd / up / "localdata" / "DragonSky.json"`

- Player config is kept in PlayerConfig, see `val AlexConfig = PlayerConfig(name = Alex,`
- If the game gui changes, see `val point` for how to update pointer locations (for minor gui changes)
  Sometimes adb hangs, therefore I sometimes kill the adb-server via `pskill` which is a windows sysinternals. See `KillAdbThread.sc`



# GUI

There are two rows of buttons pr device.

Start/stop: Click the top-left button with your device serial, to start merging dragons. Click again to stop.

Next Fire/Water/etc: Controls next stage after return

Rush X: Number of rushes to attempt. May rush 1 more if configured in PlayerConfig. 

Buttons in second row start an action such as daily boss fight, or search dragons. It runs these actions first in the loop. If already started, they run on next loop-start. A loop take around half a minute. A loop does return check, play other stuff, then merge and hatch for some time.

The non-gui startup types are just stuff I started with, and never cleanup up.