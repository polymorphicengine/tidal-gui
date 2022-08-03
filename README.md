# tidal-gui

Tidal-GUI is, as the name suggests, a general user interface for the live-coding language tidalcycles.

![TidalInterface](https://cdn.discordapp.com/attachments/709321653737357367/1004336914809225236/Bildschirmfoto_von_2022-08-03_11-53-08.png)

The interface has three basic parts: 
- a display for general information relating to tidal like cps/bpm, current cycle and which channels are playing
- one or more editors, where tidal code and other commands can be entered and executed
- a console where errors etc. are displayed

If you are familiar with the atom-plugin for tidal, most of the features of tidal-gui are very similar. There is one notable difference: if you want to print something to the console, you have to make sure to lift it to IO (via return).

Other commands are:
- `:t exp`, to print the type of exp
- :set path, where 'path' is a path to a folder containing .hs files that will be loaded automatically (see below for more information)
- :listen hostname:port, where 'hostname' is the remote host's name and 'port' is the port you want the editor to listen on (see below for more information)
- :hydra code, where 'code' is hydra code

If you set a path to a folder of the definitions, you need to make sure that each file in the folder only contains one single 'let' expression.

You can currently send messages of the following form to the editor:
- /eval, evaluates the current line that is selected in the editor
- /hush, hushes all patterns
- /go/line i x, moves the cursor to line x
- /eval/block i x, evaluates the block at line x
- /eval/line i x, evaluates line x
- /mute s n, mutes/unmutes channel with id n

The first steps in the development of this application have been done during the Haskell Summer of Code.
