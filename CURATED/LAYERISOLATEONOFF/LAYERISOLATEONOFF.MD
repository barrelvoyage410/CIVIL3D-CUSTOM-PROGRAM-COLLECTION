LayerIsolateOnOff.lsp with its LIO and LUO commands is similar to AutoCAD's LAYISO and LAYUNISO, except:

A)  If you change something about a Layer [such as its color or linetype] between using LIO and LUO, it leaves that changed  rather than reverting [as LAYUNISO did at least at the time I wrote this -- that may be fixed in later versions] -- it deals only  in the On/Off condition of the Layers involved.

B)  It nests -- you can isolate some Layers, and then further isolate fewer of them with another LIO command, and even further down to even fewer, as many levels deep as you want to go, and then successive LUO commands will "step back" through each successive combination of turning Layers back On in reverse order [LAYUNISO can step back only once].