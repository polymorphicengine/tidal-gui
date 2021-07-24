const controlExtraKeys = {"Ctrl-.": hush,
			   "Ctrl-Enter": evaluate,
			   "Ctrl-Up": upFocus,
			   "Ctrl-D": openDocs,
			   "Ctrl-1": mute1,
			   "Ctrl-2": mute2,
			   "Ctrl-3": mute3,
			   "Ctrl-4": mute4,
			   "Ctrl-5": mute5,
			   "Ctrl-6": mute6,
			   "Ctrl-7": mute7,
			   "Ctrl-8": mute8,
			   "Ctrl-9": mute9,
			   "Ctrl-S": controlSaveFile,
			   "Ctrl-O": controlLoadFile,
			   "Ctrl-/": 'toggleComment'}

const definitionsExtraKeys = {"Ctrl-.": hush,
			"Ctrl-Enter": evaluate,
			"Ctrl-Down": downFocus,
			"Ctrl-D": openDocs,
			"Ctrl-1": mute1,
			"Ctrl-2": mute2,
			"Ctrl-3": mute3,
			"Ctrl-4": mute4,
			"Ctrl-5": mute5,
			"Ctrl-6": mute6,
			"Ctrl-7": mute7,
			"Ctrl-8": mute8,
			"Ctrl-9": mute9,
			"Ctrl-/": 'toggleComment'}

const controlEditorSettings = {lineNumbers: true,
			 	mode: "haskell",
			 	extraKeys: controlExtraKeys
			       }


const definitionsEditorSettings = {lineNumbers: true,
			 	    mode: "haskell",
			 	    extraKeys: definitionsExtraKeys
			 	   }
