const keyMap = "default" // possible options: "default", "emacs", "vim", "sublime"

const controlExtraKeys = {"Ctrl-.": hush,
			   "Ctrl-Enter": evaluateBlock,
			   "Shift-Enter": evaluateLine,
			   "Ctrl-D": openDocs,
			   "Shift-Ctrl-1": muteH1,
			   "Shift-Ctrl-2": muteH2,
			   "Shift-Ctrl-3": muteH3,
			   "Shift-Ctrl-4": muteH4,
			   "Shift-Ctrl-5": muteH5,
			   "Shift-Ctrl-6": muteH6,
			   "Shift-Ctrl-7": muteH7,
			   "Shift-Ctrl-8": muteH8,
			   "Shift-Ctrl-9": muteH9,
			   "Ctrl-1": muteP1,
			   "Ctrl-2": muteP2,
			   "Ctrl-3": muteP3,
			   "Ctrl-4": muteP4,
			   "Ctrl-5": muteP5,
			   "Ctrl-6": muteP6,
			   "Ctrl-7": muteP7,
			   "Ctrl-8": muteP8,
			   "Ctrl-9": muteP9,
			   "Ctrl-S": controlSaveFile,
			   "Ctrl-O": controlLoadFile,
			   "Ctrl-/": 'toggleComment',
			   "Shift-Ctrl--": increaseFontSize,
			   "Ctrl--": decreaseFontSize}


const controlEditorSettings = {lineNumbers: true,
			 	mode: "haskell",
				theme: "theme",
			 	keyMap: keyMap,
			 	extraKeys: controlExtraKeys,
			 	matchBrackets: true,
			 	autoCloseBrackets: true
			       }


