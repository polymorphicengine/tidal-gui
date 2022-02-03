const keyMap = "default" // possible options: "default", "emacs", "vim", "sublime"

const controlExtraKeys = {
				 "Ctrl-.": hush,
			   "Ctrl-Enter": evaluateBlock,
			   "Shift-Enter": evaluateLine,
			   "Ctrl-D": openDocs,
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
			   "Ctrl--": decreaseFontSize,
			   "Ctrl-Up": swapLineUp,
			   "Ctrl-Down": swapLineDown,
			   "Shift-Ctrl-D": duplicateLine
			 }


const controlEditorSettings = {lineNumbers: true,
			 	mode: "haskell",
				theme: "theme",
			 	keyMap: keyMap,
			 	extraKeys: controlExtraKeys,
			 	matchBrackets: true,
			 	autoCloseBrackets: true
			 }
