const keyMap = "default" // possible options: "default", "emacs", "vim", "sublime"

const extraKeys = {
	           "Ctrl-.": hush,
		   "Ctrl-Enter": evalB,
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
		   "Ctrl-S": saveFile,
		   "Ctrl-O": loadFile,
		   "Ctrl-A": addEditor,
		   "Ctrl-B": removeEditor,
		   "Ctrl-/": 'toggleComment',
		   "Shift-Ctrl--": increaseFontSize,
		   "Ctrl--": decreaseFontSize,
		   "Ctrl-Up": swapLineUp,
		   "Ctrl-Down": swapLineDown,
		   "Shift-Ctrl-D": duplicateLine,
		   "Ctrl-M": replaceWordByDef
	          }


const editorSettings = {lineNumbers: true,
			 mode: "haskell",
			 theme: "theme",
			 keyMap: keyMap,
			 extraKeys: extraKeys,
			 matchBrackets: true,
			 autoCloseBrackets: true
			 }
			 
