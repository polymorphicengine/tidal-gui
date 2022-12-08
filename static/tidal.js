function hideAll() {

	var elems = document.getElementsByTagName('span');
	var ed = document.getElementById('editors');
	
	for (var i = 0; i < elems.length; i++) {
	    if (elems[i].style.visibility === "hidden") {
		elems[i].style.visibility = "visible";
	    } else {
        	elems[i].style.visibility = "hidden";
    		}	
	}

	if (ed.style.visibility === "hidden") {
		ed.style.visibility = "visible";
	    } else {
        	ed.style.visibility = "hidden";
    		}
}


 function loadFile(cm){
 
   document.getElementById('fileInput').onchange = e => { 
   
   	var file = e.target.files[0]; 
   	var reader = new FileReader();
   	reader.readAsText(file,'UTF-8');
   	reader.onload = function() {cm.getDoc().setValue(reader.result);};
   	
   	}
   	
   document.getElementById("fileInput").click();
}


function saveFile(cm){
    var editors = document.querySelectorAll('.CodeMirror');
    var textToSave = "";
    
    for (i = 1; i < editors.length; i++){
    	
    	textToSave = textToSave + "\n\n" + editors[i].CodeMirror.getValue();
    	
    }
    
    var textToSaveAsBlob = new Blob([textToSave], {type:"text/plain"});
    var textToSaveAsURL = window.URL.createObjectURL(textToSaveAsBlob);
 
    var downloadLink = document.createElement("a");
    downloadLink.download = 'untitled.tidal';
    downloadLink.innerHTML = "Download File";
    downloadLink.href = textToSaveAsURL;
    downloadLink.onclick = destroyClickedElement;
    downloadLink.style.display = "none";
    document.body.appendChild(downloadLink);
 
    downloadLink.click();
}

function evalB(cm){
	if (typeof cm === 'undefined' || cm == null) {
		console.log("undef");
	} else {
		//console.log(cm);
		try {
			return evaluateBlock(cm);
		} catch (err) {console.log("oh dear")}
	};
}

function getCursorLine(cm) {
	let obj = cm.getCursor();
	if (typeof obj === 'undefined' || obj == null) {
	 	console.log("WTFFF");
	} else {
		try {
			return (cm.getCursor()).line;
		} catch (err) {console.log("oh dear")}
	}
}

function getV(cm) {
	if (typeof cm === 'undefined' || cm == null) {
	 	console.log("WTF");
	} else {
		return cm.getValue();
	}
}


function destroyClickedElement(event){
    document.body.removeChild(event.target);
}


function openDocs(cm){
	var loc = cm.findWordAt(cm.getCursor());
	var word = cm.getRange(loc.anchor, loc.head);
	window.open("https://tidalcycles.org/search?q=" + word,"_blank")
}

function increaseFontSize(cm){
     ele = cm.getWrapperElement();
     style = window.getComputedStyle(ele, null).getPropertyValue('font-size');
     currentSize = parseFloat(style);
     ele.style.fontSize = (currentSize + 2) + 'px';
     cm.refresh();
}

function decreaseFontSize(cm){
     ele = cm.getWrapperElement();
     style = window.getComputedStyle(ele, null).getPropertyValue('font-size');
     currentSize = parseFloat(style);
     ele.style.fontSize = (currentSize - 2) + 'px';
     cm.refresh();
}

function record() {
	codePlayer.clear();
	codeRecorder.listen();
}

function stopAndPlay() {
	const records = codeRecorder.getRecords();
	codePlayer.addOperations(records);
	codePlayer.play();
}

var Pos = CodeMirror.Pos;

function swapLineUp(cm) {
    if (cm.isReadOnly()) return CodeMirror.Pass
    var ranges = cm.listSelections(), linesToMove = [], at = cm.firstLine() - 1, newSels = [];
    for (var i = 0; i < ranges.length; i++) {
      var range = ranges[i], from = range.from().line - 1, to = range.to().line;
      newSels.push({anchor: Pos(range.anchor.line - 1, range.anchor.ch),
                    head: Pos(range.head.line - 1, range.head.ch)});
      if (range.to().ch == 0 && !range.empty()) --to;
      if (from > at) linesToMove.push(from, to);
      else if (linesToMove.length) linesToMove[linesToMove.length - 1] = to;
      at = to;
    }
    cm.operation(function() {
      for (var i = 0; i < linesToMove.length; i += 2) {
        var from = linesToMove[i], to = linesToMove[i + 1];
        var line = cm.getLine(from);
        cm.replaceRange("", Pos(from, 0), Pos(from + 1, 0), "+swapLine");
        if (to > cm.lastLine())
          cm.replaceRange("\n" + line, Pos(cm.lastLine()), null, "+swapLine");
        else
          cm.replaceRange(line + "\n", Pos(to, 0), null, "+swapLine");
      }
      cm.setSelections(newSels);
      cm.scrollIntoView();
    });
  };

function swapLineDown(cm) {
    if (cm.isReadOnly()) return CodeMirror.Pass
    var ranges = cm.listSelections(), linesToMove = [], at = cm.lastLine() + 1;
    for (var i = ranges.length - 1; i >= 0; i--) {
      var range = ranges[i], from = range.to().line + 1, to = range.from().line;
      if (range.to().ch == 0 && !range.empty()) from--;
      if (from < at) linesToMove.push(from, to);
      else if (linesToMove.length) linesToMove[linesToMove.length - 1] = to;
      at = to;
    }
    cm.operation(function() {
      for (var i = linesToMove.length - 2; i >= 0; i -= 2) {
        var from = linesToMove[i], to = linesToMove[i + 1];
        var line = cm.getLine(from);
        if (from == cm.lastLine())
          cm.replaceRange("", Pos(from - 1), Pos(from), "+swapLine");
        else
          cm.replaceRange("", Pos(from, 0), Pos(from + 1, 0), "+swapLine");
        cm.replaceRange(line + "\n", Pos(to, 0), null, "+swapLine");
      }
      cm.scrollIntoView();
    });
  };

 function duplicateLine(cm) {
    cm.operation(function() {
      var rangeCount = cm.listSelections().length;
      for (var i = 0; i < rangeCount; i++) {
        var range = cm.listSelections()[i];
        if (range.empty())
          cm.replaceRange(cm.getLine(range.head.line) + "\n", Pos(range.head.line, 0));
        else
          cm.replaceRange(cm.getRange(range.from(), range.to()), range.from());
      }
      cm.scrollIntoView();
    });
  };
  
// The following code is copied from haskell.js from the threepenny-gui library
// the onl modification is a try/catch to prevent a random bug that can occur very rarely  
  
// Connect to the Haskell server and initialize the FFI.
// An optional string argument can be used to specify the server URL.
Haskell.initFFI = function () {
  var connection;
  var url = window.location.origin.toString();

  if (arguments[0]) {
    url = arguments[0]; // take server url from argument
  }

  /////////////////////////////////////////////////////////////////////
  // Listen to server and execute JS functions
  var reply = function (response) {
    if (response !== undefined) {
      connection.send(response);
    }
  };

  var receive = function (msg) {
    Haskell.log("Server message: %o", msg);

    switch (msg.tag) {

      case "RunEval":
        try {
          eval(msg.contents);
          reply();
        } catch (err) {
          connection.close();
          throw(err);
        }
        break;

      case "CallEval":
        try {
          var result = eval(msg.contents);
          reply({ tag : "Result"   , contents : result });
        } catch (err) {
          reply({ tag : "Exception", contents : err.toString() });
        }
        break;

      case "Debug":
        Haskell.log("Server debug: %o", msg.contents);
        reply();
        break;

      case "Timestamp":
        Haskell.log("Timestamp: %f ms", Haskell.performance.now());
        Haskell.log("Elapsed since last timestamp: %f ms",
          Haskell.performance.diff());
        reply();
        break;
    }
  };

  // Initialize connection to server.
  connection = Haskell.createWebSocket(url, receive);

  /////////////////////////////////////////////////////////////////////
  // Calling Haskell functions

  // An event is a function on the server side that can be called anytime,
  // but whose execution will be queued.
  Haskell.newEvent = function (name, args) {
    var that = function () {
      var theargs = [];
      if (args) {
        theargs = eval(args);
      } else {
        for (var i=0; i<arguments.length; i++) {
          theargs[i] = arguments[i];
        }
      }

      reply({
        tag       : "Event",
        name      : name,
        arguments : theargs
      });
      // 'args' is a string that contains the name 'arguments'
      // Evaluating it will perform appropriate marshalling.
    };
    return that;
  };

  /////////////////////////////////////////////////////////////////////
  // Stable Pointers on JavaScript objects
  //
  // Warning: We assume that each object can have at most one StablePtr
  // associated to it. We have to pay attention that we don't
  // free a stable pointer twice.
  var stablePtrs = {};
  var counter    = 0;

  var newStablePtr = function (object) {
    if (object.stablePtr === undefined) {
      object.stablePtr = 'js-' + counter.toString();
      stablePtrs[object.stablePtr] = object;
      counter++;
    }
    return object.stablePtr.toString();
  };

  Haskell.imposeStablePtr = function (object, ptr) {
    object.stablePtr = ptr;
    stablePtrs[object.stablePtr] = object;
  };

  Haskell.getStablePtr = function (object) {
    return (object.stablePtr || newStablePtr(object));
  };

  Haskell.deRefStablePtr = function (ptr) {
    return stablePtrs[ptr];
  };

  Haskell.freeStablePtr = function (ptr) {
    try {
	    delete stablePtrs[ptr].stablePtr;
	    delete stablePtrs[ptr];
    } catch { console.log("tried");
    };
  };
};

