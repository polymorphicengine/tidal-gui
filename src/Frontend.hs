module Frontend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Monad  (void)

import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Backend


setup :: Stream -> String -> Window -> UI ()
setup str stdout win = void $ do
     --setup GUI
     void $ return win # set title "TidalCycles"

     UI.addStyleSheet win "tidal.css"
     UI.addStyleSheet win "theme.css"

     setCallBufferMode NoBuffering -- important for highlighting

     editor <- UI.textarea # set (attr "id") "control-editor"

     output <- UI.pre # set UI.id_ "output"
                      #. "outputBox"
                      #+ [ string "output goes here" ]
                      # set style [("font-size","3vh")]
     display <- UI.pre # set UI.id_ "display"
                       #. "displayBox"
                       # set style [("font-size","3vh")]

     fileInput <- UI.input # set UI.id_ "fileInput"
                           # set UI.type_ "file"
                           # set style [("display","none")]

     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"


     body <- UI.getBody win

     -- put elements on body
     _ <- (element body) #. "CodeMirror cm-s-theme"
                    # set UI.style [("background-color","black")]
                    #+  [element display
                        ,UI.div #. "editor" #+ [UI.div #. "main" #+ [element editor]]
                        ,element output
                        ]

     setupBackend str stdout

     (element body) #+  [element fileInput
                        ,inputScript
                        ,tidalSettings
                        ,element makeCtrlEditor
                        ]


tidalSettings :: UI Element
tidalSettings = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
          settings <- mkElement "script" # set UI.text tidalKeys
          return settings

inputScript :: UI Element
inputScript = mkElement "script" # set UI.text  "document.getElementById(\"fileInput\").onchange = e => { var file = e.target.files[0]; var reader = new FileReader();reader.readAsText(file,'UTF-8');reader.onload = function() {controlEditor.getDoc().setValue(reader.result);};}"
