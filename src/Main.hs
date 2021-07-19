import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (runReaderT)

import Sound.Tidal.Context as T

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Parse
import Highlight
import Ui
import Config
import Hint

main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    stream <- T.startStream T.defaultConfig [(T.superdirtTarget {oLatency = 0.1},
                                              [T.superdirtShape]
                                             ),
                                             (remoteTarget,
                                              [T.OSCContext "/code/highlight"]
                                             )
                                            ]
    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup stream

setup :: Stream -> Window -> UI ()
setup stream win = void $ do
     --setup GUI
     return win C.# C.set title "Tidal"
     definitions <- UI.textarea
                 C.# C.set (attr "id") "definitions-editor"
     control <- UI.textarea #+ [ string "d1 $ s \"bd sn\"" ]
                 C.# C.set (attr "id") "control-editor"

     output <- UI.div #+ [ string "output goes here" ]
     errors <- UI.div #+ [ string "errors go here" ]
     body <- UI.getBody win
     script1 <- mkElement "script"
                       C.# C.set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: {\"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush, \"Ctrl-Up\": upFocus, \"Ctrl-D\": openDocs}}); function upFocus(cm){definitionsEditor.focus()}; function openDocs(cm){var loc = cm.findWordAt(cm.getCursor()); var word = cm.getRange(loc.anchor, loc.head); window.open(\"https://tidalcycles.org/search?q=\" + word,\"_blank\")}"
     script2 <- mkElement "script"
                       C.# C.set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: {\"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush, \"Ctrl-Down\": downFocus}}); function downFocus(cm){controlEditor.focus()}"

     --highlight (experimental)
     pats <- liftIO $ newEmptyMVar
     liftIO $ forkIO $ highlightLoop [] stream win pats

     let env = Env win stream output errors pats
         runI = runReaderT interpretC env

     createHaskellFunction "runInterpreter" runI
     createHaskellFunction "hush" (bigHush stream pats)

     -- put elements on body
     UI.getBody win #+ [element definitions, element control, element script1, element script2 , element errors, element output]
