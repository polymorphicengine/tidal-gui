{-# LANGUAGE FlexibleInstances #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Map as Map (insert, empty)

import Sound.Tidal.Context as T

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Foreign.JavaScript (JSObject)

import Parse
import Highlight
import Ui
import Configure
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

     output <- UI.pre #+ [ string "output goes here" ]
     errors <- UI.pre #+ [ string "errors go here" ]
     body <- UI.getBody win
     script1 <- mkElement "script"
                       C.# C.set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: {\"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush, \"Ctrl-Up\": upFocus, \"Ctrl-D\": openDocs}}); function upFocus(cm){definitionsEditor.focus()}; function openDocs(cm){var loc = cm.findWordAt(cm.getCursor()); var word = cm.getRange(loc.anchor, loc.head); window.open(\"https://tidalcycles.org/search?q=\" + word,\"_blank\")}; function betterTab(cm) {if (cm.somethingSelected()) {cm.indentSelection(\"add\");} else {cm.replaceSelection(cm.getOption(\"indentWithTabs\")? \"\t\": Array(cm.getOption(\"indentUnit\") + 1).join(\" \"), \"end\", \"+input\");}}"
     script2 <- mkElement "script"
                       C.# C.set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: {\"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush, \"Ctrl-Down\": downFocus}}); function downFocus(cm){controlEditor.focus()}"

     --highlight (experimental)
     pats <- liftIO $ newEmptyMVar
     liftIO $ forkIO $ highlightLoop [] stream win pats

     let env = Env win stream output errors pats
         runI = runReaderT interpretCommands env

     createHaskellFunction "runInterpreter" runI
     createHaskellFunction "hush" (bigHush stream pats)

     -- put elements on body
     UI.getBody win #+ [element definitions, element control, element script1, element script2 , element errors, element output]

data Env = Env {window :: Window
                ,stream :: Stream
                ,output :: Element
                ,errors :: Element
                ,patS :: MVar PatternStates
                }

-- to combine UI and IO actions with an environment
instance MonadUI (ReaderT Env IO) where
 liftUI m = do
           env <- ask
           let win = window env
           liftIO $ runUI win m

interpretCommands :: ReaderT Env IO ()
interpretCommands  = do
       env <- ask
       let out = output env
           err = errors env
           str = stream env
       contentsControl <- liftUI $ editorValueControl
       contentsDef <- liftUI $ editorValueDefinitions
       line <- liftUI getCursorLine
       let blocks = getBlocks contentsControl
           blockMaybe = getBlock line blocks
       case blockMaybe of
           Nothing -> void $ liftUI $ element err C.# C.set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   let parsed = parse parseCommand "" block
                       p = streamReplace str
                   case parsed of
                         Left e -> do
                           liftUI $ flashError blockLineStart blockLineEnd
                           void $ liftUI $ element err C.# C.set UI.text ( "Parse error in " ++ show e )
                         Right command -> case command of
                                         (D num string) -> do
                                                 res <- liftIO $ runHintSafe string contentsDef
                                                 case res of
                                                     Right (Right pat) -> do
                                                                       liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                       let patStatesMVar = patS env
                                                                           win = window env
                                                                       liftUI $ element out C.# C.set UI.text (show pat )
                                                                       liftUI $ element err C.# C.set UI.text ""
                                                                       liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                       patStates <- liftIO $ tryTakeMVar patStatesMVar
                                                                       case patStates of
                                                                             Just pats -> do
                                                                                 let newPatS = Map.insert num (PS pat blockLineStart False False) pats
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                                             Nothing -> do
                                                                                 let newPatS = Map.insert num (PS pat blockLineStart False False) Map.empty
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                     Right (Left e) -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element err C.# C.set UI.text (parseError e)
                                                     Left e -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element err C.# C.set UI.text (show e)
                                         (Hush)      -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ bigHush str (patS env)
                                         (Cps x)     -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ streamOnce str $ cps (pure x)
