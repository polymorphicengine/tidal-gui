import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Time
import Data.IORef
import Prelude hiding (catch)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

{-----------------------------------------------------------------------------
    Chat
------------------------------------------------------------------------------}

main :: IO ()
main = do
        putStrLn "Please enter path to static:"
        static <- readLn
        startGUI defaultConfig {
              jsStatic = Just static,
              jsCustomHTML     = Just "tidal.html"
            } $ setup

setup :: Window -> UI ()
setup win = void $ do

  return win # set title "Tidal"

  input <- UI.textarea
              # set (attr "id") "code"

  on UI.sendValue input $ \content -> do
                    element input # set value ""

  script <- mkElement "script"
              # set UI.text "const codemirrorEditor = CodeMirror.fromTextArea(document.getElementById('code'), {lineNumbers: true, mode: \"haskell\"});"

  UI.getBody win #+ [element input, element script]
