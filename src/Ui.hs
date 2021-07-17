module Ui where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Sound.Tidal.Context

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (runReaderT)

import Data.Map as Map  (insert, fromList, assocs)

import Highlight

--get the contents of the codeMirror editor
editorValueDefinitions :: UI String
editorValueDefinitions = callFunction $ ffi "definitionsEditor.getValue()"

editorValueControl :: UI String
editorValueControl = callFunction $ ffi "controlEditor.getValue()"

createHaskellFunction name fn = do
   handler <- ffiExport fn
   runFunction $ ffi ("window." ++ name ++ " = %1") handler

getCursorLine :: UI Int
getCursorLine = callFunction $ ffi "(controlEditor.getCursor()).line"

bigHush :: Stream -> MVar PatternStates -> IO ()
bigHush str patStatesMVar = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        let newPatS = Map.fromList $ map (\(i, p) -> (i, p {muted = True})) (Map.assocs pats)
                        streamHush str
                        putMVar patStatesMVar $ newPatS
                    Nothing -> return ()
