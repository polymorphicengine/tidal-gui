module Ui where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Sound.Tidal.Context hiding (solo)

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (runReaderT)

import Data.Map as Map  (insert, fromList, assocs, lookup)

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

mute :: Stream -> MVar PatternStates -> Int -> IO ()
mute str patStatesMVar i = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just p -> do
                            let newPatS = Map.insert i (p {muted = not (muted p)}) pats
                            if muted p then streamUnmute str i else streamMute str i
                            putMVar patStatesMVar $ newPatS
                          Nothing -> return ()
                    Nothing -> return ()

soloStr :: Stream -> MVar PatternStates -> Int -> IO ()
soloStr str patStatesMVar i = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just p -> do
                            let newPatS' = Map.fromList $ map (\(i, p) -> (i, p {muted = True})) (Map.assocs pats)
                            let newPatS = Map.insert i (p {solo = not (solo p)}) newPatS'
                            if solo p then streamUnsolo str i else streamSolo str i
                            putMVar patStatesMVar $ newPatS
                          Nothing -> return ()
                    Nothing -> return ()
