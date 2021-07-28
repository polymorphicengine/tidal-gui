module Ui where

import Graphics.UI.Threepenny.Core as C hiding (text)

import Sound.Tidal.Context hiding (solo)

import Control.Concurrent.MVar  (tryTakeMVar, MVar, tryPutMVar)

import Data.Map as Map  (insert, fromList, assocs, lookup, empty)

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
              _ <- tryTakeMVar patStatesMVar
              _ <- tryPutMVar patStatesMVar $ Map.empty
              streamHush str

mute :: Stream -> MVar PatternStates -> Int -> IO ()
mute str patStatesMVar i = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just p -> do
                            let newPatS = Map.insert i (p {muted = not (muted p)}) pats
                            if muted p then streamUnmute str i else streamMute str i
                            _ <- tryPutMVar patStatesMVar $ newPatS
                            return ()
                          Nothing -> return ()
                    Nothing -> return ()

soloStr :: Stream -> MVar PatternStates -> Int -> IO ()
soloStr str patStatesMVar i = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just pat -> do
                            let newPatS' = Map.fromList $ map (\(j, p) -> (j, p {muted = True})) (Map.assocs pats)
                            let newPatS = Map.insert i (pat {solo = not (solo pat)}) newPatS'
                            if solo pat then streamUnsolo str i else streamSolo str i
                            _ <- tryPutMVar patStatesMVar $ newPatS
                            return ()
                          Nothing -> return ()
                    Nothing -> return ()
