module Ui where

import Graphics.UI.Threepenny.Core as C hiding (text)

import Sound.Tidal.Context hiding (solo)

import Control.Concurrent.MVar  (tryTakeMVar, MVar, tryPutMVar, takeMVar, putMVar)
import Control.Monad (void)

import Data.Map as Map  (Map, insert, fromList, assocs, lookup, empty)

import Highlight

data PatternState = PS {psChan :: Int,
                        sMuted :: Bool,
                        sSolo :: Bool
                       }

type PatternStates = Map Int PatternState

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

hush :: Stream -> MVar PatternStates -> MVar HighlightStates -> IO ()
hush str patStatesMVar highStatesMVar = do
              _ <- takeMVar patStatesMVar
              _ <- putMVar patStatesMVar $ Map.empty
              _ <- tryTakeMVar highStatesMVar
              _ <- tryPutMVar highStatesMVar $ Map.empty
              streamHush str

muteP :: Stream -> MVar PatternStates -> Int -> IO ()
muteP str patStatesMVar i = do
            patStates <- tryTakeMVar patStatesMVar
            case patStates of
                  Just pats -> do
                      case Map.lookup i pats of
                        Just p -> do
                          let newPatS = Map.insert i (p {sMuted = not (sMuted p)}) pats
                          if sMuted p then streamUnmute str i else streamMute str i
                          _ <- tryPutMVar patStatesMVar $ newPatS
                          return ()
                        Nothing -> void $ tryPutMVar patStatesMVar $ pats
                  Nothing -> return ()

muteH :: Stream -> MVar HighlightStates -> String -> IO ()
muteH str highStatesMVar i = do
              highStates <- tryTakeMVar highStatesMVar
              case highStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just p -> do
                            let newPatS = Map.insert i (p {hMuted = not (hMuted p)}) pats
                            if hMuted p then streamUnmute str i else streamMute str i
                            _ <- tryPutMVar highStatesMVar $ newPatS
                            return ()
                          Nothing -> void $ tryPutMVar highStatesMVar $ pats
                    Nothing -> return ()

soloH :: Stream -> MVar HighlightStates -> String -> IO ()
soloH str highStatesMVar i = do
              highStates <- tryTakeMVar highStatesMVar
              case highStates of
                    Just pats -> do
                        case Map.lookup i pats of
                          Just pat -> do
                            let newPatS' = Map.fromList $ map (\(j, p) -> (j, p {hMuted = True})) (Map.assocs pats)
                            let newPatS = Map.insert i (pat {hSolo = not (hSolo pat)}) newPatS'
                            if hSolo pat then streamUnsolo str i else streamSolo str i
                            _ <- tryPutMVar highStatesMVar $ newPatS
                            return ()
                          Nothing -> return ()
                    Nothing -> return ()
