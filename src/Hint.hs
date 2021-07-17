{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Hint where

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Exception  (SomeException,try)
import Control.Monad  (void)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)

import Text.Parsec  (parse)

import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Graphics.UI.Threepenny as UI

import Sound.Tidal.Context
import Sound.Tidal.Utils (deltaMini)

import Data.Map as Map (insert, empty)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Highlight
import Parse
import Ui
import Config

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

interpretC :: ReaderT Env IO ()
interpretC  = do
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
            Just (blockLine, block) -> do
                    let parsed = parse parseCommand "" block
                        p = streamReplace str
                    case parsed of
                          Left e -> void $ liftUI $ element err C.# C.set UI.text ( "Parse Error:" ++ show e )
                          Right command -> case command of
                                          (D num string) -> do
                                                  res <- liftIO $ runHintSafe string contentsDef
                                                  case res of
                                                      Right (Right pat) -> do
                                                                        let patStatesMVar = patS env
                                                                            win = window env
                                                                        liftUI $ element out C.# C.set UI.text ( "control pattern:" ++ show pat )
                                                                        liftUI $ element err C.# C.set UI.text ""
                                                                        liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                        patStates <- liftIO $ tryTakeMVar patStatesMVar
                                                                        case patStates of
                                                                              Just pats -> do
                                                                                  let newPatS = Map.insert num (PS pat blockLine False False) pats
                                                                                  liftIO $ putMVar patStatesMVar $ newPatS
                                                                              Nothing -> do
                                                                                  let newPatS = Map.insert num (PS pat blockLine False False) Map.empty
                                                                                  liftIO $ putMVar patStatesMVar $ newPatS
                                                      Right (Left e) -> void $ liftUI $ element err C.# C.set UI.text ( "Interpreter Error:" ++ show e )
                                                      Left e -> void $ liftUI $ element err C.# C.set UI.text ( "Error:" ++ show e )
                                          (Hush)      -> liftIO $ bigHush str (patS env)
                                          (Cps x)     -> liftIO $ streamOnce str $ cps (pure x)

runHintSafe :: String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintSafe input stmts = try $ do
                      i <- Hint.runInterpreter $ do
                                  Hint.set [languageExtensions := exts]
                                  Hint.setImports libs
                                  Hint.runStmt stmts
                                  Hint.interpret (deltaMini input) (Hint.as :: ControlPattern)
                      evalDummy i
                      return i

--notice the bang pattern
evalDummy :: (Either InterpreterError ControlPattern) -> IO ()
evalDummy e = do
          case e of
            Left _ -> return ()
            Right !pat -> return ()
