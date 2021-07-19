{-# LANGUAGE BangPatterns #-}

module Hint where

import Control.Exception  (SomeException,try)

import Sound.Tidal.Context (ControlPattern)
import Sound.Tidal.Utils (deltaMini)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Configure

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
