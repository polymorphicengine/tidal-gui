{-# LANGUAGE BangPatterns #-}

module Hint where

import Control.Exception  (SomeException,try)

import Sound.Tidal.Context (ControlPattern)
import Sound.Tidal.Utils (deltaMini)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)

import Configure
import Parse

runHintSafe :: String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintSafe input stmts = try $ do
                      i <- Hint.runInterpreter $ do
                                  Hint.set [languageExtensions := exts]
                                  Hint.setImports libs
                                  Hint.runStmt stmts
                                  case deltaMini' input of
                                    Right s -> Hint.interpret s (Hint.as :: ControlPattern)
                                    Left _ -> error "can this happen?"
                      evalDummy i
                      return i

--notice the bang pattern
evalDummy :: (Either InterpreterError ControlPattern) -> IO ()
evalDummy e = do
          case e of
            Left _ -> return ()
            Right !pat -> return ()

parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
