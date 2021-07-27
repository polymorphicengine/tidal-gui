{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}

module Hint where

import Control.Exception  (SomeException,try, catch)

import Sound.Tidal.Context (ControlPattern,Stream)
import Sound.Tidal.Utils (deltaMini)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)
import Data.IORef
import Data.Typeable

import Configure
import Parse

runHintSafe :: String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintSafe input stmts = try $ do
                      i <- Hint.runInterpreter $ do
                                  Hint.set [languageExtensions := exts]
                                  Hint.setImportsF libs
                                  Hint.runStmt stmts
                                  Hint.interpret (deltaMini input) (Hint.as :: ControlPattern)
                      evalDummy i
                      return i

runHintSafeOther :: String -> String -> Stream -> IO (Either SomeException (Either InterpreterError (IO ())))
runHintSafeOther input stmts stream = try $ do
                      i <- Hint.runInterpreter $ do
                                  Hint.set [languageExtensions := exts]
                                  Hint.setImportsF libs
                                  bind "tidal" stream
                                  Hint.runStmt bootTidal
                                  Hint.runStmt stmts
                                  Hint.interpret input (Hint.as :: IO ())
                      evalDummy i
                      return i

runHintSafeStatement :: String -> String -> Stream -> IO (Either InterpreterError String)
runHintSafeStatement input stmts stream = Hint.runInterpreter $ runHintSafeStatIO input stmts stream

runHintSafeStatIO :: String -> String -> Stream -> Interpreter String
runHintSafeStatIO input stmts stream = do
                      Hint.set [languageExtensions := exts]
                      Hint.setImportsF libs
                      bind "tidal" stream
                      Hint.runStmt bootTidal
                      Hint.runStmt stmts
                      Hint.runStmt ("temp <- " ++ input)
                      Hint.eval "temp"

runHintSafeStatBase :: String -> String -> Stream -> Interpreter String
runHintSafeStatBase input stmts stream = do
                      Hint.set [languageExtensions := exts]
                      Hint.setImportsF libs
                      bind "tidal" stream
                      Hint.runStmt bootTidal
                      Hint.runStmt stmts
                      Hint.runStmt ("temp <- return $ " ++ input)
                      Hint.eval "temp"

getTypeSafe :: String -> String -> Stream -> IO (Either InterpreterError String)
getTypeSafe s stmts stream = Hint.runInterpreter $ do
                  Hint.set [languageExtensions := exts]
                  Hint.setImportsF libs
                  bind "tidal" stream
                  Hint.runStmt bootTidal
                  Hint.runStmt stmts
                  Hint.typeOf s

--notice the bang pattern
evalDummy :: (Either InterpreterError a) -> IO ()
evalDummy e = do
          case e of
            Left !_ -> return ()
            Right !_ -> return ()

parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s


bind :: String -> Stream -> Interpreter ()
bind var value = do
  Hint.runStmt "tmpIORef <- newIORef (undefined :: Stream)"
  tmpIORef <- Hint.interpret "tmpIORef" (Hint.as :: IORef Stream)
  liftIO $ writeIORef tmpIORef value
  Hint.runStmt (var ++ " <- readIORef tmpIORef")
