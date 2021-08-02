{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}

module Hint where

import Control.Exception  (SomeException,try)

import Sound.Tidal.Context (ControlPattern,Stream)
import Sound.Tidal.Utils (deltaMini)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)
import Data.IORef

import Configure

unsafeInterpreter :: Interpreter a -> IO (Either InterpreterError a)
unsafeInterpreter = Hint.unsafeRunInterpreterWithArgsLibdir ["-v"] "haskell-libs"

patternInterpreter :: String -> String -> Interpreter ControlPattern
patternInterpreter input stmts  = do
  Hint.set [languageExtensions := exts]
  Hint.setImportsF libs
  Hint.runStmt stmts
  Hint.interpret (deltaMini input) (Hint.as :: ControlPattern)

runHintPatternCombined :: (Interpreter ControlPattern -> IO (Either InterpreterError ControlPattern)) -> String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintPatternCombined interpreter input stmts = try $ do
                      i <- interpreter $ patternInterpreter input stmts
                      evalDummy i
                      return i

runHintPattern :: Bool -> String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintPattern safe input stmts | safe = runHintPatternCombined Hint.runInterpreter input stmts
                                | otherwise = runHintPatternCombined unsafeInterpreter input stmts

statementInterpreter :: String -> String -> Stream -> Interpreter String
statementInterpreter input stmts stream = do
  Hint.set [languageExtensions := exts]
  Hint.setImportsF libs
  bind "tidal" stream
  Hint.runStmt bootTidal
  Hint.runStmt stmts
  Hint.runStmt ("temp <- " ++ input)
  Hint.eval "temp"

runHintStatementCombined :: (Interpreter String -> IO (Either InterpreterError String)) -> String -> String -> Stream -> IO (Either InterpreterError String)
runHintStatementCombined interpreter input stmts stream = interpreter $ statementInterpreter input stmts stream

runHintStatement :: Bool -> String -> String -> Stream -> IO (Either InterpreterError String)
runHintStatement safe input stmts stream | safe = runHintStatementCombined Hint.runInterpreter input stmts stream
                                         | otherwise = runHintStatementCombined unsafeInterpreter input stmts stream

typeInterpreter :: String -> String -> Stream -> Interpreter String
typeInterpreter s stmts stream = do
                  Hint.set [languageExtensions := exts]
                  Hint.setImportsF libs
                  bind "tidal" stream
                  Hint.runStmt bootTidal
                  Hint.runStmt stmts
                  Hint.typeOf s

getTypeCombined :: (Interpreter String -> IO (Either InterpreterError String)) -> String -> String -> Stream -> IO (Either InterpreterError String)
getTypeCombined interpreter s stmts stream = interpreter $ typeInterpreter s stmts stream

getType :: Bool -> String -> String -> Stream -> IO (Either InterpreterError String)
getType safe input stmts stream | safe = getTypeCombined Hint.runInterpreter input stmts stream
                                | otherwise = getTypeCombined unsafeInterpreter input stmts stream



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
