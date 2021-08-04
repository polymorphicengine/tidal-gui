{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}

module Hint where

import Control.Exception  (SomeException, try, catch)
import Control.Concurrent.MVar  (newEmptyMVar, tryTakeMVar, MVar, putMVar, newMVar, takeMVar, readMVar)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Sound.Tidal.Context (ControlPattern,Stream)
import Sound.Tidal.Utils (deltaMini)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)
import Data.IORef

import Configure

args:: String -> [String]
args lib = ["-clear-package-db", "-package-db", lib ++ "haskell-libs/package.conf.d", "-package-db", lib ++ "haskell-libs/package.db", "-v"]

unsafeInterpreter :: Interpreter a -> IO (Either InterpreterError a)
unsafeInterpreter interpreter = do
  execPath <- dropFileName <$> getExecutablePath
  Hint.unsafeRunInterpreterWithArgsLibdir (args execPath) (execPath ++ "haskell-libs") interpreter

type Definitions = String
type Contents = String

data InterpreterMessage = MHigh Contents Definitions
                        | MStat Contents Definitions
                        | MType Contents Definitions
                        deriving Show

data InterpreterResponse = RHigh ControlPattern
                         | RStat String
                         | RType String
                         | RError String

startHintJob :: Bool -> Stream -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
startHintJob safe str mMV rMV | safe = hintJob Hint.runInterpreter str mMV rMV
                              | otherwise = hintJob unsafeInterpreter str mMV rMV

hintJob :: (Interpreter () -> IO (Either InterpreterError ())) ->  Stream -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob interpreter str mMV rMV = do
                result <- catch (interpreter $ (staticInterpreter str) >> (interpreterLoop mMV rMV))
                          (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob interpreter str mMV rMV

staticInterpreter :: Stream -> Interpreter ()
staticInterpreter str = do
                    Hint.set [languageExtensions := exts]
                    Hint.setImportsF libs
                    bind "tidal" str
                    Hint.runStmt bootTidal

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    message <- liftIO $ takeMVar mMV
                    case message of
                      MHigh cont defs -> interpretPat cont defs rMV
                      MStat cont defs -> interpretStat cont defs rMV
                      MType cont defs -> interpretType cont defs rMV
                    interpreterLoop mMV rMV

interpretPat :: String -> String -> MVar InterpreterResponse -> Interpreter ()
interpretPat cont defs rMV = do
                  let munged = deltaMini cont
                  Hint.runStmt defs
                  t <- Hint.typeChecksWithDetails munged
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ "Didn't typecheck " ++ concatMap show errors
                    Right _ -> do
                      !pat <- Hint.interpret munged (Hint.as :: ControlPattern)
                      liftIO $ putMVar rMV $ RHigh pat

interpretStat :: String -> String -> MVar InterpreterResponse -> Interpreter ()
interpretStat cont defs rMV = do
                  Hint.runStmt defs
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ "Didn't typecheck " ++ concatMap show errors
                    Right _ -> do
                      Hint.runStmt ("temp <- " ++ cont)
                      out <- Hint.eval "temp"
                      liftIO $ putMVar rMV $ RStat out

interpretType :: String -> String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont defs rMV = do
                  Hint.runStmt defs
                  !t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ "Didn't typecheck " ++ concatMap show errors
                    Right out -> liftIO $ putMVar rMV $ RType out

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
