{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}

module Hint where

import Control.Exception  (SomeException, catch)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar, readMVar)

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

type Contents = String

data InterpreterMessage = MHigh Contents
                        | MStat Contents
                        | MType Contents
                        | MDef Contents
                        deriving Show

data InterpreterResponse = RHigh ControlPattern
                         | RStat String
                         | RType String
                         | RDef String
                         | RError String
                         deriving Show

startHintJob :: Bool -> Stream -> [String] -> MVar [String] -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
startHintJob safe str bootDefs defsMV mMV rMV | safe = hintJob Hint.runInterpreter str bootDefs defsMV mMV rMV
                                          | otherwise = hintJob unsafeInterpreter str bootDefs defsMV mMV rMV

hintJob :: (Interpreter () -> IO (Either InterpreterError ())) ->  Stream -> [String] -> MVar [String] -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob interpreter str bootDefs defsMV mMV rMV = do
                result <- catch (interpreter $ (staticInterpreter str bootDefs) >> (interpreterLoop defsMV mMV rMV))
                          (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob interpreter str bootDefs defsMV mMV rMV

staticInterpreter :: Stream -> [String] -> Interpreter ()
staticInterpreter str bootDefs = do
                    Hint.set [languageExtensions := exts]
                    Hint.setImportsF libs
                    bind "tidal" str
                    _ <- sequence $ map Hint.runStmt bootDefs
                    Hint.runStmt bootTidal

interpreterLoop :: MVar [String] -> MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop defsMV mMV rMV = do
                    message <- liftIO $ takeMVar mMV
                    ds <- liftIO $ readMVar defsMV
                    runManyStmt ds
                    case message of
                      MHigh cont -> interpretPat cont rMV
                      MStat cont -> interpretStat cont rMV
                      MType cont -> interpretType cont rMV
                      MDef cont  -> interpretDef cont rMV
                    interpreterLoop defsMV mMV rMV

interpretPat :: String -> MVar InterpreterResponse -> Interpreter ()
interpretPat cont rMV = do
                  let munged = deltaMini cont
                  t <- Hint.typeChecksWithDetails munged
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
                    Right _ -> do
                      !pat <- Hint.interpret munged (Hint.as :: ControlPattern)
                      liftIO $ putMVar rMV $ RHigh pat

interpretStat :: String -> MVar InterpreterResponse -> Interpreter ()
interpretStat cont rMV = do
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
                    Right _ -> do
                      Hint.runStmt ("(tmpMsg, !temp) <- hCapture [stderr] $ " ++ cont)
                      out <- Hint.eval "temp"
                      msg <- Hint.interpret "tmpMsg" (Hint.as :: String)
                      case msg of
                        "" -> liftIO $ putMVar rMV $ RStat out
                        _ -> liftIO $ putMVar rMV $ RError msg

interpretType :: String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont rMV = do
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
                    Right out -> liftIO $ putMVar rMV $ RType out

interpretDef :: String -> MVar InterpreterResponse -> Interpreter ()
interpretDef cont rMV = do
                  Hint.runStmt cont
                  liftIO $ putMVar rMV $ RDef cont

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

runManyStmt :: [String] -> Interpreter ()
runManyStmt [] = return ()
runManyStmt (x:xs) = do
                runStmt x
                runManyStmt xs
