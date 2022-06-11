{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}

module Hint where

import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.DeepSeq (deepseq)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Sound.Tidal.Context (Stream)

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

data InterpreterMessage = MStat Contents
                        | MType Contents
                        deriving Show

data InterpreterResponse = RStat (Maybe String)
                         | RType String
                         | RError String
                         deriving Show


startHintJob :: Bool -> Stream -> [String] -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
startHintJob safe str bootDefs mMV rMV | safe = hintJob Hint.runInterpreter str bootDefs mMV rMV
                                       | otherwise = hintJob unsafeInterpreter str bootDefs mMV rMV

hintJob :: (Interpreter () -> IO (Either InterpreterError ())) ->  Stream -> [String] -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob interpreter str bootDefs mMV rMV = do
                result <- catch (interpreter $ (staticInterpreter str bootDefs) >> (interpreterLoop mMV rMV))
                          (\e -> return (Left $ UnknownError $ show (e :: SomeException)))
                -- can this happen? If it happens all definitions made interactively are lost...
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob interpreter str bootDefs mMV rMV

staticInterpreter :: Stream -> [String] -> Interpreter ()
staticInterpreter str bootDefs = do
                    Hint.set [languageExtensions := exts]
                    Hint.setImportsF libs
                    bind "tidal" str
                    Hint.runStmt bootTidal
                    _ <- sequence $ map Hint.runStmt bootDefs
                    return ()

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    message <- liftIO $ takeMVar mMV
                    case message of
                      MStat cont -> catch (interpretStatement cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                      MType cont -> catch (interpretType cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                    interpreterLoop mMV rMV


interpretStatement :: String -> MVar InterpreterResponse -> Interpreter ()
interpretStatement cont rMV = do
                        t <- Hint.typeChecksWithDetails cont
                        case t of
                          -- if the expression doesn't type check try to just evaluate it (it could be a definition or binding)
                          Left errors -> catch (Hint.runStmt cont >> (liftIO $ putMVar rMV $ RStat Nothing))
                                         (\e -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors ++ ([show (e :: SomeException)]))
                          Right _ -> do
                            Hint.runStmt ("(tmpMsg, !temp) <- hCapture [stderr] $ " ++ cont)
                            out <- Hint.eval "temp"
                            -- force complete evaluation of 'out', so that any possible error is thrown here
                            msg <- deepseq out (Hint.interpret "tmpMsg" (Hint.as :: String))
                            case msg of
                              "" -> liftIO $ putMVar rMV $ RStat (Just out)
                              _ -> liftIO $ putMVar rMV $ RError msg

interpretType :: String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont rMV = do
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
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

runManyStmt :: [String] -> Interpreter ()
runManyStmt [] = return ()
runManyStmt (x:xs) = do
                runStmt x
                runManyStmt xs
