{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

import       Language.Haskell.Interpreter as Hint
import       Language.Haskell.Interpreter.Unsafe as Hint
import System.FilePath                       (dropFileName)
import System.Environment                    (getExecutablePath)
import       Sound.Tidal.Context as T
import       Control.Concurrent.MVar
import       Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader
import Sound.Tidal.Stream (Target(..))
import Text.Parsec(parse, Line, Column, sourceLine, sourceColumn)
import Sound.OSC.FD(time)
import Sound.Tidal.Tempo(timeToCycles)
import Control.Exception
import Text.Parsec(ParseError)
import           Sound.Tidal.Utils
import Foreign.JavaScript(NewJSObject , JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
-- import Foreign.JavaScript.Marshal
-- import Graphics.UI.Threepenny.Internal(unUI)


import Parse

data Env = Env {window :: Window
               ,stream :: Stream
               ,output :: Element
               ,errors :: Element
               ,high :: MVar Highlight
               }

libs = [
    "Sound.Tidal.Context"
  , "Sound.Tidal.Simple"
  , "Control.Applicative"
  , "Data.Bifunctor"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Either"
  , "Data.Foldable"
  , "Data.Function"
  , "Data.Functor"
  , "Data.Int"
  , "Data.List"
  , "Data.Map"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Semigroup"
  , "Data.String"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Typeable"
  , "GHC.Float"
  , "GHC.Real"
  , "Text.Parsec"
  ]

exts = [OverloadedStrings, NoImplicitPrelude]

listenPort = 6011
remotePort = 6012

remoteTarget = Target {oName = "atom"
                      ,oAddress = "127.0.0.1"
                      ,oPort = remotePort
                      ,oBusPort = Nothing
                      ,oLatency = 0.1
                      ,oWindow = Nothing
                      ,oSchedule = T.Live
                      ,oHandshake = True
                      }

data Highlight = Highlight {line :: Int
                           ,col :: Int
                           ,shift :: Int
                           ,streamH :: Stream
                           ,pat :: ControlPattern
                           ,winH :: Window
                           }

main :: IO ()
main = do
        execPath <- dropFileName <$> getExecutablePath
        stream <- T.startStream T.defaultConfig [(T.superdirtTarget {oLatency = 0.1},
                                                  [T.superdirtShape]
                                                 ),
                                                 (remoteTarget,
                                                  [T.OSCContext "/code/highlight"]
                                                 )
                                                ]
        startGUI C.defaultConfig {
              jsStatic = Just $ execPath ++ "static",
              jsCustomHTML     = Just "tidal.html"
            } $ setup stream


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

highlight :: (Int, Int, Int) -> UI JSObject
highlight (line, start, end) = callFunction $ ffi "(controlEditor.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"background-color: red\"}))" line start end

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "%1.clear();" mark

setup :: Stream -> Window -> UI ()
setup stream win = void $ do
      --setup GUI
      return win C.# C.set title "Tidal"
      definitions <- UI.textarea
                  C.# C.set (attr "id") "definitions-editor"
      control <- UI.textarea #+ [ string "d1 $ s \"bd sn\"" ]
                  C.# C.set (attr "id") "control-editor"

      output <- UI.div #+ [ string "output goes here" ]
      errors <- UI.div #+ [ string "errors go here" ]
      body <- UI.getBody win
      script1 <- mkElement "script"
                        C.# C.set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: { Tab: betterTab, \"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush}}); function betterTab(cm) {if (cm.somethingSelected()) {cm.indentSelection(\"add\");} else {cm.replaceSelection(cm.getOption(\"indentWithTabs\")? \"\t\": Array(cm.getOption(\"indentUnit\") + 1).join(\" \"), \"end\", \"+input\");}}"
      script2 <- mkElement "script"
                        C.# C.set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: {\"Ctrl-Enter\": runInterpreter, \"Ctrl-.\": hush}});"

      --highlight (experimental)
      highlight <- liftIO $ newEmptyMVar
      liftIO $ forkIO $ highlightLoop highlight

      let env = Env win stream output errors highlight
          runI = runReaderT interpretC env
          hush = streamHush stream

      createHaskellFunction "runInterpreter" runI
      createHaskellFunction "hush" hush

      -- put elements on body
      UI.getBody win #+ [element definitions, element control, element script1, element script2 , element errors, element output]

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
                                                              let start = parse startPos "" block
                                                              case start of
                                                                  Right pos -> do
                                                                          let line = sourceLine pos
                                                                              col = sourceColumn pos
                                                                              highlight = high env
                                                                              win = window env
                                                                          liftUI $ element out C.# C.set UI.text ( "control pattern:" ++ show pat )
                                                                          liftUI $ element err C.# C.set UI.text ""
                                                                          liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                          liftIO $ tryTakeMVar highlight
                                                                          liftIO $ putMVar highlight $ Highlight line col blockLine str pat win
                                                                  Left e -> error "this cannot happen"
                                                      Right(Left e) -> void $ liftUI $ element err C.# C.set UI.text ( "Interpreter Error:" ++ show e )
                                                      Left e -> void $ liftUI $ element err C.# C.set UI.text ( "Error:" ++ show e )
                                          (Hush)      -> liftIO $ streamHush str
                                          (Cps x)     -> liftIO $ streamOnce str $ cps (pure x)

runHintSafe :: String -> String -> IO (Either SomeException (Either InterpreterError ControlPattern))
runHintSafe input stmts = try $ do
                      i <- Hint.runInterpreter $ do
                                  Hint.set [languageExtensions := exts]
                                  Hint.setImports libs
                                  Hint.runStmt stmts
                                  Hint.interpret input (Hint.as :: ControlPattern)
                      evalDummy i
                      return i

--notice the bang pattern
evalDummy :: (Either InterpreterError ControlPattern) -> IO ()
evalDummy e = do
          case e of
            Left _ -> return ()
            Right !pat -> return ()

--doesn't really work
locs :: Rational -> Line -> Column -> Int -> ControlPattern -> [(Int,Int,Int)]
locs t line col n pat = concatMap (evToLocs line col n) $ queryArc pat (Arc t t)
        where evToLocs line col n (Event {context = Context xs}) = map (toLoc line col n) xs
              -- assume an event doesn't span a line..
              toLoc line col n ((bx, by), (ex, _)) = (n+by+line - 2, bx+col - 2, ex+col - 2)

highlightLoop :: MVar Highlight -> IO ()
highlightLoop high = do
                env <- liftIO $ readMVar high
                tempo <- liftIO $ readMVar $ sTempoMV $ streamH env
                t <-  time
                let win = winH env
                    p = pat env
                    ln = line env
                    cl = col env
                    sh = shift env
                    c = timeToCycles tempo t
                    ls = locs c ln cl sh p
                mark <-  runUI win (highlight (head ls))
                threadDelay 100000
                runUI win (unHighlight mark)
                highlightLoop high
