{-# LANGUAGE FlexibleInstances #-}

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

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
-- import Foreign.JavaScript.Marshal
-- import Graphics.UI.Threepenny.Internal(unUI)

import Parse

data Env = Env {window :: Window
               ,stream :: Stream
               ,output :: Element
               ,errors :: Element
               ,strKey :: MVar Bool
               ,enterKey :: MVar Bool
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

-- instance (FromJS a, FromJS b) => FromJS (a,b) where
--   fromJs (x,y) = (fromJS x, fromJS y)

getCursorLine :: UI Int
getCursorLine = callFunction $ ffi "(controlEditor.getCursor()).line"

highlight :: (Int, Int, Int) -> UI ()
highlight (line, start, end) = runFunction $ ffi "controlEditor.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"background-color: red\"});" line start end

unHighlight :: (Int, Int, Int) -> UI ()
unHighlight (line, start, end) = runFunction $ ffi "controlEditor.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"background-color: white\"});" line start end

setup :: Stream -> Window -> UI ()
setup stream win = void $ do
      --setup GUI
      return win C.# C.set title "Tidal"
      definitions <- UI.textarea
                  C.# C.set (attr "id") "definitions-editor"
      control <- UI.textarea
                  C.# C.set (attr "id") "control-editor"

      output <- UI.div #+ [ string "output goes here" ]
      errors <- UI.div #+ [ string "errors go here" ]
      body <- UI.getBody win
      script1 <- mkElement "script"
                        C.# C.set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), {lineNumbers: true, mode: \"haskell\", extraKeys: { Tab: betterTab }}); function betterTab(cm) {if (cm.somethingSelected()) {cm.indentSelection(\"add\");} else {cm.replaceSelection(cm.getOption(\"indentWithTabs\")? \"\t\": Array(cm.getOption(\"indentUnit\") + 1).join(\" \"), \"end\", \"+input\");}}"
      script2 <- mkElement "script"
                        C.# C.set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), {lineNumbers: true, mode: \"haskell\"});"

      --setup env
      strKey <- liftIO $ newMVar False
      enterKey <- liftIO $ newMVar False

      let env = Env win stream output errors strKey enterKey

      --handle Events
      on UI.keydown body $ \x -> liftIO $ runReaderT (keydownEvent x) env
      on UI.keyup body $ \x -> liftIO $ runReaderT (keyupEvent x) env

      -- put elements on bod
      UI.getBody win #+ [element definitions, element control, element script1, element script2 , element errors, element output]

-- to combine UI and IO actions with an environment
instance MonadUI (ReaderT Env IO) where
  liftUI m = do
            env <- ask
            let win = window env
            liftIO $ runUI win m

keydownEvent :: Int -> ReaderT Env IO ()
keydownEvent x = do
            env <- ask
            let str = strKey env
                enter = enterKey env
            case x of
                17 -> do
                  liftIO $ takeMVar str
                  liftIO $ putMVar str True
                  enter <- liftIO $ readMVar enter
                  case enter of
                    True -> do
                          interpretC
                    _ -> return ()
                13 -> do
                  liftIO $ takeMVar enter
                  liftIO $ putMVar enter True
                  str <- liftIO $ readMVar str
                  case str of
                    True -> interpretC
                    _ -> return ()
                _ -> return ()

keyupEvent :: Int -> ReaderT Env IO ()
keyupEvent x = do
              env <- ask
              let str = strKey env
                  enter = enterKey env
              case x of
                  17 -> do
                    liftIO $ takeMVar str
                    liftIO $ putMVar str False
                  13 -> do
                    liftIO $ takeMVar enter
                    liftIO $ putMVar enter False
                  _ -> return ()

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
            Nothing -> do
                    liftUI $ element err C.# C.set UI.text "Failed to get Block"
                    return ()
            Just (blockLine, block) -> do
                    let parsed = parse parseCommand "" block
                        p = streamReplace str
                    case parsed of
                          Left e -> do
                                liftUI $ element err C.# C.set UI.text ( "Parse Error:" ++ show e )
                                return ()
                          Right command -> case command of
                                                  (D num string) -> do
                                                          res <- liftIO $ runHintSafe string contentsDef
                                                          case res of
                                                              Right pat -> do
                                                                let start = parse startPos "" block
                                                                case start of
                                                                  Right pos -> do
                                                                          let line = sourceLine pos
                                                                              col = sourceColumn pos
                                                                          liftUI $ element out C.# C.set UI.text ( "control pattern:" ++ show pat )
                                                                          liftUI $ element err C.# C.set UI.text ""
                                                                          liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                          highlightLoop line col blockLine pat
                                                                  Left err -> error "this cannot happen"
                                                              Left  e -> do
                                                                liftUI $ element err C.# C.set UI.text ( "Interpreter Error:" ++ show e )
                                                                return ()
                                                  (Hush)      -> liftIO $ streamHush str
                                                  (Cps x)   -> liftIO $ streamOnce str $ cps (pure x)

runHintSafe :: String -> String -> IO (Either InterpreterError ControlPattern)
runHintSafe input stmts = Hint.runInterpreter $ do
                          Hint.set [languageExtensions := exts]
                          Hint.setImports libs
                          Hint.runStmt stmts
                          Hint.interpret input (Hint.as :: ControlPattern)

--doesn't really work
locs :: Rational -> Line -> Column -> Int -> ControlPattern -> [(Int,Int,Int)]
locs t line col n pat = concatMap (evToLocs line col n) $ queryArc pat (Arc t t)
        where evToLocs line col n (Event {context = Context xs}) = map (toLoc line col n) xs
              -- assume an event doesn't span a line..
              toLoc line col n ((bx, by), (ex, _)) = (n+by+line - 2, bx+col - 2, ex+col - 2)

highlightLoop :: Line -> Column -> Int -> ControlPattern -> ReaderT Env IO ()
highlightLoop line col shift pat = do
                          env <- ask
                          tempo <- liftIO $ readMVar $ sTempoMV $ stream env
                          t <- liftUI $ time
                          let c = timeToCycles tempo (t-0.2)
                              ls = locs c line col shift pat
                          liftUI $ highlight (head ls)
                          liftIO $ threadDelay 100000
                          liftUI $ unHighlight (head ls)
