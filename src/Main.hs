{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

import       Language.Haskell.Interpreter as Hint
import       Language.Haskell.Interpreter.Unsafe as Hint
import System.FilePath                       (dropFileName)
import System.Environment                    (getExecutablePath)
import Sound.Tidal.Context as T
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State as ST
import Sound.Tidal.Stream (Target(..))
import Text.Parsec(parse, Line, Column, sourceLine, sourceColumn)
import Sound.OSC.FD(time)
import Sound.Tidal.Tempo(timeToCycles)
import Data.Map as Map (Map,elems,insert,empty,null,fromList,assocs)
import Data.List((\\))
import Control.Exception
import Text.Parsec(ParseError)
import Sound.Tidal.Utils
import Foreign.JavaScript(NewJSObject , JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)


import Parse

data Env = Env {window :: Window
               ,stream :: Stream
               ,output :: Element
               ,errors :: Element
               ,patS :: MVar PatternStates
               }

data PatternState = PS {sPat :: ControlPattern
                       ,blockLine :: Int
                       ,muted :: Bool
                       ,solo :: Bool
                       } deriving Show

type PatternStates = Map Int PatternState

data Env' = Env' {windowE :: Window
                 ,streamE :: Stream
                 ,outputE :: Element
                 ,errorsE :: Element
                 ,patE :: PatternState
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

data Highlight = Highlight {shift :: Int
                           ,streamH :: Stream
                           ,pat :: ControlPattern
                           ,winH :: Window
                           }

type Buffer = [((Int,Int,Int), Maybe Arc)]

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

-- unHighlight :: JSObject -> UI ()
-- unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

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
      pats <- liftIO $ newEmptyMVar
      liftIO $ forkIO $ highlightLoop [] stream win pats

      let env = Env win stream output errors pats
          runI = runReaderT interpretC env

      createHaskellFunction "runInterpreter" runI
      createHaskellFunction "hush" (bigHush stream pats)

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

locs :: Rational -> Int -> ControlPattern -> [((Int,Int,Int), Maybe Arc)]
locs t bShift pat = concatMap (evToLocs bShift) $ queryArc pat (Arc t t)
        where evToLocs bShift (Event {context = Context xs, whole = wh}) = map (\x -> ((toLoc bShift) x, wh)) xs
              -- assume an event doesn't span a line..
              toLoc bShift ((bx, by), (ex, _)) | by == 1 = (bShift+by - 1, bx + 4, ex + 4)
                                               | otherwise = (bShift+by - 1, bx, ex)
              locsWithArc ls = zip ls (map whole $ queryArc pat (Arc t t))

highlightLoop :: Buffer -> Stream -> Window -> MVar PatternStates -> IO ()
highlightLoop buffer stream win patStatesMVar = do
                patStates <- readMVar patStatesMVar
                tempo <- readMVar $ sTempoMV stream
                t <- time
                let pats = Map.elems patStates
                    c = timeToCycles tempo t
                putStrLn $ show pats
                (marks,buffer') <- highlightPats c buffer win pats
                threadDelay 100000
                unhighlightMany marks win
                runUI win flushCallBuffer
                highlightLoop buffer' stream win patStatesMVar

highlightPats :: Rational -> Buffer -> Window -> [PatternState] -> IO ([JSObject],Buffer)
highlightPats c buffer win [] = return ([], buffer)
highlightPats c buffer win ((PS pat shift True _):ps) = do
                                              let ls = locs c shift pat
                                              (marks,buffer') <- highlightPats c (buffer \\ ls) win ps
                                              return (marks, buffer')
highlightPats c buffer win ((PS pat shift False _):ps) = do
                                              let ls = locs c shift pat
                                              (marks,buffer') <- highlightMany buffer ls win
                                              (marks',buffer'') <- highlightPats c buffer' win ps
                                              return ((marks ++ marks'), buffer'')

highlightMany :: Buffer -> [((Int,Int,Int), Maybe Arc)] -> Window -> IO ([JSObject],Buffer)
highlightMany buffer [] win = return ([],buffer)
highlightMany buffer (x@(i,a):xs) win = do
                                case elem x buffer of
                                  True -> highlightMany buffer xs win
                                  False -> do
                                      mark <- runUI win (highlight i)
                                      let buf = filter (\(j,_) -> j /= i) buffer -- so the buffer doesn't clog up
                                      (marks, buffer') <- highlightMany (x:buf) xs win
                                      return ((mark:marks),buffer')

unhighlightMany :: [JSObject] -> Window -> IO ()
unhighlightMany [] win = return ()
unhighlightMany (x:xs) win = do
                    runUI win (unHighlight x)
                    unhighlightMany xs win


bigHush :: Stream -> MVar PatternStates -> IO ()
bigHush str patStatesMVar = do
              patStates <- tryTakeMVar patStatesMVar
              case patStates of
                    Just pats -> do
                        let newPatS = Map.fromList $ map (\(i, p) -> (i, p {muted = True})) (Map.assocs pats)
                        streamHush str
                        putMVar patStatesMVar $ newPatS
                    Nothing -> return ()
