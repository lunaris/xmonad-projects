module Dzen
  ( ScreenNumber
  , DzenWidth(..)
  , DzenAlignment(..)
  , DzenConfig(..)
  , defaultDzenConfig

  , spawnDzen
  , spawnToDzen
  ) where

import Control.Monad
import Data.Functor
import Data.List
import Graphics.X11.Xinerama
import Graphics.X11.Xlib hiding (ScreenNumber)
import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Posix.Process

type ScreenNumber
  = Int

data DzenWidth
  = Pixels Int
  | Percent Double

data DzenAlignment
  = AlignLeft
  | AlignRight
  | Center

instance Show DzenAlignment where
  show AlignLeft  = "l"
  show AlignRight = "r"
  show Center     = "c"

data DzenConfig
  = DzenConfig  { dzcX          :: Maybe DzenWidth
                , dzcY          :: Maybe Int
                , dzcScreen     :: Maybe ScreenNumber
                , dzcWidth      :: Maybe DzenWidth
                , dzcHeight     :: Maybe Int
                , dzcAlignment  :: Maybe DzenAlignment
                , dzcFont       :: Maybe String
                , dzcForeground :: Maybe String
                , dzcBackground :: Maybe String
                , dzcFlags      :: [String]
                , dzcArguments  :: [String]
                }

defaultDzenConfig :: DzenConfig
defaultDzenConfig
  = DzenConfig  { dzcX          = Nothing
                , dzcY          = Nothing
                , dzcScreen     = Nothing
                , dzcWidth      = Nothing
                , dzcHeight     = Nothing
                , dzcAlignment  = Just AlignLeft
                , dzcFont       = Nothing
                , dzcForeground = Nothing
                , dzcBackground = Nothing
                , dzcFlags      = ["onstart=lower"]
                , dzcArguments  = ["-p"]
                }

getScreenWidth :: ScreenNumber -> IO Double
getScreenWidth n
  = do
      display <- openDisplay ""
      maybe 0 (fromIntegral . xsi_width . flip (!!) n) <$>
        xineramaQueryScreens display

getActualWidth :: Maybe ScreenNumber -> Maybe DzenWidth -> IO (Maybe Int)
getActualWidth Nothing w
  = getActualWidth (Just 0) w

getActualWidth _ Nothing
  = return Nothing

getActualWidth _ (Just (Pixels x))
  = return (Just x)

getActualWidth (Just n) (Just (Percent x))
  = f <$> getScreenWidth n
    where
      f 0 = Nothing
      f w = Just (round ((x / 100) * w))

dzenArguments :: DzenConfig -> IO [String]
dzenArguments config
  = do
      x <- getActualWidth (dzcScreen config) (dzcX config)
      w <- getActualWidth (dzcScreen config) (dzcWidth config)

      let s                 = fmap succ (dzcScreen config)
          quote             = ('\'' :) . (++ "'")

          option _ Nothing  = id
          option o (Just v) = (o :) . (v :)

          rest  [] as       = as
          rest  fs as       = "-e" : quote (intercalate ";" fs) : as

      return $
        option "-fn" (quote <$> dzcFont config) .
        option "-fg" (quote <$> dzcForeground config) .
        option "-bg" (quote <$> dzcBackground config) .
        option "-ta" (show <$> dzcAlignment config) .
        option "-y" (show <$> dzcY config) .
        option "-h" (show <$> dzcHeight config) .
        option "-xs" (show <$> s) .
        option "-x" (show <$> x) .
        option "-w" (show <$> w) $
        rest (dzcFlags config) (dzcArguments config)

dzenCommand :: DzenConfig -> IO String
dzenCommand config
  = unwords . ("dzen2" :) <$> dzenArguments config

initialiseDescriptor :: Fd -> IO Handle
initialiseDescriptor fd
  = do
      setFdOption fd CloseOnExec True
      handle <- fdToHandle fd
      hSetBuffering handle LineBuffering
      return handle

tieProcess :: Fd -> Fd -> IO () -> IO ()
tieProcess fd1 fd2 m
  = void $ forkProcess $ do
      void createSession
      void (dupTo fd1 fd2)
      m

readingFrom :: IO () -> Fd -> IO ()
readingFrom m reader
  = tieProcess reader stdInput m

writingTo :: IO () -> Fd -> IO ()
writingTo m writer
  = tieProcess writer stdOutput m

runShell :: String -> IO ()
runShell command
  = executeFile "/bin/sh" False ["-c", command] Nothing

spawnDzen :: DzenConfig -> IO Handle
spawnDzen config
  = do
      command <- dzenCommand config
      (reader, writer) <- createPipe
      handle <- initialiseDescriptor writer
      runShell command `readingFrom` reader
      return handle

spawnToDzen :: String -> DzenConfig -> IO ()
spawnToDzen inputCommand config
  = do
      command <- dzenCommand config
      (reader, writer) <- createPipe
      mapM_ initialiseDescriptor [reader, writer]
      runShell command `readingFrom` reader
      runShell inputCommand `writingTo` writer
