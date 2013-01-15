module Projects.PrettyPrint
  ( module Projects.PrettyPrint

  , pad
  , shorten
  , trim
  , wrap
  ) where

import Projects.Core

import Codec.Binary.UTF8.String (encodeString)
import Data.List
import Data.Maybe
import XMonad
import XMonad.Hooks.DynamicLog (pad, shorten, trim, wrap)
import XMonad.Hooks.UrgencyHook
import XMonad.StackSet hiding (filter)
import XMonad.Util.NamedWindows

import qualified XMonad.Util.WorkspaceCompare as WC

data PP
  = PP  { _ppCurrent          :: WorkspaceId -> String
        , _ppVisible          :: WorkspaceId -> String
        , _ppHidden           :: WorkspaceId -> String
        , _ppHiddenNoWindows  :: WorkspaceId -> String
        , _ppUrgent           :: WorkspaceId -> String
        , _ppSep              :: String
        , _ppWsSep            :: String
        , _ppTitle            :: String -> String
        , _ppProject          :: String -> String
        , _ppLayout           :: String -> String
        , _ppOrder            :: [String] -> [String]
        , _ppSort             :: X ([WindowSpace] -> [WindowSpace])
        , _ppExtras           :: [X (Maybe String)]
        , _ppOutput           :: String -> IO ()
        }

defaultPP :: PP
defaultPP
  = PP  { _ppCurrent          = wrap "[" "]"
        , _ppVisible          = wrap "<" ">"
        , _ppHidden           = id
        , _ppHiddenNoWindows  = const ""
        , _ppUrgent           = id
        , _ppSep              = " : "
        , _ppWsSep            = " "
        , _ppTitle            = shorten 80
        , _ppProject          = id
        , _ppLayout           = id
        , _ppOrder            = id
        , _ppSort             = WC.getSortByIndex
        , _ppExtras           = []
        , _ppOutput           = putStrLn
        }

projectsLog :: X ()
projectsLog
  = projectsLogWithPP defaultPP

projectsLogWithPP :: PP -> X ()
projectsLogWithPP pp
  = projectsLogString pp >>= io . _ppOutput pp

projectsLogString :: PP -> X String
projectsLogString pp
  = do
      ws <- gets windowset
      us <- readUrgents
      sf <- _ppSort pp

      let ppWs  = ppWindowSet pp sf ws us
          ld    = description (layout (workspace (current ws)))

      (wss, p) <- withProjectsState (return (ppWs Nothing, "")) $
        \state ->
          let mpid  = _psCurrentProjectId state
              wss   = ppWindowSet pp sf ws us mpid
              p     = maybe "" (_ppProject pp) mpid

          in  return (wss, p)

      t <- maybe (return "") (fmap show . getName) (peek ws)
      es <- mapM (flip catchX (return Nothing)) (_ppExtras pp)

      return $ encodeString $ separateBy (_ppSep pp) $ _ppOrder pp $
        [wss, p, _ppLayout pp ld, _ppTitle pp t] ++ catMaybes es

ppCurrentProjectId :: PP -> ProjectsState -> String
ppCurrentProjectId pp state
  = maybe "" (_ppProject pp) (_psCurrentProjectId state)

ppWindowSet :: PP
            -> WC.WorkspaceSort
            -> WindowSet
            -> [Window]
            -> Maybe ProjectId
            -> String

ppWindowSet pp sf ws us mpid
  = separateBy (_ppWsSep pp) . map format . sf $
      foldr (prune . workspace) [] (current ws : visible ws) ++
        foldr prune [] (hidden ws)

    where
      format w  = ppWindowSpace ws us w pp
                    (maybe wid snd (splitProjectWorkspaceName wid))

                    where
                      wid = tag w

      prune     = case mpid of
                    Just cpid -> \w ->
                      case splitProjectWorkspaceName (tag w) of
                        Just (pid, wid')  -> if pid == cpid then (w :) else id
                        Nothing           -> (w :)

                    Nothing -> \w ->
                      if isProjectWorkspaceName (tag w) then id else (w :)

ppWindowSpace :: WindowSet
              -> [Window]
              -> WindowSpace
              -> (PP -> String -> String)

ppWindowSpace ws us w
  | any (maybe False (== wid) . flip findTag ws) us = _ppUrgent
  | wid == currentTag ws                            = _ppCurrent
  | wid `elem` vs                                   = _ppVisible
  | isJust (stack w)                                = _ppHidden
  | otherwise                                       = _ppHiddenNoWindows
  where
    wid = tag w
    vs = map (tag . workspace) (visible ws)

separateBy :: String -> [String] -> String
separateBy s
  = intercalate s . filter (not . null)
