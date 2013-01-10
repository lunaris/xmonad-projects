{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Projects
  ( ProjectId(..)
  , ProjectsConfig(..)

  , initialiseProjects
  , addProject
  , removeProject
  , selectProject
  , withCurrentProjectNthWorkspace

  , PP(..)
  , defaultPP
  , projectsLog
  , projectsLogWithPP
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import XMonad hiding (Screen)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog (pad, shorten, trim, wrap)
import XMonad.Hooks.UrgencyHook
import XMonad.StackSet hiding (filter)
import XMonad.Util.NamedWindows

import qualified Data.Sequence as S
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.WorkspaceCompare as WC

type WindowSpaceScreen
  = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

type ProjectId
  = String

data ProjectsConfig
  = ProjectsConfig
      { _pcDefaultWorkspaceId     :: WorkspaceId
      , _pcWorkspaceIdsPerProject :: [WorkspaceId]
      }

data ProjectsState
  = ProjectsState
      { _psConfig           :: ProjectsConfig
      , _psProjectIds       :: S.Seq ProjectId
      , _psCurrentProjectId :: Maybe ProjectId
      }

  deriving (Typeable)

newtype MaybeProjectsState
  = MaybeProjectsState { getMaybeProjectsState :: Maybe ProjectsState }
  deriving (Typeable)

instance ExtensionClass MaybeProjectsState where
  {-# INLINE initialValue #-}
  initialValue
    = MaybeProjectsState Nothing

{-# INLINE initialiseProjects #-}
initialiseProjects :: ProjectsConfig -> X ()
initialiseProjects conf
  = XS.put $ MaybeProjectsState $ Just ProjectsState
      { _psConfig           = conf
      , _psProjectIds       = S.empty
      , _psCurrentProjectId = Nothing
      }

modifyProjectsState :: (ProjectsState -> X ProjectsState) -> X ()
modifyProjectsState f
  = XS.get >>=
      maybe (return ()) (XS.put . MaybeProjectsState . Just <=< f) .
        getMaybeProjectsState

withProjectsState :: (ProjectsState -> X a) -> X ()
withProjectsState f
  = XS.get >>= maybe (return ()) (void . f) . getMaybeProjectsState

withCurrentProjectId :: (ProjectsState -> ProjectId -> X a) -> X ()
withCurrentProjectId f
  = withProjectsState $ \state -> do
      case _psCurrentProjectId state of
        Just pid  -> void (f state pid)
        Nothing   -> return ()

{-# INLINE mkWorkspaceName #-}
mkWorkspaceName :: ProjectId -> WorkspaceId -> String
mkWorkspaceName pid wid
  = pid ++ '[' : wid ++ "]"

addProject :: ProjectId -> X ()
addProject pid
  = modifyProjectsState $ \state -> do
      let conf = _psConfig state
          wids = _pcWorkspaceIdsPerProject conf

      forM_ wids (addHiddenWorkspace . mkWorkspaceName pid)

      let pids = _psProjectIds state
      return state { _psProjectIds = pid S.<| pids }

removeProject :: ProjectId -> X ()
removeProject pid
  = modifyProjectsState $ \state -> do
      let conf  = _psConfig state
          wids  = _pcWorkspaceIdsPerProject conf

      forM_ wids (removeProjectWorkspace conf . mkWorkspaceName pid)

      let pids  = _psProjectIds state
          cpid  = _psCurrentProjectId state

          pids' = S.filter (/= pid) pids
          cpid' = if cpid == Just pid then Nothing else cpid

      return state { _psProjectIds = pids', _psCurrentProjectId = cpid' }

removeProjectWorkspace :: ProjectsConfig -> WorkspaceId -> X ()
removeProjectWorkspace conf wid
  = do
      s <- get

      let ws    = windowset s
          dwid  = _pcDefaultWorkspaceId conf

      whenJust (removeWorkspaceFrom wid dwid ws) $ \(w, ws') ->
        put s { windowset = ws' }

removeWorkspaceFrom :: WorkspaceId
                    -> WorkspaceId
                    -> WindowSet
                    -> Maybe (WindowSpace, WindowSet)

removeWorkspaceFrom wid dwid ws
  | wid == tag cw
      = msum
          [ do  (ds, vss') <- removeWindowSpaceFrom dwid vss
                return (cw, updateWindowSet ds vss' hws ws)

          , do  (dw, hws') <- removeWindowSpaceFrom dwid hws
                return (cw, updateWindowSet (c { workspace = dw }) vss hws' ws)
          ]

  | otherwise
      = msum
          [ do  (s, vss') <- removeWindowSpaceFrom wid vss
                return (workspace s, updateWindowSet c vss' hws ws)

          , do  (w, hws') <- removeWindowSpaceFrom wid hws
                return (w, updateWindowSet c vss hws' ws)
          ]

  where
    c   = current ws
    cw  = workspace c
    vss = visible ws
    hws = hidden ws

updateWindowSet :: WindowSpaceScreen
                -> [WindowSpaceScreen]
                -> [WindowSpace]
                -> WindowSet
                -> WindowSet

updateWindowSet c vs hs ws
  = ws  { current = c
        , visible = vs
        , hidden  = hs
        }

class HasWindowSpace a where
  getWindowSpace :: a -> WindowSpace
  putWindowSpace :: WindowSpace -> a -> a

instance HasWindowSpace WindowSpace where
  {-# INLINE getWindowSpace #-}
  getWindowSpace
    = id

  {-# INLINE putWindowSpace #-}
  putWindowSpace
    = const

instance HasWindowSpace WindowSpaceScreen where
  {-# INLINE getWindowSpace #-}
  getWindowSpace
    = workspace

  {-# INLINE putWindowSpace #-}
  putWindowSpace w s
    = s { workspace = w }

removeWindowSpaceFrom :: HasWindowSpace a
                      => WorkspaceId
                      -> [a]
                      -> Maybe (a, [a])

removeWindowSpaceFrom wid []
  = Nothing

removeWindowSpaceFrom wid (x : xs)
  = if wid == tag (getWindowSpace x)
      then Just (x, xs)
      else do
        (y, ys) <- removeWindowSpaceFrom wid xs
        return (y, x : ys)

selectProject :: ProjectId -> X ()
selectProject pid
  = modifyProjectsState $ \state -> do
      return state { _psCurrentProjectId = Just pid }

withCurrentProjectNthWorkspace  :: (WorkspaceId -> WindowSet -> WindowSet)
                                -> Int
                                -> X ()

withCurrentProjectNthWorkspace f i
  = withCurrentProjectId $ \state pid -> do
      case drop i (_pcWorkspaceIdsPerProject (_psConfig state)) of
        (wid : _) -> windows (f (mkWorkspaceName pid wid))
        []        -> return ()

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


      let wss = ppWindowSet pp sf ws us
          ld  = description (layout (workspace (current ws)))

      p <- maybe "" (ppCurrentProjectId pp) . getMaybeProjectsState <$> XS.get
      t <- maybe (return "") (fmap show . getName) (peek ws)
      es <- mapM (flip catchX (return Nothing)) (_ppExtras pp)

      return $ encodeString $ separateBy (_ppSep pp) $ _ppOrder pp $
        [wss, p, _ppLayout pp ld, _ppTitle pp t] ++ catMaybes es

ppCurrentProjectId :: PP -> ProjectsState -> String
ppCurrentProjectId pp state
  = maybe "" (_ppProject pp) (_psCurrentProjectId state)

ppWindowSet :: PP -> WC.WorkspaceSort -> WindowSet -> [Window] -> String
ppWindowSet pp sf ws us
  = separateBy (_ppWsSep pp) . map format . sf $
      map workspace (current ws : visible ws) ++ hidden ws

    where
      format w  = ppWindowSpace ws us w pp (tag w)

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
