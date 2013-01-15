{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Projects.Core where

import Control.Monad
import Data.Functor
import Data.List
import XMonad hiding (Screen)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.UrgencyHook
import XMonad.StackSet

import qualified Data.Sequence as S
import qualified XMonad.Util.ExtensibleState as XS

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

runLogHook :: X ()
runLogHook
  = asks (logHook . config) >>= userCodeDef ()

modifyProjectsState :: (ProjectsState -> X ProjectsState) -> X ()
modifyProjectsState f
  = XS.get >>=
      maybe (return ()) (XS.put . MaybeProjectsState . Just <=< f) .
        getMaybeProjectsState >>
          runLogHook

withProjectsState :: X a -> (ProjectsState -> X a) -> X a
withProjectsState m f
  = XS.get >>= maybe m f . getMaybeProjectsState

{-# INLINE withProjectsState_ #-}
withProjectsState_ :: (ProjectsState -> X a) -> X ()
withProjectsState_ f
  = withProjectsState (return ()) (void . f)

withCurrentProjectId :: (ProjectsState -> ProjectId -> X a) -> X ()
withCurrentProjectId f
  = withProjectsState_ $ \state -> do
      case _psCurrentProjectId state of
        Just pid  -> void (f state pid)
        Nothing   -> return ()

{-# INLINE projectWorkspaceNamePrefix #-}
projectWorkspaceNamePrefix :: String
projectWorkspaceNamePrefix
  = "//project//"

{-# INLINE mkProjectWorkspaceName #-}
mkProjectWorkspaceName :: ProjectId -> WorkspaceId -> WorkspaceId
mkProjectWorkspaceName pid wid
  = projectWorkspaceNamePrefix ++ pid ++ "//" ++ wid

splitProjectWorkspaceName :: WorkspaceId -> Maybe (ProjectId, WorkspaceId)
splitProjectWorkspaceName wid
  = do
      pidWid <- stripPrefix projectWorkspaceNamePrefix wid

      let f c1 c2
            = (c1 /= '/' && c2 /= '/') || (c1 == '/' && c2 == '/')

      case groupBy f pidWid of
        [pid, _, wid] -> Just (pid, wid)
        _             -> Nothing

{-# INLINE isProjectWorkspaceName #-}
isProjectWorkspaceName :: WorkspaceId -> Bool
isProjectWorkspaceName
  = isPrefixOf projectWorkspaceNamePrefix

addProject :: ProjectId -> X ()
addProject pid
  = modifyProjectsState $ \state -> do
      let conf = _psConfig state
          wids = _pcWorkspaceIdsPerProject conf

      forM_ wids (addHiddenWorkspace . mkProjectWorkspaceName pid)

      let pids = _psProjectIds state
      return state { _psProjectIds = pid S.<| pids }

removeProject :: ProjectId -> X ()
removeProject pid
  = modifyProjectsState $ \state -> do
      let conf  = _psConfig state
          wids  = _pcWorkspaceIdsPerProject conf

      forM_ wids (removeProjectWorkspace conf . mkProjectWorkspaceName pid)

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
        (wid : _) -> windows (f (mkProjectWorkspaceName pid wid))
        []        -> return ()
