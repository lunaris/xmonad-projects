import Dzen
import Projects

import System.IO
import XMonad
import XMonad.Hooks.DynamicLog (dzenColor)
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as SS

xftFont
  = "Monaco:pixelsize=14:antialias=true"

_staticWorkspaces
  = [ "dashboard"
    , "irssi"
    , "audio"
    , "web"
    ]

_projectsConfig
  = ProjectsConfig
      { _pcDefaultWorkspaceId     = "dashboard"
      , _pcWorkspaceIdsPerProject = ["local", "remote", "gimp"]
      }

dzenPP handle
  = defaultPP { _ppCurrent          = dzenColor "#fdddef" "#353535" .
                                        pad

              , _ppVisible          = dzenColor "#fdddef" "#222222" .
                                        pad

              , _ppHidden           = dzenColor "#fdddef" "#222222" .
                                        pad

              , _ppHiddenNoWindows  = const ""
              , _ppUrgent           = id
              , _ppWsSep            = ""
              , _ppSep              = ""
              , _ppProject          = id

              , _ppLayout           = dzenColor "#fdddef" "#353535" . pad


              , _ppTitle            = const ""
              , _ppOutput           = hPutStrLn handle
              }

leftDzen
  = defaultDzenConfig { dzcWidth      = Just (Percent 50)
                      , dzcFont       = Just xftFont
                      , dzcForeground = Just "#fdddef"
                      , dzcBackground = Just "#222222"
                      }

_keys
  = [ ("M-g", workspacePrompt defaultXPConfig (windows . SS.greedyView))
    , ("M-a p", addProject "pp")
    , ("M-a c", addProject "crs")
    , ("M-r p", removeProject "pp")
    , ("M-r c", removeProject "crs")
    , ("M-s p", selectProject "pp")
    , ("M-s c", selectProject "crs")
    ] ++

    [ ("M-" ++ show i, withCurrentProjectNthWorkspace SS.greedyView (i - 1))
      | i <- [1..3]
      ] ++

    [ ("M-S-" ++ show i, withCurrentProjectNthWorkspace SS.shift (i - 1))
      | i <- [1..3]
      ]

_terminal
  = "urxvt"

main :: IO ()
main
  = do
      leftHandle <- spawnDzen leftDzen

      xmonad $ defaultConfig
        { borderWidth = 0
        , layoutHook  = avoidStruts (layoutHook defaultConfig)
        , logHook     = projectsLogWithPP (dzenPP leftHandle)
        , startupHook = initialiseProjects _projectsConfig
        , terminal    = _terminal
        , workspaces  = _staticWorkspaces
        } `additionalKeysP` _keys
