import Projects

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as SS

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
      xmonad $ defaultConfig
        { borderWidth = 0
        , layoutHook  = avoidStruts (layoutHook defaultConfig)
        , startupHook = initialiseProjects _projectsConfig
        , terminal    = _terminal
        , workspaces  = _staticWorkspaces
        } `additionalKeysP` _keys
