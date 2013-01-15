import Dzen
import Projects

import System.IO
import XMonad
import XMonad.Hooks.DynamicLog (dzenColor)
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn, spawnPipe)

import qualified XMonad.StackSet as SS

_xftFont
  = "Monaco:pixelsize=14:antialias=true"

_staticWorkspaces
  = ["6", "7", "8", "9", "0"]

_projectsConfig
  = ProjectsConfig
      { _pcDefaultWorkspaceId     = "0"
      , _pcWorkspaceIdsPerProject = ["1", "2", "3", "4", "5"]
      }

_dzenPP handle
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
              , _ppProject          = pad

              , _ppLayout           = dzenColor "#fdddef" "#353535" . pad


              , _ppTitle            = const ""
              , _ppOutput           = hPutStrLn handle
              }

_leftDzen
  = defaultDzenConfig { dzcWidth      = Just (Percent 50)
                      , dzcFont       = Just _xftFont
                      , dzcForeground = Just "#fdddef"
                      , dzcBackground = Just "#222222"
                      }

_rightDzen
  = defaultDzenConfig { dzcX          = Just (Percent 50)
                      , dzcWidth      = Just (Percent 50)
                      , dzcAlignment  = Just AlignRight
                      , dzcFont       = Just _xftFont
                      , dzcForeground = Just "#fdddef"
                      , dzcBackground = Just "#222222"
                      }

_dmenuArguments
  = [ "-nb", "#222222"
    , "-nf", "#fdddef"
    , "-sb", "#353535"
    , "-sf", "#fdddef"
    , "-fn", _xftFont
    ]

_keys
  = [ ("M-a a", addProject "")
    , ("M-a s", selectProject "")
    , ("M-a d", removeProject "")

    , ("M-b", sendMessage ToggleStruts)
    , ("M-p", safeSpawn "dmenu_run" _dmenuArguments)
    , ("M-s", safeSpawn "/home/will/.dzen/conch" (_terminal : _dmenuArguments))

    , ("M4-f", spawn $
        "google-chrome --disk-cache-dir=\"/tmp/google-chrome-cache\" " ++
          "--disk-cache-size=" ++ show (128 * 1024 * 1024))

    , ("M4-t", spawn _terminal)
    , ("M4-x", spawn "slock")
    ] ++

    [ ("M-" ++ m ++ show i, withCurrentProjectNthWorkspace f (i - 1))
      | i <- [1..5], (m, f) <- [("", SS.greedyView), ("S-", SS.shift)]
      ] ++

    [ ("M-" ++ m ++ show i, windows (f i))
      | i <- _staticWorkspaces, (m, f) <- [("", SS.greedyView), ("S-", SS.shift)]
      ]

_terminal
  = "urxvt"

main :: IO ()
main
  = do
      lh <- spawnDzen _leftDzen
      spawnToDzen "conky -c ~/.dzen/conky" _rightDzen

      xmonad $ defaultConfig
        { borderWidth = 0
        , layoutHook  = avoidStruts (layoutHook defaultConfig)
        , logHook     = projectsLogWithPP (_dzenPP lh)
        , startupHook = initialiseProjects _projectsConfig
        , terminal    = _terminal
        , workspaces  = _staticWorkspaces
        } `additionalKeysP` _keys
