module Projects.Dmenu
  ( addProjectMenu
  , removeProjectMenu
  , selectProjectMenu
  ) where

import Projects.Core

import XMonad
import XMonad.Util.Dmenu

addProjectMenu :: String -> [String] -> X ()
addProjectMenu cmd args
  = menuArgs cmd args [] >>= addProject

removeProjectMenu :: String -> [String] -> X ()
removeProjectMenu cmd args
  = menuArgs cmd args [] >>= removeProject

selectProjectMenu :: String -> [String] -> X ()
selectProjectMenu cmd args
  = menuArgs cmd args [] >>= selectProject
