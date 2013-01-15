module Projects.Dmenu
  ( addProjectMenu
  , addProjectDmenu
  , removeProjectMenu
  , removeProjectDmenu
  , selectProjectMenu
  , selectProjectDmenu
  ) where

import Projects.Core

import XMonad
import XMonad.Util.Dmenu

{-# INLINE addProjectMenu #-}
addProjectMenu :: String -> [String] -> X ()
addProjectMenu cmd args
  = menuArgs cmd args [] >>= addProject

{-# INLINE addProjectDmenu #-}
addProjectDmenu :: [String] -> X ()
addProjectDmenu
  = addProjectMenu "dmenu"

{-# INLINE removeProjectMenu #-}
removeProjectMenu :: String -> [String] -> X ()
removeProjectMenu cmd args
  = getProjects >>= menuArgs cmd args >>= removeProject

{-# INLINE removeProjectDmenu #-}
removeProjectDmenu :: [String] -> X ()
removeProjectDmenu
  = removeProjectMenu "dmenu"

{-# INLINE selectProjectMenu #-}
selectProjectMenu :: String -> [String] -> X ()
selectProjectMenu cmd args
  = getProjects >>= menuArgs cmd args >>= selectProject

{-# INLINE selectProjectDmenu #-}
selectProjectDmenu :: [String] -> X ()
selectProjectDmenu
  = selectProjectMenu "dmenu"
