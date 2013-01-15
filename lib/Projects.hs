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
  , pad
  , shorten
  , trim
  , wrap

  , projectsLog
  , projectsLogWithPP
  ) where

import Projects.Core
import Projects.PrettyPrint
