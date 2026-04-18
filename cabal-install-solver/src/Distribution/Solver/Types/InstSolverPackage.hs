{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.InstSolverPackage
    ( InstSolverPackage(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package
  ( HasMungedPackageId (..)
  , HasUnitId (..)
  , Package (..)
  , packageName
  )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.SolverId
import Distribution.Types.LibraryName
  ( LibraryName (LMainLibName)
  )
import Distribution.Types.LibraryVisibility
  ( LibraryVisibility (LibraryVisibilityPublic)
  )
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.PackageId
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI

-- | An 'InstSolverPackage' is a pre-existing installed package
-- specified by the dependency solver.
data InstSolverPackage = InstSolverPackage {
      instSolverPkgIPI :: InstalledPackageInfo,
      -- | Installed units required to keep the elaborated pre-existing package
      -- closed in the final install plan.
      instSolverPkgClosureDeps :: [InstalledPackageInfo],
      instSolverPkgLibDeps :: ComponentDeps [SolverId],
      instSolverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary InstSolverPackage
instance Structured InstSolverPackage

instance Package InstSolverPackage where
    packageId i =
        let ipi = instSolverPkgIPI i
            MungedPackageId mpn v = mungedId i
            pn
              | IPI.libVisibility ipi == LibraryVisibilityPublic
              , IPI.sourceLibName ipi /= LMainLibName
              = packageName ipi
              | otherwise
              = encodeCompatPackageName mpn
        in PackageIdentifier pn v

instance HasMungedPackageId InstSolverPackage where
    mungedId = mungedId . instSolverPkgIPI

instance HasUnitId InstSolverPackage where
    installedUnitId = installedUnitId . instSolverPkgIPI
