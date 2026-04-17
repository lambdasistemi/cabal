module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)
import qualified Data.Set as Set

import Distribution.Package (UnitId, packageId)
import qualified Distribution.InstalledPackageInfo as IPI

import qualified Distribution.Simple.PackageIndex as SI

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $
                  InstSolverPackage {
                    instSolverPkgIPI = fromJust $ SI.lookupUnitId iidx pi,
                    instSolverPkgClosureDeps = installedDepsClosure iidx pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

-- | Collect the transitive installed-unit closure of a selected pre-existing
-- unit so the final install plan remains closed even when some of those units
-- never appear as solver nodes.
installedDepsClosure :: SI.InstalledPackageIndex -> UnitId -> [IPI.InstalledPackageInfo]
installedDepsClosure iidx rootUnitId =
  case SI.lookupUnitId iidx rootUnitId of
    Nothing -> []
    Just rootIpi -> go (Set.singleton rootUnitId) (IPI.depends rootIpi)
      where
        go _ [] = []
        go seen (depUnitId : depUnitIds)
          | depUnitId `Set.member` seen = go seen depUnitIds
          | otherwise =
              case SI.lookupUnitId iidx depUnitId of
                Just depIpi ->
                  depIpi : go (Set.insert depUnitId seen) (IPI.depends depIpi ++ depUnitIds)
                _ -> go seen depUnitIds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI pi                     = Right (packageId (either id id (convConfId pi)))

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (PackagePath _ q) pn) (I v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId sourceId pi)
        _otherwise
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId sourceId)
          | otherwise    -> Left  (PlannedId sourceId)
  where
    sourceId    = PackageIdentifier pn v
