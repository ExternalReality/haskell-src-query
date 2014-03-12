module Data.Cabal where

import Data.Monoid
import Distribution.Package
import Distribution.PackageDescription


------------------------------------------------------------------------------
emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = 
  GenericPackageDescription emptyPackageDescription
                            mempty
                            Nothing
                            mempty
                            mempty
                            mempty

------------------------------------------------------------------------------
cabalPackageWithTargets :: GenericPackageDescription
cabalPackageWithTargets = 
  emptyGenericPackageDescription 
    { condExecutables = [(executableBuildTargetName, emptyCondTree)] 
    , condTestSuites  = [(testSuiteBuildTargetName,  emptyCondTree)] 
    , condBenchmarks  = [(benchmarkBuildTargetName,  emptyCondTree)] 
    } 
                                                                                
------------------------------------------------------------------------------
emptyCondTree :: (Monoid c, Monoid a) => CondTree ConfVar c a
emptyCondTree = CondNode mempty mempty mempty

------------------------------------------------------------------------------
cabalPackageWithSrcDirs = 
  emptyGenericPackageDescription
    { condExecutables = [("executable", condTreeWithExecutable)] }

------------------------------------------------------------------------------
condTreeWithExecutable :: CondTree v [Dependency] Executable
condTreeWithExecutable = CondNode executableWithBuildInfo mempty  mempty

------------------------------------------------------------------------------
executableWithBuildInfo :: Executable
executableWithBuildInfo = emptyExecutable { buildInfo = buildInfoWithSrcDirs }

------------------------------------------------------------------------------
buildInfoWithSrcDirs :: BuildInfo
buildInfoWithSrcDirs = emptyBuildInfo { hsSourceDirs = sourceDirectories }

------------------------------------------------------------------------------
sourceDirectories :: [FilePath]
sourceDirectories = ["path1", "path2"] 

------------------------------------------------------------------------------
executableBuildTargetName :: [Char]
executableBuildTargetName = "executable"

------------------------------------------------------------------------------
testSuiteBuildTargetName :: [Char]
testSuiteBuildTargetName = "test-suite"

------------------------------------------------------------------------------
benchmarkBuildTargetName :: [Char]
benchmarkBuildTargetName = "benchmark"
