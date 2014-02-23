{-# LANGUAGE RecordWildCards #-}

module Cabal (buildTargetSrcDirs) where

import Data.List
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

type BuildTargetName = String
type SrcDirs = String
type Cond a = (BuildTargetName, CondTree ConfVar [Dependency] a)

------------------------------------------------------------------------------
buildTargetSrcDirs :: FilePath -> BuildTargetName -> IO (Maybe [SrcDirs])
buildTargetSrcDirs cabalFilePath btn = do
  gPDesc <- readPackageDescription silent cabalFilePath
  return $ findSrcDirs gPDesc btn

------------------------------------------------------------------------------
findSrcDirs :: GenericPackageDescription -> BuildTargetName -> Maybe [SrcDirs]
findSrcDirs GenericPackageDescription{..} btn =
  case findTarget btn condExecutables of
    Just execTarget -> Just . hsSourceDirs . buildInfo $ execTarget
    Nothing         -> case findTarget btn condTestSuites of
      Just testTarget -> Just . hsSourceDirs . testBuildInfo $ testTarget
      Nothing         -> case findTarget btn condBenchmarks of
        Just benchmarkTarget -> Just . hsSourceDirs . benchmarkBuildInfo $ benchmarkTarget
        Nothing              -> Nothing

------------------------------------------------------------------------------
findTarget :: BuildTargetName -> [(Cond a)] -> Maybe a
findTarget btn targets = do
  (_, CondNode a _ _ ) <- find isBuildTarget targets
  return a 
  where
    isBuildTarget (name, _) = name == btn          
