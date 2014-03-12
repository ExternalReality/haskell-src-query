{-# LANGUAGE RecordWildCards #-}

module Cabal ( buildTargetSrcDirs
             , buildTargetNames'
             , findSrcDirs 
             ) where

import Control.Applicative
import Data.List
import Data.Monoid
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

type BuildTargetName = String
type SrcDirs = String
type Cond a = (BuildTargetName, CondTree ConfVar [Dependency] a)

------------------------------------------------------------------------------
buildTargetNames' :: GenericPackageDescription -> [String]
buildTargetNames' GenericPackageDescription{..} =  (fst <$> condExecutables) 
                                                <> (fst <$> condTestSuites)
                                                <> (fst <$> condBenchmarks)
    
------------------------------------------------------------------------------
buildTargetSrcDirs :: FilePath -> BuildTargetName -> IO [SrcDirs]
buildTargetSrcDirs cabalFilePath btn = do
  gPDesc <- readPackageDescription silent cabalFilePath
  return $ findSrcDirs gPDesc btn

------------------------------------------------------------------------------
findSrcDirs :: GenericPackageDescription -> BuildTargetName -> [SrcDirs]
findSrcDirs GenericPackageDescription{..} btn =
  case findTarget btn condExecutables of
    Just execTarget -> hsSourceDirs . buildInfo $ execTarget
    Nothing         -> case findTarget btn condTestSuites of
      Just testTarget -> hsSourceDirs . testBuildInfo $ testTarget
      Nothing         -> case findTarget btn condBenchmarks of
        Just benchmarkTarget -> hsSourceDirs . benchmarkBuildInfo $ benchmarkTarget
        Nothing              -> []

------------------------------------------------------------------------------
findTarget :: BuildTargetName -> [Cond a] -> Maybe a
findTarget btn targets = do
  (_, CondNode a _ _ ) <- find isBuildTarget targets
  return a 
  where
    isBuildTarget (name, _) = name == btn          
