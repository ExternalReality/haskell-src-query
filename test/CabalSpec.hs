{-# LANGUAGE RecordWildCards #-}
module CabalSpec (spec) where

import Data.Monoid
import Distribution.PackageDescription
import Test.Hspec
------------------------------------------------------------------------------
import Cabal
import Data.Cabal

------------------------------------------------------------------------------
spec :: Spec
spec = describe "Cabal" $ do
  it "can build a list of build target names" $ buildTargetNamesTest
  it "can build a list of target source directories" $ buildTargetSrcDirsTest


------------------------------------------------------------------------------
buildTargetNamesTest :: Expectation
buildTargetNamesTest = names `shouldContain` [ executableBuildTargetName
                                             , testSuiteBuildTargetName
                                             , benchmarkBuildTargetName
                                             ]
  where 
    names = buildTargetNames' cabalPackageWithTargets

------------------------------------------------------------------------------
buildTargetSrcDirsTest :: Expectation
buildTargetSrcDirsTest = directories `shouldContain` sourceDirectories
  where
    directories = findSrcDirs cabalPackageWithSrcDirs 
                              executableBuildTargetName
