module HDevTools (isInModuleScope) where

import Data.List
import System.Exit
import System.Process
------------------------------------------------------------------------------
import Cabal

------------------------------------------------------------------------------
type SymbolName = String

------------------------------------------------------------------------------
isInModuleScope :: FilePath -> FilePath -> FilePath -> String ->  SymbolName -> IO Bool
isInModuleScope filePath pkgConfigPath cabalFilePath buildTargetName symName = do 
  (exitCode, _, stderr) <- search
  case exitCode of
    ExitFailure _ -> return $ "Not in scope" `isInfixOf` stderr
    _             -> return False
  where
    search = do
      let packageConfigOption = ghcOptionPkgConfig pkgConfigPath
      srcDirs <- ghcOptionSrcDirs cabalFilePath buildTargetName 
      let args = ["info", filePath, symName, packageConfigOption] ++ srcDirs
      readProcessWithExitCode hdevtools args ""
                                                                               
------------------------------------------------------------------------------
ghcOptionSrcDirs :: String -> String ->  IO [String]
ghcOptionSrcDirs cabalFilePath buildTargetName = do 
  srcDirs <- buildTargetSrcDirs cabalFilePath buildTargetName
  return $ map ghcOptionSrcDir srcDirs
 
------------------------------------------------------------------------------
hdevtools :: String           
hdevtools = "hdevtools"

------------------------------------------------------------------------------
ghcOptionPkgConfig :: String -> String
ghcOptionPkgConfig =  (++) "-g-package-conf"

------------------------------------------------------------------------------
ghcOptionSrcDir :: String -> String
ghcOptionSrcDir = (++) "-g-i"
