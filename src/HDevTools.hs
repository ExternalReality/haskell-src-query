module HDevTools (isInModuleScope) where

import Control.Monad
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
      maybeOptionSrcDirs <- ghcOptionSrcDirs cabalFilePath buildTargetName 
      case maybeOptionSrcDirs of
        Just optionSrcDirs -> do 
          let args = ["info", filePath, symName, packageConfigOption] ++ optionSrcDirs
          readProcessWithExitCode hdevtools args ""
        Nothing -> readProcessWithExitCode hdevtools ["info", filePath, symName, packageConfigOption]  ""
                                                                         
------------------------------------------------------------------------------
ghcOptionSrcDirs :: String -> String ->  IO (Maybe [String])
ghcOptionSrcDirs cabalFilePath buildTargetName = do 
  maybeSrcDirs <- buildTargetSrcDirs cabalFilePath buildTargetName
  case maybeSrcDirs of
    Just srcDirs -> return $ Just $ map ghcOptionSrcDir srcDirs
    Nothing      -> return $ Nothing


------------------------------------------------------------------------------
hdevtools :: String           
hdevtools = "hdevtools"

------------------------------------------------------------------------------
ghcOptionPkgConfig :: String -> String
ghcOptionPkgConfig =  (++) "-g-package-conf"

------------------------------------------------------------------------------
ghcOptionSrcDir :: String -> String
ghcOptionSrcDir = (++) "-g-i"

fn = isInModuleScope "src/HSrcQuery.hs" ".cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/" "haskell-src-query.cabal" "haskell-src-query" "runQuery"
