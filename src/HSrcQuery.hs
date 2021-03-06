{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Options.Applicative.Types
------------------------------------------------------------------------------
import Cabal
import Client
import HLint
import Lambda
import ParseAST
------------------------------------------------------------------------------
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

------------------------------------------------------------------------------
data Query = FreeVariables
           | LambdaBody
           | LambdaArgs
           | HLint
           | ParseAST
           | BuildTargets
             deriving Show

------------------------------------------------------------------------------
data Request = Request  { query            :: Query
                        , client           :: Client
                        , srcFilePath      :: FilePath
                        , pkgConfigDirPath :: FilePath
                        , cabalFilePath    :: FilePath
                        , buildTargetName  :: String
                        }

------------------------------------------------------------------------------
requestParser :: Parser Request
requestParser = Request <$> argument parseQueryArg
                        ( metavar "QUERY"
                          <> help "Query to run on selected code.")
                  <*> argument parseClientArg
                        ( metavar "CLIENT"
                          <> help "Client Emacs or Sublime" )
                  <*> strOption ( long "source-file"
                                       <> metavar "FILE"
                                       <> help "File containing selected code."
                                       <> value "")
                  <*> strOption ( long "package-conf"
                                       <> metavar "DIRECTORY"
                                       <> help "GHC package config directory."
                                       <> value "")
                  <*> strOption ( long "cabal-file"
                                       <> metavar "FILE"
                                       <> help "Cabal file"
                                       <> value "")
                  <*> strOption ( long "build-target"
                                       <> metavar "TARGET"
                                       <> help "Target of build"
                                       <> value "")

------------------------------------------------------------------------------
requestParserInfo :: ParserInfo Request
requestParserInfo = info (helper <*> requestParser)
                      ( fullDesc
                      <> progDesc "Query infromation about Haskell source."
                      <> header "haskell-src-query - get info about haskell src")

------------------------------------------------------------------------------
main :: IO ()
main = do
  Request{..} <- execParser requestParserInfo
  code        <- getContents
  putStrLn =<< runQuery query
                        srcFilePath
                        pkgConfigDirPath
                        cabalFilePath
                        buildTargetName
                        code

------------------------------------------------------------------------------
parseQueryArg :: ReadM Query
parseQueryArg = do
  s <- readerAsk
  case readQueryArg s of
    Just query -> return query
    Nothing    -> readerError "Invalid query"
  where
    readQueryArg 
        s | s == "freeVariables" = Just FreeVariables
          | s == "lambdaBody"    = Just LambdaBody
          | s == "lambdaArgs"    = Just LambdaArgs
          | s == "hlint"         = Just HLint
          | s == "parse"         = Just ParseAST
          | s == "targets"       = Just BuildTargets
          | otherwise            = Nothing

------------------------------------------------------------------------------
parseClientArg :: ReadM Client
parseClientArg = do
    s <- readerAsk
    case readClientArg s of
      Just query -> return query
      Nothing    -> readerError "Invalid query"
   where 
    readClientArg s | s == "SublimeText" = Just SublimeText
                    | s == "Emacs"       = Just Emacs
                    | otherwise          = Nothing

------------------------------------------------------------------------------
runQuery
  :: Query
     -> FilePath
     -> FilePath
     -> FilePath
     -> String
     -> String
     -> IO String
runQuery FreeVariables srcFile
                       pkgConfigPath
                       cabalFilePath
                       buildTargetName
                       code =
  freeVariables srcFile
                pkgConfigPath
                cabalFilePath
                buildTargetName
                code
runQuery LambdaBody   _ _ _ _ code = return $ lambdaBody code
runQuery LambdaArgs   _ _ _ _ code = return $ lambdaArgs code
runQuery HLint        _ _ _ _ code = hlint code
runQuery ParseAST     _ _ _ _ code = return $ parseAST code
runQuery BuildTargets _ _ cabaFilePath _ _ = targets cabaFilePath


------------------------------------------------------------------------------
targets :: FilePath -> IO String
targets cabalFilePath =  do 
  gpDesc <- readPackageDescription silent cabalFilePath
  return . show . buildTargetNames' $ gpDesc 
