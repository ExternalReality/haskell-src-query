{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
------------------------------------------------------------------------------
import Client
import HLint
import Lambda
import ParseAST

------------------------------------------------------------------------------
data Query = FreeVariables
           | LambdaBody
           | LambdaArgs
           | HLint
           | ParseAST
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
parseQueryArg :: String -> Maybe Query
parseQueryArg s | s == "freeVariables" = Just FreeVariables
                | s == "lambdaBody"    = Just LambdaBody
                | s == "lambdaArgs"    = Just LambdaArgs
                | s == "hlint"         = Just HLint
                | s == "parse"         = Just ParseAST
                | otherwise            = Nothing

------------------------------------------------------------------------------
parseClientArg :: String -> Maybe Client
parseClientArg s | s == "SublimeText" = Just SublimeText
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
runQuery LambdaBody _ _ _ _ code = return $ lambdaBody code
runQuery LambdaArgs _ _ _ _ code = return $ lambdaArgs code
runQuery HLint      _ _ _ _ code = hlint code
runQuery ParseAST   _ _ _ _ code = return $ parseAST code

------------------------------------------------------------------------------
fn c = (\a b -> a + b) c
