{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Data.ByteString.Char8                 (unpack, pack)
import Distribution.PackageDescription.Parse hiding (ParseResult (..))
import Distribution.Verbosity
import Language.Aspell
import Language.Haskell.Exts.Annotated
import Options.Applicative
------------------------------------------------------------------------------
import Cabal
import HLint
import Lambda
import ParseAST
import SourceQuery
import SpellCheck

------------------------------------------------------------------------------
data Query = FreeVariables
           | LambdaBody
           | LambdaArgs
           | SpellCheck
           | SpellSuggest
           | HLint
           | ParseAST
           | BuildTargets
             deriving Show

------------------------------------------------------------------------------
parseQueryArg :: String -> Maybe Query
parseQueryArg s | s == "freeVariables" = Just FreeVariables
                | s == "lambdaBody"    = Just LambdaBody
                | s == "lambdaArgs"    = Just LambdaArgs
                | s == "hlint"         = Just HLint
                | s == "parse"         = Just ParseAST
                | s == "targets"       = Just BuildTargets
                | s == "spellcheck"    = Just SpellCheck
                | s == "spellsuggest"  = Just SpellSuggest
                | otherwise            = Nothing

------------------------------------------------------------------------------
data Request = Request  { query            :: Query
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
-- TODO make a sensible monad for queries!
runQuery :: Query
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
runQuery BuildTargets _ _ cabalFilePath _ _ = targets cabalFilePath
runQuery SpellCheck   _ _ _ _ code = spellCheck code
runQuery SpellSuggest _ _ _ _ code = spellSuggest code

------------------------------------------------------------------------------
targets :: FilePath -> IO String
targets cabalFilePath = do
  gPDesc <- readPackageDescription silent cabalFilePath
  return . show . buildTargetNames' $ gPDesc

------------------------------------------------------------------------------
spellCheck :: String -> IO String
spellCheck code = case parseTopLevel parseMode code of
  ParseOk (D ast) -> do sp <- createEnglishSpellChecker
                        return . show
                               . misspelledBindingNames sp
                               . bindingNamesInScope $ ast
  ParseFailed _ _ -> error "error parsing"
  where
    bindingNamesInScope ast = allNamesWithLocations (allBindings ast) ++
                              allMatchNames (allMatches ast)

------------------------------------------------------------------------------
spellSuggest :: String -> IO String
spellSuggest bindingName = do
  sp <- createEnglishSpellChecker
  suggestions <- bindingNameSuggestion sp bindingName
  return . show $ suggestions

------------------------------------------------------------------------------
createEnglishSpellChecker :: IO SpellChecker
createEnglishSpellChecker = either (error . unpack) id <$> spellChecker
