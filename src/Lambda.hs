module Lambda ( freeVariables
              , lambdaArgs
              , lambdaBody
              ) where

import Control.Monad
import Data.Char
import Language.Haskell.Exts
------------------------------------------------------------------------------
import HDevTools
import SourceQuery

------------------------------------------------------------------------------
freeVariables :: FilePath -> FilePath -> FilePath -> String -> String -> IO String
freeVariables srcPath pkgConfigPath cabalFilePath buildTargetName code = case parseExp code of
  ParseOk ast -> do
   names <- dropModuleVariableNames srcPath
                                    pkgConfigPath
                                    cabalFilePath
                                    buildTargetName $ extractFreeVariables ast
   return . dropCommas $ show names
  _ -> return "Error parsing freeVars"

------------------------------------------------------------------------------
lambdaBody ::  String -> String
lambdaBody code = case parseExp code of
  ParseOk ast -> show . extractLambdaBody $ ast
  _           -> "[]"

------------------------------------------------------------------------------
lambdaArgs :: String -> String
lambdaArgs code = case parseExp code of
  ParseOk ast ->  show $ prettyLambdaArgs ast
  _           -> "[]"
  where
    prettyLambdaArgs = trim
                     . takeWhile (/= '-')
                     . dropWhile (== '\\')
                     . prettyPrint
                     
------------------------------------------------------------------------------
dropCommas :: String -> String
dropCommas = filter (/= ',')

------------------------------------------------------------------------------
extractLambdaBody :: Exp -> String
extractLambdaBody (Lambda _ _ ast) = prettyPrint ast
extractLambdaBody _ = "[]"

------------------------------------------------------------------------------
dropModuleVariableNames :: FilePath 
                        -> FilePath 
                        -> FilePath 
                        -> String 
                        -> [String] 
                        -> IO [String]
dropModuleVariableNames srcPath pkgConfigPath cabalFilePath buildTargetName  =
  filterM $  (liftM not) .  isInModuleScope srcPath
                                            pkgConfigPath
                                            cabalFilePath
                                            buildTargetName
                                            
------------------------------------------------------------------------------
trim :: [Char] -> [Char]
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

------------------------------------------------------------------------------
dropSpaceTail :: [Char] -> [Char] -> [Char]
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x       = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

