module Lambda ( freeVariables
              , lambdaArgs
              , lambdaBody
              ) where

import Control.Monad
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.List
import Data.Char
import Language.Haskell.Exts
------------------------------------------------------------------------------
import HDevTools

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
extractLambdaArgs :: Exp -> String
extractLambdaArgs (Lambda _ ast _) = dropCommas . show $ allNames ast
extractLambdaArgs _                = "[]"

------------------------------------------------------------------------------
allVariables :: GenericQ [Exp]
allVariables = listify isVar

------------------------------------------------------------------------------
allBindings :: GenericQ [Pat]
allBindings = listify isBinding

------------------------------------------------------------------------------
allNames :: GenericQ [String]
allNames = everything (++) ([] `mkQ` fmap (: []) getStringFromName)

------------------------------------------------------------------------------
isVar :: Exp -> Bool
isVar (Var  _) = True
isVar  _       = False

------------------------------------------------------------------------------
isBinding :: Pat -> Bool
isBinding (PVar _) = True
isBinding _        = False

------------------------------------------------------------------------------
getStringFromName :: Name -> String
getStringFromName (Symbol str) = str
getStringFromName (Ident str)  = str

------------------------------------------------------------------------------
dropCommas :: String -> String
dropCommas = filter (/= ',')

------------------------------------------------------------------------------
extractLambdaBody :: Exp -> String
extractLambdaBody (Lambda _ _ ast) = prettyPrint ast
extractLambdaBody _ = "[]"

------------------------------------------------------------------------------
extractFreeVariables :: GenericQ [String]
extractFreeVariables ast = allNames (allVariables ast) \\
                           allNames (allBindings ast)

------------------------------------------------------------------------------
dropModuleVariableNames :: FilePath -> FilePath -> FilePath -> String -> [String] -> IO [String]
dropModuleVariableNames srcPath pkgConfigPath cabalFilePath buildTargetName  =
  filterM $  (liftM not) .  isInModuleScope srcPath
                                            pkgConfigPath
                                            cabalFilePath
                                            buildTargetName


------------------------------------------------------------------------------

trim :: [Char] -> [Char]
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: [Char] -> [Char] -> [Char]
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x       = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

