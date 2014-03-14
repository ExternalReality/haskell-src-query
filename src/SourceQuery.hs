module SourceQuery ( allNames
                   , extractFreeVariables
                   , allBindings
                   ) where


import Language.Haskell.Exts
import Data.Generics.Aliases
import Data.Generics.Schemes
------------------------------------------------------------------------------
import Data.List

------------------------------------------------------------------------------
extractFreeVariables :: GenericQ [String]
extractFreeVariables ast = allNames (allVariables ast) \\
                           allNames (allBindings ast)

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


