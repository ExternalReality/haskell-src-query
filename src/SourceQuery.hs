{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module SourceQuery ( allNames
                   , extractFreeVariables
                   , allBindings
                   , allNamesWithLocations
                   , allMatches
                   , allMatchNames
                   ) where


import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Language.Haskell.Exts.Annotated
------------------------------------------------------------------------------
import           Data.List

------------------------------------------------------------------------------
extractFreeVariables :: GenericQ [String]
extractFreeVariables ast = allNames (allVariables ast) \\
                           allNames (allBindings ast)

------------------------------------------------------------------------------
allVariables :: GenericQ [Exp SrcSpanInfo]
allVariables = listify isVar

------------------------------------------------------------------------------
allBindings :: GenericQ [Pat SrcSpanInfo]
allBindings = listify isBinding

------------------------------------------------------------------------------
allMatches :: GenericQ [Match SrcSpanInfo]
allMatches = listify isMatch

------------------------------------------------------------------------------
matchNames :: [Match SrcSpanInfo] -> [[String]]
matchNames names = map matchName names
  where matchName (Match _ (Ident l s)  _ _ _ ) = nameLocations l s 
        matchName (Match _ (Symbol l s)  _ _ _ ) = nameLocations l s
        matchName (InfixMatch _ _ (Ident l s) _ _ _) = nameLocations l s
        matchName (InfixMatch _ _ (Symbol l s) _ _ _) = nameLocations l s
        
------------------------------------------------------------------------------
allMatchNames :: GenericQ [[String]]
allMatchNames = ([] `mkQ` matchNames)

------------------------------------------------------------------------------
allNames :: GenericQ [String]
allNames = allNamesWith nameString

------------------------------------------------------------------------------
allNamesWithLocations :: GenericQ [[String]]
allNamesWithLocations = allNamesWith nameLocation

------------------------------------------------------------------------------
allNamesWith :: (Name SrcSpanInfo -> a) -> GenericQ [a]
allNamesWith fn = everything (++) ([] `mkQ` fmap (: []) fn)

------------------------------------------------------------------------------
isVar :: Exp SrcSpanInfo -> Bool
isVar (Var _ _) = True
isVar  _       = False

------------------------------------------------------------------------------
isBinding :: Pat SrcSpanInfo -> Bool
isBinding (PVar _ _ ) = True
isBinding _           = False

------------------------------------------------------------------------------
isMatch :: Match SrcSpanInfo -> Bool
isMatch (Match _ _ _ _ _) = True
isMatch _                 = False 

------------------------------------------------------------------------------
nameString :: Name SrcSpanInfo -> String
nameString (Symbol _ str) = str
nameString (Ident _ str)  = str

------------------------------------------------------------------------------
nameLocation :: Name SrcSpanInfo -> [String]
nameLocation (Symbol srcSpanInfo str) = nameLocations srcSpanInfo str
nameLocation (Ident  srcSpanInfo str) = nameLocations srcSpanInfo str

------------------------------------------------------------------------------
nameLocations :: SrcSpanInfo -> String -> [String]
nameLocations SrcSpanInfo{..} str = ([ str
                                     , show (srcSpanStartLine srcInfoSpan)
                                     , show (srcSpanStartColumn srcInfoSpan)
                                     , show (srcSpanEndLine srcInfoSpan)
                                     , show (srcSpanEndColumn srcInfoSpan)
                                     ])
