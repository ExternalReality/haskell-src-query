{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpellCheck ( misspelledBindingNames
                  , bindingNameSuggestion
                  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Char8           (pack, unpack)
import           Data.Monoid                     (Monoid, (<>))
import           Language.Aspell
import           Language.Haskell.Exts.Annotated
import           Text.Inflections
import           Text.Inflections.Parse.Types

------------------------------------------------------------------------------
data Misspelling = Mispelling { misspelledName     :: String
                              , misspelledSegments :: [String]
                              , location           :: SrcSpan
                              }

$(deriveToJSON defaultOptions ''Misspelling)

instance ToJSON SrcSpan where
  toJSON SrcSpan{..} = toJSON [ srcSpanStartLine
                              , srcSpanEndLine
                              , srcSpanStartColumn
                              , srcSpanEndColumn
                              ]

------------------------------------------------------------------------------
bindingNameSuggestion :: SpellChecker -> String -> IO [String]
bindingNameSuggestion sp bindingName = do
  case parseCamelCase [] bindingName of
    Left _ -> return []
    Right words -> do
      constituents <- sequence $ map (constituentSuggestions sp) words
      return $ combineConstituentSuggestion constituents

------------------------------------------------------------------------------
constituentSuggestions :: SpellChecker -> Word -> IO [String]
constituentSuggestions _  (Acronym s)                    = return [s]
constituentSuggestions sp (Word s)   | check sp (pack s) = return [s]
                                     | otherwise         = suggestions
  where
    suggestions :: IO [String]
    suggestions =  map unpack `fmap` suggest sp (pack s)

------------------------------------------------------------------------------
combineConstituentSuggestion :: [[String]] -> [String]
combineConstituentSuggestion [] = []
combineConstituentSuggestion suggestions = take 10
                                         . filterPunctuation
                                         . (foldl1 (combine)) $ suggestions
 where
   filterPunctuation = filter (\s -> (notElem '\'' s) &&
                                     (notElem ' ' s)  &&
                                     (notElem '-' s))


------------------------------------------------------------------------------
-- General utility

combine :: Monoid a => [a] -> [a] -> [a]
combine = combineWith (<>)

combineWith :: (a -> a -> a) -> [a] -> [a] -> [a]
combineWith fn a b = do
  a' <- a
  b' <- b
  return $ a' `fn` b'

------------------------------------------------------------------------------
misspelledBindingNames :: SpellChecker -> [Name SrcSpanInfo] -> [Misspelling]
misspelledBindingNames sp = filterMap (nameToMisspelling sp)

------------------------------------------------------------------------------
nameToMisspelling :: SpellChecker -> Name SrcSpanInfo ->  Maybe Misspelling
nameToMisspelling sp (Ident SrcSpanInfo{..} nameStr) =
  case findMisspelledSegments sp nameStr  of
    [] -> Nothing
    segments -> Just $ Mispelling nameStr
                                  segments
                                  srcInfoSpan

------------------------------------------------------------------------------
findMisspelledSegments :: SpellChecker -> String -> [String]
findMisspelledSegments checker name = either (const [])
                                              misspelledNameSegments
                                              (parseCamelCase [] name)
  where
    misspelledNameSegments = (map stringFromWord) . filter (misspelledWord checker)
    stringFromWord (Word str)    = str
    stringFromWord (Acronym str) = str

------------------------------------------------------------------------------
misspelledWord :: SpellChecker -> Word -> Bool
misspelledWord checker (Word nameSegment) = not $ check checker $ pack nameSegment
misspelledWord _ _                        = True

------------------------------------------------------------------------------
-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = go
    where
    go []     = []
    go (x:xs) = maybe (go xs) (: (go xs)) (f x)


------------------------------------------------------------------------------
-- add one to initial time
timeFunction :: Num a => a -> a
timeFunction initialTXyme = 2 + initialTyme 
