module SpellCheck ( misspelledBindingNames
                  , bindingNameSuggestion
                  ) where

import Data.ByteString.Char8        (pack, unpack)
import Data.Monoid      ((<>), Monoid)
import Language.Aspell
import Text.Inflections
import Text.Inflections.Parse.Types

-- data Misspelling = Mispelling { misspelledName     :: String
--                               , misspelledSegments :: String
--                               }

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
                                         . (foldl1 (combineM)) $ suggestions
 where 
   filterPunctuation = filter (\s -> (notElem '\'' s) && 
                                     (notElem ' ' s)  &&
                                     (notElem '-' s))

------------------------------------------------------------------------------
combineM :: Monoid a => [a] -> [a] -> [a]
combineM = combineWith (<>)

------------------------------------------------------------------------------
-- General utility
combineWith :: (a -> a -> a) -> [a] -> [a] -> [a]
combineWith fn a b = do
  a' <- a
  b' <- b
  return $ a' `fn` b'
     
------------------------------------------------------------------------------
misspelledBindingNames :: SpellChecker -> [[String]] -> [[String]]
misspelledBindingNames checker = filter (misspelledName checker)

------------------------------------------------------------------------------
misspelledName :: SpellChecker -> [String] -> Bool
misspelledName _  [] = True
misspelledName checker (name:_) = either (const True) -- if is not camel case ignore
                                         (any (misspelledWord checker))
                                         (parseCamelCase [] name)

------------------------------------------------------------------------------
misspelledWord :: SpellChecker -> Word -> Bool
misspelledWord checker (Word nameSegment) = not $ check checker $ pack nameSegment
misspelledWord _ _                        = True
