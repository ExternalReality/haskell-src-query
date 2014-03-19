module SpellCheck (misspelledBindingNames) where

import           Data.ByteString.Char8        (pack)
import           Language.Aspell
import           Text.Inflections
import           Text.Inflections.Parse.Types

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
