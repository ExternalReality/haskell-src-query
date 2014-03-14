{-# LANGUAGE ExistentialQuantification #-}
module SpellCheck (misspelledBindingNames) where

import Data.ByteString.Char8 (pack)
import Language.Aspell
import Text.Inflections
import Text.Inflections.Parse.Types
import Language.Haskell.Exts

------------------------------------------------------------------------------
misspelledBindingNames :: SpellChecker -> [String] -> [String]
misspelledBindingNames checker = filter (misspelledName checker)

------------------------------------------------------------------------------
misspelledName :: SpellChecker -> String -> Bool
misspelledName checker name = either (const True) -- if is not camel case ignore
                                     (any (mispelledWord checker))
                                     (parseCamelCase [] name)
  
------------------------------------------------------------------------------
mispelledWord :: SpellChecker -> Word -> Bool
mispelledWord checker (Word nameSegment) = not $ check checker $ pack nameSegment
mispelledWord _ _                        = True 

------------------------------------------------------------------------------
parseMode :: ParseMode
parseMode =
  defaultParseMode { extensions = allExtensions
                   , fixities   = Nothing
                   }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True
