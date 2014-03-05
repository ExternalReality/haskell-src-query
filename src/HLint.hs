{-# LANGUAGE RecordWildCards #-}

module HLint (hlint) where

import Control.Applicative
import Data.Maybe
import Temporary.API
import Language.Haskell.Exts.Annotated (SrcSpan(..))

------------------------------------------------------------------------------
hlint :: String -> IO String
hlint code = do
 ideas <- genIdeas code
 if length ideas > 0 
   then return $ serializeIdeas ideas
   else return "[]"

------------------------------------------------------------------------------
genIdeas :: String -> IO [Idea]
genIdeas code =  do 
  (parseFlags, classifications, hint) <- autoSettings
  eitherErrorModule <- parseModuleEx parseFlags "" (Just code)
  case eitherErrorModule of
     Left _    -> error "error parsing code" 
     Right msi -> return $ applyHints classifications hint [msi]

------------------------------------------------------------------------------
serializeIdeas :: [Idea] -> String
serializeIdeas ideas = "[" ++ (concat $ serializeIdea <$> ideas) ++ "]"

------------------------------------------------------------------------------
serializeIdea ::  Idea -> String
serializeIdea idea = 
  case ideaTo idea of
    Just to -> "[" ++ concat (ideaContent to (ideaSpan idea)) ++ "]"
    Nothing -> ""
  where 
    ideaContent to SrcSpan{..} =  
       [ show . show $ ideaSeverity idea
       , show $ findSuggestion (ideaHint idea)
       , show $ ideaFrom idea 
       , show to
       , show . show $ srcSpanStartLine
       , show . show $ srcSpanStartColumn
       , show . show $ srcSpanEndLine
       , show . show $ srcSpanEndColumn
       ]
             
------------------------------------------------------------------------------
findSuggestion :: String -> String
findSuggestion i = fromMaybe "Error: No suggestion string mapping yet" $ 
                  lookup i [ ("Redundant lambda", "remove redundant lambda")
                           , ("Avoid lambda", "move lambda to top level")
                           , ("Collapse lambdas", "collapse nested lambdas") 
                           , ("Redundant bracket", "remove redundant bracket")
                           , ("Eta reduce", "perform eta reduction")
                           ]
