{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParseAST (parseAST, parseMode, D(..)) where

------------------------------------------------------------------------------
import Language.Haskell.Exts.Annotated
------------------------------------------------------------------------------
import Control.Applicative
import Data.Data
import Data.List
import Data.Maybe

------------------------------------------------------------------------------
data D = forall a. Data a => D a

-----------------------------------------------------------------------------------------
-- | The 'empty' method isn't (shouldn't be) used, so this isn't a
-- real Alternative instance (perhaps a Semigroup might do?). But it's
-- handy.
instance Alternative ParseResult where
  empty = ParseFailed undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x

------------------------------------------------------------------------------
parseAST :: String -> [Char]
parseAST code = case parseTopLevel parseMode code of
  ParseOk (D ast) -> "[" ++  (intercalate "," (genHSE ast)) ++ "]"
  ParseFailed _ _ -> "[]"

-----------------------------------------------------------------------------------------
parseTopLevel :: ParseMode -> String -> ParseResult D 
parseTopLevel mode code =
  D . fix <$> parseDeclWithMode mode code   <|>
  D       <$> parseImport mode code         <|>
  D . fix <$> parseModuleWithMode mode code <|>
  D       <$> parseModulePragma mode code

-----------------------------------------------------------------------------------------
fix :: AppFixity ast => ast SrcSpanInfo -> ast SrcSpanInfo
fix ast = fromMaybe ast (applyFixities baseFixities ast) 

-----------------------------------------------------------------------------------------
-- | Pre-children tweaks for a given parent at index i.
--
pre :: (Typeable a) => a -> Integer -> [String]
pre x i =
  case cast x of
    -- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
    Just (RecUpdate SrcSpanInfo{srcInfoPoints=(start:_),srcInfoSpan=end} _ _)
      | i == 1 ->
        [spanHSE (show "RecUpdates")
                 "RecUpdates"
                 (SrcSpan (srcSpanFilename start)
                          (srcSpanStartLine start)
                          (srcSpanStartColumn start)
                          (srcSpanEndLine end)
                          (srcSpanEndColumn end))]
    _ -> case cast x :: Maybe (Deriving SrcSpanInfo)  of
           -- <deriving (X,Y,Z)> becomes <deriving (<X,Y,Z>)
           Just (Deriving _ ds@(_:_)) ->
             [spanHSE (show "InstHeads")
                      "InstHeads"
                      (SrcSpan (srcSpanFilename start)
                               (srcSpanStartLine start)
                               (srcSpanStartColumn start)
                               (srcSpanEndLine end)
                               (srcSpanEndColumn end))
             |Just (IHead (SrcSpanInfo start _) _ _) <- [listToMaybe ds]
             ,Just (IHead (SrcSpanInfo end _) _ _) <- [listToMaybe (reverse ds)]]
           _ -> []

-----------------------------------------------------------------------------------------
-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> String
spanHSE typ cons SrcSpan{..} = "[" ++ (intercalate "," spanContent) ++ "]"
  where unqualify   = dropUntilLast '.'
        spanContent = [ show $ dropSrcSpanText (filter (/= '"') (unqualify typ)) 
                      , show cons
                      , show srcSpanStartLine
                      , show srcSpanStartColumn
                      , show srcSpanEndLine
                      , show srcSpanEndColumn]
          where
            dropSrcSpanText str = if "SrcSpanInfo" `isSuffixOf` str
                                    then takeWhile (/= ' ') str
                                    else str

------------------------------------------------------------------------------
-- | Like 'dropWhile', but repeats until the last match.
dropUntilLast :: Char -> String -> String
dropUntilLast ch = go []
  where
    go _ (c:cs) | c == ch = go [] cs
    go acc (c:cs)         = go (c:acc) cs
    go acc []             = reverse acc

------------------------------------------------------------------------------
parseMode :: ParseMode
parseMode =
  defaultParseMode { extensions = allExtensions
                   , fixities   = Nothing
                   }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True

------------------------------------------------------------------------------
-- Parsers that HSE hackage doesn't have
parseImport :: ParseMode -> String -> ParseResult (ImportDecl SrcSpanInfo)
parseImport mode code =
  case parseModuleWithMode mode code of
    ParseOk (Module _ _ _ [i] _) -> return i
    ParseOk _ -> ParseFailed noLoc "parseImport"
    ParseFailed x y -> ParseFailed x y

------------------------------------------------------------------------------
parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ "\nmodule X where") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc "parseModulePragma"
    ParseFailed x y -> ParseFailed x y

------------------------------------------------------------------------------
genHSE :: Data a => a -> [String]
genHSE x =
  case gmapQ D x of
    zs@(D y:ys) ->
      case cast y of
        Just s ->
          spanHSE (show (show (typeOf x)))
                  (showConstr (toConstr x))
                  (srcInfoSpan s) :
          concatMap (\(i,D d) -> pre x i ++ genHSE d)
                    (zip [0..] ys)
        _ ->
          concatMap (\(D d) -> genHSE d) zs
    _ -> []
