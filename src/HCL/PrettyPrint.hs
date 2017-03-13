module HCL.PrettyPrint (
    pPrintHCL
  , Pretty (..)
  , Doc
  ) where

import Data.HashMap.Strict (toList, HashMap)
import HCL.Types
import qualified Data.Text as T
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.List(foldl')

pPrintHCL :: HCLDocument -> Doc
pPrintHCL = pPrint

instance Pretty HCLDocument where
  pPrint (HCLDocument statements) = hcat $ punctuate (text "\n\n") $ map pPrint statements

instance Pretty HCLObject where
  pPrint (HCLObject ks h)         = hsep (map pPrintKey ks) <+> "{" $+$ nest 2 (pPrintFields h) $+$ "}"

instance Pretty HCLValue where
  pPrint v = case v of
    HCLNumber n         -> text $ show n
    HCLString ps        -> "\"" <> (hcat $ map pPrint ps) <> "\""
    HCLBoolean True     -> text "true"
    HCLBoolean False    -> text "false"
    HCLObjectValue obj  -> pPrint obj
    HCLList vs          -> "[" <> (hcat $ punctuate comma $ map pPrint vs) <> "]"

instance Pretty HCLStringPart where
  pPrint s = case s of
    HCLStringPlain plainText                -> text $ T.unpack plainText
    HCLStringInterpolation interpolatedText -> "${" <> text (T.unpack interpolatedText) <> "}"

pPrintKey :: T.Text -> Doc
pPrintKey key =
  let keyText = T.unpack key
      keyIsEmpty = T.null $ T.strip key
      containsSpace = T.any (== ' ') key
  in  case (keyIsEmpty, containsSpace) of
        (True, _)     -> text ""
        (_, True)     -> text (show keyText)
        (_, _)        -> text keyText

printAssignment :: [T.Text] -> HCLValue -> Doc
printAssignment key value = hcat (punctuate "." (map (text . T.unpack) key)) <+> "=" <+> pPrint value

pPrintFields :: HashMap [T.Text] HCLValue -> Doc
pPrintFields h = foldl' ($+$) empty $ fmap (\(k, v) -> printAssignment k v) $ toList h