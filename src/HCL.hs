module HCL where

import HCL.Types
import HCL.Parser
import HCL.PrettyPrint
import qualified Data.Text as T
import Data.Text hiding (intersperse)
import Text.Megaparsec

parseHCL :: String -> Text -> Either (ParseError Char Dec) HCLDocument
parseHCL = runParser hcl

hclDocumentToText :: HCLDocument -> Text
hclDocumentToText document = T.pack $ show $ pPrint document