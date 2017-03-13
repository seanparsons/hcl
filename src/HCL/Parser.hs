module HCL.Parser where

import Control.Monad
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec hiding (string)
import qualified Text.Megaparsec as Megaparsec (string)
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Text (Parser)
import HCL.Types

hcl :: Parser HCLDocument
hcl = fmap HCLDocument $ many (skipSpace >> topValue)

topValue :: Parser HCLObject
topValue = object

value :: Parser HCLValue
value = label "HCL - value" $ try number
                            <|> HCLBoolean <$> try boolean
                            <|> HCLObjectValue <$> try object
                            <|> HCLList <$> try list
                            <|> HCLString <$> try stringParts
                            <|> HCLString <$> (stringPlainMultiline >>= \s -> return [HCLStringPlain s])

boolean :: Parser Bool
boolean = label "HCL - boolean" $ try (fmap (const True) (Megaparsec.string "true"))
                                <|> (fmap (const False) (Megaparsec.string "false"))

surroundedBySpace :: Parser a -> Parser a
surroundedBySpace parser = try $ do
  skipSpace
  result <- parser
  skipSpace
  return result

object :: Parser HCLObject
object = label "HCL - object" $ do
  ks <- keys
  label "HCL - object - start" $ surroundedBySpace $ vchar '{'
  fs <- manyTill assignment (label "HCL - object - end" $ surroundedBySpace $ vchar '}')
  skipSpace
  return $ HCLObject ks $ HashMap.fromList fs

keys :: Parser [Text]
keys = label "HCL - keys" $ many $ do
  k <- key
  skipSpace
  return k

assignment :: Parser ([Text], HCLValue)
assignment = label "HCL - assignment" $ do
  i <- sepBy1 ident (char '.')
  surroundedBySpace $ vchar '='
  v <- value
  skipSpace
  return (i, v)

vchar :: Char -> Parser ()
vchar = void . char

key :: Parser Text
key = string <|> ident

list :: Parser HCLList
list = label "HCL - list" $ do
  label "HCL - list - start" $ surroundedBySpace $ vchar '['
  vs <- (value `sepBy` (label "HCL - list - required comma" comma))
  skipSpace
  _ <- label "HCL - list - optional comma" $ optional comma
  skipSpace
  label "HCL - list - end" $ surroundedBySpace $ vchar ']'
  return vs

comma :: Parser ()
comma = surroundedBySpace $ vchar ','

quote :: Parser ()
quote = vchar '"'

stringParts :: Parser [HCLStringPart]
stringParts = label "HCL - stringParts" $ do
  _ <- quote
  manyTill stringPart quote

stringPart :: Parser HCLStringPart
stringPart = label "HCL - stringPart" $ try (HCLStringInterpolation <$> stringInterp)
                                      <|> HCLStringPlain <$> stringPlain

stringInterp :: Parser Text
stringInterp = label "HCL - stringInterp" $ do
  _ <- Megaparsec.string "${"
  Text.pack <$> manyTill anyChar (Megaparsec.string "}")

stringPlain :: Parser Text
stringPlain = label "HCL - stringPlain" $ do
  let end =
          try (lookAhead eof)
          <|> void (try (lookAhead (Megaparsec.string "${")))
          <|> void (try (lookAhead quote))
  s <- manyTill Lexer.charLiteral end
  return $ Text.pack s

stringPlainMultiline :: Parser Text
stringPlainMultiline = label "HCL - stringPlainMultiline" $ do
  _ <- Megaparsec.string "<<"
  multilineBounds <- manyTill anyChar eol
  Text.pack <$> manyTill Lexer.charLiteral
      (try (skipSpace >> Megaparsec.string multilineBounds))

string :: Parser Text
string = label "HCL - string" $ try stringPlainMultiline <|> str
  where
    str = do
        _ <- quote
        s <- manyTill Lexer.charLiteral quote
        return $ Text.pack s

number :: Parser HCLValue
number = label "HCL - number" (HCLNumber <$> (Lexer.signed skipSpace Lexer.number))

ident :: Parser Text
ident = Text.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

skipSpace :: Parser ()
skipSpace = label "HCL - skipSpace" $ skipMany $ skipLineComment
                                               <|> skipBlockComment
                                               <|> void eol
                                               <|> void spaceChar
                                               <|> void tab

skipLineComment :: Parser ()
skipLineComment = Lexer.skipLineComment "#"
              <|> Lexer.skipLineComment "//"

skipBlockComment :: Parser ()
skipBlockComment = Lexer.skipBlockComment "/*" "*/"