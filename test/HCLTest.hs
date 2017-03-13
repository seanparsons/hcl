module HCLTest where

import Data.Text
import Prelude hiding (print)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances()
import Test.Tasty
import HCL.Types
import HCL
import Data.DeriveTH
import Data.HashMap.Strict

derive makeArbitrary ''HCLStringPart

identText :: Gen Text
identText = fmap pack $ listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-'])

objectKey :: Gen [Text]
objectKey = listOf1 identText

notNestedValue :: Gen HCLValue
notNestedValue = oneof [ fmap HCLNumber arbitrary
                       , fmap HCLBoolean arbitrary
                       , fmap HCLString $ scale capSize $ listOf arbitrary
                       ]

capSize :: Int -> Int
capSize = min 10

mapContent :: Int -> Gen HCLMapContent
mapContent maxDepth = fmap fromList $ scale capSize $ listOf $ pure (,) <*> objectKey <*> nestedValue (maxDepth - 1)

objectAtDepth :: Int -> Gen HCLObject
objectAtDepth maxDepth = HCLObject <$> objectKey <*> mapContent maxDepth

nestedValue :: Int -> Gen HCLValue
nestedValue maxDepth =
  let nested = oneof [ notNestedValue
                     , fmap HCLObjectValue $ objectAtDepth maxDepth
                     , fmap HCLList $ scale capSize $ listOf $ nestedValue (maxDepth - 1)
                     ]
  in  if maxDepth > 1 then nested else notNestedValue

instance Arbitrary HCLValue where
  arbitrary = nestedValue 4

instance Arbitrary HCLDocument where
  arbitrary = fmap HCLDocument $ listOf $ objectAtDepth 4

toHCLAndBack :: TestTree
toHCLAndBack = testProperty "Printing and re-parsing produces the same document" $ \document ->
                  let printedResult = hclDocumentToText document
                      parsedResult = parseHCL "test" printedResult
                  in  counterexample ("printedResult = " ++ (show printedResult)) $
                      counterexample ("parsedResult = " ++ (show parsedResult)) $
                      (parsedResult == Right document)

test_HCL :: [TestTree]
test_HCL =  [
              testGroup "HCL"
                [ toHCLAndBack
                ]
            ]