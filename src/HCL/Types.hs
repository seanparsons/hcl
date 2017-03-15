module HCL.Types where

import Data.Text
import Data.Scientific
import Data.HashMap.Strict
import GHC.Generics
import Data.List (sortOn)

type HCLMapContent = HashMap [Text] HCLValue

instance Ord HCLMapContent where
  compare firstMap secondMap = compare (sortOn fst $ toList firstMap) (sortOn fst $ toList secondMap)

data HCLObject = HCLObject [Text] HCLMapContent
               deriving (Generic, Show, Eq, Ord)

data HCLValue = HCLNumber Scientific
              | HCLBoolean Bool
              | HCLString Text
              | HCLObjectValue HCLObject
              | HCLList [HCLValue]
              deriving (Generic, Show, Eq, Ord)

type HCLList = [HCLValue]

newtype HCLDocument = HCLDocument [HCLObject]
                      deriving (Generic, Show, Eq, Ord)

