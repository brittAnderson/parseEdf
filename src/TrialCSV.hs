{-# LANGUAGE OverloadedStrings, TemplateHaskell ,FlexibleInstances #-}

module TrialCSV (
  module TrialCSV
  ) where


import EDFParse 
import Data.Char as DC
import Data.List as DL
import Control.Applicative hiding (many,optional)
import Data.Csv as Csv
import qualified Data.ByteString.Char8 as BLC (pack)
import Data.List.Utils


data Item =
  Item
    {trialT :: Integer
    , trialN :: Integer
    , fixSTL :: [Integer]
    , rewST  :: Integer
    , rewET  :: Integer}
  deriving(Eq,Show)


trialRecord = [Trial {time = 123, trialNum = 1},Efix 'R' 123 234 111 3.4 3.5 112, RewStart {time = 234}, RewEnd {time = 345}]

instance ToRecord Item where
  toRecord i = record [
    (toField $ trialT i)
    , (toField $ trialN i)
    , (toField $ fixSTL i)
    , (toField $ rewST i)
    , (toField $ rewET i)]
    
instance ToField ([Integer]) where
  toField a = BLC.pack $ (replace "," "|" ) $ show a

