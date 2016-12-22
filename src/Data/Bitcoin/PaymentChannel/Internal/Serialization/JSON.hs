{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Serialization.JSON where

-- import           Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Amount
-- import           Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
-- import qualified Network.Haskoin.Transaction as HT
-- import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Word (Word64)


parseJSONWord :: Scientific -> Parser Word64
parseJSONWord s =
    case toBoundedInteger s of
        Just w -> return w
        Nothing -> fail $ "failed to decode JSON number to Word64. data: " ++ show s

