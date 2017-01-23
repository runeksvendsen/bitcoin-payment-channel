{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PaymentChannel.Internal.Serialization.JSON where

import           Data.Aeson.Types   (Parser)
import           Data.Scientific    (Scientific, toBoundedInteger)
import           Data.Word          (Word64)
import           Data.Typeable      (Typeable, typeOf)

parseJSONWord :: forall i. (Typeable i, Integral i, Bounded i) => Scientific -> Parser i
parseJSONWord s =
    case toBoundedInteger s of
        Just w -> return w
        Nothing -> fail $  "failed to decode "
                        ++ show (typeOf (undefined :: i))
                        ++ " from JSON number. data: " ++ show s

