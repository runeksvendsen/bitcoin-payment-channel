{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Bitcoin.Dust
( NonDustyAmount
, mkNonDusty
, nonDusty
, nullAmount
, HasConfDustLimit(..)
)

where

import Bitcoin.Amount
import Bitcoin.Internal.Util
import Bitcoin.Error


class Monad m => HasConfDustLimit m where
    confDustLimit :: m BtcAmount

-- | Cannot represent an amount that is less than the "dust limit"
newtype NonDustyAmount = NonDustyAmount BtcAmount
    deriving (Eq, Show, Typeable, Generic, NFData)

nonDusty :: NonDustyAmount -> BtcAmount
nonDusty (NonDustyAmount a) = a

nullAmount :: NonDustyAmount
nullAmount = NonDustyAmount 0

mkNonDusty :: HasConfDustLimit m => BtcAmount -> m (Either BtcError NonDustyAmount)
mkNonDusty amt = do
    limit <- confDustLimit
    if amt < limit && amt /= 0
        then return $ Left  $ DustOutput limit
        else return $ Right $ NonDustyAmount amt

instance Ord NonDustyAmount where
    compare (NonDustyAmount a1) (NonDustyAmount a2) = compare a1 a2

instance Serialize NonDustyAmount where
    put (NonDustyAmount a) = put a
    get = do
        a <- get
        return $ NonDustyAmount a

instance ToJSON NonDustyAmount where
    toJSON (NonDustyAmount a) = toJSON a

instance FromJSON NonDustyAmount where
    parseJSON v = parseJSON v >>= \(amt :: BtcAmount) ->
        return (NonDustyAmount amt)

