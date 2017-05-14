{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Bitcoin.Dust
(
  NonDusty
, nonDusty
, nullAmount
, PossiblyDusty(..)
)

where

import Bitcoin.Amount
import Bitcoin.Config
import Bitcoin.Util
import Bitcoin.Error


-- |Connot represent an amount/output/transaction that contains "dust"
data NonDusty a = NonDusty a
    deriving (Eq, Show, Typeable, Generic, NFData)

class PossiblyDusty a where
    mkNonDusty :: a -> Either BtcError (NonDusty a)

nonDusty :: PossiblyDusty a => NonDusty a -> a
nonDusty (NonDusty a) = a

nullAmount = either (error "BUG") id $ mkNonDusty (0 :: BtcAmount)

instance PossiblyDusty BtcAmount where
    mkNonDusty amt =
        if amt < configDustLimit && amt /= 0
            then Left $ DustOutput configDustLimit
            else Right $ NonDusty amt

instance Ord (NonDusty BtcAmount) where
    compare (NonDusty a1) (NonDusty a2) = compare a1 a2

instance Serialize (NonDusty BtcAmount) where
    put (NonDusty a) = put a
    get =
        (get :: Get BtcAmount) >>=
        either (error "Dusty BtcAmount") return . mkNonDusty

instance ToJSON (NonDusty BtcAmount) where
    toJSON (NonDusty a) = toJSON a

instance FromJSON (NonDusty BtcAmount) where
    parseJSON v = parseJSON v >>= \(amt :: BtcAmount) ->
        case mkNonDusty amt of
            Left _    -> fail $ "dusty BtcAmount: " ++ show amt
            Right nda -> return nda

