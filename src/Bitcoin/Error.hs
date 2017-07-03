{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bitcoin.Error where

import Bitcoin.Amount
import Bitcoin.Internal.Types
import Bitcoin.Types.ExpectFail
--import Data.List                    (intercalate)


data BtcError =
    InsufficientFunds BtcAmount
  | DustOutput        BtcAmount
  | WrongSigningKey   [SignKeyError]
        deriving (Eq, Typeable, Generic, NFData, ToJSON, FromJSON, Serialize)

data SignKeyError
  = SignKeyError
  { skeInputIndex :: Word32   -- ^ Index of input for which error ocurred
  , skePubKeys    :: ExpectFail PubKeyC
  }     deriving (Eq, Typeable, Generic, NFData, ToJSON, FromJSON, Serialize)

isKeyError :: BtcError -> Bool
isKeyError (WrongSigningKey _) = True
isKeyError _ = False

instance Show BtcError where
    show (InsufficientFunds amountMissing) = unwords
        [ "insufficient funds, amount missing:", show amountMissing]
    show (DustOutput dustLimit) = unwords
        [ "dusty output in transaction, dust limit:", show dustLimit]
    show (WrongSigningKey expecFails) = unwords
        [ "transaction signing function was delivered a private key whose"
        , "pubkey doesn't match the pubkey specified by HasSigner(signerPubKey)."
        ] ++ unlines ["", concatMap show expecFails]

instance Show SignKeyError where
    show (SignKeyError inIdx expecFail) = unwords
        [ "wrong sign key. index:"
        , show inIdx ++ ","
        , show expecFail
        ]

newtype Bug a = Bug a
    deriving (Eq, Typeable, Generic, NFData, ToJSON, FromJSON, Serialize)

instance Exception a => Exception (Bug a)
instance Exception BtcError

instance Show a => Show (Bug a) where
    show (Bug a) = unwords ["BUG:", show a]

