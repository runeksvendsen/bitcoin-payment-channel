{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Serialization.JSON where

import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.Util
import qualified Network.Haskoin.Transaction as HT
import           Data.Aeson
import           Data.Aeson.Types (Parser, Pair)
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.Word (Word64)


deriving instance ToJSON SendPubKey
deriving instance FromJSON SendPubKey
deriving instance ToJSON RecvPubKey
deriving instance FromJSON RecvPubKey


-- Generic
instance ToJSON PaymentChannelState
instance FromJSON PaymentChannelState
instance ToJSON PaymentTxConfig
instance FromJSON PaymentTxConfig
instance ToJSON FundingTxInfo
instance FromJSON FundingTxInfo
instance ToJSON Config
instance FromJSON Config
instance ToJSON ChannelParameters
instance FromJSON ChannelParameters


instance ToJSON d => ToJSON (ReceiverPaymentChannelI d) where
    toJSON rpc = object
        [ "state"       .= rpcState rpc
        , "metadata"    .= rpcMetadata rpc
        ]

instance FromJSON d => FromJSON (ReceiverPaymentChannelI d) where
    parseJSON = withObject "ReceiverPaymentChannelI" $ \o ->
        CReceiverPaymentChannel <$>
            o .: "state" <*>
            o .: "metadata"

instance ToJSON BitcoinLockTime where
    toJSON blt = Number $ scientific
        (fromIntegral $ toWord32 blt) 0

instance FromJSON BitcoinLockTime where
    parseJSON = withScientific "BitcoinLockTime" $
        fmap (parseBitcoinLocktime . fromIntegral) . parseJSONInt

instance ToJSON Payment where
    toJSON = object . paymentJSONLst

instance ToJSON PaymentSignature where
    toJSON = object . paySigJSONLst

instance FromJSON PaymentSignature where
    parseJSON = withObject "PaymentSignature" $ \o ->
        CPaymentSignature <$>
            (o .: "signature_data" >>= withText "SigDataHex" deserHex) <*>
            (o .: "sighash_flag"   >>= withText "SigHashFlagHex" deserHex)


paySigJSONLst :: PaymentSignature -> [Pair]
paySigJSONLst  (CPaymentSignature sig flag) =
    [ "signature_data"  .= String (serHex sig)
    , "sighash_flag"  .= String (serHex flag)
    ]

paymentJSONLst :: Payment -> [Pair]
paymentJSONLst (CPayment changeVal paySig) =
    ("change_value"      .= changeVal) :
        paySigJSONLst paySig


parseJSONPayment :: Object -> Parser Payment
parseJSONPayment o = CPayment
       <$>      o .: "change_value"
       <*>     (CPaymentSignature <$>
                   (o .: "signature_data" >>= withText "SigDataHex" deserHex) <*>
                   (o .: "sighash_flag"   >>= withText "SigHashFlagHex" deserHex))

instance ToJSON FullPayment where
    toJSON CFullPayment {
        fpPayment       = payment,
        fpOutPoint      = (HT.OutPoint txid vout), fpRedeemScript = script,
        fpChangeAddr    = addr } =
            object $
                paymentJSONLst payment ++
                [   "funding_txid"     .= txid
                ,   "funding_vout"     .= vout
                ,   "redeem_script"    .= String (serHex script)
                ,   "change_address"   .= addr
                ]

instance FromJSON FullPayment where
    parseJSON = withObject "FullPayment" parseFullPayment

parseFullPayment :: Object -> Parser FullPayment
parseFullPayment o = CFullPayment
    <$>     parseJSONPayment o
    <*>     (HT.OutPoint <$>
                 o .: "funding_txid" <*>
                 o .: "funding_vout")
    <*>     (o .: "redeem_script" >>= withText "RedeemScriptHex" deserHex)
    <*>      o .: "change_address"

instance ToJSON BitcoinAmount where
    toJSON amt = Number $ scientific
        (fromIntegral $ toInteger amt) 0

instance FromJSON BitcoinAmount where
    parseJSON = withScientific "BitcoinAmount" $
        fmap fromIntegral . parseJSONInt


parseJSONInt :: Scientific -> Parser Integer
parseJSONInt s =
    case toBoundedInteger s of
        Just (BitcoinAmount i) -> return i
        Nothing -> fail $ "failed to decode JSON number to integer. data: " ++ show s

parseJSONWord :: Scientific -> Parser Word64
parseJSONWord s =
    case toBoundedInteger s of
        Just w -> return w
        Nothing -> fail $ "failed to decode JSON number to Word64. data: " ++ show s

