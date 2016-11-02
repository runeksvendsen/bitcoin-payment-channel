{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Bitcoin.PaymentChannel.Internal.Serialization.Binary where

import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.Error

import qualified Data.Serialize     as Bin
import qualified Data.Serialize.Put as BinPut
import qualified Data.Serialize.Get as BinGet
import qualified Data.ByteString as B
import qualified Data.Tagged as Tag


instance Bin.Serialize PayChanError -- Generic PayChanError instance

instance Bin.Serialize ChanScript where
    put (ChanScript s) =
        BinPut.putWord16be scriptBSLen >>
        BinPut.putByteString scriptBS
            where scriptBS    = Bin.encode s
                  scriptBSLen = fromIntegral $ B.length scriptBS
    get = either error ChanScript . Bin.decode <$>
            (BinGet.getWord16be >>=
             BinGet.getByteString . fromIntegral)

instance Bin.Serialize PaymentChannelState where
    put (CPaymentChannelState cfg par fti payConf payCount valLeft sig) =
        Bin.put cfg >> Bin.put par >> Bin.put fti >> Bin.put payConf >> Bin.put payCount >>
        Bin.put valLeft >> Bin.put sig
    get = CPaymentChannelState <$> Bin.get <*> Bin.get <*>
        Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Serialize ChannelParameters where
    put (CChannelParameters pks pkr lt) =
        Bin.put pks >> Bin.put pkr >> Bin.put lt
    get = CChannelParameters <$> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Serialize FundingTxInfo where
    put (CFundingTxInfo h idx val) =
        Bin.put h >> BinPut.putWord32be idx >> Bin.put val
    get = CFundingTxInfo <$> Bin.get <*> BinGet.getWord32be <*> Bin.get

instance Bin.Serialize PaymentTxConfig where
    put (CPaymentTxConfig sendAddr) =
        Bin.put sendAddr
    get = CPaymentTxConfig <$> Bin.get

instance Bin.Serialize Config where
    put (Config dl sp) =
        Bin.put dl >> Bin.put (Tag.unTagged sp)
    get = Config <$> Bin.get <*> fmap Tag.Tagged Bin.get

instance Bin.Serialize Payment where
    put (CPayment val sig) =
        Bin.put val >> Bin.put sig
    get = CPayment <$> Bin.get <*> Bin.get

instance Bin.Serialize FullPayment where
    put (CFullPayment p op script addr) =
        Bin.put p >> Bin.put op >> Bin.put (ChanScript script) >> Bin.put addr
    get = CFullPayment <$> Bin.get <*> Bin.get <*> fmap getScript Bin.get <*> Bin.get

instance Bin.Serialize PaymentSignature where
    put (CPaymentSignature sig sigHash) =
        Bin.put sig >> Bin.put sigHash
    get = CPaymentSignature <$> Bin.get <*> Bin.get

instance Bin.Serialize ReceiverPaymentChannel where
    put (CReceiverPaymentChannel rpc _ ) =
        Bin.put rpc >> Bin.putWord8 0x01
    get = CReceiverPaymentChannel <$> Bin.get <*> return ()

instance Bin.Serialize ReceiverPaymentChannelX where
    put (CReceiverPaymentChannel rpc pki ) =
        Bin.put rpc >> Bin.putWord8 0x02 >> Bin.put pki
    get = CReceiverPaymentChannel <$> Bin.get <*> Bin.get
