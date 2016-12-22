{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module Data.Bitcoin.PaymentChannel.Internal.Settlement
-- (
--     createSignedSettlementTx
-- ,   signedSettlementTxFromState
-- ,   settleReceivedValue
-- ,   UnsignedSettlementTx, mkUnsignedSettleData
-- )
where

import Data.Bitcoin.PaymentChannel.Internal.Settlement.Types
import Data.Bitcoin.PaymentChannel.Internal.Receiver.Util
import Data.Bitcoin.PaymentChannel.Internal.Class.Value     (HasValue(..))
import Data.Bitcoin.PaymentChannel.Internal.Types
import Data.Bitcoin.PaymentChannel.Internal.State           (pcsFundingSource)
import Data.Bitcoin.PaymentChannel.Internal.Payment
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Script
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Util
import Data.Bitcoin.PaymentChannel.Internal.Util
import Data.Bitcoin.PaymentChannel.Internal.Bitcoin.Fee
import Data.Bitcoin.PaymentChannel.Types                    (fundingAddress)
import Data.Bitcoin.PaymentChannel.Internal.Class.Value     (HasValue(..))

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Script as HS
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.List                          (zipWith, sort, sortOn)
import           Data.Word                          (Word32)
{-# ANN module ("HLint: ignore Use mapMaybe"::String) #-}




--   sign :: Monad m =>
--           (KeyDeriveIndex -> m HC.PrvKeyC)
--        -> Config
--        -> UnsignedSettlementTxIdx
--        -> m (Either String HT.Tx)



-- settleReceivedValue :: Monad m =>
--                        Config
--                     -> (KeyDeriveIndex -> m HC.PrvKeyC)
--                     -> UnsignedSettlementTxIdx
--                     -> m (Either String [RecvPayChanX])
-- settleReceivedValue cfg keyGetter ust =
--     let res txE = do
--             tx <- txE
--             chanStatus <- getChannelStatus <$> decodeTxOut output
--
--     in do
--        txE <- sign keyGetter cfg ust
--
--        return undefined
--



-- |Sign everything, and do not allow additional inputs to be added afterwards.
serverSigHash :: HS.SigHash
serverSigHash = HS.SigAll False



mkUnsignedSettleData
    :: [RecvPayChanX]
    -> [(HC.Address, BitcoinAmount)]
    -> HC.Address
    -> BitcoinAmount
    -> Either String UnsignedSettlementTxIdx
mkUnsignedSettleData rpcL adrAmtL chgAdr txFee =
    let
        clientPayIdx = sort $ map cPaymentIdxFromState rpcL
        outgoingVal  = sum (map snd adrAmtL) + txFee
    in
        if valueOf rpcL >= outgoingVal then
            Right $ UnsignedSettlementTx clientPayIdx adrAmtL chgAdr txFee
        else
            Left $ "Insufficient payment value. Have " ++
                   show (valueOf rpcL) ++ ", need: " ++ show outgoingVal


sign :: Monad m =>
        (KeyDeriveIndex -> m HC.PrvKeyC)
     -> Config
     -> UnsignedSettlementTxIdx
     -> m HT.Tx
sign signFunc cfg ust = prvSign cfg <$> applyPrvKeyData signFunc ust

applyPrvKeyData :: Monad m =>
          (KeyDeriveIndex -> m HC.PrvKeyC)
       -> UnsignedSettlementTxIdx
       -> m UnsignedSettlementTxPrv
applyPrvKeyData prvKeyFromIndexM ust@UnsignedSettlementTx{..} = do
    prvCsp <- mapM (cspReplaceKeyDataM prvKeyFromIndexM) sspClientPayments
    return $ ust { sspClientPayments = prvCsp}

prvSign :: Config
        -> UnsignedSettlementTxPrv
        -> HT.Tx
prvSign cfg ust@UnsignedSettlementTx{..} = do
    let baseTx = toUnsignedSettleTx cfg ust
    let getHashToSign csp inpIdx = HS.txSigHash
            baseTx (cspRedeemScript csp) (fromIntegral inpIdx) serverSigHash

    let serverSignCSP :: (ClientSignedPaymentPrv,Word32) -> Script
        serverSignCSP (csp,inpIdx) = getSignInputScript
            csp (getHashToSign csp inpIdx) (`HC.signMsg` csKeyData csp)

    let signReplaceInput :: (ClientSignedPaymentPrv,Word32) -> HT.Tx -> HT.Tx
        signReplaceInput signArgs@(_, inpIdx) =
            replaceScriptInput inpIdx (serverSignCSP signArgs)

    foldr signReplaceInput baseTx (zip sspClientPayments [0..])


toUnsignedSettleTx :: Show a => Config -> UnsignedSettlementTx a -> HT.Tx
toUnsignedSettleTx Config{..} UnsignedSettlementTx{..} =
    let
        inOutPairs  :: [(HT.TxIn, Maybe HT.TxOut)]
        inOutPairs   = map toUnsignedInMaybeOut sspClientPayments
        senderOuts   = catMaybes $ map snd inOutPairs
        extraOuts    = map mkTxOut sspAdditionalOuts
        incomingVal  = valueOf sspClientPayments
        outgoingVal  = outsVal senderOuts + outsVal extraOuts + sspTxFee
        freeVal      = incomingVal - outgoingVal
        mkRecvOut v  = mkTxOut (sspReceiverAddress,v)
        outsVal      = fromIntegral . sum . map HT.outValue
    in
        HT.createTx
            1   -- Version
            ( map fst inOutPairs )
            ( senderOuts ++ extraOuts ++ [mkRecvOut freeVal | freeVal >= cDustLimit] )
            0   -- Locktime


getSignInputScript
    :: ClientSignedPaymentI kd
    -> HC.Hash256
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> Script
getSignInputScript (ClientSignedPayment UnsignedPayment{..} clientSig _)
                   hash signFunc =
    getP2SHInputScript upChannelParams $ paymentTxScriptSig clientSig serverSig
        where serverSig = CPaymentSignature (signFunc hash) serverSigHash


toUnsignedSettlementTx :: ClientSignedPayment -> HC.Address -> BitcoinAmount -> HT.Tx
toUnsignedSettlementTx
        (ClientSignedPayment
            unsignedPayment@(UnsignedPayment _ valueAvailable _ _ senderVal)
        (CPaymentSignature _ sigHash) _) recvAddr txFee =
    let
        baseTx = toUnsignedBitcoinTx unsignedPayment
        adjustedTx = if sigHash == HS.SigNone True then removeOutputs baseTx else baseTx
        -- TODO: check dust?
        receiverAmount = valueAvailable - senderVal - txFee
        recvOut = HT.TxOut
                (fromIntegral . toInteger $ receiverAmount)
                (addressToScriptPubKeyBS recvAddr)
    in
        appendOutput adjustedTx recvOut

getSettlementTxHashForSigning
    :: ClientSignedPayment
    -> HC.Address    -- ^Receiver destination address
    -> BitcoinAmount           -- ^Bitcoin transaction fee
    -> HC.Hash256
getSettlementTxHashForSigning csPayment recvAddr txFee =
    HS.txSigHash tx (redeemScript csPayment) 0 serverSigHash
        where tx = toUnsignedSettlementTx csPayment recvAddr txFee
              redeemScript = getRedeemScript . upChannelParams . cspPayment

getSignedSettlementTx
    :: ClientSignedPayment
    -> HC.Address       -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> BitcoinAmount    -- ^Bitcoin tx fee
    -> HT.Tx
getSignedSettlementTx csPayment@(ClientSignedPayment UnsignedPayment{..} clientSig _)
                      recvAddr signFunc txFee =
        let
            rawServerSig = signFunc $ getSettlementTxHashForSigning csPayment recvAddr txFee
            unsignedTx = toUnsignedSettlementTx csPayment recvAddr txFee
            serverSig = CPaymentSignature rawServerSig serverSigHash
            inputScript = getP2SHInputScript upChannelParams $ paymentTxScriptSig clientSig serverSig
        in
            replaceScriptInput 0 inputScript unsignedTx

createSignedSettlementTx
    :: HasFee fee
    => ClientSignedPayment
    -> HC.Address                   -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> fee                          -- ^Bitcoin tx fee
    -> HT.Tx
createSignedSettlementTx csp addr signFunc fee =
    mkRelativeFeeTx fee $ getSignedSettlementTx csp addr signFunc

signedSettlementTxFromState
    :: HasFee fee
    => PaymentChannelState
    -> HC.Address                   -- ^Receiver/server funds destination address
    -> (HC.Hash256 -> HC.Signature) -- ^ Server/receiver's signing function. Produces a signature which verifies against 'cpReceiverPubKey'
    -> fee                          -- ^Bitcoin tx fee
    -> HT.Tx
signedSettlementTxFromState cs =
    createSignedSettlementTx (cPaymentFromState cs)


mkRelativeFeeTx
    :: HasFee fee
    => fee                          -- ^Desired (variable) fee
    -> (BitcoinAmount -> HT.Tx)     -- ^Produce Bitcoin tx with constant fee
    -> HT.Tx
mkRelativeFeeTx fee mkTx =
    mkTx (absoluteFee (calcTxSize dummyTxPeekSize) fee)
    where
        dummyTxPeekSize = mkTx (0 :: BitcoinAmount)



