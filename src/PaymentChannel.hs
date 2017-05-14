{-|
Module      : PaymentChannel
Description : Bitcoin payment channel library
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

In order to set up a payment channel between a sender and a receiver, the two parties must
 first agree on three [1] parameters for the channel:

    (1) sender public key
    (2) receiver public key
    (3) channel expiration date

 These parameters
 are contained in 'ChanParams', from which the Bitcoin address used to fund
 the payment channel can be derived using
 'getFundingAddress'. The transaction which pays to this address is the channel funding
 transaction, and information about it is contained in a 'FundingTxInfo'.
 So, the channel funding transaction will contain an output which pays to the address returned by
 'getFundingAddress', and once this transaction is created and in
 the blockchain, a 'ClientPayChan' and
 'ServerPayChan' instance can be created, after first creating the 'FundingTxInfo' instance.
 'FundingTxInfo' contains three pieces of information about the funding transaction:

    (1) hash/transaction ID
    (2) index/vout of the funding output (paying to 'getFundingAddress' address),
    (3) value of the funding output (paying to 'getFundingAddress' address)

With 'ChanParams' and 'FundingTxInfo',
 the sender can create a new 'ClientPayChanI', plus
 the first channel payment, using 'channelWithInitialPaymentOf'. 'channelWithInitialPaymentOf'
 takes two additional arguments:

    (1) a signing function which, given a hash, produces a signature that verifies against
        'cpSenderPubKey' in 'ChanParams'
    (2) the value of the first channel payment

 The sender will want to use @flip 'Network.Haskoin.Crypto.signMsg' senderPrivKey@ as the signing
 function, where @senderPrivKey@ is the private key from which 'cpSenderPubKey' is derived.
 'channelWithInitialPaymentOf' will return the first channel 'Payment' as well as
 the new 'ClientPayChanI' state. The new state is stored, and the 'Payment'
 transferred to the receiver.

The receiver will now create its own channel state object, 'ServerPayChan', using
 'channelFromInitialPayment'.
 'channelFromInitialPayment' takes [TODO].

Now the payment channel is open and ready for transmitting value. A new 'Payment' is created by
 the sender with 'createPayment', which yields a new payment, that increases the total value transmitted
 to the receiver by the specified amount, and an updated 'PaymentChannel.Types.ClientPayChanI' state.
 The receiver will verify and register this 'Payment' on its side using 'acceptPayment', which, on success,
 returns the value received with this payment plus the updated
 'ServerPayChan' state object.

Payments can flow from the sender to receiver until either the channel is exhausted, or getting
 close to expiration (see important note below). In either case the receiver will use 'getSettlementBitcoinTx' to create the settlement
 Bitcoin transaction, and publish this transaction to the Bitcoin network. The settlement Bitcoin
 transaction pays the total value transmitted over the channel to the receiver and the rest back
 to the sender.

A settlement transaction can be produced by the value receiver using 'getSettlementBitcoinTx'.
 The receiver will want to use @flip 'Network.Haskoin.Crypto.signMsg' receiverPrivKey@ as the
 signing function passed to 'getSettlementBitcoinTx',
where @receiverPrivKey@ is the private key from which 'cpReceiverPubKey' is derived.

[1] In addition to this, two configuration options must also be agreed upon: a "dust limit",
 and a "settlement period" (measured in hours), which is subtracted from the channel expiration
 date in order to arrive at the effective (earlier) expiration date. This is necessary to give the
 server/receiver time to publish the settlement transaction before the refund transaction becomes
 valid. The dust limit is the minimum amount that the server is willing to accept
 as the client change value in the
 payment transaction. It's only relevant if the channel is
 emptied of value completely, but it is necessary because the server doesn't want to accept
 payments based on transactions it cannot publish via the Bitcoin P2P network, because they
 contain an output of minuscule value. Sensible values are contained in 'defaultConfig', but
 client/sender and server/receiver need to agree on these parameter as well, although they are
 only relevant 1) (in the case of the dust limit) if the channel is compleltely exchausted and
 2) (in case of the "settlemend period") if the client tries to make payments close to expiration
 (and, in case the client does, it will just receive an error in response, saying the channel
 is now closed).

__/IMPORTANT:/__ /Channel setup is risk free because the sender can derive a refund Bitcoin transaction/
 /using 'getRefundBitcoinTx', which returns the bitcoins used to fund the channel back to the sender./
 /This refund transaction, however, is not valid until the expiration date specified in 'ChanParams',/
 /but it is paramount that the value receiver get a settlement transaction included in a block/
 /before the refund transaction becomes valid. Due to the fact that Bitcoin network time is allowed/
 /to drift up to two hours from actual time, and the fact that finding new Bitcoin blocks does not occur/
 /according to any schedule, it would be wise for the receiver to publish a settlement transaction at least/
 /6 hours before the specified channel expiration time, and possibly earlier, if the receiver wants to/
 /be cautious./

-}
{-# LANGUAGE DeriveAnyClass #-}
module PaymentChannel
(
    -- *Initialization

    -- **Funding
    getFundingAddress,

    -- **State creation
    channelWithInitialPayment,
    channelFromInitialPayment,
    setMetadata,

    -- *Payment
    createPayment, Capped(..),
    acceptPayment,
    ClosedServerChanX, getClosedState,

    -- *Settlement
    getRefundBitcoinTx,
    createClosingPayment,
    acceptClosingPayment,
    getSettlementBitcoinTx,
    closedGetSettlementTx,
    DustPolicy(..),
    ChangeOutFee,

    -- *Types
    module PaymentChannel.Types,

    -- *RESTful Bitcoin Payment Channel Protocol
    module PaymentChannel.RBPCP.Parse
)
where

import PaymentChannel.Internal.Payment
import PaymentChannel.RBPCP.Parse
import PaymentChannel.Internal.Receiver.Util
import PaymentChannel.Internal.Receiver.Open            (initialServerState)

import Bitcoin.Util                                     (calcTxSize)
import PaymentChannel.Internal.Error
import qualified PaymentChannel.Internal.Receiver.Util  as S
import PaymentChannel.Internal.Settlement
import PaymentChannel.Internal.Refund                   (mkRefundTx)

import PaymentChannel.Util                              (getFundingAddress)
import PaymentChannel.Types

import Control.Exception                                (throw)
import Data.Functor.Identity                            (Identity(..))

import qualified  Network.Haskoin.Crypto        as HC
import qualified  Network.Haskoin.Transaction   as HT


channelWithInitialPayment :: Monad m =>
       HC.PrvKeyC
       -- ^ Used to sign payments from client
    -> ChanParams
       -- ^ Specifies channel sender, receiver, and expiration date
    -> FundingTxInfo
       -- ^ Holds information about the Bitcoin transaction used to fund the channel
    -> HC.Address
       -- ^ Client change address
    -> BtcAmount
       -- ^ Value of initial channel payment. This payment is sent to the receiver
       --    in the initial handshake that opens the channel. Can be zero, in which
       --    case the payment simply acts as a proof that the client owns the private
       --    key whose corresponding public key is in 'ChanParams'.
    -> m (Either BtcError (ClientPayChanI BtcSig, SignedPayment))
       -- ^ Client state and first payment. Error if the channel funding value
       --    can't cover the specified payment value.
channelWithInitialPayment prvKey cp fundInf sendAddr payVal =
    let mkPayChanState sp = MkPayChanState sp (fromInitialPayment sp)
        mkClientChan sp = (MkClientPayChan (mkPayChanState sp) prvKey, sp)
    in
        fmap mkClientChan <$> createPaymentOfValue
            prvKey (mkUnsignedPayment cp fundInf sendAddr) payVal

-- |Create new payment of specified value, along with updated state containing this payment.
createPayment :: forall value ret.
       PaymentValueSpec value ret BtcSig
    => ClientPayChanI BtcSig
    -- ^ Sender state object
    -> value
    -- ^ Amount to send (capped or uncapped)
    -> ret
    -- ^ Updated sender state & payment
createPayment = createPaymentInternal

-- |Create new payment of specified value, along with updated state containing this payment.
createPaymentInternal :: forall value ret sd.
       PaymentValueSpec value ret sd
    => ClientPayChanI sd
    -- ^ Sender state object
    -> value
    -- ^ Amount to send (the actual payment amount is capped, so no invalid payment is created)
    -> ret
    -- ^ Updated sender state & payment
createPaymentInternal cpc@MkClientPayChan{..} payVal =
    let
        (Identity paymentE) = createPaymentOfValue
              spcPrvKey (clearSig $ pcsPayment spcState) (paymentValue cpc payVal)
        updateState pcs p = pcs { pcsPayment = p }
        addUpdatedState p = (cpc { spcState = updateState spcState p }, p)
    in
        mkReturnVal (Tagged $ paymentValue cpc payVal :: Tagged (value,sd) BtcAmount)
                    (addUpdatedState <$> paymentE)

createClosingPayment
    :: (ChangeOutFee fee, HasFee fee ) -- , PaymentValueSpec fee ret)
    => ClientPayChanI BtcSig
    -> HC.Address
    -> fee
    -> (ClientPayChanI BtcSig, SignedPayment, BtcAmount)
createClosingPayment clientState changeAddress fee =
    createPayment newState (Capped $ absoluteFee 0 (dummySettleTxSize newState actualAmount) fee)
  where
    (newState, _, actualAmount) = createPaymentInternal
          newChangeAddrState (Capped $ absoluteFee 0 (dummySettleTxSize fakeSigNewAddrState (0 :: BtcAmount)) fee)

    dummySettleTxSize cpc' fee' = calcTxSize $ dummyClientSettleTx cpc' fee'
    newChangeAddrState = _setClientChangeAddr clientState changeAddress
    -- ### Dummy
    fakeSigNewAddrState = mapSigData _invalidBtcSig newChangeAddrState
    handleSettleRet (Identity (Right dummySettleTx)) = dummySettleTx
    handleSettleRet (Identity _) = error "woops"
    dummyAddress = HC.PubKeyAddress "0000000000000000000000000000000000000000"
    dummyClientSettleTx :: ChangeOutFee txFee => ClientPayChan -> txFee -> HT.Tx
    dummyClientSettleTx  cpc txFee = handleSettleRet $ getSettlementBitcoinTx
                          (dummyFromClientState cpc) dummyAddress (const $ return $ spcPrvKey cpc) txFee DropDust


-- |Produces a Bitcoin transaction which sends all channel funds back to the sender.
-- Will not be accepted by the Bitcoin network until the expiration time specified in
-- 'ChanParams'. Receiver should be aware of Bitcoin network time drift and the
-- unpreditable nature of finding new blocks.
getRefundBitcoinTx
    :: Monad m
    => HC.PrvKeyC
    -> ChanParams
    -> FundingTxInfo
    -> HC.Address           -- ^ Refund address
    -> SatoshisPerByte      -- ^ Refund transaction fee
    -- ^ Refund Bitcoin transaction.
    -- Error only in case of insufficient value to cover fee
    --  (dust outputs are accepted).
    -> m (Either BtcError HT.Tx)
getRefundBitcoinTx prvKey cp fti refundAddr txFee =
    fmap toHaskoinTx <$>
        mkRefundTx prvKey cp fti refundAddr txFee


-- |Create new 'ServerPayChan'.
-- A channel is initialized with various information
-- about the payment channel, as well as the first channel payment
-- produced by the sender.
channelFromInitialPayment ::
       MonadTime m
    => HT.Tx
    -> PaymentData
    -> m (Either PayChanError (ServerPayChan, BtcAmount)) -- ^Error or: value_received plus state object
channelFromInitialPayment tx paymentData =
    either
      (return . Left)
      (acceptPaymentInternal paymentData)
      (fmapL OpenError $ initialServerState tx paymentData)

-- | Register, on the receiving side, a payment made by 'createPayment' on the sending side.
--   Returns error if either the signature or payment amount is invalid, and otherwise
--    the amount received with this 'Payment' and a new state object.
-- | NB: Throws 'BadSignatureInState' on invalid old/in-state payment.
acceptPayment :: MonadTime m =>
       PaymentData          -- ^Payment to verify and register
    -> ServerPayChanI kd    -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount)) -- ^Value received plus new receiver state object
acceptPayment = acceptPaymentInternal

acceptPaymentInternal ::
      ( MonadTime m
      , StateSignature sd
      ) =>
       PaymentData            -- ^Payment to verify and register
    -> ServerPayChanG kd sd   -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount)) -- ^Value received plus new receiver state object
acceptPaymentInternal paymentData rpc =
    either (return . Left) (acceptPaymentIgnoreStatus paymentData) (S.checkChannelStatus rpc)

-- | Accept the payment that closes the payment channel.
--   The payment accepted here is allowed to have a different client change address
--    from that found in the state.
-- | NB: Throws 'BadSignatureInState' on invalid old/in-state payment.
acceptClosingPayment :: MonadTime m =>
       PaymentData         -- ^Payment to verify and register
    -> ServerPayChanI kd    -- ^Receiver state object
    -> m (Either PayChanError (ClosedServerChanI kd))
acceptClosingPayment paymentData oldState =
    fmap handleResult <$> acceptClosingPaymentInternal paymentData oldState
  where
    mkNewStatus newState = ChannelClosed $ getPayment newState
    getPayment = pcsPayment . rpcState
    handleResult (newState, _) = MkClosedServerChan
            (setChannelStatus (mkNewStatus newState) oldState)
            (getPayment newState)

acceptPaymentIgnoreStatus ::
      ( MonadTime m
      , StateSignature sd
      ) =>
       PaymentData              -- ^Payment to verify and register
    -> ServerPayChanG kd sd     -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanG kd BtcSig, BtcAmount)) -- ^Value received plus new receiver state object
acceptPaymentIgnoreStatus paymentData rpc@MkServerPayChan{..} =
    either (return . Left) checkPayment (getPayment paymentData)
  where
    getPayment pd = fmapL RBPCPError (fromPaymentData (getFundingAmount rpc) pd)
    mkReturnValue p val = (updateMetadata $ updState rpc p, val)
    checkPayment p = do
            valRecvdE <- paymentValueIncrease (pcsPayment rpcState) p
            return $ mkReturnValue p <$> valRecvdE

acceptClosingPaymentInternal ::
      ( MonadTime m
      -- , StateSignature sd
      ) =>
       PaymentData        -- ^Payment to verify and register
    -> ServerPayChanI kd   -- ^Receiver state object
    -> m (Either PayChanError (ServerPayChanI kd, BtcAmount))
acceptClosingPaymentInternal paymentData oldState =
    acceptPaymentIgnoreStatus paymentData newChangeAddrState
  where
    newChangeAddrState = _setClientChangeAddr oldState (paymentDataChangeAddress paymentData)


-- |The value transmitted over the channel is settled when this transaction is in the Blockchain.
-- The receiver will want to make sure a transaction produced by this function
-- is included in a Bitcoin block before the refund transaction becomes valid (see 'getRefundBitcoinTx').
-- The sender can only close the channel before expiration by requesting this transaction
-- from the receiver and publishing it to the Bitcoin network.
getSettlementBitcoinTx ::
     ( Monad m
     , ChangeOutFee fee
     , HasKeyDeriveIndex kd
     ) =>
       ServerPayChanI kd                 -- ^ Receiver state object
    -> HC.Address                       -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> (KeyDeriveIndex -> m HC.PrvKeyC) -- ^ Function which produces a signature which verifies against 'cpReceiverPubKey'
    -> fee                              -- ^ Bitcoin transaction fee
    -> DustPolicy                       -- ^ Whether to keep or drop receiver change output if below dust limit
    -> m (Either ReceiverError HT.Tx)   -- ^ Settling Bitcoin transaction
getSettlementBitcoinTx rpc recvAdr signFunc txFee dp =
    fmap toHaskoinTx <$>
        getSignedSettlementTx rpc signFunc (mkChangeOut recvAdr txFee dp)

-- |Get the settlement tx for a 'ClosedServerChanI', where the closing payment
--   pays the Bitcoin transaction fee
closedGetSettlementTx ::
       ( Monad m
       , HasKeyDeriveIndex a
       )
    => ClosedServerChanI a              -- ^ Produced by 'acceptClosingPayment'
    -> HC.Address                       -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> (KeyDeriveIndex -> m HC.PrvKeyC) -- ^ Function which produces a signature which verifies against 'cpReceiverPubKey'
    -> DustPolicy                       -- ^ Whether to keep or drop receiver change output if below dust limit
    -> m (Either ReceiverError HT.Tx)   -- ^ Settling Bitcoin transaction
closedGetSettlementTx MkClosedServerChan{..} recvAdr signFunc dp = do
    let resE  = resultFromThePast $ acceptClosingPaymentInternal (toPaymentData cscClosingPayment) cscState
        (settleState,txFee) = either (throw . BadClosedServerChan) id resE
    getSettlementBitcoinTx settleState recvAdr signFunc txFee dp

{-# SPECIALIZE createPaymentInternal :: ClientPayChanI BtcSig -> BtcAmount -> Either BtcError (ClientPayChan, SignedPayment) #-}
{-# SPECIALIZE createPaymentInternal :: ClientPayChanI BtcSig -> Capped BtcAmount -> (ClientPayChan, SignedPayment, BtcAmount) #-}
{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChan -> m (Either PayChanError (ServerPayChan, BtcAmount)) #-}
{-# SPECIALIZE acceptPaymentInternal :: MonadTime m => PaymentData -> ServerPayChanX -> m (Either PayChanError (ServerPayChanX, BtcAmount)) #-}
