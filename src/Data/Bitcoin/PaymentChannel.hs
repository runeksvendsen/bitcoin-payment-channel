{-|
Module      : Data.Bitcoin.PaymentChannel
Description : Bitcoin payment channel library
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

In order to set up a payment channel between a sender and a receiver, the two parties must
 first agree on three parameters for the channel:

    (1) sender public key
    (2) receiver public key
    (3) channel expiration date

 These parameters
 are contained in 'ChannelParameters', from which a channel funding
 address can be derived using
 'getFundingAddress'. The transaction which pays to this address is the channel funding
 transaction, and information about it is contained in a 'FundingTxInfo'.
 So, the channel funding transaction will contain an output which pays to the address returned by
 'getFundingAddress', and once this transaction is created and in
 the blockchain, a 'SenderPaymentChannel' and
 'ReceiverPaymentChannel' instance can be created, after first creating the 'FundingTxInfo' instance.
 'FundingTxInfo' contains three pieces of information about the funding transaction:

    (1) hash/transaction ID
    (2) index/vout of the funding output (paying to 'getFundingAddress' address),
    (3) value of the funding output (paying to 'getFundingAddress' address)

With 'ChannelParameters' and 'FundingTxInfo',
 the sender can create a new 'SenderPaymentChannel', plus
 the first channel payment, using 'channelWithInitialPaymentOf'. 'channelWithInitialPaymentOf'
 takes two additional arguments:

    (1) a signing function which, given a hash, produces a signature that verifies against
        'cpSenderPubKey' in 'ChannelParameters'
    (2) the value of the first channel payment

 The sender will want to use @flip 'Network.Haskoin.Crypto.signMsg' senderPrivKey@ as the signing
 function, where @senderPrivKey@ is the private key from which 'cpSenderPubKey' is derived.
 'channelWithInitialPaymentOf' will return the first channel 'Payment' as well as
 the new 'SenderPaymentChannel' state. The new state is stored, and the 'Payment'
 transferred to the receiver.

The receiver will now create its own channel state object, 'ReceiverPaymentChannel', using
 'channelFromInitialPayment'.
 'channelFromInitialPayment' takes the same 'ChannelParameters' and 'FundingTxInfo'
 as was provided by the sender, and, in addition, the first channel 'Payment', received from the sender.

Now the payment channel is open and ready for transmitting value. A new 'Payment' is created by
 the sender with 'sendPayment', which yields a new payment, that increases the total value transmitted
 to the receiver by the specified amount, and an updated 'Data.Bitcoin.PaymentChannel.Types.SenderPaymentChannel' state.
 The receiver will verify and register this 'Payment' on its side using 'recvPayment', which, on success,
 returns the value received with this payment plus the updated
 'ReceiverPaymentChannel' state object.

Payments can flow from the sender to receiver until either the channel is exhausted, or getting
 close to expiration (see important note below). In either case the receiver will use 'getSettlementBitcoinTx' to create the settlement
 Bitcoin transaction, and publish this transaction to the Bitcoin network. The settlement Bitcoin
 transaction pays the total value transmitted over the channel to the receiver and the rest back
 to the sender.

A settlement transaction can be produced by the value receiver using 'getSettlementBitcoinTx'.
 The receiver will want to use @flip 'Network.Haskoin.Crypto.signMsg' receiverPrivKey@ as the
 signing function passed to 'getSettlementBitcoinTx',
where @receiverPrivKey@ is the private key from which 'cpReceiverPubKey' is derived.


__/IMPORTANT:/__ /Channel setup is risk free because the sender can derive a refund Bitcoin transaction/
 /using 'getRefundBitcoinTx', which returns the bitcoins used to fund the channel back to the sender./
 /This refund transaction, however, is not valid until the expiration date specified in 'ChannelParameters',/
 /but it is paramount that the value receiver get a settlement transaction included in a block/
 /before the refund transaction becomes valid. Due to the fact that Bitcoin network time is allowed/
 /to drift up to two hours from actual time, and the fact that finding new Bitcoin blocks does not occur/
 /according to any schedule, it would be wise for the receiver to publish a settlement transaction at least/
 /4 hours before the specified channel expiration time, and possibly earlier, if the receiver wants to/
 /be cautious./

-}

module Data.Bitcoin.PaymentChannel
(
    channelWithInitialPaymentOf,
    sendPayment,

    channelFromInitialPayment,
    recvPayment,

    getSettlementBitcoinTx,
    getRefundBitcoinTx,

    getFundingAddress
)
where

import Data.Bitcoin.PaymentChannel.Internal.Error
    (PayChanError(..))
import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentTxConfig(..), pcsValueLeft, cpChannelValueLeft)
import Data.Bitcoin.PaymentChannel.Internal.State
    (newPaymentChannelState, updatePaymentChannelState)
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
    (channelValueLeft)
import Data.Bitcoin.PaymentChannel.Internal.Payment
    (createPayment, verifyPaymentSig)
import Data.Bitcoin.PaymentChannel.Internal.Settlement
    (getSignedSettlementTx, getSettlementTxHashForSigning)
import Data.Bitcoin.PaymentChannel.Internal.Refund
    (refundTxAddSignature, getRefundTxHashForSigning)
import Data.Bitcoin.PaymentChannel.Internal.Util
    (bitcoinPayPKBS, mapRight)

import Data.Bitcoin.PaymentChannel.Util (getFundingAddress)
import Data.Bitcoin.PaymentChannel.Types

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT
import qualified  Network.Haskoin.Script as HS


-- |Create a new 'SenderPaymentChannel'.
-- A 'SenderPaymentChannel' object is created by supplying information about
-- the channel and the funding transaction, as well as the value of the first payment.
-- Returns a new 'SenderPaymentChannel' state object and the first channel payment.
channelWithInitialPaymentOf ::
    ChannelParameters -- ^ Specifies channel sender and receiver, plus channel expiration date
    -> FundingTxInfo -- ^ Holds information about the transaction used to fund the channel
    -> (HC.Hash256 -> HC.Signature) -- ^ See 'spcSignFunc'
    -> HC.Address   -- ^ Value sender/client change address
    -> BitcoinAmount -- ^ Value of initial payment. Must be greater than or equal to 'minimumInitialPayment'
    -> (Payment, SenderPaymentChannel) -- ^Initial payment and new sender state object
channelWithInitialPaymentOf
    cp@(CChannelParameters sendPK recvPK _)
    fundInf@(CFundingTxInfo hash idx chanVal) signFunc sendAddr amount =
        let pConf = CPaymentTxConfig sendAddr in
        sendPayment
            (CSenderPaymentChannel (newPaymentChannelState cp fundInf pConf) signFunc)
            amount

-- |Create new payment of specified value.
sendPayment ::
    SenderPaymentChannel -- ^Sender state object
    -> BitcoinAmount -- ^ Amount to send (the actual payment amount is capped so that it doesn't overflow the maximum channel value)
    -> (Payment, SenderPaymentChannel) -- ^ Payment and updated sender state object
sendPayment (CSenderPaymentChannel cs signFunc) amountToSend =
    let
        valSent = min (pcsValueLeft cs) amountToSend
        newSenderValue = pcsValueLeft cs - valSent
        payment = createPayment cs newSenderValue signFunc
    in
        case updatePaymentChannelState cs payment of
            Right newCS ->
                (payment
                ,CSenderPaymentChannel newCS signFunc)
            Left _ -> error "BUG: 'createPayment' created value-backtracking tx"

-- |Produces a Bitcoin transaction which sends all channel funds back to the sender.
-- Will not be accepted by the Bitcoin network until the expiration time specified in
-- 'ChannelParameters'. Receiver beware of Bitcoin network time drift and the
-- unpreditable nature of finding new blocks.
getRefundBitcoinTx ::
    SenderPaymentChannel -- ^Sender state object
    -> BitcoinAmount -- ^Refund transaction fee
    -> HT.Tx -- ^Refund Bitcoin transaction
getRefundBitcoinTx (CSenderPaymentChannel cs signFunc) txFee =
    refundTxAddSignature cs txFee $ signFunc $ getRefundTxHashForSigning cs txFee


-- |Create new 'ReceiverPaymentChannel'.
-- A channel is initialized with various information
-- about the payment channel, as well as the first channel payment
-- produced by the sender.
channelFromInitialPayment ::
    ChannelParameters -- ^ Specifies channel sender and receiver, plus channel expiration date
    -> FundingTxInfo -- ^ Holds information about the transaction used to fund the channel
    -> HC.Address   -- ^ Value sender/client change address
    -> Payment -- ^Initial channel payment
    -> Either PayChanError (BitcoinAmount, ReceiverPaymentChannel) -- ^Error or: value_received plus state object
channelFromInitialPayment cp@(CChannelParameters sendPK recvPK _)
    fundInf sendAddr paymnt =
        let
            pConf = CPaymentTxConfig sendAddr
        in
            flip recvPayment paymnt $ CReceiverPaymentChannel
                (newPaymentChannelState cp fundInf pConf)

-- |Register, on the receiving side, a payment made by 'sendPayment' on the sending side.
-- Returns error if either the signature or payment amount is invalid, and otherwise
-- the amount received with this 'Payment' and a new state object.
recvPayment ::
    ReceiverPaymentChannel -- ^Receiver state object
    -> Payment -- ^Payment to verify and register
    -> Either PayChanError (BitcoinAmount, ReceiverPaymentChannel) -- ^Value received plus new receiver state object
recvPayment rpc@(CReceiverPaymentChannel oldState) paymnt =
    let verifyFunc hash pk sig = HC.verifySig hash sig pk in
    if verifyPaymentSig oldState paymnt verifyFunc then
        updatePaymentChannelState oldState paymnt >>=
        (\newState -> Right (
            S.channelValueLeft oldState - S.channelValueLeft newState
            , rpc { rpcState = newState })
        )
    else
        Left BadSignature


-- |The value transmitted over the channel is settled when this transaction is in the Blockchain.
-- The receiver will want to make sure a transaction produced by this function
-- is included in a Bitcoin block before the refund transaction becomes valid (see 'getRefundBitcoinTx').
-- The sender can only close the channel before expiration by requesting this transaction
-- from the receiver and publishing it to the Bitcoin network.
-- Returns NoValueTransferred if no payment has been received yet.
getSettlementBitcoinTx ::
    ReceiverPaymentChannel -- ^Receiver state object
    -> (HC.Hash256 -> HC.Signature) -- ^ Function which produces a signature, over a hash, which verifies against 'cpReceiverPubKey'
    -> HC.Address
    -> BitcoinAmount -- ^Bitcoin transaction fee
    -> HT.Tx -- ^Settling Bitcoin transaction
getSettlementBitcoinTx (CReceiverPaymentChannel cs) signFunc recvAddr txFee =
    nothingThrowInternal $ getSignedSettlementTx cs recvAddr txFee serverSig
        where serverSig = signFunc (getSettlementTxHashForSigning cs recvAddr txFee)
              nothingThrowInternal Nothing =  error $
                  "tried to produce settlement transaction without any payments in state." ++
                  " this should not be possible via the library interface."
              nothingThrowInternal (Just tx) = tx


