{-|
Module      : Data.Bitcoin.PaymentChannel
Description : Bitcoin payment channel library
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

In order to set up a payment channel between a sender and a receiver, the two parties must
 first agree on four parameters for the channel:

    (1) sender public key
    (2) receiver public key
    (3) channel expiration date
    (4) sender change output "dust limit" (see note [1])

 These parameters
 are contained in 'ChannelParameters', from which the Bitcoin address used to fund
 the payment channel can be derived using
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

[1] The minimum amount that the server is willing to accept as the client change value in the
 payment transaction. Set this to 700 and you'll be fine. It's only relevant if the channel is
 emptied of value completely, but it is necessary because the server doesn't want to accept
 payments based on transactions it cannot publish via the Bitcoin P2P network, because they
 contain an output of minuscule value.

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
    recvPaymentForClose,

    getSettlementBitcoinTx,
    getRefundBitcoinTx,

    getFundingAddress
)
where

import Data.Bitcoin.PaymentChannel.Internal.Types
    (PaymentTxConfig(..), Payment(..), FullPayment(..), pcsClientChangeVal)
import Data.Bitcoin.PaymentChannel.Internal.State
    (newPaymentChannelState, updatePaymentChannelState)
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
import Data.Bitcoin.PaymentChannel.Internal.Payment
    (createPayment, paymentFromState, verifyPaymentSigFromState)
import Data.Bitcoin.PaymentChannel.Internal.Settlement
    (signedSettlementTxFromState)
import Data.Bitcoin.PaymentChannel.Internal.Refund
    (refundTxAddSignature, getRefundTxHashForSigning)

import Data.Bitcoin.PaymentChannel.Util (getFundingAddress)
import Data.Bitcoin.PaymentChannel.Types

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Transaction as HT


-- |Create a new 'SenderPaymentChannel'.
-- A 'SenderPaymentChannel' object is created by supplying information about
-- the channel and the funding transaction, as well as the value of the first payment.
-- Returns a new 'SenderPaymentChannel' state object and the first channel payment.
channelWithInitialPaymentOf ::
    ChannelParameters               -- ^ Specifies channel sender and receiver, channel expiration date and "dust limit"
    -> FundingTxInfo                -- ^ Holds information about the Bitcoin transaction used to fund the channel
    -> (HC.Hash256 -> HC.Signature) -- ^Used to sign payments from sender. When given a 'HC.Hash256', produces a signature that verifies against sender PubKey.
    -> HC.Address                   -- ^ Value sender/client change address
    -> BitcoinAmount                -- ^ Value of initial payment. Can be zero.
    -> (BitcoinAmount, FullPayment, SenderPaymentChannel) -- ^Initial payment amount (may be capped), initial payment, and new sender state object
channelWithInitialPaymentOf cp fundInf signFunc sendAddr amount =
    let pConf = CPaymentTxConfig sendAddr
        (CFullPayment (CPayment _ tmpSig) _ _ _) =
            createPayment cp fundInf sendAddr amount signFunc
    in
        flip sendPayment amount $ CSenderPaymentChannel
            (newPaymentChannelState cp fundInf pConf tmpSig) signFunc

-- |Create new payment of specified value.
sendPayment ::
    SenderPaymentChannel                -- ^Sender state object
    -> BitcoinAmount                    -- ^ Amount to send (the actual payment amount is capped)
    -> (BitcoinAmount, FullPayment, SenderPaymentChannel)  -- ^ Actual amount actually sent, payment, and updated sender state object
sendPayment (CSenderPaymentChannel cs signFunc) amountToSend =
    let
        valSent = pcsClientChangeVal cs - newSenderValue
        newSenderValue = max (S.pcsDustLimit cs) (pcsClientChangeVal cs - amountToSend)
        fullPay = paymentFromState cs newSenderValue signFunc
    in
        case updatePaymentChannelState cs fullPay of
            Right newCS ->
                (valSent
                ,fullPay
                ,CSenderPaymentChannel newCS signFunc)
            Left e ->
                error $ "BUG: bad payment" ++ show e

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
    -> FundingTxInfo -- ^ Holds information about the Bitcoin transaction used to fund the channel
    -> HC.Address   -- ^ Value sender/client change address
    -> FullPayment -- ^Initial channel payment
    -> Either PayChanError (BitcoinAmount, ReceiverPaymentChannel) -- ^Error or: value_received plus state object
channelFromInitialPayment cp fundInf sendAddr fp@(CFullPayment (CPayment _ sig) _ _ _) =
        let
            pConf = CPaymentTxConfig sendAddr
        in
            flip recvPayment fp $ CReceiverPaymentChannel
                -- Create a new state with the unverified signature; then verify the same signature in 'recvPayment'
                (newPaymentChannelState cp fundInf pConf sig)

-- |Register, on the receiving side, a payment made by 'sendPayment' on the sending side.
-- Returns error if either the signature or payment amount is invalid, and otherwise
-- the amount received with this 'Payment' and a new state object.
recvPayment ::
    ReceiverPaymentChannel -- ^Receiver state object
    -> FullPayment -- ^Payment to verify and register
    -> Either PayChanError (BitcoinAmount, ReceiverPaymentChannel) -- ^Value received plus new receiver state object
recvPayment rpc@(CReceiverPaymentChannel oldState) fp@(CFullPayment paymnt _ _ _) =
    updatePaymentChannelState oldState fp >>=
    verifyPaymentSignature paymnt >>=
    (\newState -> Right (
        S.channelValueLeft oldState - S.channelValueLeft newState
        , rpc { rpcState = newState })
    )

verifyPaymentSignature ::
    Payment -- ^Payment whose signature to verify
    -> PaymentChannelState -- ^State object
    -> Either PayChanError PaymentChannelState
verifyPaymentSignature paymnt state =
    if verifyPaymentSigFromState state verifySenderPayment paymnt then
        Right state
    else
        Left SigVerifyFailed
    where verifySenderPayment hash pk sig = HC.verifySig hash sig (getPubKey pk)

-- |Same as 'recvPayment' but accept only a payment of zero value
--  with a new client change address. Used to produce the settlement
--  transaction that returns unsent funds to the client.
recvPaymentForClose ::
    ReceiverPaymentChannel -- ^Receiver state object
    -> FullPayment -- ^Payment to verify and register
    -> Either PayChanError ReceiverPaymentChannel -- ^ Receiver state object
recvPaymentForClose (CReceiverPaymentChannel state) fp =
    recvPayment newAddressState fp >>=
        \(amtRecv, newState) -> case amtRecv of
            0 -> Right newState
            _ -> Left ClosingPaymentBadValue
    where newAddressState = CReceiverPaymentChannel $
            S.setClientChangeAddress state (fpChangeAddr fp)

-- |The value transmitted over the channel is settled when this transaction is in the Blockchain.
-- The receiver will want to make sure a transaction produced by this function
-- is included in a Bitcoin block before the refund transaction becomes valid (see 'getRefundBitcoinTx').
-- The sender can only close the channel before expiration by requesting this transaction
-- from the receiver and publishing it to the Bitcoin network.
getSettlementBitcoinTx ::
    ReceiverPaymentChannel          -- ^ Receiver state object
    -> (HC.Hash256 -> HC.Signature) -- ^ Function which produces a signature which verifies against 'cpReceiverPubKey'
    -> HC.Address                   -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> BitcoinAmount                -- ^ Bitcoin transaction fee
    -> HT.Tx                        -- ^ Settling Bitcoin transaction
getSettlementBitcoinTx (CReceiverPaymentChannel cs) =
    signedSettlementTxFromState cs


