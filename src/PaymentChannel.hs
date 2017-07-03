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

module PaymentChannel
( -- *Initialization
  hasMinimumDuration

-- **Funding
, getFundingAddress
, validFundingInfo

-- **State creation
, channelWithInitialPayment
, channelFromInitialPayment
, setMetadata

-- *Payment
, createPayment, createPaymentCapped, Capped(..)
, acceptPayment
, ClosedServerChanX, getClosedState

-- *Settlement
, getRefundBitcoinTx
, createClosingPayment
, acceptClosingPayment
, getSettlementBitcoinTx
, closedGetSettlementTx
, DustPolicy(..)
, ChangeOutFee
, RefundTx
, SettleTx

-- *Types
, module PaymentChannel.Types

  -- *RESTful Bitcoin Payment Channel Protocol
, module PaymentChannel.RBPCP.Parse
)
where

import           PaymentChannel.Client
import           PaymentChannel.Internal.Receiver.Util
import           PaymentChannel.RBPCP.Parse
import           PaymentChannel.Server
import           PaymentChannel.Types                  hiding (ChangeOutFee,
                                                        ClosedServerChanX,
                                                        getClosedState)
import           PaymentChannel.Util
