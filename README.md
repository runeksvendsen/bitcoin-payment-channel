# Bitcoin payment channel library
[![Build Status](https://travis-ci.org/runeksvendsen/bitcoin-payment-channel.svg?branch=master)](https://travis-ci.org/runeksvendsen/bitcoin-payment-channel)
## Description

A Bitcoin payment channel allows secure and instant transfer of bitcoins from one party to another. Payments are created and verified in less than a millisecond (plus network latency), and cannot be double spent, as the receiver of funds is defined during channel setup.

When the channel is closed, the settlement transaction transfers the appropriate value to each party, thus paying the Bitcoin transaction fee only once, regardless of the number of payments made over the channel. The channel setup procedure is trustless, because the funding party - after the chosen expiration date - is able to reclaim the bitcoins used to fund the channel, in case the receiving party goes missing.

This library implements a type of payment channel where channel setup is safe from transaction ID malleability, and value transfer is uni-directional (one party sends and the other party receives) (CHECKLOCKTIMEVERIFY-style).

## Documentation/usage

In order to set up a payment channel between a sender and a receiver,
the two parties must first agree on three \[1\] parameters for the
channel:

1.  sender public key

2.  receiver public key

3.  channel expiration date

These parameters are contained in `ChannelParameters`{.haskell
.identifier}, from which the Bitcoin address used to fund the payment
channel can be derived using `getFundingAddress`{.haskell .identifier}.
The transaction which pays to this address is the channel funding
transaction, and information about it is contained in a
`FundingTxInfo`{.haskell .identifier}. So, the channel funding
transaction will contain an output which pays to the address returned by
`getFundingAddress`{.haskell .identifier}, and once this transaction is
created and in the blockchain, a `SenderPaymentChannel`{.haskell
.identifier} and `ReceiverPaymentChannel`{.haskell .identifier} instance
can be created, after first creating the `FundingTxInfo`{.haskell
.identifier} instance. `FundingTxInfo`{.haskell .identifier} contains
three pieces of information about the funding transaction:

1.  hash/transaction ID

2.  index/vout of the funding output (paying to
    `getFundingAddress`{.haskell .identifier} address),

3.  value of the funding output (paying to `getFundingAddress`{.haskell
    .identifier} address)

With `ChannelParameters`{.haskell .identifier} and
`FundingTxInfo`{.haskell .identifier}, the sender can create a new
`SenderPaymentChannel`{.haskell .identifier}, plus the first channel
payment, using `channelWithInitialPaymentOf`{.haskell .identifier}.
`channelWithInitialPaymentOf`{.haskell .identifier} takes two additional
arguments:

1.  a signing function which, given a hash, produces a signature that
    verifies against `cpSenderPubKey`{.haskell .identifier} in
    `ChannelParameters`{.haskell .identifier}

2.  the value of the first channel payment

The sender will want to use
`flip ``Network.Haskoin.Crypto.signMsg`{.haskell
.identifier}` senderPrivKey` as the signing function, where
`senderPrivKey` is the private key from which `cpSenderPubKey`{.haskell
.identifier} is derived. `channelWithInitialPaymentOf`{.haskell
.identifier} will return the first channel `Payment`{.haskell
.identifier} as well as the new `SenderPaymentChannel`{.haskell
.identifier} state. The new state is stored, and the `Payment`{.haskell
.identifier} transferred to the receiver.

The receiver will now create its own channel state object,
`ReceiverPaymentChannel`{.haskell .identifier}, using
`channelFromInitialPayment`{.haskell .identifier}.
`channelFromInitialPayment`{.haskell .identifier} takes the same
`ChannelParameters`{.haskell .identifier} and `FundingTxInfo`{.haskell
.identifier} as was provided by the sender, and, in addition, the first
channel `Payment`{.haskell .identifier}, received from the sender.

Now the payment channel is open and ready for transmitting value. A new
`Payment`{.haskell .identifier} is created by the sender with
`sendPayment`{.haskell .identifier}, which yields a new payment, that
increases the total value transmitted to the receiver by the specified
amount, and an updated
`Data.Bitcoin.PaymentChannel.Types.SenderPaymentChannel`{.haskell
.identifier} state. The receiver will verify and register this
`Payment`{.haskell .identifier} on its side using `recvPayment`{.haskell
.identifier}, which, on success, returns the value received with this
payment plus the updated `ReceiverPaymentChannel`{.haskell .identifier}
state object.

Payments can flow from the sender to receiver until either the channel
is exhausted, or getting close to expiration (see important note below).
In either case the receiver will use `getSettlementBitcoinTx`{.haskell
.identifier} to create the settlement Bitcoin transaction, and publish
this transaction to the Bitcoin network. The settlement Bitcoin
transaction pays the total value transmitted over the channel to the
receiver and the rest back to the sender.

A settlement transaction can be produced by the value receiver using
`getSettlementBitcoinTx`{.haskell .identifier}. The receiver will want
to use `flip ``Network.Haskoin.Crypto.signMsg`{.haskell
.identifier}` receiverPrivKey` as the signing function passed to
`getSettlementBitcoinTx`{.haskell .identifier}, where `receiverPrivKey`
is the private key from which `cpReceiverPubKey`{.haskell .identifier}
is derived.

1

:   In addition to this, two configuration options must also be agreed
    upon: a "dust limit", and a "settlement period" (measured in hours),
    which is subtracted from the channel expiration date in order to
    arrive at the effective (earlier) expiration date. This is necessary
    to give the server/receiver time to publish the settlement
    transaction before the refund transaction becomes valid. The dust
    limit is the minimum amount that the server is willing to accept as
    the client change value in the payment transaction. It's only
    relevant if the channel is emptied of value completely, but it is
    necessary because the server doesn't want to accept payments based
    on transactions it cannot publish via the Bitcoin P2P network,
    because they contain an output of minuscule value. Sensible values
    are contained in `defaultConfig`{.haskell .identifier}, but
    client*sender and server*receiver need to agree on these parameter
    as well, although they are only relevant 1) (in the case of the
    dust limit) if the channel is compleltely exchausted and 2) (in case
    of the "settlemend period") if the client tries to make payments
    close to expiration (and, in case the client does, it will just
    receive an error in response, saying the channel is now closed).

***IMPORTANT:*** *Channel setup is risk free because the sender can
derive a refund Bitcoin transaction* *using
`getRefundBitcoinTx`{.haskell .identifier}, which returns the bitcoins
used to fund the channel back to the sender.* *This refund transaction,
however, is not valid until the expiration date specified in
`ChannelParameters`{.haskell .identifier},* *but it is paramount that
the value receiver get a settlement transaction included in a block*
*before the refund transaction becomes valid. Due to the fact that
Bitcoin network time is allowed* *to drift up to two hours from actual
time, and the fact that finding new Bitcoin blocks does not occur*
*according to any schedule, it would be wise for the receiver to publish
a settlement transaction at least* *6 hours before the specified channel
expiration time, and possibly earlier, if the receiver wants to* *be
cautious.*


## Contact/questions/bugs

Please file an issue here or contact me if you have any questions or discover bugs.
