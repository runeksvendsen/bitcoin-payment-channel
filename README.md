# Bitcoin payment channel library
[![Build Status](https://travis-ci.org/runeksvendsen/bitcoin-payment-channel.svg?branch=master)](https://travis-ci.org/runeksvendsen/bitcoin-payment-channel)
## Description

A Bitcoin payment channel allows secure and instant transfer of bitcoins from one party to another. Payments are created and verified in less than a millisecond (plus network latency), and cannot be double spent, as the receiver of funds is defined during channel setup.

When the channel is closed, the settlement transaction transfers the appropriate value to each party, thus paying the Bitcoin transaction fee only once, regardless of the number of payments made over the channel. The channel setup procedure is trustless, because the funding party - after the chosen expiration date - is able to reclaim the bitcoins used to fund the channel, in case the receiving party goes missing.

This library implements a type of payment channel where channel setup is safe from transaction ID malleability, and value transfer is uni-directional (one party sends and the other party receives) (CHECKLOCKTIMEVERIFY-style).

## Documentation/usage

See documentation for `PaymentChannel` module on Hackage: http://hackage.haskell.org/package/bitcoin-payment-channel

## Contact/questions/bugs

Please file an issue here or contact me if you have any questions or discover bugs.
