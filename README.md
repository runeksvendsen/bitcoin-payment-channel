# Bitcoin payment channel library

A Bitcoin payment channel allows two parties two send value to each other very rapidly. The speed of the value transfer is limited primarily by network latency, and payments cannot be double spent, as the receiver of funds is defined during channel setup. The channel setup procedure is trustless, in that the funding party is able to retrieve the funds, after a chosen expiration date, in case the receiving party goes missing.

This library implements a type of payment channel (CHECKLOCKTIMEVERIFY-style) where channel setup is safe from transaction ID malleability, and value transfer is uni-directional (one party sends and the other party receives).

## Documentation/usage

Please see the soon-to-be [Hackage documentation](https://hackage.haskell.org/package/bitcoin-payment-channel/docs/Data-Bitcoin-PaymentChannel.html).

## Contact/questions/bugs

Please file an issue here or contact me if you have any questions or discover bugs.