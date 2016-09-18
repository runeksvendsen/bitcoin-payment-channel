{-|
Module      : Data.Bitcoin.PaymentChannel.Movable
Description : Bitcoin payment channel library
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

TODO
-}

module Data.Bitcoin.PaymentChannel.Movable where

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Internal.Types
import           Data.Bitcoin.PaymentChannel.Internal.State
import qualified Network.Haskoin.Crypto as HC


-- |A ReceiverPaymentChannel whose received value can be redeemed while
--  keeping the channel open, by switching between two different OutPoints
--  in the FundingTxInfo.
data MovableChan =
      Settled   {
        beginVal    :: BitcoinAmount
      , beginState  :: ReceiverPaymentChannel
  } | Unsettled {
        beginVal'   ::  BitcoinAmount
      , oldState    ::  ReceiverPaymentChannel
      , newState    ::  ReceiverPaymentChannel
      -- If payment 1/2 has been accepted, accept 2/2 with this function
      , finishPay   ::  Maybe (FullPayment -> Either PayChanError (BitcoinAmount, MovableChan))
  }

newMovableChan ::
    ChannelParameters
    -> FundingTxInfo
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan)
newMovableChan cp fti@(CFundingTxInfo _ _ chanVal)
               fullPayment@(CFullPayment _ _ _ payChgAddr) =
    checkChangeAddr >>= channelFromInitialPayment cp fti desiredChangeAddr >>=
        \(payAmount, rpc) -> Right (payAmount, Settled chanVal rpc)
    where desiredChangeAddr = getFundingAddress cp
          checkChangeAddr   =
                if payChgAddr /= desiredChangeAddr then
                    Left $ ChangeAddrMismatch desiredChangeAddr
                else
                    Right fullPayment

moveChannel ::
    MovableChan
    -> (HC.Hash256 -> HC.Signature) -- ^ Function which produces a signature which verifies against 'cpReceiverPubKey'
    -> HC.Address                   -- ^ Receiver destination address. Funds sent over the channel will be sent to this address, the rest back to the client change address (an argument to 'channelWithInitialPaymentOf').
    -> BitcoinAmount                -- ^ Bitcoin transaction fee
    -> Maybe (Tx, MovableChan)
moveChannel Unsettled{} _ _ _ = Nothing
moveChannel (Settled v rpc) signFunc destAddr txFee =
    -- If we haven't received any value, moving the channel makes no sense
    if valueToMe rpc > 0 then
        Just (settleTx, Unsettled v rpc newRpc Nothing)
    else
        Nothing
    where settleTx = getSettlementBitcoinTx rpc signFunc destAddr txFee
          newRpc = CReceiverPaymentChannel $ setFundingSource (getChannelState rpc) fti
          fti = CFundingTxInfo (txHash settleTx)
                -- Client output always at index zero
                0 (senderChangeValue rpc)

markAsSettled ::
    MovableChan
    -> Maybe MovableChan
markAsSettled Settled{} = Nothing
markAsSettled (Unsettled _ _ _ (Just _)) = Nothing
markAsSettled (Unsettled v _ newRpc Nothing) =
    if valueToMe newRpc > 0 then
        Just $ Settled v newRpc
    else
        Nothing

recvSinglePayment ::
    MovableChan
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan)
recvSinglePayment (Settled v rpc) fp = recvPayment rpc fp >>=
    \(a, newRpc) -> Right (a, Settled v newRpc)
-- Accept payment 1/2
recvSinglePayment (Unsettled v old new Nothing) fp@(CFullPayment _ op _ _)
    | pcsPrevOut (getChannelState old) == op = updateOld (old,new,v) fp
    | pcsPrevOut (getChannelState new) == op = updateNew (old,new,v) fp
    | otherwise = Left . OutPointMismatch . pcsPrevOut . getChannelState $ new
-- Accept payment 2/2
recvSinglePayment (Unsettled _ _ _ (Just recvPaymentFunc)) fp = recvPaymentFunc fp

updateOld ::
    (ReceiverPaymentChannel,ReceiverPaymentChannel,BitcoinAmount)
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan)
updateOld (old,new,v) fp = recvPayment old fp >>=
    -- Update old state, and leave behind a function that updates the new state
    \(amt, newOld) -> Right (amt, Unsettled v newOld new
        (Just $ \fp -> checkChangeValueMatch amt fp >>= updateNew (newOld,new,v)))

updateNew ::
    (ReceiverPaymentChannel,ReceiverPaymentChannel,BitcoinAmount)
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan)
updateNew (old,new,v) fp = recvPayment new fp >>=
    -- Update new state, and leave behind a function that updates the old state
     \(amt, newNew) -> Right (amt, Unsettled v old newNew
        (Just $ \fp -> checkChangeValueMatch amt fp >>= updateOld (old,newNew,v)))

-- | We want to make sure that payment 1/2 and 2/2 are of equal value
checkChangeValueMatch :: BitcoinAmount -> FullPayment -> Either PayChanError FullPayment
checkChangeValueMatch prevValue fp@(CFullPayment (CPayment val _) _ _ _) =
    if val /= prevValue then Left $ PartialPaymentBadValue prevValue else Right fp
