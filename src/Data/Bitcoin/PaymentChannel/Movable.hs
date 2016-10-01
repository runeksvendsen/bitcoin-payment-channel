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
import qualified Data.Bitcoin.PaymentChannel.Internal.State as S
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize         as Bin
import           Control.Applicative    ((<|>))

-- |A ReceiverPaymentChannel whose received value can be redeemed while
--  keeping the channel open, by switching between two different OutPoints
--  in the FundingTxInfo.
data MovableChan =
      Settled   {
        beginVal    :: BitcoinAmount
      , beginState  :: ReceiverPaymentChannel
  } | Unsettled {
        channels    ::  ChannelPair
      -- If payment 1/2 has been accepted, accept 2/2 with 'receiveSecondPayment pp'
      , finishPay   ::  Maybe PartialPayment
  } -- deriving (Show)

data ChannelPair = ChannelPair {
    beginVal'   ::  BitcoinAmount
  , oldState    ::  ReceiverPaymentChannel
  , newState    ::  ReceiverPaymentChannel
}

-- |We wrap the state in which only payment 1/2 has been received in a data
--  type, in order to be able to serialize it to disk.
data PartialPayment =
    NewNeedsUpdate ChannelPair BitcoinAmount
  | OldNeedsUpdate ChannelPair BitcoinAmount

instance PaymentChannel MovableChan where
    valueToMe = channelValueLeft . fst . getStateForClosing
    getChannelState = rpcState . fst . getStateForClosing
    _setChannelState (Settled v rpc) s = Settled v (rpc { rpcState = s })
    _setChannelState (Unsettled (ChannelPair v old new) fp) s =
        Unsettled (ChannelPair v (setState s old) (setState s new)) fp
            where setState s rpc = rpc { rpcState = s }

newMovableChan ::
    ChannelParameters
    -> FundingTxInfo
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan, BitcoinAmount)
newMovableChan cp fti@(CFundingTxInfo _ _ chanVal)
               fullPayment@(CFullPayment _ _ _ payChgAddr) =
    checkChangeAddr >>= channelFromInitialPayment cp fti desiredChangeAddr >>=
        \(payAmount, rpc) -> Right (payAmount, Settled chanVal rpc, channelValueLeft rpc)
    where desiredChangeAddr = getFundingAddress cp
          checkChangeAddr   =
                if payChgAddr /= desiredChangeAddr then
                    Left $ ChangeAddrMismatch desiredChangeAddr
                else
                    Right fullPayment

getStateForClosing = getCurrentState

getCurrentState :: MovableChan -> (ReceiverPaymentChannel,BitcoinAmount)
getCurrentState (Settled v rpc) = (rpc,v)
getCurrentState (Unsettled (ChannelPair v _ newRpc) _) = (newRpc,v)

getStateByInfo :: MovableChan -> BitcoinLockTime -> OutPoint -> Maybe (ReceiverPaymentChannel,BitcoinAmount)
getStateByInfo mc lt op = case mc of
    (Settled v rpc)                       -> checkInfo rpc v
    (Unsettled (ChannelPair v old new) _) -> checkInfo new v <|> checkInfo old v
    where checkInfo rpc v =
            if  getChannelFunding      rpc == op &&
                getExpirationDate rpc == lt then
                    Just (rpc,v)
                else
                    Nothing

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
        Just (settleTx, Unsettled (ChannelPair v rpc newRpc) Nothing)
    else
        Nothing
    where settleTx = getSettlementBitcoinTx rpc signFunc destAddr txFee
          newRpc = CReceiverPaymentChannel $ S.setFundingSource (getChannelState rpc) fti
          fti = CFundingTxInfo (txHash settleTx)
                -- Client output always at index zero
                0 (senderChangeValue rpc)

markAsSettled ::
    MovableChan
    -> Maybe MovableChan
markAsSettled Settled{} = Nothing
markAsSettled (Unsettled _ (Just _)) = Nothing
markAsSettled (Unsettled (ChannelPair v _ newRpc) Nothing) =
    if valueToMe newRpc > 0 then
        Just $ Settled v newRpc
    else
        Nothing

recvSinglePayment ::
    MovableChan
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan, BitcoinAmount)
recvSinglePayment (Settled v rpc) fp = recvPayment rpc fp >>=
    \(a, newRpc) -> Right (a, Settled v newRpc, channelValueLeft newRpc)
-- Accept payment 1/2
recvSinglePayment (Unsettled cp@(ChannelPair _ old new) Nothing) fp@(CFullPayment _ op _ _)
    | S.pcsPrevOut (getChannelState old) == op = oldRecvPay cp fp
    | S.pcsPrevOut (getChannelState new) == op = newRecvPay cp fp
    | otherwise = Left . OutPointMismatch . S.pcsPrevOut . getChannelState $ new
-- If we've already received paymed 1/2, we receive 2/2 with this
recvSinglePayment (Unsettled _ (Just pp)) fp = receiveSecondPayment pp fp

oldRecvPay ::
    ChannelPair
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan, BitcoinAmount)
oldRecvPay (ChannelPair v old new) fp = recvPayment old fp >>=
    -- Update old state, and leave behind a function that updates the new state
    \(amt, newOld) -> Right
            (amt, Unsettled (ChannelPair v newOld new)
            (Just $ NewNeedsUpdate (ChannelPair v newOld new) amt),
            channelValueLeft newOld)

newRecvPay ::
    ChannelPair
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan, BitcoinAmount)
newRecvPay (ChannelPair v old new) fp = recvPayment new fp >>=
    -- Update new state, and leave behind a function that updates the old state
     \(amt, newNew) -> Right
            (amt, Unsettled (ChannelPair v old newNew)
            (Just $ OldNeedsUpdate (ChannelPair v old newNew) amt),
            channelValueLeft newNew)

receiveSecondPayment ::
    PartialPayment
    -> FullPayment
    -> Either PayChanError (BitcoinAmount, MovableChan, BitcoinAmount)
receiveSecondPayment (OldNeedsUpdate cp amt) fp =
    checkChangeValueMatch amt fp >>= oldRecvPay cp >>=
        \(amt, mc, vLeft) -> Right (amt, mc { finishPay = Nothing }, vLeft)
receiveSecondPayment (NewNeedsUpdate cp amt) fp =
    checkChangeValueMatch amt fp >>= newRecvPay cp >>=
        \(amt, mc, vLeft) -> Right (amt, mc { finishPay = Nothing }, vLeft)

-- | We want to make sure that payment 1/2 and 2/2 are of equal value
checkChangeValueMatch :: BitcoinAmount -> FullPayment -> Either PayChanError FullPayment
checkChangeValueMatch firstPayVal fp@(CFullPayment (CPayment val _) _ _ _) =
    if val /= firstPayVal then Left $ PartialPaymentBadValue firstPayVal else Right fp


instance Bin.Serialize MovableChan where
    put (Settled bVal rpc) =
        Bin.putWord8 0x01 >> Bin.put bVal >> Bin.put rpc
    put (Unsettled cPair mPP) =
        Bin.putWord8 0x02 >> Bin.put cPair >> Bin.put mPP
    get = Bin.getWord8 >>= \w -> case w of
        0x01 -> Settled <$> Bin.get <*> Bin.get
        0x02 -> Unsettled <$> Bin.get <*> Bin.get
        n    -> fail $ "unknown start byte: " ++ show n

instance Bin.Serialize ChannelPair where
    put (ChannelPair bVal old new) =
        Bin.put bVal >> Bin.put old >> Bin.put new
    get = ChannelPair <$> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Serialize PartialPayment where
    put (NewNeedsUpdate cp amt) = Bin.putWord8 0x01 >> Bin.put cp >> Bin.put amt
    put (OldNeedsUpdate cp amt) = Bin.putWord8 0x02 >> Bin.put cp >> Bin.put amt
    get = Bin.getWord8 >>= \w -> case w of
            0x01 -> NewNeedsUpdate <$> Bin.get <*> Bin.get
            0x02 -> OldNeedsUpdate <$> Bin.get <*> Bin.get
            n    -> fail $ "unknown start byte: " ++ show n
