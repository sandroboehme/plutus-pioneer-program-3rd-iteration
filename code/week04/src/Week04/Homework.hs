{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract


errorHandledPayContract :: Contract () PaySchema Text ()
errorHandledPayContract = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace l1 l2 = do
     h1 <- activateContractWallet (knownWallet 1) errorHandledPayContract
     let pp1 = PayParams
                { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
                , ppLovelace    = l1
                }
     callEndpoint @"pay" h1 pp1
     void $ Emulator.waitNSlots 1
     let pp2 = PayParams
                { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
                , ppLovelace    = l2
                }
     -- TODO: Check if the below line (and the whole solution) is correct.
     h2 <- activateContractWallet (knownWallet 1) errorHandledPayContract
     callEndpoint @"pay" h2 pp2
     void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
