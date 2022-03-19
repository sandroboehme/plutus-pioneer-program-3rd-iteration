{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.ModelSandro
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                                 hiding (elements)
import           Control.Monad                                (forM_, void, when)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isJust, isNothing)
import           Data.Monoid                                  (Last (..))
import           Data.String                                  (IsString (..))
import           Data.Text                                    (Text)
import           Plutus.Contract
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel           as Test
import           Plutus.Contract.Test.ContractModel.Symbolics
import           Plutus.Trace.Emulator                        as Trace
import           Ledger                                       hiding (singleton)
import           Ledger.Ada                                   as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week08.TokenSaleFixedSandro                        (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints')

type TSUseSchema' = TSUseSchema .\/ Endpoint "init" TokenSale

useEndpoints'' :: Contract () TSUseSchema' Text ()
useEndpoints'' = awaitPromise $ endpoint @"init" useEndpoints'

data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

makeLenses ''TSState

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

tests :: TestTree
tests = testProperty "token sale model" prop_TS

instance ContractModel TSModel where
    -- That's an associated data type (advanced feature) see https://youtu.be/49oAwySp6Ys?t=552
    -- Represents the actions that QuickCheck will generate.
    data Action TSModel =
              -- This wallet starts a token sale
              Start Wallet
              -- The second wallet sets the price for the token sale operated by the first one.
              -- If both wallets aren't the same and don't belong to the owner it should throw an error.
            | SetPrice Wallet Wallet Integer
              -- The second wallet want's to add tokens to the token sale operated by the first wallet.
            | AddTokens Wallet Wallet Integer
              -- The second wallet wants to withdraw Integer tokens and Integer ADA from the token sale run by the first wallet.
              -- If both wallets aren't the same and don't belong to the owner it should throw an error.
            | Withdraw Wallet Wallet Integer Integer
            -- The second wallet wants to buy from the first wallets token sale Integer amount of tokens.
            | BuyTokens Wallet Wallet Integer
            | Close Wallet Wallet
        deriving (Show, Eq)

    -- associated data type / generalized algebraic data type (gadt) has different format see: https://youtu.be/49oAwySp6Ys?t=720
    -- Normally the type parameters are all the same for all constructures. Here the contracts have different type parameters.
    -- The key that identifies the instance of the contract that we are running
    data ContractInstanceKey TSModel w s e p where
        -- The constructor `StartKey` takes a wallet as an argument and produces `ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text ()`
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text ()
        -- Second wallet owns the token sale and first wallet runs the contract
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema'  Text ()

    -- Tells the system how to extract the wallet that a given contract is running on by its `ContractInstanceKey`.
    -- As a convention in our token sale the fist wallet is always the one running the token sale
    -- and the second is the one interacting with it.
    instanceWallet :: ContractInstanceKey TSModel w s e p -> Wallet
    instanceWallet (StartKey w) = w
    instanceWallet (UseKey _ w) = w

    -- Used to identify running instances of contracts.
    -- see https://youtu.be/49oAwySp6Ys?t=1090
    -- Has to result in a different tag for each instance that we will ever run in our tests.
    -- we are forced to implement it ourself because we have 3 contract instances per wallet (one start and two use instances)
    -- and the default only allows one wallet.
    instanceTag :: SchemaConstraints w s e => ContractInstanceKey TSModel w s e p -> ContractInstanceTag
    instanceTag key = fromString $ "instance tag for: " ++ show key

    -- see https://youtu.be/49oAwySp6Ys?t=1365
    arbitraryAction :: ModelState TSModel -> Gen (Action TSModel)
    arbitraryAction _ = oneof
        [ Start     <$> genWallet
        , SetPrice  <$> genWallet <*> genWallet <*> genNonNeg
        , AddTokens <$> genWallet <*> genWallet <*> genNonNeg
        , BuyTokens <$> genWallet <*> genWallet <*> genNonNeg
        , Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg
        , Close  <$> genWallet <*> genWallet
        ]

    initialState :: TSModel
    initialState = TSModel Map.empty

    initialInstances :: [StartContract TSModel]
    -- list comprehension
    initialInstances =    [StartContract (StartKey v) () | v <- wallets]
                       ++ [StartContract (UseKey v w) () | v <- wallets, w <- wallets]

    -- see https://youtu.be/49oAwySp6Ys?t=1912
    precondition :: ModelState TSModel -> Action TSModel -> Bool
    -- makes sure that precondition to start the token sale is the ts state `Nothing`
    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v
    precondition s (Close v _)        = isJust    $ getTSState' s v

    -- describes the effects the corresponding action is expected to have on the model
    nextState :: Action TSModel -> Spec TSModel ()
    nextState (Start w) = do
        wait 3
        -- `$=` comes from the Spec monad.
        -- The value at the right hand side is the one to be set on
        -- the value retrieved from the lens at the left hand side
        --
        -- `at` returns a `Maybe` type
        (tsModel . at w) $= Just (TSState 0 0 0)
        withdraw w $ Ada.toValue minAdaTxOut
    nextState (SetPrice v w p) = do
        wait 3
        when (v == w) $
            -- like `at` but doesn't return a `Maybe` type
            (tsModel . ix v . tssPrice) $= p
    nextState (AddTokens v w n) = do
        wait 3
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            -- `view` is the same as `^.`
            -- `bc` is the balance change of the wallet
            bc <- actualValPart <$> askModelState (view $ balanceChange w)
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                -- the wallet should have less tokens
                withdraw w $ assetClassValue token n
                -- `$~` is similar to `$=` but `$~` applies a function to the value
                -- the token sale should have more tokens
                (tsModel . ix v . tssToken) $~ (+ n)
    nextState (BuyTokens v w n) = do
        wait 3
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
    nextState (Withdraw v w n l) = do
        wait 3
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
    nextState (Close v w) = do
        wait 3
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t  -> do
                        deposit w $ lovelaceValueOf (t ^. tssLovelace)
--                           <> Ada.toValue (negate minAdaTxOut)
                           <> assetClassValue (tokens Map.! w) (t ^. tssToken)
                        withdraw w $ Ada.toValue (-minAdaTxOut)
                        (tsModel . at w) $= Nothing
                _ -> return ()

    startInstances :: ModelState TSModel -> Action TSModel -> [StartContract TSModel]
    startInstances _ _ = []

    instanceContract :: (SymToken -> AssetClass) -> ContractInstanceKey TSModel w s e p -> p -> Contract w s e ()
    instanceContract _ (StartKey _) () = startEndpoint
    instanceContract _ (UseKey _ _) () = useEndpoints''

    -- Tells how an action is expressed in the emulator
    -- `HandleFun` allows us to get from `ContractInstanceKey`s to handles that allow us to interact with contracts via endpoints
    -- `SpecificationEmulatorTrace` is very similar to an emulator trace
    perform :: HandleFun TSModel -> (SymToken -> AssetClass) -> ModelState TSModel -> Action TSModel -> SpecificationEmulatorTrace ()
    -- `Start`: see https://youtu.be/49oAwySp6Ys?t=3686
    perform h _ m (Start v)         = do
        let handle = h $ StartKey v
        withWait m $ callEndpoint @"start" handle (tokenCurrencies Map.! v, tokenNames Map.! v)
        Last mts <- observableState handle
        case mts of
            Nothing -> Trace.throwError $ GenericError $ "starting token sale for wallet " ++ show v ++ " failed"
            Just ts -> forM_ wallets $ \w ->
                callEndpoint @"init" (h $ UseKey v w) ts -- kicks off the use contract
    perform h _ m (SetPrice v w p)   = withWait m $ callEndpoint @"set price"  (h $ UseKey v w) p
    perform h _ m (AddTokens v w n)  = withWait m $ callEndpoint @"add tokens" (h $ UseKey v w) n
    perform h _ m (BuyTokens v w n)  = withWait m $ callEndpoint @"buy tokens" (h $ UseKey v w) n
    perform h _ m (Withdraw v w n l) = withWait m $ callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)
    perform h _ m (Close v w)  = withWait m $ callEndpoint @"close" (h $ UseKey v w) ()

-- `withWait` ensures that something takes 3 slots
withWait :: ModelState TSModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait m c = void $ c >> waitUntilSlot ((m ^. Test.currentSlot) + 3)

deriving instance Eq (ContractInstanceKey TSModel w s e p)
deriving instance Show (ContractInstanceKey TSModel w s e p)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v -- ^. is to view a lens

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

tokenAmt :: Integer
tokenAmt = 1_000

-- `Property` is similar to `Bool`
prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig . initialChainState .~ Left d)
    defaultCoverageOptions
    -- checks if the model generated in the `nextState` function matches what happens in our emulator trace
    (const $ pure True)
  where

    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS
