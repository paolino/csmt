{-# LANGUAGE OverloadedLists #-}

module CSMT.InterfaceSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , compareKeys
    , getKey
    , putKey
    )
import CSMT.Test.Lib (genKey)
import Data.Serialize.Extra (evalPutM, unsafeEvalGet)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), forAll, vectorOf)

spec :: Spec
spec = do
    describe "compareKeys" $ do
        it "handles empty keys"
            $ compareKeys [] []
            `shouldBe` ([], [], [])
        it "handles identical keys"
            $ compareKeys [L, R, L] [L, R, L]
            `shouldBe` ([L, R, L], [], [])
        it "handles common prefixes"
            $ compareKeys [L, R, R, R] [L, R, L, R]
            `shouldBe` ([L, R], [R, R], [L, R])
        it "maintains information"
            $ property
            $ forAll (vectorOf 2 genKey)
            $ \case
                [k1, k2] ->
                    let (common, suffix1, suffix2) = compareKeys k1 k2
                    in  common <> suffix1 == k1
                            && common <> suffix2 == k2
                _ -> error "vectorOf produced wrong number of keys"
        it "produces suffixes without common prefixes"
            $ property
            $ forAll (vectorOf 2 genKey)
            $ \case
                [k1, k2] ->
                    let (_, suffix1, suffix2) = compareKeys k1 k2
                    in  case (suffix1, suffix2) of
                            (d1 : _, d2 : _) -> d1 /= d2
                            _ -> True
                _ -> error "vectorOf produced wrong number of keys"
    describe "key convert to bytes and back" $ do
        it "is identity"
            $ property
            $ forAll genKey
            $ \key -> do
                let bs = evalPutM $ putKey key
                    key' = unsafeEvalGet getKey bs
                key' `shouldBe` key
