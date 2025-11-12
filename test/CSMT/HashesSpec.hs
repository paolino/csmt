module CSMT.HashesSpec (spec) where

import CSMT.Hashes
    ( Hash
    , mkHash
    , parseProof
    , renderProof
    )
import CSMT.Interface (Direction (..))
import CSMT.Proofs (Proof)
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Testable (..), elements, forAll, listOf)

genProofs :: Gen (Proof Hash)
genProofs = listOf $ do
    dir <- elements [L, R]
    hashValue <- mkHash . B.pack <$> listOf (elements [0 .. 255])
    return (dir, hashValue)

spec :: Spec
spec = describe "Hashes" $ do
    it "renders and parses proofs correctly"
        $ property
        $ forAll genProofs
        $ \proof -> do
            let
                rendered = renderProof proof
                parsed = parseProof rendered
            parsed `shouldBe` Just proof
