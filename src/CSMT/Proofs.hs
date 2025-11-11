{-# LANGUAGE StrictData #-}

module CSMT.Proofs
    ( Proof
    , mkInclusionProof
    , foldProof
    , verifyInclusionProof
    )
where

import CSMT.Interface
    ( CSMT (CSMT, query)
    , Direction (..)
    , Indirect (Indirect, value)
    , Key
    , opposite
    )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (Foldable (..))
import Data.List (isPrefixOf)

import Control.Monad (guard)

type Proof a = [(Direction, a)]

-- | Collect a proof for the presence of a key in the CSMT
mkInclusionProof :: Monad m => CSMT m a -> Key -> m (Maybe (Proof a))
mkInclusionProof CSMT{query} key = runMaybeT $ go [] key []
  where
    go _ [] rs = pure rs
    go u ks rs = do
        Indirect jump _ <- MaybeT $ query u
        guard $ jump `isPrefixOf` ks
        case drop (length jump) ks of
            [] -> pure rs
            (k : ks') -> do
                o <- MaybeT $ query (u <> jump <> [opposite k])
                go (u <> jump <> [k]) ks' ((k, value o) : rs)

-- | Fold a proof into a single value
foldProof :: (a -> a -> a) -> a -> Proof a -> a
foldProof add = foldl' step
  where
    step acc (L, h) = add acc h
    step acc (R, h) = add h acc

-- | Verify a proof of given the included value
verifyInclusionProof
    :: (Eq a, Monad m)
    => CSMT m a
    -> (a -> a -> a)
    -> a
    -> Proof a
    -> m Bool
verifyInclusionProof CSMT{query} add value proof = do
    mv <- query []
    pure $ case mv of
        Just Indirect{value = rootHash} -> rootHash == foldProof add value proof
        Nothing -> False
