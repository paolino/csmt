{-# LANGUAGE StrictData #-}

module CSMT.Backend.RocksDB
    ( rocksDBCSMT
    , withRocksDB
    , RocksDB
    , RunRocksDB (..)
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Indirect
    , Insert
    , Key
    , Query
    )

import CSMT.Backend.RocksDB.Key
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteArray (ByteArray)
import Database.RocksDB
    ( BatchOp (Put)
    , Config (..)
    , DB
    , get
    , withDB
    , write
    )

type RocksDB = ReaderT DB IO

rocksDBQuery :: ByteArray a => Query RocksDB a
rocksDBQuery k = do
    let rdbk = rocksPathKeyToRocksKey $ keyToRocksPathKey k
    db <- ask
    r <- lift $ get db rdbk
    pure $ rocksValueToIndirect <$> r

rocksDBInsert :: ByteArray a => Insert RocksDB a
rocksDBInsert kvs = do
    let ops = prepare kvs
    db <- ask
    lift $ write db ops

prepare :: ByteArray a => [(Key, Indirect a)] -> [BatchOp]
prepare = fmap $ \(k, ind) ->
    let rdbk = rocksPathKeyToRocksKey $ keyToRocksPathKey k
        rdbv = indirectToRocksValue ind
    in  Put rdbk rdbv

rocksDBCSMT :: ByteArray a => CSMT RocksDB a
rocksDBCSMT =
    CSMT
        { insert = rocksDBInsert
        , query = rocksDBQuery
        }

newtype RunRocksDB = RunRocksDB (forall a. RocksDB a -> IO a)

withRocksDB :: FilePath -> (RunRocksDB -> IO b) -> IO b
withRocksDB path action = do
    withDB path config $ \db -> do
        action $ RunRocksDB $ flip runReaderT db

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = True
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }
