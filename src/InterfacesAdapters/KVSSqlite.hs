{-# LANGUAGE OverloadedStrings #-}
module InterfacesAdapters.KVSSqlite where

import Polysemy

import Polysemy.Internal.Union (Member)
import Data.Aeson.Types (ToJSON, FromJSON)
import UseCases.KVS (KVS (..))
import Polysemy.Input 
import           Data.Maybe             (listToMaybe)
import           Data.Function          ((&))
import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import           Database.SQLite.Simple.Types
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy.Char8 (unpack, pack)

{--
  ListAllKvs :: KVS k v m [(k, v)]
  GetKvs     :: k -> KVS k v m (Maybe v)
  InsertKvs  :: k -> v -> KVS k v m ()
  DeleteKvs  :: k -> KVS k v m ()
--}

data KeyValueRow = KeyValueRow T.Text T.Text deriving (Show)

instance FromRow KeyValueRow where
  fromRow = KeyValueRow <$> field <*> field

instance ToRow KeyValueRow where
  toRow (KeyValueRow key_ val) = toRow (key_, val)

-- | Run a KVStore effect on an SQLite backend. Requires a DB connection as input.
runKVStoreAsSQLite :: (Member (Embed IO) r, Show k, Read k, ToJSON v, FromJSON v)
                   => Sem (KVS k v : r) a
                   -> Sem (Input SQL.Connection : r) a
runKVStoreAsSQLite = reinterpret $ \case
  GetKvs key -> do
    conn <- input
    rows <- embed (SQL.queryNamed conn
                        "SELECT value FROM store WHERE key = :key"
                        [":key" := show key] :: IO [KeyValueRow])           
    case rows of
      []   -> return Nothing
      (KeyValueRow _key value):xs -> return $ (decode . encodeUtf8) value

  InsertKvs key value -> do
    let (query, params) = ("INSERT INTO store (key, value) VALUES (:key, :value) "
                           <> "ON CONFLICT (key) DO UPDATE SET value = excluded.value", 
                           [":key" := show key, ":value" := encodedValue])
                          where
                            encodedValue = (decodeUtf8 . encode) value -- :: T.Text
    conn <- input
    embed $ SQL.executeNamed conn query params


getConnection :: FilePath -> IO SQL.Connection
getConnection dbFile = do 
  conn <- SQL.open dbFile
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
  return conn



