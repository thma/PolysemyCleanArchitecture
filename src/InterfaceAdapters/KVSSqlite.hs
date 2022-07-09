{-# LANGUAGE OverloadedStrings #-}
module InterfaceAdapters.KVSSqlite where

import           Polysemy

import           Data.Aeson                     (decode, encode)
import           Data.Aeson.Types               (FromJSON, ToJSON)
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Database.SQLite.Simple         (NamedParam ((:=)))
import qualified Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.FromRow
import           InterfaceAdapters.Config
import           Polysemy.Input                 (Input, input)
import           Polysemy.Trace                 (Trace, trace)
import           UseCases.KVS                   (KVS (..))


data KeyValueRow = KeyValueRow T.Text T.Text
    deriving (Show)

instance FromRow KeyValueRow where
  fromRow = KeyValueRow <$> field <*> field

--instance ToRow KeyValueRow where
--  toRow (KeyValueRow key_ val) = toRow (key_, val)

-- | Run a KVStore effect against a SQLite backend. Requires a Config object as input.
runKvsAsSQLite :: (Member (Embed IO) r, Member (Input Config) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v)
                   => Sem (KVS k v : r) a
                   -> Sem r a
runKvsAsSQLite = interpret $ \case
  GetKvs k      -> getAction k
  ListAllKvs    -> listAction
  InsertKvs k v -> insertAction k v
  DeleteKvs k   -> deleteAction k

  where

    getAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, FromJSON v) => k -> Sem r (Maybe v)
    getAction key = do
      trace $ "getAction: " ++ show key
      conn <- connectionFrom input
      rows <- embed (SQL.queryNamed conn
                          "SELECT key, value FROM store WHERE key = :key"
                          [":key" := show key] :: IO [KeyValueRow])
      case rows of
        []                         -> return Nothing
        (KeyValueRow _key value):_ -> return $ (decode . encodeUtf8) value


    listAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Read k, FromJSON v) => Sem r [(k, v)]
    listAction = do
      trace "listAction:"
      conn <- connectionFrom input
      rows <- embed (SQL.query_ conn "SELECT key, value FROM store" :: IO [KeyValueRow])
      let maybeList = map toKV rows
      return $ catNestedMaybe maybeList
        where
          toKV (KeyValueRow key value) =  ((read . T.unpack) key, (decode . encodeUtf8) value)
          catNestedMaybe [] = []
          catNestedMaybe ((key, Just value):xs) = (key, value):catNestedMaybe xs
          catNestedMaybe ((_  , Nothing):xs)    = catNestedMaybe xs


    insertAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, ToJSON v) => k -> v -> Sem r ()
    insertAction key value = do
      trace $ "insertAction: " ++ show key ++ " " ++ show (encode value)
      let (query, params) = ("INSERT INTO store (key, value) VALUES (:key, :value) "
                          <> "ON CONFLICT (key) DO UPDATE SET value = excluded.value",
                          [":key" := show key, ":value" := encodedValue])
                          where
                            encodedValue = (decodeUtf8 . encode) value
      conn <- connectionFrom input
      embed $ SQL.executeNamed conn query params


    deleteAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k) => k -> Sem r ()
    deleteAction key = do
      trace $ "deleteAction: " ++ show key
      conn <- connectionFrom input
      embed $ SQL.executeNamed conn "DELETE FROM store WHERE key = :key" [":key" := show key]


    -- | create a connection based on configuration data, make sure table "store" exists.
    connectionFrom :: (Member (Embed IO) r, Member Trace r) => Sem r Config -> Sem r SQL.Connection
    connectionFrom c = do
      config <- c
      trace $ "open connection to: " ++ dbPath config
      embed (getConnection (dbPath config))
        where
          getConnection :: FilePath -> IO SQL.Connection
          getConnection dbFile = do
            conn <- SQL.open dbFile
            SQL.execute_ conn "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
            return conn
