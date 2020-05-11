module InterfaceAdaptersSpec where

import           Data.Function                ((&))
import qualified Data.Map.Strict              as M
import           InterfacesAdapters.Config
import           InterfacesAdapters.KVSSqlite
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input               (Input, runInputConst)
import           Polysemy.Trace
import           Test.Hspec
import           UseCases.KVS

main :: IO ()
main = hspec spec

-- Testing the KVS SQLLite implementation

-- -- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: (forall r. Members [KeyValueTable, Input Config] r => Sem r a) -> IO a
runAllEffects program =
  program
    & runKvsAsSQLite
    & runInputConst config
    & ignoreTrace
    & runM
  where config = Config {port = 8080, dbPath = "kvs-test.db", backend = SQLite}

-- errors are rethrown as Runtime errors, which can be verified by HSpec.
handleErrors :: IO (Either err a) -> IO a
handleErrors e = do
  either <- e
  case either of
    Right v -> return v
    Left _  -> error "something bad happend"

-- | a key value table mapping Int to a list of Strings
type KeyValueTable = KVS Int [String]

data Memo = Memo Int [String]
    deriving (Show)

persistEntry :: (Member KeyValueTable r)  => Memo -> Sem r ()
persistEntry (Memo id lines ) = insertKvs id lines

fetchEntry :: (Member KeyValueTable r) => Int -> Sem r (Maybe [String])
fetchEntry = getKvs

fetchAll :: (Member KeyValueTable r) => Sem r (M.Map Int [String])
fetchAll = fmap M.fromList listAllKvs

deleteEntry :: (Member KeyValueTable r)  => Int -> Sem r ()
deleteEntry = deleteKvs

-- Helper functions for interpreting all effects in IO
runPersist :: Memo -> IO ()
runPersist memo = runAllEffects (persistEntry memo)

runFetch :: Int -> IO (Maybe [String])
runFetch k = runAllEffects (fetchEntry k)

runFetchAll :: IO (M.Map Int [String])
runFetchAll = runAllEffects fetchAll

runDelete :: Int -> IO ()
runDelete k = runAllEffects (deleteEntry k)

key = 4711
text = ["In the morning", "I don't drink coffee", "But lots of curcuma chai."]
memo = Memo key text

spec :: Spec
spec =
  describe "The KV Store SQLite Implementation" $ do
    it "returns Nothing if nothing can be found for a given id" $ do
      maybeMatch <- runFetch key
      maybeMatch `shouldBe` Nothing

    it "persists an key value pair to the SQLite database" $ do
      runPersist memo
      maybeMatch <- runFetch key
      maybeMatch `shouldBe` Just text

    it "fetches a Map of all key value entries from the KV store" $ do
      map <- runFetchAll
      M.size map `shouldBe` 1

    it "can delete an entry from the key value store" $ do
      runDelete key
      maybeMatch <- runFetch key
      maybeMatch `shouldBe` Nothing



