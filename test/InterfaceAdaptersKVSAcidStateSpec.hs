module InterfaceAdaptersKVSAcidStateSpec where

import           Data.Function                ((&))
import qualified Data.Map.Strict              as M
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSAcidState
import           Polysemy
import           Polysemy.Input               (Input, runInputConst)
import           Polysemy.Trace
import           Test.Hspec
import           UseCases.KVS
import           System.Directory (removeDirectoryRecursive)
import           Control.Monad.Cont (liftIO)
import           Data.List                        (isSuffixOf)
import Control.Exception

main :: IO ()
main = hspec spec

-- Testing the KVS AcidState implementation

-- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: Sem '[KeyValueTable, Input Config, Trace, Embed IO] a -> IO a 
runAllEffects program =
  program
    & runKvsAsAcidState    -- use AcidState based interpretation of the (KVS Int [String]) effect
    & runInputConst config -- use the variable config as source for (Input Config) effect
    & ignoreTrace          -- ignore all traces
    & runM                 -- reduce Sem r (Embed IO a) to IO a
  where config = Config {port = 8080, dbPath = "kvs-test.db", backend = SQLite, verbose = False}

-- errors are rethrown as Runtime errors, which can be verified by HSpec.
handleErrors :: IO (Either err a) -> IO a
handleErrors e = do
  eitherError <- e
  case eitherError of
    Right v -> return v
    Left _  -> error "something bad happend"

-- | a key value table mapping Int to a list of Strings
type KeyValueTable = KVS Int [String]

data Memo = Memo Int [String]
    deriving (Show)

persistMemo :: (Member KeyValueTable r)  => Memo -> Sem r ()
persistMemo (Memo k val ) = insertKvs k val

fetchMemo :: (Member KeyValueTable r) => Int -> Sem r (Maybe [String])
fetchMemo = getKvs

fetchAll :: (Member KeyValueTable r) => Sem r (M.Map Int [String])
fetchAll = fmap M.fromList listAllKvs

deleteMemo :: (Member KeyValueTable r)  => Int -> Sem r ()
deleteMemo = deleteKvs

-- Helper functions for interpreting all effects in IO
runPersist :: Memo -> IO ()
runPersist val = runAllEffects (persistMemo val)

runFetch :: Int -> IO (Maybe [String])
runFetch k = runAllEffects (fetchMemo k)

runFetchAll :: IO (M.Map Int [String])
runFetchAll = runAllEffects fetchAll

runDelete :: Int -> IO ()
runDelete k = runAllEffects (deleteMemo k)

key :: Int
key = 4711

v :: [String]
v = ["In the morning", "I don't drink coffee", "But lots of curcuma chai."]

memo :: Memo
memo = Memo key v

-- | helper function to clean the test data files
--deleteAllFiles :: IO [()]
deleteAllFiles =
  catch (removeDirectoryRecursive "state") handler
    where 
      handler :: SomeException -> IO ()
      handler ex = putStrLn $ "caught exception " ++ show ex


spec :: Spec
spec = do
  describe "The KV AcidState Implementation" $ do
    it "needs a file cleaning for repeatable tests in the file system..." $ do
      liftIO deleteAllFiles
      True `shouldBe` True
    it "returns Nothing if nothing can be found for a given id" $ do
      maybeMatch <- runFetch key
      maybeMatch `shouldBe` Nothing

    it "persists a key-value pair to the AcidState database" $ do
      runPersist memo
      maybeMatch <- runFetch key
      maybeMatch `shouldBe` Just v

--    it "fetches a Map of all key-value entries from the KV store" $ do
--      map <- runFetchAll
--      M.size map `shouldBe` 1
--
--    it "deletes an entry from the key value store" $ do
--      runDelete key
--      maybeMatch <- runFetch key
--      maybeMatch `shouldBe` Nothing



