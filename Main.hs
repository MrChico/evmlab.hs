{-# LANGUAGE OverloadedStrings #-}
module Evmlab where

import System.Process
import System.IO
import Data.ByteString (ByteString)
import Test.Tasty
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy
import Data.Text                  (Text)
import Data.Aeson (FromJSON (..), (.:))
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

newtype Bytes = Bytes ByteString
  deriving Eq

instance Show Bytes where
  showsPrec _ (Bytes x) _ = show (BS.unpack x)

instance Arbitrary Bytes where
  arbitrary = fmap (Bytes . BS.pack) arbitrary

newtype Bytes213 = Bytes213 ByteString
  deriving Eq

instance Show Bytes213 where
  showsPrec _ (Bytes213 x) _ = show (BS.unpack x)

instance Arbitrary Bytes213 where
  arbitrary = fmap (Bytes213 . BS.pack) (vectorOf 213 arbitrary)


newtype ByteStringS = ByteStringS ByteString

instance Show ByteStringS where
  show (ByteStringS x) = Text.unpack . fromBinary $ x
    where
      fromBinary =
        Text.decodeUtf8 . toStrict . toLazyByteString . byteStringHex

data Output = Output {
  _out :: Text
  } deriving Show

instance FromJSON Output where
  parseJSON (JSON.Object p) = do
    outp <-  p .: "output"
    return $ Output outp
  parseJSON invalid = JSON.typeMismatch "no output field" invalid

gethPath, parityPath :: FilePath
gethPath = "/Users/martinlundfall/go/src/github.com/ethereum/go-ethereum/build/bin/evm"
parityPath = "/Users/martinlundfall/parity-ethereum/target/debug/parity-evm"

gethArgs :: ByteString -> [String]
gethArgs bs = ["--code", (show $ ByteStringS bs), "run"]

parityArgs :: ByteString -> [String]
parityArgs bs = ["stats", "--code", (show $ ByteStringS bs), "--chain", "/Users/martinlundfall/efWork/customgenesisparity.json"]

b2bCallGeth, b2bCallParity :: Bytes213 -> [String]
b2bCallGeth (Bytes213 bs) = ["--input", (show $ ByteStringS bs), "--receiver", "0000000000000000000000000000000000000009", "--json", "run"]
b2bCallParity (Bytes213 bs) = ["stats", "--input", (show $ ByteStringS bs), "--chain", "/Users/martinlundfall/efWork/customgenesisparity.json", "--to", "0000000000000000000000000000000000000009", "--json"]

parseOutput :: Lazy.ByteString -> Either String Output
parseOutput = JSON.eitherDecode'

main :: IO ()
main = do gethOut <- readProcess gethPath (gethArgs $ BS.pack [0]) []
          parityOut <- readProcess parityPath (parityArgs $ BS.pack [0]) []
          putStrLn $ "geth output:" <> gethOut
          putStrLn $ "parity output:" <> parityOut

blake2bTest :: IO ()
blake2bTest = defaultMain $ testGroup "blake2b"
  [ testProperty "blake2b geth <-> parity" $ \(Bytes213 v) ->
      withMaxSuccess 100000 $
      monadicIO $ do
        gethOut <- run $ readProcess gethPath (b2bCallGeth (Bytes213 v)) []
        same <- run $ bothOut v
        assert same

        -- parityOut <- run $ readProcess parityPath (b2bCallParity (Bytes213 v)) []
        -- case (parseOutput $ Char8.pack $ gethOut, parseOutput $ Char8.pack $  parityOut) of
        --    (Left er1, Left er2) -> run $ putStrLn $ er1 <> er2
        --    (Left er, Right outp) -> run $ putStrLn $ "error; geth no output: " ++ gethOut
        --                            ++ "but parity outputs: " ++ (show $ outp)
        --    (Right outp, Left er) -> run $ putStrLn $ "error; geth output: " ++ (show $ outp)
        --                            ++ "but parity no output: " ++ parityOut
        --    (Right (Output gethRes), Right (Output parityRes)) -> assert (gethRes == parityRes)
  ]


bothOut :: ByteString -> IO Bool
bothOut bs = do
  gethOut <- readProcess gethPath (b2bCallGeth (Bytes213 bs)) []
  parityOut <- readProcess gethPath (b2bCallGeth (Bytes213 bs)) []
  case (parseOutput $ Char8.pack $ gethOut, parseOutput $ Char8.pack $ parityOut) of  
       (Right (Output gethRes), Right (Output parityRes)) -> return (gethRes == parityRes)
       _ -> error "idk why"
