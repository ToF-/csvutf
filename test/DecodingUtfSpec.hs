{-# LANGUAGE OverloadedStrings #-}
module DecodingUtfSpec
    where

import Test.Hspec
import System.IO
import Data.Vector
import Data.Csv
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding as TE
import Data.ByteString.Lazy as BS

data A = A { a :: Text } deriving (Eq, Show)
instance FromNamedRecord A where parseNamedRecord v = A <$> v .: "Donnee"

data U = U { u :: Text } deriving (Eq, Show)
instance FromNamedRecord U where parseNamedRecord v = U <$> v .: TE.encodeUtf8 "Donnée"

testUtf8File = "test/myTestUtf8File.csv"
testStdFile = "test/myTestStdFile.csv"

spec :: SpecWith ()
spec = do
    describe "decoding utf8 csv data" $ do
        it "should process unicode in field data" $ do
            BS.writeFile testStdFile (TLE.encodeUtf8 (T.unlines ["Donnee" ,"42€"]))
            content <- BS.readFile testStdFile
            let result = decodeByName content :: Either String (Header, Vector A)
            result `shouldBe`   Right (fromList ["Donnee"],fromList [A {a = "42€"}])

        it "should process unicode including in field names" $ do
            let writeContent = TLE.encodeUtf8 (T.unlines ["Donnée" ,"42€"])
            BS.writeFile testUtf8File writeContent

            -- sanity checks
            encoding <- withFile testUtf8File ReadMode hGetEncoding
            show <$> encoding `shouldBe` Just "UTF-8"

            readContent <- BS.readFile testUtf8File
            readContent `shouldBe` writeContent 

            -- can you decode by name this content ?
            content <- BS.readFile testUtf8File
            let result = decodeByName content :: Either String (Header, Vector U)
            result `shouldBe`   Right (fromList [TE.encodeUtf8 "Donnée"],fromList [U {u = "42€"}])

            
