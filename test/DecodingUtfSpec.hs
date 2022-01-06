{-# LANGUAGE OverloadedStrings #-}
module DecodingUtfSpec
    where

import Test.Hspec
import System.IO
import Data.Vector
import Data.Csv
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BS
import Prix

data A = A { a :: T.Text } deriving (Eq, Show)
instance FromNamedRecord A where parseNamedRecord v = A <$> v .: "Donnee"

data U = U { u :: T.Text } deriving (Eq, Show)
instance FromNamedRecord U where parseNamedRecord v = U <$> v .: TE.encodeUtf8 "Donnée"


data Achat = Achat {
    libelle :: T.Text,
    prixUnitaire :: Prix,
    quantite :: Integer,
    acheteur :: T.Text }
    deriving (Show, Eq)
instance FromNamedRecord Achat
    where parseNamedRecord v = Achat <$> v .: TE.encodeUtf8 "Description"
                                     <*> v .: TE.encodeUtf8 "Prix unitaire"
                                     <*> v .: TE.encodeUtf8 "Quantité"
                                     <*> v .: TE.encodeUtf8 "Pour qui ?"



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

        it "should process unicode including in several fields" $ do
            let writeContent = TLE.encodeUtf8 (T.unlines ["Description,Prix unitaire,Quantité,Prix,Pour qui ?,"
                                                         ,"Capsules pour Jetons 35mm,\"3,95€\",3,\"11,85€\",Icon"])
            BS.writeFile "contenu.txt" writeContent
            let result = decodeByName writeContent :: Either String (Header, Vector Achat)
            let expectedHeader = Data.Vector.map TE.encodeUtf8 $ fromList ["Description"
                                                                          ,"Prix unitaire"
                                                                          ,"Quantité"
                                                                          ,"Prix"
                                                                          ,"Pour qui ?"
                                                                          ,"" ] 
            (fst <$> result)  `shouldBe` Right expectedHeader
            let expectedData = fromList [Achat "Capsules pour Jetons 35mm" (Prix 3.95) 3 "Icon"]
            (snd <$> result)  `shouldBe` Right expectedData
        it "should process unicode including in field names" $ do
            BS.writeFile testStdFile (TLE.encodeUtf8 (T.unlines ["Donnée" ,"42€"]))
            content <- BS.readFile testStdFile
            BS.writeFile testUtf8File content

            -- sanity checks
            encoding <- withFile testUtf8File ReadMode hGetEncoding
            show <$> encoding `shouldBe` Just "UTF-8"

            readContent <- BS.readFile testUtf8File
            readContent `shouldBe` content 

            -- can you decode by name this content ?
            content <- BS.readFile testUtf8File
            let result = decodeByName content :: Either String (Header, Vector U)
            result `shouldBe`   Right (fromList [TE.encodeUtf8 "Donnée"],fromList [U {u = "42€"}])

            
