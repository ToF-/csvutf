{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import Data.Char
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL

data Commune = Commune { depcom :: String
                       , com :: String
                       , pmun :: Integer
                       , pcap :: Integer
                       , ptot :: Integer }
    deriving (Eq,Show)

instance FromNamedRecord Commune
    where
        parseNamedRecord v = Commune <$> v .: "DEPCOM"
                                     <*> v .: "COM"
                                     <*> v .: "PMUN"
                                     <*> v .: "PCAP"
                                     <*> v .: "PTOT"

myOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ';')
    }
main :: IO ()
main = do
    contenu <- BSL.readFile "data/Communes.csv"
    let result = decodeByNameWith myOptions contenu :: Either String (Header, V.Vector Commune)
    case result of
        Left msg -> putStrLn msg
        Right (_,communes) -> mapM_ putStrLn (V.map com communes)


