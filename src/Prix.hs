{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Prix where

import Data.Text                     as T
import Data.Text.Encoding
import Data.Csv
import Data.ByteString (ByteString)
import Data.ByteString.Char8         as BS
import Data.Fixed
import Data.Char

data Prix = Prix {
    valeur :: Centi }
    deriving (Show, Eq)

type Flottant = Double

instance Num Prix where

    (Prix v) + (Prix w) = Prix (v + w)

    (Prix v) * (Prix w) = Prix (v * w)

    fromInteger i = Prix (fromInteger i)


instance Fractional Prix where
    (Prix v) / (Prix w) = Prix (v / w)

instance FromField (Fixed E2)
    where parseField f = parseField f >>= parserCenti

parserCenti :: String -> Parser Centi
parserCenti s = case reads s :: [(Centi,String)] of
              []        -> fail $  "Error: expected price, got " ++ s
              ((v,_):_) -> return v

instance FromField Prix
    where parseField f = Prix
                       <$> parseField (BS.map virguleEnPoint (BS.filter chiffreSeulement f))

virguleEnPoint :: Char -> Char
virguleEnPoint ',' = '.'
virguleEnPoint  c  = c

pointEnVirgule :: Char -> Char
pointEnVirgule '.' = ','
pointEnVirgule  c  = c

chiffreSeulement :: Char -> Bool
chiffreSeulement ',' = True
chiffreSeulement c = isDigit c

instance ToField Prix
    where toField = (<> "€") . BS.map pointEnVirgule . encodeUtf8 . T.pack . show . valeur

flottant :: Prix -> Double
flottant (Prix (MkFixed v)) =  fromInteger v / 100

entierEnPrix :: Integer -> Prix
entierEnPrix = Prix . fromIntegral
