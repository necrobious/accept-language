module Data.HTTP.AcceptLanguage where

import Data.Attoparsec.Text
import Data.Text (Text)
import Data.LanguageTag (LanguageTag) 
import Control.Applicative ((<$>))
import Data.LanguageTag.Parse (parseLanguageTag)
import Data.List (sortBy) 
import Data.Ord (Ordering(LT,EQ,GT)) 

parseAcceptLanguage :: Text -> [(Double,LanguageTag)]
parseAcceptLanguage input = 
  case parseOnly parseLanguages input of
    Left _   -> []
    Right ls -> ls 


parseLanguages :: Parser [(Double,LanguageTag)]
parseLanguages = reverse <$> (sortBy comparison) <$> (parseLang `sepBy` (skipSpace >> (char ',') >> skipSpace)) 

comparison :: (Double,LanguageTag) -> (Double,LanguageTag) -> Ordering
comparison (one,_) (two,_)
  | one  < two = LT
  | one  > two = GT
  | one == two = EQ

parseLang :: Parser (Double,LanguageTag)
parseLang = do
  ltag   <- skipSpace >> parseLanguageTag
  qvalue <- option (1.0) parseQValue
  return (qvalue,ltag)


parseQValue :: Parser Double
parseQValue = skipSpace >> (char ';') >> skipSpace >> (char 'q') >> skipSpace >> (char '=') >> skipSpace >> ((loosePrecision 3) <$> abs <$> double) 

loosePrecision :: Int -> Double -> Double
loosePrecision precision input = 
  let p = (10.0::Double) ^ precision
  in  (fromIntegral $ floor (input * p)) / p
--((fromIntegral (floor ((1.23456789::Double) * 1000)))::Double) / (1000.0::Double)
