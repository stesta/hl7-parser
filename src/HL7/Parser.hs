module HL7.Parser where

import HL7.DataTypes
import HL7.Segments

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Segment 
    = MessageHeader MSH
    | Event EVN 
    | Unknown UNK
    deriving (Eq, Show)

type HL7Message = [Segment]

type Parser = Parsec Void String


pSegment :: Parser Segment
pSegment = choice 
    [ MessageHeader <$> pMSH
    , Event <$> pEVN
    , Unknown <$> pUNK ]


parseHL7 :: Parser HL7Message
parseHL7 = sepBy pSegment newline 
