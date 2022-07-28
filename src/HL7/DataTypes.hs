{-# LANGUAGE DuplicateRecordFields #-}

module HL7.DataTypes where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

segmentTerminator = '\n'
fieldSeparator = '|'
componentSeparator = '^'
subcomponentSeparator = '&'
repetitionSeparator = '~'
reserved = [segmentTerminator, fieldSeparator, componentSeparator, subcomponentSeparator, repetitionSeparator]
escapeChar = '\\'

pMetadata :: Parsec Void String String
{-# INLINE pMetadata #-}
pMetadata = takeWhileP (Just "") (/='|')

pNonReserved :: Parsec Void String String
{-# INLINE pNonReserved #-}
pNonReserved = takeWhileP (Just "") (`notElem` reserved)

sc :: Parsec Void String ()
{-# INLINE sc #-}
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)


type CM = [String]

pCM :: Parsec Void String CM
{-# INLINE pCM #-}
pCM = sepBy pNonReserved (char componentSeparator)

-- Coded Element 
data CE = CE { identifier :: ST
             , text :: Maybe ST
             , nameOfCodingSystem :: Maybe ST
             , alternateIdentifier :: Maybe ST
             , alternateText :: Maybe ST
             , nameOfAlternateCodingSystem :: Maybe ST
             } deriving (Eq, Show)

pCE :: Parsec Void String CE
{-# INLINE pCE #-}
pCE = do
    f01 <- pST
    f02 <- optional . try $ char componentSeparator *> pST
    f03 <- optional . try $ char componentSeparator *> pST
    f04 <- optional . try $ char componentSeparator *> pST
    f05 <- optional . try $ char componentSeparator *> pST
    f06 <- optional . try $ char componentSeparator *> pST

    pure CE { identifier = f01
            , text = f02
            , nameOfCodingSystem = f03
            , alternateIdentifier = f04
            , alternateText = f05
            , nameOfAlternateCodingSystem = f06 }

-- Extended Composite ID 
data CX = 
    CX { identifier :: ST 
       , checkDigit :: Maybe ST  }

-- Hierarchical Data 
data HD = HD { namespaceId :: IS
             , universalId :: Maybe ST
             , universalIdType :: Maybe ID
             } deriving (Eq, Show)

pHD :: Parsec Void String HD
{-# INLINE pHD #-}
pHD = do
    f01 <- pIS
    f02 <- optional . try $ char componentSeparator *> pST
    f03 <- optional . try $ char componentSeparator *> pID

    pure HD { namespaceId = f01
            , universalId = f02
            , universalIdType = f03 }

type ID = String

pID :: Parsec Void String ID
{-# INLINE pID #-}
pID = pNonReserved

type IS = String

pIS :: Parsec Void String IS
{-# INLINE pIS #-}
pIS = pNonReserved

type NM = String

pNM :: Parsec Void String NM
{-# INLINE pNM #-}
pNM = pNonReserved

-- Processing Type 
type PT = [ID]

pPT :: Parsec Void String PT
{-# INLINE pPT #-}
pPT = sepBy pID (char componentSeparator)

-- Sequence ID 
type SI = String

pSI :: Parsec Void String SI
pSI = pNonReserved

-- String Data
type ST = String

pST :: Parsec Void String ST
{-# INLINE pST #-}
pST = pNonReserved

-- Time Stamp
type TS = String

pTS :: Parsec Void String ST
{-# INLINE pTS #-}
pTS = pNonReserved

-- Version ID
data VID = VID { versionId :: ID
               , internationalizationCode :: Maybe CE
               , internationalVersionId :: Maybe CE
               } deriving (Eq, Show)

pVID :: Parsec Void String VID
{-# INLINE pVID #-}
pVID = do
    f01 <- pID
    f02 <- optional . try $ char componentSeparator *> pCE
    f03 <- optional . try $ char componentSeparator *> pCE

    pure VID { versionId = f01
            , internationalizationCode = f02
            , internationalVersionId = f03 }


-- Extended Address
data XAD = 
    XAD { streetAddress :: Maybe ST
        , otherDesignation :: Maybe ST 
        , city :: Maybe ST
        , stateOrProvince :: Maybe ST
        , zipOrPostalCode :: Maybe ST
        , country :: Maybe ID 
        , addressType :: Maybe ID 
        , otherGeographicDesignation :: Maybe ST
        , countyOrParishCode :: Maybe IS 
        , censusTract :: Maybe IS
        , addressRepresentationCode :: Maybe ID 
        } deriving (Eq, Show)

pXAD :: Parsec Void String XAD
pXAD = do 
    f01 <- optional       $ pST 
    f02 <- optional . try $ pST
    f03 <- optional . try $ pST
    f04 <- optional . try $ pST
    f05 <- optional . try $ pST
    f06 <- optional . try $ pID
    f07 <- optional . try $ pID
    f08 <- optional . try $ pST
    f09 <- optional . try $ pIS
    f10 <- optional . try $ pIS
    f11 <- optional . try $ pID
    
    pure XAD { streetAddress = f01
             , otherDesignation = f02 
             , city = f03
             , stateOrProvince = f04
             , zipOrPostalCode = f05
             , country = f06 
             , addressType = f07
             , otherGeographicDesignation = f08
             , countyOrParishCode = f09
             , censusTract = f10
             , addressRepresentationCode = f11 }


type XCN = String

pXCN :: Parsec Void String XCN
pXCN = pNonReserved
