{-# LANGUAGE DuplicateRecordFields #-}

module HL7.Segments where

import HL7.DataTypes

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Unknown Segment (meant to be the catch-all)
type UNK = [String]

pUNK :: Parsec Void String UNK
pUNK = sepBy pAny (char fieldSeparator)
    where pAny = many (noneOf ['|', '\n'])

-- Message Header
data MSH = 
    MSH { sendingApplication :: Maybe HD
        , sendingFacility :: Maybe HD
        , receivingApplication :: Maybe HD
        , receivingFacility :: Maybe HD
        , dtOfMessage :: Maybe TS
        , security :: Maybe ST
        , messageType :: CM
        , messageControlId :: ST
        , processingId :: PT
        , version :: VID
        , sequenceNumber :: Maybe NM
        , continuationPointer :: Maybe ST
        , acceptAcknowledgementType :: Maybe ID
        , applicationAcknowledgementType :: Maybe ID
        , countryCode :: Maybe ID
        , characterSet :: Maybe ID
        , principleLanguageOfMessage :: Maybe CE
        , alternateCharacterSetHandlingScheme :: Maybe ID
        } deriving (Eq, Show)

pMSH :: Parsec Void String MSH
pMSH = do
    f00 <- string "MSH"   
    f01 <-                  char fieldSeparator *> pMetadata
    f02 <- optional       $ char fieldSeparator *> pHD 
    f03 <- optional       $ char fieldSeparator *> pHD 
    f04 <- optional       $ char fieldSeparator *> pHD 
    f05 <- optional       $ char fieldSeparator *> pHD 
    f06 <- optional       $ char fieldSeparator *> pTS 
    f07 <- optional       $ char fieldSeparator *> pST 
    f08 <-                  char fieldSeparator *> pCM            
    f09 <-                  char fieldSeparator *> pST            
    f10 <-                  char fieldSeparator *> pPT            
    f11 <-                  char fieldSeparator *> pVID           
    f12 <- optional . try $ char fieldSeparator *> pNM
    f13 <- optional . try $ char fieldSeparator *> pST
    f14 <- optional . try $ char fieldSeparator *> pID
    f15 <- optional . try $ char fieldSeparator *> pID
    f16 <- optional . try $ char fieldSeparator *> pID
    f17 <- optional . try $ char fieldSeparator *> pID
    f18 <- optional . try $ char fieldSeparator *> pCE
    f19 <- optional . try $ char fieldSeparator *> pID

    pure MSH
        { sendingApplication = f02
        , sendingFacility = f03
        , receivingApplication = f04
        , receivingFacility = f05
        , dtOfMessage = f06
        , security = f07
        , messageType = f08
        , messageControlId = f09
        , processingId = f10
        , version = f11
        , sequenceNumber = f12
        , continuationPointer = f13
        , acceptAcknowledgementType = f14
        , applicationAcknowledgementType = f15
        , countryCode = f16
        , characterSet = f17 
        , principleLanguageOfMessage = f18
        , alternateCharacterSetHandlingScheme = f19
        }

-- Event 
data EVN = 
    EVN { eventTypeCode :: ID
        , recordedDateTime :: TS
        , dateTimePlannedEvent :: Maybe TS
        , eventReasonCode :: Maybe IS
        , operatorId :: Maybe XCN
        , eventOccurred :: Maybe TS
        } deriving (Eq, Show)

pEVN :: Parsec Void String EVN
pEVN = do
    f00 <- string "EVN"   
    f01 <-            char fieldSeparator *> pID
    f02 <-            char fieldSeparator *> pTS 
    f03 <- optional $ char fieldSeparator *> pTS 
    f04 <- optional $ char fieldSeparator *> pIS 
    f05 <- optional $ char fieldSeparator *> pXCN 
    f06 <- optional $ char fieldSeparator *> pTS

    pure EVN { eventTypeCode = f01
             , recordedDateTime = f02
             , dateTimePlannedEvent = f03
             , eventReasonCode = f04
             , operatorId = f05
             , eventOccurred = f06 }

-- Patient Identification
-- data PID = 
--     PID { setId :: Maybe SI
--         , patientId :: Maybe CX 
--         , patientIdentifierList :: CX 
--         , alternatePatientId :: Maybe CX 
--         , patientName :: XPN 
--         , mothersMaidenName :: Maybe XPN
--         , dateTimeOfBirth :: Maybe TS
--         , sex :: Maybe IS
--         , patientAlias :: Maybe XPN 
--         , race :: Maybe CE
--         , patientAddress :: Maybe XAD
--         , countryCode :: Maybe IS
--         , phoneNumberHome :: Maybe XTN
--         , phoneNumberBusiness :: Maybe XTN
--         , primaryLanguage :: Maybe CE 
--         , maritalStatus :: Maybe CE
--         , religion :: Maybe CE 
--         , patientAccountNumber :: Maybe CX 
--         , socialSecurityNumber :: Maybe ST
--         , driversLicenseNumber :: Maybe DLN
--         , mothersIdentifier :: Maybe CX
--         , ethnicGroup :: Maybe CE 
--         , birthPlace :: Maybe ST
--         , multipleBirthIndicator :: Maybe ID
--         , birthOrder :: Maybe NM
--         , citizenship :: Maybe CE
--         , veteransMilitaryStatus :: Maybe CE
--         , nationality :: Maybe CE 
--         , patientDeathDateAndTimem :: Maybe TS
--         , patientDeathIndicator :: Maybe ID
--         } deriving (Eq, Show)

-- pPID :: Parsec Void String PID
-- pPID = do 
--     f00 <- string "PID"   
--     f01 <- optional         char fieldSeparator *> pSI
--     f02 <- optional       $ char fieldSeparator *> pCX 
--     f03 <-                  char fieldSeparator *> pCX 
--     f04 <- optional       $ char fieldSeparator *> pCX 
--     f05 <-                  char fieldSeparator *> pXPN 
--     f06 <- optional       $ char fieldSeparator *> pXPN 
--     f07 <- optional       $ char fieldSeparator *> pTS 
--     f08 <- optional         char fieldSeparator *> pIS            
--     f09 <- optional         char fieldSeparator *> pXPN            
--     f10 <-                  char fieldSeparator *> pPT            
--     f11 <-                  char fieldSeparator *> pVID           
--     f12 <- optional . try $ char fieldSeparator *> pNM
--     f13 <- optional . try $ char fieldSeparator *> pST
--     f14 <- optional . try $ char fieldSeparator *> pID
--     f15 <- optional . try $ char fieldSeparator *> pID
--     f16 <- optional . try $ char fieldSeparator *> pID
--     f17 <- optional . try $ char fieldSeparator *> pID
--     f18 <- optional . try $ char fieldSeparator *> pCE
--     f19 <- optional . try $ char fieldSeparator *> pID

--     pure PID { setId = f01
--              , patientId = f02
--              , patientIdentifierList = f03
--              , alternatePatientId = f04
--              , patientName = f05
--              , mothersMaidenName = f06
--              , dateTimeOfBirth = f07
--              , sex = f08
--              , patientAlias = f09
--              , race = f10 }