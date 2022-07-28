import System.IO
import Control.Applicative hiding (some)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import HL7.DataTypes
import HL7.Segments
import HL7.Parser

main :: IO ()
main = hspec $ do 
    describe "Patient message parsing" $ do 
        -- it "correctly parses a Create Patient Message (A04)" $ do 
        --     message <- readFile "./test/data/A04.txt"
        --     parse pMSH "" message `shouldParse` [[ST "MSH",ST "^~\\&",ST "MS4_AZ",ST "UNV",ST "PREMISE",ST "UNV",ST "20180301010000",ST "",ST "ADT^A04",ST "IHS-20180301010000.00120",ST "P",ST "2.1"],[ST "EVN",ST "A04",ST "20180301010000"],[ST "PID",ST "1",ST "",ST "19050114293307.1082",ST "",ST "BUNNY^BUGS^RABBIT^^^MS",ST "",ST "19830215",ST "M",ST "",ST "",ST "1234 LOONEY RD^APT A^CRAIGMONT^ID^83523^USA",ST "",ST "",ST "",ST "",ST "",ST "",ST "111-11-1111",ST "111-11-1111"],[ST "PV1",ST "1",ST "E",ST "ED^^^UNV",ST "C",ST "",ST "",ST "999-99-9999^MUNCHER^CHANDRA^ANDRIA^MD^DR",ST "888-88-8888^SMETHERS^ANNETTA^JONI^MD^DR",ST "",ST "",ST "",ST "",ST "",ST "7",ST "",ST "",ST "",ST "REF",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "",ST "20180301010000"],[ST ""]]

        it "correctly parses a minimal MSH segment" $ do 
            let message = "MSH|^~\&|MS4_AZ|UNV|PREMISE|UNV|20180301010000||ADT^A04|IHS-20180301010000.00120|P|2.1"
            parse pMSH "failed" message `shouldParse` 
                MSH { sendingApplication = Just (HD {namespaceId = "MS4_AZ", universalId = Nothing, universalIdType = Nothing})
                    , sendingFacility = Just (HD {namespaceId = "UNV", universalId = Nothing, universalIdType = Nothing})
                    , receivingApplication = Just (HD {namespaceId = "PREMISE", universalId = Nothing, universalIdType = Nothing})
                    , receivingFacility = Just (HD {namespaceId = "UNV", universalId = Nothing, universalIdType = Nothing})
                    , dtOfMessage = Just "20180301010000"
                    , security = Just ""
                    , messageType = ["ADT","A04"]
                    , messageControlId = "IHS-20180301010000.00120"
                    , processingId = ["P"]
                    , version = VID {versionId = "2.1", internationalizationCode = Nothing, internationalVersionId = Nothing}
                    , sequenceNumber = Nothing
                    , continuationPointer = Nothing
                    , acceptAcknowledgementType = Nothing
                    , applicationAcknowledgementType = Nothing
                    , countryCode = Nothing
                    , characterSet = Nothing
                    , principleLanguageOfMessage = Nothing
                    , alternateCharacterSetHandlingScheme = Nothing }
        
        it "parses HL7 Messages from a file" $ do 
            message <- readFile "./test/data/A04.hl7"
            parse parseHL7 "" message `shouldParse` 
                [ MessageHeader ( MSH { sendingApplication = Just (HD {namespaceId = "MS4_AZ", universalId = Nothing, universalIdType = Nothing})
                                      , sendingFacility = Just (HD {namespaceId = "UNV", universalId = Nothing, universalIdType = Nothing})
                                      , receivingApplication = Just (HD {namespaceId = "PREMISE", universalId = Nothing, universalIdType = Nothing})
                                      , receivingFacility = Just (HD {namespaceId = "UNV", universalId = Nothing, universalIdType = Nothing})
                                      , dtOfMessage = Just "20180301010000"
                                      , security = Just ""
                                      , messageType = ["ADT","A04"]
                                      , messageControlId = "IHS-20180301010000.00120"
                                      , processingId = ["P"]
                                      , version = VID {versionId = "2.1", internationalizationCode = Nothing, internationalVersionId = Nothing}
                                      , sequenceNumber = Nothing
                                      , continuationPointer = Nothing
                                      , acceptAcknowledgementType = Nothing
                                      , applicationAcknowledgementType = Nothing
                                      , countryCode = Nothing
                                      , characterSet = Nothing
                                      , principleLanguageOfMessage = Nothing
                                      , alternateCharacterSetHandlingScheme = Nothing })
                , Event ( EVN { eventTypeCode = "A04"
                              , recordedDateTime = "20180301010000"
                              , dateTimePlannedEvent = Nothing
                              , eventReasonCode = Nothing
                              , operatorId = Nothing
                              , eventOccurred = Nothing })
                , Unknown ["PID","1","","19050114293307.1082","","BUNNY^BUGS^RABBIT^^^MS","","19830215","M","","","1234 LOONEY RD^APT A^CRAIGMONT^ID^83523^USA","","","","","","","111-11-1111","111-11-1111"]
                , Unknown ["PV1","1","E","ED^^^UNV","C","","","999-99-9999^MUNCHER^CHANDRA^ANDRIA^MD^DR","888-88-8888^SMETHERS^ANNETTA^JONI^MD^DR","","","","","","7","","","","REF","","","","","","","","","","","","","","","","","","","","","","","","","","20180301010000"]]
