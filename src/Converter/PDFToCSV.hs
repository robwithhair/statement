{-# LANGUAGE DataKinds #-}
module Converter.PDFToCSV
    ( go
    ) where

import System.Process
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.IO
import System.IO.Unsafe
import Shelly
import System.Environment
import GHC.Conc.Sync
import Data.Time.Calendar
import Data.Time.Format
import Data.Maybe
import qualified Money
import Data.Char
import Data.Csv
import qualified Data.ByteString.Lazy as L


default (T.Text)



nullString = ""
type PDFToText = FilePath



go :: IO ()
go = do
  maybePdfToText <- shelly $ which "pdftotext"
  case maybePdfToText of
    Nothing -> do
      hPutStrLn stderr "The executable pdftotext does not seem to be installed on the system path.  Could not find executable.  Please install pdftotext.  It is required for this application to run."
      die "Could not start without executable pdftotext"
    Just pdfToText -> do
      filesToProcess <- getArgs
      arrayOfLines <- processFiles pdfToText filesToProcess
      mapM_ (L.putStr . encode) arrayOfLines
      readTVarIO exitCode >>= exitWith

processFiles :: PDFToText -> [FilePath] -> IO [[[Text]]]
processFiles pdfToText filesToProcess = mapM (processFile pdfToText) filesToProcess

data Institution = BankOfScotland | Halifax | MBNA deriving Show

nameForInstitution :: Institution -> Text
nameForInstitution Halifax = "Halifax"
nameForInstitution BankOfScotland = "Bank of Scotland"
nameForInstitution MBNA = "MBNA"

fromDecimalForInstitution :: Institution -> Text -> Maybe (Money.Dense currency)
fromDecimalForInstitution _ t = Money.denseFromDecimal decimalConf t

decimalConf = Money.defaultDecimalConf{Money.decimalConf_separators=fromJust $ Money.mkSeparators '.' $ Just ','}

getInstitution :: PDFToText -> FilePath -> IO (Maybe Institution, Maybe Day)
getInstitution pdfToText file = do
  res <- shelly $ silently $ runFoldLines (Nothing, Nothing) f pdfToText [T.pack file, "-"]
  return res
  where f (Nothing, Nothing) "www.bankofscotland.co.uk" = (Just BankOfScotland, Nothing)
        f (Nothing, Nothing) "mbna.co.uk" = (Just MBNA, Nothing)
        f (Nothing, Nothing) "www.halifax.co.uk/online" = (Just Halifax, Nothing)
        f (Just i, Nothing) t = (Just i, parseDayFromStart i t)
        f out _ = out

parseDayFromStart :: Institution -> Text -> Maybe Day
parseDayFromStart i t = parseTimeM True defaultTimeLocale dateFormat $ T.unpack $ T.take (statementDateRightIndent i) t

statementDateRightIndent _ = 27

dateFormat = "%d %B %0Y"

exitCode :: TVar ExitCode
exitCode = unsafePerformIO $ newTVarIO ExitSuccess

partialFailure :: String -> IO ()
partialFailure errorMessage = do
  hPutStrLn stderr errorMessage
  atomically $ writeTVar exitCode $ ExitFailure 1

processFile :: PDFToText -> FilePath -> IO [[Text]]
processFile pdfToText file = do
  maybeInstitution <- getInstitution pdfToText file
  case maybeInstitution of
    (Nothing, Nothing) -> partialFailure ("Was unable to detect institution or statement date for file: " ++ file) >> return [[]]
    (Nothing, _) -> partialFailure ("Was unable to detect institution for file: " ++ file) >> return [[]]
    (_, Nothing) -> partialFailure ("Was unable to detect statementDate for file: " ++ file) >> return [[]]
    (Just institution, Just statementDate) -> do
      processFileForInstitution' institution statementDate
  where processFileForInstitution' = processFileForInstitution pdfToText file

processFileForInstitution :: PDFToText -> FilePath -> Institution -> Day -> IO [[Text]]
processFileForInstitution pdfToText file institution statementDate = do
  res <- shelly $ silently $ runFoldLines [] f pdfToText ["-layout", T.pack file, "-"]
  return res
  where f lastArray thisLine
          | valid = [ institutionName
                    , T.pack $ show statementDate
                    , fmtDate $ fromJust dateOfTransaction
                    , fmtDate $ fromJust dateEntered
                    , descriptionText
                    , fromJust amountSection
                    ] : lastArray
          | otherwise = lastArray
          where dateOfTransaction = getAsDate $ T.take dateOfTransactionChars thisLine
                dateEntered = getAsDate $ T.take dateEnteredChars $ T.drop dateOfTransactionChars thisLine
                descriptionText = T.strip $ T.take descriptionChars $ T.drop (dateOfTransactionChars + dateEnteredChars) thisLine
                amountSection = amountToMoney $ T.take amountChars $ T.drop (dateOfTransactionChars + dateEnteredChars + descriptionChars) thisLine
                valid = isJust dateOfTransaction && isJust dateEntered && isJust amountSection
        institutionName = nameForInstitution institution
        dateOfTransactionChars = dateOfTransactionRightLimit institution
        dateEnteredChars = dateEnteredRightLimit institution
        descriptionChars = descriptionRightLimit institution
        amountChars = amountRightLimit institution
        gregorianStatementDate = toGregorian statementDate
        yearString = let (y, _, _) = gregorianStatementDate in T.pack $ show y
        getAsDate :: Text -> Maybe Day
        getAsDate t | statementMonthIsJan = adjustYear <$> out
                    | otherwise = out
                    where out = parseTimeM False defaultTimeLocale dateFormat (T.unpack $ T.strip t `T.append` " " `T.append` yearString)
        adjustYear d | transactionDateIsDec = fromGregorian (y' - 1) m' d'
                     | otherwise = d
                     where (y', m', d') = toGregorian d
                           transactionDateIsDec = m' == 12
        statementMonthIsJan = m == 1
                            where (_, m, _) = gregorianStatementDate
        fmtDate d = T.pack $ show d
        amountToMoney :: T.Text -> Maybe Text
        amountToMoney t | isCredit = Money.denseToDecimal decimalConf Money.Round <$> amt
                        | otherwise = Money.denseToDecimal decimalConf Money.Round . negate <$> amt
          where moneyText = T.strip t
                isCredit = "CR" `T.isSuffixOf` moneyText
                amt :: Maybe (Money.Dense "GBP")
                amt = fromDecimalForInstitution institution $ T.filter (not . isSpace) $ T.filter (not . isLetter) moneyText

dateOfTransactionRightLimit Halifax = 29
dateOfTransactionRightLimit MBNA = 28
dateOfTransactionRightLimit BankOfScotland = 27

dateEnteredRightLimit Halifax = 20
dateEnteredRightLimit MBNA = 23
dateEnteredRightLimit BankOfScotland = 22

descriptionRightLimit Halifax = 65
descriptionRightLimit MBNA = 70
descriptionRightLimit BankOfScotland = 67

amountRightLimit Halifax = 35
amountRightLimit MBNA = 35
amountRightLimit BankOfScotland = 35

