{-# LANGUAGE DataKinds #-}
module Converter.PDFToCSV
    ( go
    ) where

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
import Control.Monad

default (T.Text)



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



type StatementDateWindow = (XCoord, YCoord, Width, Height)
type XCoord = Int
type YCoord = Int
type Height = Int
type Width = Int

pdfToTextWindowOptions :: StatementDateWindow -> [Text]
pdfToTextWindowOptions (x, y, w, h) = map T.pack [ "-x", show x
                                      , "-y", show y
                                      , "-W", show w
                                      , "-H", show h
                                      ]


statementDateWindow :: Institution -> Maybe StatementDateWindow
statementDateWindow MBNA = Just (450, 150, 200, 400)
statementDateWindow BankOfScotland = Nothing
statementDateWindow Halifax = Nothing

transactionsWindowArgs :: Institution -> Maybe StatementDateWindow
transactionsWindowArgs MBNA = Just (20, 10, 450, 700)
transactionsWindowArgs BankOfScotland = Nothing
transactionsWindowArgs Halifax = Nothing

fromDecimalForInstitution :: Institution -> Text -> Maybe (Money.Dense currency)
fromDecimalForInstitution _ t = Money.denseFromDecimal decimalConf t

decimalConf :: Money.DecimalConf
decimalConf = Money.defaultDecimalConf{Money.decimalConf_separators=fromJust $ Money.mkSeparators '.' $ Just ','}

getInstitution :: PDFToText -> FilePath -> Bool  -> IO (Maybe Institution, Maybe Day)
getInstitution pdfToText file tryContains = do
  res <- shelly $ silently $ runFoldLines (Nothing, Nothing) functionToUse pdfToText [T.pack file, "-"]
  case (res, tryContains) of
    ((Just _, Just _), _) -> return res
    ((Just institution, Nothing), _) -> do
      let institutionWindowArgs = pdfToTextWindowOptions <$> statementDateWindow institution
      let h args = shelly $ silently $ runFoldLines (Just institution, Nothing) f pdfToText $  args ++ ["-layout", T.pack file, "-"]
      maybe (return res) h institutionWindowArgs
    ((Nothing, Nothing), False) -> getInstitution pdfToText file True
    _ -> return res
  where f (Nothing, Nothing) "www.bankofscotland.co.uk" = (Just BankOfScotland, Nothing)
        f (Nothing, Nothing) "mbna.co.uk" = (Just MBNA, Nothing)
        f (Nothing, Nothing) "www.halifax.co.uk/online" = (Just Halifax, Nothing)
        f (Just i, Nothing) t = (Just i, parseDayFromStart i t `mplus` parseDayFromStartAltFormat i t )
        f out _ = out
        g (Nothing, Nothing) t | canFind "www.bankofscotland.co.uk" t = (Just BankOfScotland, Nothing)
                               | canFind "mbna.co.uk" t = (Just MBNA, Nothing)
                               | canFind "www.halifax.co.uk/online" t = (Just Halifax, Nothing)
                               | otherwise = (Nothing, Nothing)
        g out _ = out
        functionToUse = if tryContains then g else f
        canFind needle = not . T.null . snd . T.breakOn needle

parseDayFromStart :: Institution -> Text -> Maybe Day
parseDayFromStart i t = parseDay i dateFormat t

type DateFormatter = String

parseDay :: Institution -> DateFormatter -> Text -> Maybe Day
parseDay i f t = parseTimeM True defaultTimeLocale f $ T.unpack $ T.take (statementDateRightIndent i) t

parseDayFromStartAltFormat :: Institution -> Text -> Maybe Day
parseDayFromStartAltFormat i t = parseDay i altDateFormat t

statementDateRightIndent :: Institution -> Int
statementDateRightIndent _ = 27

dateFormat :: DateFormatter
dateFormat = "%d %B %0Y"

altDateFormat :: DateFormatter
altDateFormat = "%d %b %y"

exitCode :: TVar ExitCode
exitCode = unsafePerformIO $ newTVarIO ExitSuccess

partialFailure :: Maybe Int -> String -> IO ()
partialFailure newExit errorMessage = do
  hPutStrLn stderr errorMessage
  case newExit of
    Nothing -> return ()
    Just exitInt -> atomically $ do
            lastValue <- readTVar exitCode
            writeTVar exitCode $ case lastValue of
              ExitSuccess                 -> ExitFailure exitInt
              ExitFailure x | x < exitInt -> ExitFailure x
              _                           -> ExitFailure exitInt

processFile :: PDFToText -> FilePath -> IO [[Text]]
processFile pdfToText file = do
  maybeInstitution <- getInstitution pdfToText file False
  case maybeInstitution of
    (Nothing, Nothing) -> possibleError ("Was unable to detect institution or statement date for file: " ++ file) >> return [[]]
    (Nothing, _) -> possibleError ("Was unable to detect institution for file: " ++ file) >> return [[]]
    (_, Nothing) -> possibleError ("Was unable to detect statementDate for file: " ++ file) >> return [[]]
    (Just institution, Just statementDate) -> do
      res <- processFileForInstitution' institution statementDate
      when (null res) $ possibleWarning ("Decoded zero transactions for statement file: " ++ file)
      return res
  where processFileForInstitution' = processFileForInstitution pdfToText file
        possibleError = partialFailure (Just 1) . (++) "Error: "
        possibleWarning = partialFailure Nothing . (++) "Warning: "

processFileForInstitution :: PDFToText -> FilePath -> Institution -> Day -> IO [[Text]]
processFileForInstitution pdfToText file institution statementDate = do
  res <- shelly $ silently $ runFoldLines [] f pdfToText args
  let institutionWindowArgs = pdfToTextWindowOptions <$> transactionsWindowArgs institution
  case (res, institutionWindowArgs) of
      ([], Just newArgs) -> shelly $ silently $ runFoldLines [] f pdfToText $ newArgs ++ args
      (_, _) -> return res
  where args = ["-layout", T.pack file, "-"]
        f lastArray thisLine
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
        dateOfTransactionChars = dateOfTransactionRightLimit institution statementDate
        dateEnteredChars = dateEnteredRightLimit institution statementDate
        descriptionChars = descriptionRightLimit institution statementDate
        amountChars = amountRightLimit institution statementDate
        gregorianStatementDate = toGregorian statementDate
        yearString = let (y, _, _) = gregorianStatementDate in T.pack $ show y
        getAsDate :: Text -> Maybe Day
        getAsDate t | isJust out && statementMonthIsJan = adjustYear <$> out
                    | otherwise = out `mplus` outAlt
                    where out = parseTimeM False defaultTimeLocale dateFormat (T.unpack $ T.strip t `T.append` " " `T.append` yearString)
                          outAlt = parseTimeM False defaultTimeLocale altDateFormat (T.unpack $ T.strip t)
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
          where moneyText | startsWithDot = T.cons '0' stripped
                          | otherwise = stripped
                          where stripped = T.strip t
                                startsWithDot = "." `T.isPrefixOf` stripped
                isCredit = "CR" `T.isSuffixOf` T.toUpper moneyText
                amt :: Maybe (Money.Dense "GBP")
                amt = fromDecimalForInstitution institution $ T.filter (not . isSpace) $ T.filter (not . isLetter) moneyText

dateOfTransactionRightLimit :: Institution -> Day -> Int
dateOfTransactionRightLimit Halifax _ = 29
dateOfTransactionRightLimit MBNA d
  | afterMBNAStatementRebrand d = 28
  | otherwise = 25
dateOfTransactionRightLimit BankOfScotland _ = 27

dateEnteredRightLimit :: Institution -> Day -> Int
dateEnteredRightLimit Halifax _ = 20
dateEnteredRightLimit MBNA d
  | afterMBNAStatementRebrand d = 23
  | otherwise = 15
dateEnteredRightLimit BankOfScotland _ = 22

descriptionRightLimit :: Institution -> Day -> Int
descriptionRightLimit Halifax _ = 65
descriptionRightLimit MBNA d
  | afterMBNAStatementRebrand d = 70
  | otherwise = 55
descriptionRightLimit BankOfScotland _ = 67

amountRightLimit :: Institution -> Day -> Int
amountRightLimit Halifax _ = 35
amountRightLimit MBNA statementDate
  | afterMBNAStatementRebrand statementDate = 35
  | otherwise = 39
amountRightLimit BankOfScotland _ = 35

afterMBNAStatementRebrand :: Day -> Bool
afterMBNAStatementRebrand d = fromGregorian 2019 3 1 < d


