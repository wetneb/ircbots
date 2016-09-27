-- hurl looks for URLs in IRC discussions and replies with their titles.
--
-- Usage:
--
-- > hurl HOST PORT NICK [CHANNEL...]
--
-- Example:
--
-- > hurl event.example.com 1337 hurlbot #forgefer
--
-- Also responds to /invite and private messages.

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8, decodeLatin1, decodeUtf8')
import Data.Text.Encoding.Error

import Data.Char
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.Word8 as W8
import System.Environment
import System.IO
import System.Process (system)
import System.Exit (ExitCode(..))

import Control.Lens
-- import qualified Data.Text.ICU as ICU
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.HTTP.Types.Status
import Network.IRC.Client
import Network.Wreq
import Text.HTML.TagSoup
import Text.PDF.Info

-- Error logging to stderr
logError :: String -> IO ()
logError str = hPutStr stderr (str ++ "\n")

-- Set maximum number of HTTP redirections
httpOptions :: Options
httpOptions = defaults & redirects .~ 10

-- Act depending on MIME type, doing nothing if we don't know how to handle it
--
-- May raise HttpException.
dispatchByHeader :: String -> Response () -> IO (Maybe T.Text)
dispatchByHeader url response
  -- Just try UTF-8 then Latin-1. This is... not efficient.
  | contentTypeIs "text/html" = do
      html <- body
      (fmap . fmap)
        (T.unwords . T.words)
        . first [decodeUtf8, decodeLatin1] $ \decode ->
          forceTitle . getHTMLTitle . decode $ html
  | contentTypeIs "application/pdf" || contentTypeIs "application/x-pdf" =
      (fmap . fmap)
        (T.intercalate " | " .
          take 3 .
          filter (not . T.null) .
          fmap (T.dropWhile isSpace) .
          T.lines)
        (getPDFTitle =<< body)
  | otherwise = return Nothing
  where contentTypeIs = flip BS.isPrefixOf . normalize $ response ^. responseHeader "Content-Type"
        body = (^. responseBody) <$> httpGet url
        normalize = BS.map W8.toLower . BS.filter (not . W8.isSpace)
        httpGet = getWith httpOptions

-- Evaluate the title while catching decoding exceptions
forceTitle :: Maybe T.Text -> IO (Maybe T.Text)
forceTitle t = do
  t' <- join <$> eval t
  join <$> for t' eval
  where
    eval t = catch (Just <$> evaluate t)
      (\case
        DecodeError _ _ -> return Nothing
        e -> throw e)

-- Stop evaluating after finding the first Just element if it exists
first :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
first [] _ = return Nothing
first (m : ms) f = f m >>= \case
  Nothing -> first ms f
  x -> return x

-- Find URLs in the message
getURL :: T.Text -> [T.Text]
getURL = T.words >=> T.tails >=> looksLikeURL
  where looksLikeURL :: T.Text -> [T.Text]
        looksLikeURL x = do
          guard ("http://" `T.isPrefixOf` x || "https://" `T.isPrefixOf` x)
          return x

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right x) = Just x

safeDecodeUtf8 :: BL.ByteString -> Maybe TL.Text
safeDecodeUtf8 = hush . decodeUtf8'

-- Find the title in the body (if any)
-- Might fail due to lazy decoding of an invalid ByteString,
-- the error can be caught by forceTitle
getHTMLTitle :: TL.Text -> Maybe T.Text
getHTMLTitle body = do -- in the Maybe monad
  let tags = canonicalizeTags . parseTags $ body
  -- that's what the fail method in Monad is used for!
  -- (but it should really be mzero from MonadPlus)
  -- this is NOT equivalent to 'let (_:tags') = dropWhile ...'
  (_:tags') <- return $ dropWhile (not . isTagOpenName "title") tags
  let titleFragments = takeWhile (not . isTagCloseName "title") tags'
  return . TL.toStrict . renderTags $ titleFragments

-- Misleading name!
-- When pdfinfo does not find a title, this actually reads
-- the whole first page of the pdf (with pdftotext)
getPDFTitle :: BL.ByteString -> IO (Maybe T.Text)
getPDFTitle body = do
  BL.writeFile tmpPDF body
  eitherInfo <- pdfInfo tmpPDF
  case eitherInfo of
    Right info -> case pdfInfoTitle info of
      Just txt | not (T.null txt) -> return . Just $ txt
      _ -> do
        -- TODO: handle exceptions
        exitCode <- system $ "pdftotext -layout -f 1 -l 1 " ++ tmpPDF
        case exitCode of
          ExitFailure _ -> return Nothing
          ExitSuccess -> Just <$> TIO.readFile tmpTXT
    Left err -> Nothing <$ logError (show err)
  where
    tmp = "/tmp/hurl"
    tmpPDF = tmp ++ ".pdf"
    tmpTXT = tmp ++ ".txt"

type HttpResult = Either HttpException

-- Catch HTTP errors
handleHttp :: IO a -> IO (HttpResult a)
handleHttp req = handle handleException $ Right <$> req
  where handleException :: HttpException -> IO (HttpResult a)
        handleException x = do
          logError (show x)
          return (Left x)

-- Execute the IO action if the error was 404 (Not Found), just fail otherwise.
tryOn404 :: HttpResult (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
tryOn404 (Right x) _ = return x
tryOn404 (Left (StatusCodeException (Status 404 _) _ _)) retry = retry
tryOn404 (Left _) _ = return Nothing

-- Fetch the title of a given URL (if any…)
--
-- May raise HttpException
fetchTitle :: String -> IO (Maybe T.Text)
fetchTitle url = do
  -- Initially, only do HEAD, defer GET to when we know we want the document
  dispatchByHeader url =<< httpHead url
  where httpHead = headWith httpOptions

-- Wrapper that catches HttpExceptions while trying fetchTitle
-- with at most two different URL candidates.
fetchAndRetry :: String -> IO (Maybe T.Text)
fetchAndRetry url = do
  t <- fetch url
  tryOn404 t $ retry
  where fetch = handleHttp . fetchTitle
        hush' = join . hush
        retry = logError "Retrying..." >> hush' <$> fetch (init url)

msgToText :: Either b T.Text -> Maybe T.Text
msgToText (Right r) = Just r
msgToText _ = Nothing

hurlHandler :: EventHandler s
hurlHandler = EventHandler
  { _description = "Detects URLs in messages and replies with their titles."
  , _matchType = EPrivmsg
  , _eventFunc = handle
  } where
    handle event@(Event _ _ (Privmsg _ msg)) =
      for_ (msgToText msg) $ \txt -> do
        let urls = getURL txt
        for_ urls $ \url -> do
          title <- liftIO . fetchAndRetry $ T.unpack url
          for_ title $ reply event . T.take 200
    handle _ = error "Should not happen"

commandHandler :: EventHandler s
commandHandler = EventHandler
  { _description = "Joins invited-to channels"
  , _matchType = EInvite
  , _eventFunc = handle
  } where
    handle (Event _ _ (Invite chan _)) = send (Join chan)
    handle _ = error "Should not happen"

hurlConf :: T.Text -> [T.Text] -> InstanceConfig s
hurlConf nick channels = conf
  { _channels = channels
  , _eventHandlers = handlers
  } where
    conf = defaultIRCConf nick
    handlers = hurlHandler : commandHandler : _eventHandlers conf

main :: IO ()
main = getArgs >>= \case
  host_ : port_ : nick_ : channels_ -> do
    let host = BS8.pack host_
        port = read port_
        nick = T.pack nick_
        channels = fmap T.pack channels_
    conn <- connect host port 1
    let conf = hurlConf nick channels
    start conn conf
  _ -> do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " HOST PORT NICK [CHANNEL...]"
