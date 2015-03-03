{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad hiding (forM_, forM)
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding as LE
import Data.Text.Encoding as SE
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Monoid
import Data.Maybe
import Data.List
import qualified Data.Word8 as W8
import Data.Char
import System.IO
import Text.Regex.TDFA
import System.Process (system)
import System.Exit (ExitCode(..))

import Control.Lens
-- import qualified Data.Text.ICU as ICU
import Network.HTTP.Client (HttpException)
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
dispatchByHeader :: String -> Response () -> IO (Maybe T.Text)
dispatchByHeader url response
  -- Just try UTF-8 then Latin-1. This is... not efficient.
  | contentTypeIs "text/html" = do
      html <- body
      return $ getHTMLTitle =<<
        first ([(safeDecodeUtf8 =<<), (LE.decodeLatin1 <$>)] <*> pure html)
  | contentTypeIs "application/pdf" || contentTypeIs "application/x-pdf" =
    (maybe (return Nothing) getPDFTitle) =<< body
  | otherwise = return Nothing
  where contentTypeIs = flip BS.isPrefixOf . normalize $ response ^. responseHeader "Content-Type"
        body = ((^. responseBody) <$>) <$> requestMaybe (getWith httpOptions url)
        normalize = BS.map W8.toLower . BS.filter (not . W8.isSpace)

-- Find the first Just element if it exists
first :: [Maybe a] -> Maybe a
first = listToMaybe . catMaybes

-- Find an URL in the message
getURL :: String -> Maybe String
getURL message = message =~~ urlRegex
  where urlRegex :: String
        urlRegex = "(https?://[^ ]*)"

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right x) = Just x

safeDecodeUtf8 :: BL.ByteString -> Maybe TL.Text
safeDecodeUtf8 = hush . LE.decodeUtf8'

-- Find the title in the body (if any)
getHTMLTitle :: TL.Text -> Maybe T.Text
getHTMLTitle body = do -- in the Maybe monad
  let tags = parseTags body
  -- that's what the fail method in Monad is used for!
  -- (but it should really be mzero from MonadPlus)
  -- this is NOT equivalent to 'let (_:tags') = dropWhile ...'
  (_:tags') <- return $ dropWhile (not . isTagOpenName "title") tags
  (TagText title:_) <- return $ dropWhile (not . isTagText) tags'
  return . TL.toStrict $ title

-- Misleading name!
-- When pdfinfo does not find a title, this actually reads
-- the whole first page of the pdf (with pdftotext)
getPDFTitle :: BL.ByteString -> IO (Maybe T.Text)
getPDFTitle body = do
  BL.writeFile "/run/ircbots/temp.pdf" body
  eitherInfo <- pdfInfo "/run/ircbots/temp.pdf"
  case eitherInfo of
    Right info -> case pdfInfoTitle info of
      Just txt -> return . Just $ txt
      Nothing -> do
        -- TODO: handle exceptions
        exitCode <- system "pdftotext -layout -f 1 -l 1 /run/ircbots/temp.pdf /run/ircbots/temp.txt"
        case exitCode of
          ExitFailure _ -> return Nothing
          ExitSuccess -> Just <$> TIO.readFile "/run/ircbots/temp.txt"
    Left err -> Nothing <$ logError (show err)

requestMaybe :: IO a -> IO (Maybe a)
requestMaybe req = handle handleException $ Just <$> req
  where handleException :: HttpException -> IO (Maybe a)
        handleException x = do
          logError (show x)
          return Nothing

-- Fetch the title of a given URL (if any…)
fetchTitle :: String -> IO (Maybe T.Text)
fetchTitle url = do
  -- Initially, only do HEAD, defer GET to when we know we want the document
  response <- requestMaybe $ headWith httpOptions url
  join <$> forM response (dispatchByHeader url)

main :: IO ()
main = forever $ do
  messageURL <- getURL <$> getLine
  forM_ messageURL $ \url -> do
    title <- fetchTitle url
    forM_ title $ \t -> do
      forM_ (
        take 3 .
        filter (not . T.null) .
        map (T.dropWhile isSpace) .
        T.lines $ t) $ TIO.putStrLn . ("/say " <>)
      hFlush stdout

