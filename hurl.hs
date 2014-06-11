{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad hiding (forM_)
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding
import Data.Foldable (forM_)
import Data.Maybe
import Data.List
import System.IO
import Text.Regex.TDFA

import Control.Lens
-- import qualified Data.Text.ICU as ICU
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Text.HTML.TagSoup

-- Error logging to stderr
logError :: String -> IO ()
logError str = hPutStr stderr (str ++ "\n")

-- Set maximum number of HTTP redirections
httpOptions :: Options
httpOptions = defaults & redirects .~ 5

-- Keep only the documents that have the correct MIME type
-- If it has the correct MIME type, return the body of the response
filterByHeader :: Response BL.ByteString -> Maybe BL.ByteString
filterByHeader response
  | "text/html" `BS.isPrefixOf` contentType = Just $ response ^. responseBody
  | otherwise = Nothing
  where contentType = response ^. responseHeader "Content-Type"

-- Find an URL in the message
getURL :: String -> Maybe String
getURL message = message =~~ urlRegex
  where urlRegex :: String
        urlRegex = "(https?://[^ ]*)"
        
-- Find the title in the body (if any)
getTitle :: BL.ByteString -> Maybe T.Text
getTitle bytes = do
  let tags = parseTags . decodeUtf8 $ bytes
  -- that's what the fail method in Monad is used for!
  -- (but it should really be mzero from MonadPlus)
  -- this is NOT equivalent to 'let (_:tags') = dropWhile ...'
  (_:tags') <- return $ dropWhile (not . isTagOpenName "title") tags
  (TagText title:_) <- return $ dropWhile (not . isTagText) tags'
  return title

-- Fetch the title of a given URL (if any…)
fetchTitle :: String -> IO (Maybe T.Text)
fetchTitle url = do
  response <- handle handleException $ Just <$> getWith httpOptions url
  return $ response >>= filterByHeader >>= getTitle
  where
    handleException :: HttpException -> IO (Maybe a)
    handleException x = do
      logError (show x)
      return Nothing

main :: IO ()
main = forever $ do
  messageURL <- getURL <$> getLine
  forM_ messageURL $ \url -> do
    title <- fetchTitle url
    forM_ title $ \t -> do
      TIO.putStrLn t
      hFlush stdout

