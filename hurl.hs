{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad hiding (forM_, forM)
import Control.Exception
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Encoding
import Data.Foldable (forM_)
import Data.Traversable (forM)
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
httpOptions = defaults & redirects .~ 10

-- Act depending on MIME type, doing nothing if we don't know how to handle it
dispatchByHeader :: String -> Response () -> IO (Maybe T.Text)
dispatchByHeader url response
  | contentTypeIs "text/html" = getHTMLTitle url
  | contentTypeIs "application/pdf" || contentTypeIs "application/x-pdf" =
    return $ Just "PDF is not supported yet."
  | otherwise = return Nothing
  where contentTypeIs = (`BS.isPrefixOf` (response ^. responseHeader "Content-Type"))

-- Find an URL in the message
getURL :: String -> Maybe String
getURL message = message =~~ urlRegex
  where urlRegex :: String
        urlRegex = "(https?://[^ ]*)"
        
-- Find the title in the body (if any)
getHTMLTitle :: String -> IO (Maybe T.Text)
getHTMLTitle url = (getTitle =<<) <$> requestMaybe (getWith httpOptions url)
  where getTitle response = do -- in the Maybe monad
          let tags = parseTags . decodeUtf8 $ response ^. responseBody
          -- that's what the fail method in Monad is used for!
          -- (but it should really be mzero from MonadPlus)
          -- this is NOT equivalent to 'let (_:tags') = dropWhile ...'
          (_:tags') <- return $ dropWhile (not . isTagOpenName "title") tags
          (TagText title:_) <- return $ dropWhile (not . isTagText) tags'
          return title

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
      TIO.putStrLn t
      hFlush stdout
