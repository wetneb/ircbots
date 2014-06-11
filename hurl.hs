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
import System.IO.Error
import Text.Printf
import Text.Regex.TDFA

import Control.Lens
-- import qualified Data.Text.ICU as ICU
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Text.HTML.TagSoup.Entity (lookupEntity)


-- unescapeEntities :: String -> String
-- unescapeEntities [] = []
-- unescapeEntities ('&':xs) = 
--   let (b, a) = break (== ';') xs in
--   case (lookupEntity b, a) of
--     (Just c, ';':as) ->  c  : unescapeEntities as    
--     _                -> '&' : unescapeEntities xs
-- unescapeEntities (x:xs) = x : unescapeEntities xs

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
getTitle :: BL.ByteString -> Maybe BL.ByteString
getTitle bytes = bytes =~~ titleRegex
  where titleRegex :: String
        titleRegex = "<title>(.*)</title>"
        

-- Fetch the title of a given URL (if any…)
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
      -- TODO: unescape entities
      TIO.putStrLn . decodeUtf8 $ t
      hFlush stdout

