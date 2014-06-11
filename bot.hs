import Codec.Binary.UTF8.String
import Network.HTTP.Headers
import Network.HTTP
import Network.HTTP.Base
import Network.Stream
import Network.URI
import Data.Maybe
import Data.List
import System.IO.Error
import Control.Exception
import Text.Regex
import Text.Printf
import GHC.IO.Handle.FD
import GHC.IO.Handle

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- Error logging to stderr
logError :: String -> IO ()
logError str =
  hPutStr stderr (str ++ "\n")

-- Maximum number of HTTP redirections
maxHops = 5

-- Fetch a document via HTTP by following up to maxHops redirections
getHTTP :: Request String -> IO (Maybe (Response String))
getHTTP request = followRedirections request maxHops

-- Commutes Maybe and IO
commuteMaybeIO :: Maybe (IO (Maybe a)) -> IO (Maybe a)
commuteMaybeIO Nothing = return Nothing
commuteMaybeIO (Just x) = x

-- Helper function for the latter
followRedirections request 0 = throw . userError $ "Maximum number of hops reached."
followRedirections request hopsRemaining = do
  response <- simpleHTTP request
  commuteMaybeIO $ do
    host <- getHostFromRequest request
    resp <- eitherToMaybe response
    return $ tryAgain host resp
  where
    -- Get the host of a GET request
    getHostFromRequest :: (Request String) -> Maybe String
    getHostFromRequest req =
      (uriAuthority . rqURI $ req) >>= return . printURIAuth
      
    -- Show an URI auth in the right format
    printURIAuth :: URIAuth -> String
    printURIAuth (URIAuth a b c) = a ++ b ++ c
    
    -- Parse a response and send a new request if needed
    tryAgain :: String -> Response String -> IO (Maybe (Response String))
    tryAgain host response = do
      let (d1,d2,d3) = rspCode response
      nextReq <- getRedirectReq host response
      case rspCode response of
        (2,_,_) -> return $ Just response
        (3,_,_) -> case nextReq of
          Nothing -> return Nothing
          Just req -> followRedirections req (hopsRemaining-1) 
        _ -> throw . userError $ printf "Got HTTP response code %d%d%d" d1 d2 d3
        
    -- Get the redirection URL in an HTTP 30* response for a specific host
    getRedirectURL :: String -> Response String -> Maybe String
    getRedirectURL host response = do
        (Header _ value) <- find isRedirectURLHeader (rspHeaders response)
        if "http://" `isPrefixOf` value then
          return value
        else
          return $ "http://" ++ host ++ value
      where
        isRedirectURLHeader (Header HdrLocation _) = True
        isRedirectURLHeader _ = False
    -- Get the redirection request
    getRedirectReq :: String -> Response String -> IO (Maybe (Request String))
    getRedirectReq host response =
      return $ (getRedirectURL host response) >>= (return . getRequest)
      -- TODO catch exceptions
            
      
-- Keep only the documents that have the correct MIME type
-- If it has the correct MIME type, return the body of the response
filterByHeader :: (Response String) -> Maybe String
filterByHeader response =
  let headers = rspHeaders response in
  if any isTextHTML headers then
    Just . rspBody $ response
  else
    Nothing
  where
    isTextHTML (Header HdrContentType str) = isPrefixOf "text/html" str
    isTextHTML _ = False

-- Regex used to find the title in the body
titleRegex = mkRegex "<title>(.*)</title>"

-- Find the title in the body (if any)
getTitle :: String -> Maybe String
getTitle body =
  matchRegex titleRegex body >>= listToMaybe

-- Fetch the title of a given URL (if any…)
fetchTitle :: String -> IO (Maybe String)
fetchTitle url = do
  response <- performRequest (getRequest url)
  return $ response >>= filterByHeader >>= getTitle
  where
    -- Get the document and ignore it if any IOError happens
    performRequest req =
      handle handleException $ do
        resp <- getHTTP req
        return $ resp
    handleException :: IOError -> IO (Maybe (Response String))
    handleException x = do
      logError (show x)
      return Nothing

-- Regex used to detect URLs in messages
urlRegex = mkRegex "(https?://[^ ]*)"

-- Find an URL in the message
getURL :: String -> Maybe String
getURL message =
  matchRegex urlRegex message >>= listToMaybe

main = do
  message <- getLine
  let mURL = getURL message
  case mURL of
    Nothing -> main
    Just url -> do
      title <- fetchTitle url
      case title of
        Just title -> do
          putStrLn . decodeString $ title
          hFlush stdout
        Nothing -> return ()
      main
