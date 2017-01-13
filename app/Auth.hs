{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Web.Twitter.Conduit hiding (lookup,url)
import Web.Authenticate.OAuth as OA
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Data.Monoid
import System.Environment
import System.IO (hFlush, stdout)

tokens :: OAuth
tokens = twitterOAuth
  { oauthConsumerKey = "6WBP3dtynshO2NmZuauQ0QkJn"
  , oauthConsumerSecret = "OyCsFy62s8GFCqDOZ4gLo5jN6pJ4bu6Bu0oCKoH8rkBUe7HA5t"
  , oauthCallback = Just "oob"
  }

authorize :: OAuth -> Manager -> IO Credential
authorize oauth mgr = do
  cred <- OA.getTemporaryCredential oauth mgr
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  where
    getPIN url = do
      putStrLn $ "browse URL: " ++ url
      putStrLn "PIN Code?"
      S8.getLine

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  Credential cred <- authorize tokens mgr
  let fromJust (Just s) = s
  let get k = fromJust $ lookup k cred

  S8.writeFile ("token/" ++ S8.unpack (get "screen_name")) $ S8.unlines [
    fromJust $ lookup "oauth_token" cred,
    fromJust $ lookup "oauth_token_secret" cred
    ]

