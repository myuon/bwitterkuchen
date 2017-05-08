{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Web.Twitter.Conduit hiding (lookup,url)
import Web.Authenticate.OAuth as OA
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Data.Monoid
import System.Environment
import System.IO (hFlush, stdout)

genToken :: IO OAuth
genToken = do
  putStrLn "ConsumerKey?: "
  key <- S8.getLine
  putStrLn "ConsumerSecret?: "
  secret <- S8.getLine

  return $ twitterOAuth
    { oauthConsumerKey = key
    , oauthConsumerSecret = secret
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
      putStr "PIN Code?: "
      S8.getLine

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  token <- genToken
  Credential cred <- authorize token mgr
  let fromJust (Just s) = s
  let get k = fromJust $ lookup k cred

  S8.writeFile ("token/" ++ S8.unpack (get "screen_name")) $ S8.unlines [
    oauthConsumerKey token,
    oauthConsumerSecret token,
    fromJust $ lookup "oauth_token" cred,
    fromJust $ lookup "oauth_token_secret" cred
    ]

