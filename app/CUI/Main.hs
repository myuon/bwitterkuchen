{-# LANGUAGE GADTs, OverloadedStrings, ImplicitParams #-}
module Main where

import Control.Lens
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import System.Environment

genTWInfo :: IO TWInfo
genTWInfo = do
  putStrLn "screen_name?: "
  sc <- getLine
  xs <- BS8.lines <$> BS8.readFile ("token/" ++ sc)

  return $ setCredential
    (twitterOAuth
     { oauthConsumerKey = xs!!0
     , oauthConsumerSecret = xs!!1 })
    (Credential
     [ ("oauth_token", xs!!2)
     , ("oauth_token_secret", xs!!3) ])
    def

type AuthM = ReaderT TWInfo IO

runAuth :: AuthM a -> IO a
runAuth m = genTWInfo >>= runReaderT m

fetch :: (?mgr :: Manager) => Integer -> AuthM [Status]
fetch n = do
  twInfo <- ask
  lift $ call twInfo ?mgr $ homeTimeline & count ?~ n

main = do
  mgr <- newManager tlsManagerSettings
  let ?mgr = mgr
  xs <- getArgs

  case xs of
    ("fetch":[]) -> mapM_ (putStrLn . T.unpack) . fmap (\s -> s^.text) =<< runAuth (fetch 20)
    ("fetch":n:_) -> mapM_ (putStrLn . T.unpack) . fmap (\s -> s^.text) =<< runAuth (fetch (read n))
    _ -> print xs

