{-# LANGUAGE GADTs, OverloadedStrings, ImplicitParams #-}
module Tweets where

import Control.Lens
import Data.Aeson (FromJSON)
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

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

callM :: (FromJSON res, ?mgr :: Manager) => APIRequest api res -> AuthM res
callM api = ask >>= \twInfo -> lift $ call twInfo ?mgr api

fetch :: (?mgr :: Manager) => Integer -> AuthM [Status]
fetch n = callM $ homeTimeline & count ?~ n

favo :: (?mgr :: Manager) => StatusId -> AuthM Status
favo st = callM $ favoritesCreate st

tweet :: (?mgr :: Manager) => String -> AuthM Status
tweet txt = callM $ update $ T.pack txt

replyTo :: (?mgr :: Manager) => StatusId -> String -> AuthM Status
replyTo st txt = callM $ update (T.pack txt) & inReplyToStatusId ?~ st

