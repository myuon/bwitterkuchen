{-# LANGUAGE GADTs, OverloadedStrings, ImplicitParams, FlexibleContexts #-}
module Tweets where

import Control.Lens
import Data.Aeson (FromJSON)
import Data.Conduit (ResumableSource)
import Web.Twitter.Conduit
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadResource)
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

data Config = Config
  { manager :: Manager
  , twInfo :: TWInfo
  }

type AuthM = ReaderT Config IO

runAuth :: AuthM a -> IO a
runAuth m = do
  mgr <- newManager tlsManagerSettings  
  tw <- genTWInfo
  runReaderT m $ Config mgr tw

forkAuth :: AuthM a -> AuthM (IO a)
forkAuth m = do
  cfg <- ask
  return $ runReaderT m cfg

callM :: (FromJSON res) => APIRequest api res -> AuthM res
callM api = ask >>= \config -> lift $ call (twInfo config) (manager config) api

streamM :: (FromJSON res, MonadResource m, MonadReader Config m) => APIRequest api res -> m (ResumableSource m res)
streamM api = ask >>= \config -> stream (twInfo config) (manager config) api

fetchTimeline :: Integer -> AuthM [Status]
fetchTimeline n = callM $ homeTimeline & count ?~ n

fetchStatus :: StatusId -> AuthM Status
fetchStatus = callM . showId

favo :: StatusId -> AuthM Status
favo st = callM $ favoritesCreate st

tweet :: T.Text -> AuthM Status
tweet = callM . update

replyTo :: StatusId -> T.Text -> AuthM Status
replyTo st txt = callM $ update txt & inReplyToStatusId ?~ st

fetchThread :: Status -> AuthM [Status]
fetchThread = go where
  go st = do
    (st:) <$> case st^.statusInReplyToStatusId of
      Just k -> go =<< fetchStatus k
      Nothing -> return []

