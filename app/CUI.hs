{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams, RankNTypes #-}
module CUI where

import Tweets
import Brick
import Graphics.Vty
import Web.Twitter.Conduit hiding (map, index, inReplyToStatusId)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types.Lens hiding (Event, text)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Control.Lens hiding (index)
import Control.Concurrent
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Zipper (insertChar, textZipper)

data Timeline =
  TStatus Status
  | TStatusRT RetweetedStatus
  | TStatusReply
    { status :: Status
    , unfolded :: Bool
    , replyTo :: [Status]
    }
  deriving (Eq, Show)

makePrisms ''Timeline

getStatusReplies :: Status -> (StatusId -> IO Status) -> IO Timeline
getStatusReplies status getter = TStatusReply status False <$> go status
  where
    go s = do
      let mid = s ^. statusInReplyToStatusId
      case mid of
        Just i -> getter i >>= \s' -> (s':) <$> go s'
        Nothing -> return []

fetchTweetThread :: Chan Timeline -> AuthM ()
fetchTweetThread channel = do
  config <- ask
  runResourceT $ do
    src <- stream (twInfo config) (manager config) userstream
    src C.$$+- CL.mapM_ (liftIO . runAuth . getStream (manager config))

  where
    getStream :: Manager -> StreamingAPI -> AuthM ()
    getStream mgr t = do
      case t of
        SStatus tw ->
          case tw ^. statusInReplyToStatusId of
            Just sid -> lift $ writeChan channel =<< getStatusReplies tw (runAuth . callM . showId)
            Nothing -> lift $ writeChan channel $ TStatus tw
        SRetweetedStatus tw -> lift $ writeChan channel $ TStatusRT tw
        SEvent ev | ev ^. evEvent == "favorite" -> return ()
        _ -> return ()

main = runAuth $ do
  channel <- lift newChan
  lift . forkIO . runReaderT (fetchTweetThread channel) =<< ask

  size <- lift $ displayBounds =<< outputForConfig =<< standardIOConfig
  me <- callM accountVerifyCredentials
  lift $ print me

  return ()
