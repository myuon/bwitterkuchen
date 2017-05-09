{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams, RankNTypes, DataKinds #-}
module CUI where

import Tweets
import Brick
import qualified Brick.Widgets.Edit as W
import qualified Brick.Widgets.List as W
import qualified Graphics.Vty as Vty
import Web.Twitter.Conduit hiding (map, index, inReplyToStatusId)
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Reader (lift, ask, runReaderT)
import Control.Lens hiding (index)
import Control.Concurrent (forkIO, Chan, newChan, writeChan)
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

data TabName = HomeTab | NotificationTab
  deriving (Eq, Ord, Show)

data Client = Client {
  _screenSize :: (Int,Int),
  _tweetbox :: W.Editor T.Text String,
  _currentTab :: TabName,
  _timeline :: W.List T.Text Timeline,
  _meUser :: User
  }


makePrisms ''TabName
makeLenses ''Client

defClient :: Client
defClient = Client
  (0,0)
  (W.editorText "tweetbox" (vBox . fmap txt) (Just 5) "")
  HomeTab
  (W.list "tweetList" (V.empty) 1)
  (error "not initialized")

app :: App Client Timeline T.Text
app = App widgets showFirstCursor eventHandler return def where
  renderTimeline focused tl = case tl of
    TStatus tw -> txt $ tw^.text
    _ -> txt "other"

  widgets client = [W.renderList renderTimeline False (client^.timeline)]
  eventHandler client ev =
    case ev of
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt client
      AppEvent tw ->
        continue $ client & timeline %~ W.listInsert 0 tw
      _ -> continue client

main = runAuth $ do
  channel <- lift newChan
  lift . forkIO . runReaderT (fetchTweetThread channel) =<< ask

  size <- lift $ Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  me <- callM accountVerifyCredentials
  xs <- fetch 10

  lift $ customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just channel)
    app
    (defClient & screenSize .~ size & meUser .~ me & timeline %~ W.listReplace (V.fromList $ TStatus <$> xs) Nothing)

