{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams #-}
{-# LANGUAGE RankNTypes, DataKinds, LambdaCase #-}
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

fetchTweetThread :: Chan Timeline -> AuthM ()
fetchTweetThread channel = do
  runResourceT $ do
    src <- streamM userstream
    src C.$$+- CL.mapM_ (lift . getStream)

  where
    getStream :: StreamingAPI -> AuthM ()
    getStream =
      \case
        SStatus tw -> do
          case tw ^. statusInReplyToStatusId of
            Just sid -> do
              lift . writeChan channel . TStatusReply tw False =<< fetchThread tw
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
  (W.list "tweetList" V.empty 2)
  (error "not initialized")

app :: App Client Timeline T.Text
app = App widgets showFirstCursor eventHandler return attrmap where
  attrmap client =
    attrMap (fg Vty.white)
    [ ("user-name", Vty.withStyle mempty Vty.bold)
    , ("screen-name", fg Vty.red)
    , (W.listSelectedFocusedAttr, Vty.black `on` Vty.white)
    , (W.listSelectedAttr, Vty.black `on` Vty.white)
    ]
  
  renderTimeline _ =
    \case
      TStatus tw -> padRight Max $
        hBox [ withAttr "user-name" $ txt $ tw^.user^.name
             , withAttr "screen-name" $ txt " @"
             , withAttr "screen-name" $ txt $ tw^.user^.screen_name ]
        <=>
        hBox [ txt $ tw^.text ]
      q -> txt $ T.pack $ show q

  widgets client = [W.renderList renderTimeline True $ client^.timeline]
  eventHandler client =
    \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt client
      VtyEvent ev -> continue =<< handleEventLensed client timeline W.handleListEvent ev
      AppEvent tw ->
        continue $ client & timeline %~ W.listInsert (V.length $ client ^. timeline ^. W.listElementsL) tw
      _ -> continue client

main = runAuth $ do
  channel <- lift newChan
  lift . forkIO =<< forkAuth (fetchTweetThread channel)

  size <- lift $ Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  me <- callM accountVerifyCredentials
  xs <- fetchTimeline 10

  lift $ customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just channel)
    app
    (defClient
     & screenSize .~ size
     & meUser .~ me
     & timeline %~ W.listReplace (V.fromList $ TStatus <$> reverse xs) (Just 0))

