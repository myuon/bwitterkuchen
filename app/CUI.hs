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

data CState = TL | Anything | Tweet | Notification deriving (Eq, Ord, Show)

data Client = Client {
  _screenSize :: (Int,Int),
  _cstate :: CState,
  _timeline :: W.List T.Text Timeline,
  _minibuffer :: W.Editor T.Text T.Text,
  _anything :: W.List T.Text T.Text,
  _tweetBox :: W.Editor T.Text T.Text,
  _meUser :: User
  }

makePrisms ''CState
makeLenses ''Client

commandList :: V.Vector (T.Text,T.Text)
commandList = V.fromList
  [ ("tweet", "tweet (C-t)")
  , ("quit", "quit") ]

defClient :: Client
defClient = Client
  (0,0)
  TL
  (W.list "tweetList" V.empty 2)
  (W.editorText "minibuffer" (vBox . fmap txt) (Just 1) "")
  (W.list "anything" (fmap snd commandList) 5)
  (W.editorText "tweetBox" (vBox . fmap txt) (Just 5) "")
  (error "not initialized")


app :: App Client Timeline T.Text
app = App widgets showFirstCursor eventHandler return attrmap where
  attrmap client =
    attrMap (fg Vty.white)
    [ ("user-name", Vty.withStyle mempty Vty.bold)
    , ("screen-name", fg Vty.red)
    , (W.listSelectedFocusedAttr, Vty.black `on` Vty.white)
    , (W.listSelectedAttr, Vty.black `on` Vty.white)
    , ("inverted", Vty.black `on` Vty.white)
    , ("brGreen", Vty.black `on` Vty.brightGreen)
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

  widgets client = case client^.cstate of
    TL ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 2) $ W.renderList renderTimeline True $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " ---"
      , vLimit 1 $ W.renderEditor False $ client^.minibuffer
      ]
    Anything ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 8) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "brGreen" $ padRight Max $ txt " ---"
      , vLimit 5 $ W.renderList (\_ e -> padRight Max $ padLeft (Pad 1) $ txt e) True $ client^.anything
      , withAttr "brGreen" $ padRight Max $ txt " *anything*"
      , W.renderEditor True (client^.minibuffer)
      ]
    Tweet ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 6) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " *tweet* (C-c)send (C-q)cancel"
      , vLimit 5 $ W.renderEditor True $ client^.tweetBox
      ]

  eventHandler client =
    \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) | client^.cstate == TL -> halt client
      VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) | client^.cstate == TL -> continue $ client & cstate .~ Tweet
      VtyEvent (Vty.EvKey (Vty.KChar 'x') modfs) | (Vty.MMeta `elem` modfs || Vty.MAlt `elem` modfs) && client^.cstate == TL -> continue $ client & cstate .~ Anything
      VtyEvent ev | client^.cstate == TL -> continue =<< handleEventLensed client timeline W.handleListEvent ev

      VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) | client^.cstate == Tweet -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Tweet -> continue $ client & cstate .~ TL
      VtyEvent ev | client^.cstate == Tweet -> continue =<< handleEventLensed client tweetBox W.handleEditorEvent ev

      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Anything -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KEnter) []) | client^.cstate == Anything ->
        case W.listSelectedElement (client^.anything) of
          Just (_,com) | "quit" `T.isPrefixOf` com -> halt client
          Just (_,com) | "tweet" `T.isPrefixOf` com -> continue $ client & cstate .~ Tweet
          _ -> continue client
      VtyEvent ev | client^.cstate == Anything -> do
        client' <- handleEventLensed client minibuffer W.handleEditorEvent ev
        client'' <- handleEventLensed client' anything W.handleListEvent ev

        -- anything¤Î¼ÂÁõ
        -- ¸úÎ¨Ž©¡ª
        let ws = T.words $ head $ W.getEditContents $ client'' ^. minibuffer
        continue $ client'' & anything . W.listElementsL .~ fmap snd (V.filter (\com -> all (\w -> w `T.isInfixOf` fst com) ws) commandList)

      AppEvent tw ->
        continue $ client & timeline %~ W.listInsert (V.length $ client ^. timeline ^. W.listElementsL) tw
      _ -> continue client

main = runAuth $ do
  channel <- lift newChan
  lift . forkIO =<< forkAuth (fetchTweetThread channel)

  size <- lift $ Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  me <- callM accountVerifyCredentials
  xs <- return [] --fetchTimeline 10

  lift $ customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just channel)
    app
    (defClient
     & screenSize .~ size
     & meUser .~ me
     & timeline %~ W.listReplace (V.fromList $ TStatus <$> reverse xs) (Just 0))

