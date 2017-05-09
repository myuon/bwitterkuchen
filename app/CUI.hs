{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams #-}
{-# LANGUAGE RankNTypes, DataKinds, LambdaCase #-}
module CUI where

import Tweets
import Brick
import qualified Brick.Widgets.Edit as W
import qualified Brick.Widgets.List as W
import qualified Graphics.Vty as Vty
import Web.Twitter.Conduit hiding (map, index, inReplyToStatusId)
import Web.Twitter.Types (RetweetedStatus(RetweetedStatus))
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Reader (lift, ask, runReaderT)
import Control.Monad.State
import Control.Lens hiding (index)
import Control.Concurrent (forkIO, Chan, newChan, writeChan)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Zipper (clearZipper)

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

fromStatus :: Status -> Timeline
fromStatus st =
  case st^.statusRetweetedStatus of
    Just u ->
      TStatusRT $ RetweetedStatus
              (st^.statusCreatedAt)
              (st^.statusId)
              (st^.statusText)
              (st^.statusSource)
              (st^.statusTruncated)
              (st^.statusEntities)
              (st^.statusUser)
              u
              (st^.statusCoordinates)
    Nothing -> TStatus st
              
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
  , ("favo", "favo (C-f)")
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


-- 結局IO絡みになりそう
-- EventMがMonadIOだがReaderT経由で呼べないのでおそらくImplicitでもらうしかない
app :: (?config :: Config) => App Client Timeline T.Text
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
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.user^.screen_name ]
             ]
        <=>
        hBox [ txt $ tw^.text ]
      TStatusReply tw _ threads -> padRight Max $
        hBox [ txt "! "
             , withAttr "user-name" $ txt $ tw^.user^.name
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.user^.screen_name ]
             ]
        <=>
        hBox [ txt $ tw^.text ]
      TStatusRT tw -> padRight Max $
        hBox [ withAttr "user-name" $ txt $ tw^.rsRetweetedStatus^.user^.name
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.rsRetweetedStatus^.user^.screen_name ]
             , txt " [RT by "
             , txt $ tw^.user^.name
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.user^.screen_name ]
             , txt "]"
             ]
        <=>
        hBox [ txt $ tw^.rsRetweetedStatus^.text ]

  widgets client = case client^.cstate of
    TL ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 2) $ W.renderList renderTimeline True $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " ---"
      , W.renderEditor False $ client^.minibuffer
      ]
    Anything ->
      -- anythingの画面で選択アイテムが上下すると
      -- 表示がおかしい？
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

  eventHandler client = do
    let runA m = liftIO $ runReaderT m ?config
    let favoCommand = case W.listSelectedElement $ client ^. timeline of
          Just (_, TStatus st) -> runA (favo $ st^.status_id) >> continue client
          _ -> continue client
    
    \case
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) | client^.cstate == TL -> halt client
      VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) | client^.cstate == TL -> continue $ client & cstate .~ Tweet
      VtyEvent (Vty.EvKey (Vty.KChar 'x') modfs) | (Vty.MMeta `elem` modfs || Vty.MAlt `elem` modfs) && client^.cstate == TL -> continue $ client & cstate .~ Anything
      VtyEvent ev | client^.cstate == TL -> continue =<< handleEventLensed client timeline W.handleListEvent ev

      VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) | client^.cstate == Tweet -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Tweet -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) | client^.cstate == Tweet -> do
        let text = foldl1 (\x y -> x `T.append` "\n" `T.append` y) $ W.getEditContents $ client ^. tweetBox
        liftIO $ flip runReaderT ?config $ tweet text
        continue $ client & cstate .~ TL & tweetBox . W.editContentsL %~ clearZipper

      VtyEvent ev | client^.cstate == Tweet -> continue =<< handleEventLensed client tweetBox W.handleEditorEvent ev

      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Anything -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KEnter) []) | client^.cstate == Anything ->
        -- ここの実装さすがにひどい                                     
        case W.listSelectedElement (client^.anything) of
          Just (_,com) | "quit" `T.isPrefixOf` com -> halt client
          Just (_,com) | "tweet" `T.isPrefixOf` com -> continue $ client & cstate .~ Tweet
          Just (_,com) | "favo" `T.isPrefixOf` com -> favoCommand
          _ -> continue client
      VtyEvent ev | client^.cstate == Anything -> do
        client' <- handleEventLensed client minibuffer W.handleEditorEvent ev
        client'' <- handleEventLensed client' anything W.handleListEvent ev

        -- anythingの実装
        -- 効率ｩ！
        let ws = T.words $ head $ W.getEditContents $ client'' ^. minibuffer
        continue $ client'' & anything . W.listElementsL .~ fmap snd (V.filter (\com -> all (\w -> w `T.isInfixOf` fst com) ws) commandList)

      AppEvent tw -> continue $ flip execState client $ do
        timeline %= W.listInsert (V.length $ client ^. timeline ^. W.listElementsL) tw
        let sel = client ^. timeline ^. W.listSelectedL
        let elems = client ^. timeline ^. W.listElementsL
        when (Just (V.length elems - 1) == sel) $ timeline %= W.listMoveDown
      _ -> continue client

main = runAuth $ do
  channel <- lift newChan
  lift . forkIO =<< forkAuth (fetchTweetThread channel)

  size <- lift $ Vty.displayBounds =<< Vty.outputForConfig =<< Vty.standardIOConfig
  me <- callM accountVerifyCredentials
  xs <- fetchTimeline 20

  cfg <- ask
  let ?config = cfg

  lift $ customMain
    (Vty.standardIOConfig >>= Vty.mkVty)
    (Just channel)
    app
    (defClient
     & screenSize .~ size
     & meUser .~ me
     & timeline %~ W.listReplace (V.fromList $ reverse $ fmap fromStatus xs) (Just 0))

