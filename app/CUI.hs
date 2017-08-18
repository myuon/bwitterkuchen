{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams #-}
{-# LANGUAGE RankNTypes, DataKinds, LambdaCase #-}
module CUI where

import Tweets
import Brick
import Brick.BChan
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
import Control.Concurrent (forkIO)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Zipper (textZipper, clearZipper)
import qualified Text.Wrap as Wrap

listSelectedElemL :: Lens' (W.List n e) (Maybe e)
listSelectedElemL =
  lens (\w -> ((w ^. W.listElementsL) V.!) <$> w ^. W.listSelectedL)
       (\w me -> case (me, w^.W.listSelectedL) of
                   (Just e, Just n) -> w & W.listElementsL %~ (V.// [(n,e)])
                   _ -> w
       )

data Timeline =
  TStatus Status
  | TStatusRT RetweetedStatus
  | TStatusReply
    { status :: Status
    , unfolded :: Bool
    , thread :: [Status]
    }
  | TFavorite User Status
  deriving (Eq, Show)

makePrisms ''Timeline

asStatus :: Lens' Timeline Status
asStatus = lens getter setter where
  getter (TStatus st) = st
  getter (TStatusRT st) = st^.rsRetweetedStatus
  getter (TStatusReply st _ _) = st

  setter (TStatus _) st = TStatus st
  setter (TStatusRT rs) st = TStatusRT $ rs & rsRetweetedStatus .~ st
  setter (TStatusReply _ b t) st = TStatusReply st b t

fromStatus :: Status -> Timeline
fromStatus st
  | st^.statusRetweetedStatus /= Nothing =
    TStatusRT $ RetweetedStatus
                (st^.statusCreatedAt)
                (st^.statusId)
                (st^.statusText)
                (st^.statusSource)
                (st^.statusTruncated)
                (st^.statusEntities)
                (st^.statusUser)
                ((\(Just u) -> u) $ st^.statusRetweetedStatus)
                (st^.statusCoordinates)
  | st^.statusInReplyToStatusId /= Nothing =
    TStatusReply st False []
  | otherwise = TStatus st
              
fetchTweetThread :: BChan Timeline -> AuthM ()
fetchTweetThread channel = do
  runResourceT $ do
    src <- streamM userstream
    src C.$$+- CL.mapM_ (lift . getStream)

  where
    getStream :: StreamingAPI -> AuthM ()
    getStream =
      \case
        SStatus tw -> lift $ writeBChan channel $ fromStatus tw
        SRetweetedStatus tw -> lift $ writeBChan channel $ TStatusRT tw
        SEvent ev | ev ^. evEvent == "favorite" ->
          case (ev^.evSource, ev^.evTargetObject) of
            (ETUser u, Just (ETStatus s)) -> lift $ writeBChan channel $ TFavorite u s
            _ -> return ()
        _ -> return ()

data CState = TL | Anything | Tweet | Reply | Notification deriving (Eq, Ord, Show)

data Client = Client {
  _screenSize :: (Int,Int),
  _cstate :: CState,
  _timeline :: W.List T.Text Timeline,
  _notification :: W.List T.Text Timeline,
  _minibuffer :: W.Editor T.Text T.Text,
  _anything :: W.List T.Text T.Text,
  _tweetBox :: W.Editor T.Text T.Text,
  _meUser :: User
  }

makePrisms ''CState
makeLenses ''Client

data Action =
  -- user action
  ATweet | AReplyTweet | AFavo | AUnfold | AQuit
  -- other action
  | ACloseAnything | ARunAnything | AOpenReplyTweet
  | ACloseTweet | AMoveToTLTab | AMoveToNotificationTab
  deriving (Eq, Ord)

manual :: Action -> T.Text
manual =
  \case
    ATweet -> "tweet (C-t)"
    AReplyTweet -> "reply (M-t)"
    AFavo -> "fav (C-f)"
    AUnfold -> "unfold (C-u)"
    AQuit -> "quit (q)"

functionList :: V.Vector Action
functionList = V.fromList [ATweet, AReplyTweet, AFavo, AUnfold, AQuit]

runClient :: (?config :: Config) => Action -> StateT Client (EventM n) ()
runClient ATweet = do
  text <- foldl1 (\x y -> x `T.append` "\n" `T.append` y) . W.getEditContents <$> use tweetBox
  lift $ liftIO $ flip runReaderT ?config $ tweet text
  tweetBox . W.editContentsL %= clearZipper
runClient AReplyTweet = do
  use (timeline . listSelectedElemL) >>= \case
    Just st -> do
      text <- foldl1 (\x y -> x `T.append` "\n" `T.append` y) . W.getEditContents <$> use tweetBox
      lift $ liftIO $ flip runReaderT ?config $ replyTo (st^.asStatus^.status_id) text
      tweetBox . W.editContentsL %= clearZipper
    Nothing -> return ()
runClient AFavo =
  use (timeline . listSelectedElemL) >>= \case
    Just st | st^.asStatus^.statusFavorited == Just True -> do
      lift $ liftIO $ flip runReaderT ?config $ unfavo $ st^.asStatus^.status_id
      timeline . listSelectedElemL . _Just . asStatus . statusFavorited .= Just False
    Just st -> do
      lift $ liftIO $ flip runReaderT ?config $ favo $ st^.asStatus^.status_id
      timeline . listSelectedElemL . _Just . asStatus . statusFavorited .= Just True
    Nothing -> return ()
runClient AUnfold =
  use (timeline . listSelectedElemL) >>= \case
    Just (TStatusReply st False []) -> do
      th <- lift $ liftIO $ flip runReaderT ?config $ fetchThread st
      timeline . listSelectedElemL . _Just .= TStatusReply st True th
    Just (TStatusReply st b th) -> do
      timeline . listSelectedElemL . _Just .= TStatusReply st (not b) th
    _ -> return ()
runClient ARunAnything = do
  ws <- T.words . head . W.getEditContents <$> use minibuffer
  anything . W.listElementsL .= V.filter (\com -> all (\w -> w `T.isInfixOf` com) ws) (fmap manual functionList)
runClient ACloseAnything = do
  cstate .= TL
  minibuffer . W.editContentsL %= clearZipper
  anything . W.listElementsL .= fmap manual functionList
runClient ACloseTweet = do
  cstate .= TL
  tweetBox . W.editContentsL %= clearZipper
runClient AOpenReplyTweet = do
  use (timeline . listSelectedElemL) >>= \case
    Just st -> do
      cstate .= Reply
      tweetBox . W.editContentsL .= textZipper ["@" `T.append` (st^.asStatus^.user^.screen_name) `T.append` " "] Nothing
    Nothing -> return ()
runClient AMoveToTLTab = cstate .= TL
runClient AMoveToNotificationTab = cstate .= Notification
runClient _ = return ()

defClient :: Client
defClient = Client
  (0,0)
  TL
  (W.list "timeline" V.empty 2)
  (W.list "notification" V.empty 2)
  (W.editorText "minibuffer" (Just 1) "")
  (W.list "anything" (fmap manual functionList) 1)
  (W.editorText "tweetBox" (Just 5) "")
  (error "not initialized")


-- 結局IO絡みになりそう
-- EventMがMonadIOだがReaderT経由で呼べないのでおそらくImplicitでもらうしかない
app :: (?config :: Config) => App Client Timeline T.Text
app = App widgets showFirstCursor eventHandler return attrmap where
  attrmap client =
    attrMap (fg Vty.white)
    [ ("user-name", Vty.withStyle mempty Vty.bold)
    , ("screen-name", fg Vty.red)
    , ("screen-name-favo", fg Vty.blue)
    , (W.listSelectedFocusedAttr, Vty.black `on` Vty.white)
    , (W.listSelectedAttr, Vty.black `on` Vty.white)
    , ("inverted", Vty.black `on` Vty.white)
    , ("brGreen", Vty.black `on` Vty.brightGreen)
    ]
  
  renderTimeline b =
    \case
      TStatus tw -> padRight Max $
        hBox [ withAttr "user-name" $ txt $ tw^.user^.name
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.user^.screen_name ]
             , if (tw^.statusFavorited == Just True) then txt " ★" else txt ""
             , if (tw^.statusRetweeted == Just True) then txt " 🔃" else txt ""
             ]
        <=>
        hBox [ txtWrapWith (Wrap.defaultWrapSettings { Wrap.breakLongWords = True }) $ tw^.text ]
      TStatusReply tw unfolded threads -> padRight Max $
        hBox [ txt "! "
             , withAttr "user-name" $ txt $ tw^.user^.name
             , withAttr "screen-name" $
               hBox [ txt " @"
                    , txt $ tw^.user^.screen_name ]
             , if (tw^.statusFavorited == Just True) then txt " ★" else txt ""
             , if (tw^.statusRetweeted == Just True) then txt " 🔃" else txt ""
             ]
        <=>
        hBox [ txtWrapWith (Wrap.defaultWrapSettings { Wrap.breakLongWords = True }) $ tw^.text ]
        <=>
        if unfolded
        then hBox [ txt "┗"
                  , padLeft (Pad 2) $ vBox $ fmap (renderTimeline b . TStatus) $ tail threads
                  ]
        else hBox []
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
             , if (tw^.rsRetweetedStatus^.statusFavorited == Just True) then txt " ★" else txt ""
             , if (tw^.rsRetweetedStatus^.statusRetweeted == Just True) then txt " 🔃" else txt ""
             ]
        <=>
        hBox [ txtWrapWith (Wrap.defaultWrapSettings { Wrap.breakLongWords = True }) $ tw^.rsRetweetedStatus^.text ]
      TFavorite usr tw -> padRight Max $
        hBox [ txt "★ "
             , withAttr "user-name" $ txt $ usr^.name
             , withAttr "screen-name-favo" $
               hBox [ txt " @"
                    , txt $ usr^.screen_name ]
             ]
        <=>
        hBox [ txt "| "
             , txtWrapWith (Wrap.defaultWrapSettings { Wrap.breakLongWords = True }) $ tw^.text ]

  widgets client = case client^.cstate of
    TL ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 2) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " --- *timeline*"
      , W.renderEditor (vBox . fmap txt) False $ client^.minibuffer
      ]
    Anything ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 8) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "brGreen" $ padRight Max $ txt " --- timeline"
      , W.renderList (\_ e -> padRight Max $ padLeft (Pad 1) $ txt e) True $ client^.anything
      , withAttr "brGreen" $ padRight Max $ txt " *anything*"
      , W.renderEditor (vBox . fmap txt) True (client^.minibuffer)
      ]
    Tweet ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 6) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " *tweet* (C-c)send (C-q)cancel"
      , vLimit 5 $ W.renderEditor (vBox . fmap txt) True $ client^.tweetBox
      ]
    Reply ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 6) $ W.renderList renderTimeline False $ client^.timeline
      , withAttr "inverted" $ padRight Max $ txt " *reply* (C-c)send (C-q)cancel"
      , vLimit 5 $ W.renderEditor (vBox . fmap txt) True $ client^.tweetBox
      ]
    Notification ->
      return $ vBox
      [ vLimit (client^.screenSize^._2 - 2) $ W.renderList renderTimeline True $ client^.notification
      , withAttr "inverted" $ padRight Max $ txt " --- *notification*"
      , W.renderEditor (vBox . fmap txt) False $ client^.minibuffer
      ]

  -- keybindは事前に指定したものを勝手にやってくれる感じにしたい
  eventHandler client = do
    \case
      -- TL keybind
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) | client^.cstate == TL || client^.cstate == Notification -> halt client
      VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) | client^.cstate == TL -> continue $ client & cstate .~ Tweet
      VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MMeta]) | client^.cstate == TL -> continue =<< (flip execStateT client $ runClient AOpenReplyTweet)
      VtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MMeta]) | client^.cstate == TL -> continue $ client & cstate .~ Anything
      VtyEvent (Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl]) | client^.cstate == TL -> continue =<< (flip execStateT client $ runClient AUnfold)
      VtyEvent (Vty.EvKey (Vty.KChar 'f') [Vty.MCtrl]) | client^.cstate == TL -> continue =<< (flip execStateT client $ runClient AFavo)
      VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) | client^.cstate == TL -> continue =<< (flip execStateT client $ runClient AMoveToNotificationTab)

      -- Notification tab
      VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) | client^.cstate == Notification -> continue =<< (flip execStateT client $ runClient AMoveToTLTab)

      -- Tweetbox keybind
      VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) | client^.cstate == Tweet || client^.cstate == Reply -> continue =<< (flip execStateT client $ runClient ACloseTweet)
      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Tweet || client^.cstate == Reply -> continue =<< (flip execStateT client $ runClient ACloseTweet)
      VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) | client^.cstate == Tweet -> (flip execStateT client $ runClient ATweet >> runClient ACloseTweet) >>= continue
      VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) | client^.cstate == Reply -> (flip execStateT client $ runClient AReplyTweet >> runClient ACloseTweet) >>= continue

      -- anything keybind
      VtyEvent (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) | client^.cstate == Anything -> continue $ client & cstate .~ TL
      VtyEvent (Vty.EvKey (Vty.KEnter) []) | client^.cstate == Anything ->
        case W.listSelectedElement (client^.anything) of
          Just (n,com) -> case (functionList V.!) <$> V.elemIndex com (fmap manual functionList) of
            Just AQuit -> halt client
            Just ATweet -> continue $ client & cstate .~ Tweet
            Just AReplyTweet -> (flip execStateT client $ runClient AOpenReplyTweet) >>= continue
            Just AFavo -> (flip execStateT client $ (runClient AFavo >> runClient ACloseAnything)) >>= continue
            Just AUnfold -> (flip execStateT client $ (runClient AUnfold >> runClient ACloseAnything)) >>= continue
            _ -> continue client
          _ -> continue client
      VtyEvent ev | client^.cstate == Anything -> do
        client' <- handleEventLensed client minibuffer W.handleEditorEvent ev
        client'' <- handleEventLensed client' anything W.handleListEvent ev
        continue =<< (flip execStateT client'' $ runClient ARunAnything)

      -- handling event
      VtyEvent ev | client^.cstate == TL -> continue =<< handleEventLensed client timeline W.handleListEvent ev
      VtyEvent ev | client^.cstate == Notification -> continue =<< handleEventLensed client notification W.handleListEvent ev
      VtyEvent ev | client^.cstate == Tweet || client^.cstate == Reply -> continue =<< handleEventLensed client tweetBox W.handleEditorEvent ev

      AppEvent tw -> continue $ flip execState client $ do
        case tw of
          (TFavorite _ _) -> do
            notification %= W.listInsert (V.length $ client ^. notification ^. W.listElementsL) tw
          _ -> do
            timeline %= W.listInsert (V.length $ client ^. timeline ^. W.listElementsL) tw
            when (tw ^. asStatus ^. statusInReplyToUserId == Just (client ^. meUser ^. user_id)) $ notification %= W.listInsert (V.length $ client ^. notification ^. W.listElementsL) tw
            
            let sel = client ^. timeline ^. W.listSelectedL
            let elems = client ^. timeline ^. W.listElementsL
            when (Just (V.length elems - 1) == sel) $ timeline %= W.listMoveDown
      _ -> continue client

main = runAuth $ do
  channel <- lift $ newBChan 2
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

