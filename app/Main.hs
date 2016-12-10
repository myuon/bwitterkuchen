{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams #-}
module Main where

import Lib
import Credential
import Brick
import Brick.Util
import Brick.Types
import Brick.Widgets.Edit
import Graphics.Vty (mkVty, outputForConfig)
import Graphics.Vty.Config
import Graphics.Vty.Output.Interface
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Web.Twitter.Conduit hiding (map, index, inReplyToStatusId)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types.Lens hiding (Event, text)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
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
  | TStatusReply {
      status :: Status,
      unfolded :: Bool,
      replyTo :: [Status]}
  | TStatusQT Status
  | TFavorite User Status
  deriving (Eq, Show)

ofStatus :: Lens' Timeline Status
ofStatus = lens get set where
  get (TStatus s) = s
  get (TStatusRT rs) = rs ^. rsRetweetedStatus
  get (TStatusReply r _ _) = r
  get (TStatusQT s) = s

  set (TStatus _) s = TStatus s
  set (TStatusRT rs) s = TStatusRT $ rs & rsRetweetedStatus .~ s
  set (TStatusReply _ u rs) s = TStatusReply s u rs
  set (TStatusQT _) s = TStatusQT s

getStatusReplies :: Status -> (StatusId -> IO Status) -> IO Timeline
getStatusReplies status getter = TStatusReply status False <$> go status
  where
    go s = do
      let mid = s ^. statusInReplyToStatusId
      case mid of
        Just i -> getter i >>= \s' -> (s':) <$> go s'
        Nothing -> return []

data ClientMode = TLView | Tweet | Reply StatusId
  deriving (Eq, Ord, Show)

data Tab =
  Home { _clientMode :: ClientMode }
  | Notification
  deriving (Eq, Ord, Show)
data TabName = HomeTab | NotificationTab
  deriving (Eq, Ord, Show)

tabName :: Getter Tab TabName
tabName = to toTabName where
  toTabName (Home _) = HomeTab
  toTabName Notification = NotificationTab

data ListView = ListView {
  _items :: [Timeline],
  _index :: Int
  }

data Client = Client {
  _screenSize :: (Int,Int),
  _tweetbox :: Editor T.Text String,
  _currentTab :: Tab,
  _tweetLists :: M.Map TabName ListView,
  _meUser :: User
  }

makeLenses ''ListView
makeLenses ''Client
makePrisms ''Tab

currentTabName :: Getter Client TabName
currentTabName = currentTab . tabName

currentTweetList :: Lens' Client ListView
currentTweetList = lens (\c -> (c ^. tweetLists) ^?! ix (c ^. currentTabName)) (\c v -> c & tweetLists . ix (c ^. currentTabName) .~ v)

defClient :: Client
defClient = Client
  (0,0)
  (editorText "tweetbox" (vBox . fmap txt) (Just 5) "")
  (Home TLView)
  (M.fromList [(tab, ListView [] 0) | tab <- [HomeTab, NotificationTab]])
  (error "not initialized")

fetchTweetThread :: Chan Timeline -> IO ()
fetchTweetThread channel = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    src <- stream twInfo mgr userstream
    src C.$$+- CL.mapM_ (liftIO . getStream mgr)

  where
    getStream :: Manager -> StreamingAPI -> IO ()
    getStream mgr t = do
      case t of
        SStatus tw ->
          case tw ^. statusInReplyToStatusId of
            Just sid -> writeChan channel =<< getStatusReplies tw (call twInfo mgr . showId)
            Nothing -> writeChan channel $ TStatus tw
        SRetweetedStatus tw -> writeChan channel $ TStatusRT tw
        SEvent ev | ev ^. evEvent == "favorite" ->
          case (ev ^. evSource, ev ^. evTarget, ev ^. evTargetObject) of
            (ETUser u, _, Just (ETStatus s)) ->
              writeChan channel $ TFavorite u s
            _ -> return ()
        _ -> return ()

textWrap :: Int -> T.Text -> T.Text
textWrap w text
  | T.length text == 0 = T.empty
  | otherwise = let (x,xs) = T.splitAt maxWidth text in x `T.append` "\n" `T.append` textWrap w xs
  where
    maxWidth = maximum [n | n <- [0..T.length text], textWidth (T.take n text) < w]

main = do
  mgr <- newManager tlsManagerSettings
  let ?twInfo = twInfo
  let ?mgr = mgr
  
  channel <- newChan
  forkIO $ fetchTweetThread channel

  size <- displayBounds =<< outputForConfig =<< standardIOConfig
  me <- call twInfo mgr accountVerifyCredentials

  customMain
    (standardIOConfig >>= mkVty)
    (Just channel)
    app
    (defClient & screenSize .~ size & meUser .~ me)

  where
    app :: (?mgr :: Manager, ?twInfo :: TWInfo) => App Client Timeline String
    app = App widgets cursor eventHandler return def

    transparent = Attr KeepCurrent KeepCurrent KeepCurrent
    fore = withForeColor
    back = withBackColor
    style = withStyle
    resetStyle (Attr _ x y) = Attr Default x y

    onAttr :: Attr -> Widget n -> Widget n
    onAttr tt = withAttr uid . updateAttrMap (applyAttrMappings [(uid, tt)])
      where uid = attrName "unique-attr-id"

    cursor client xs = cursorTab $ client ^. currentTab where
      cursorTab (Home v) = case v of
        TLView -> Just $ CursorLocation (Location (0,0)) Nothing
        Tweet -> Just $ head xs
        Reply _ -> Just $ head xs
      cursorTab Notification = Just $ CursorLocation (Location (0,0)) Nothing

    eventHandler client ev = case client ^. currentTab of
      Home TLView -> tlviewHandler client ev
      Home Tweet -> editKeyHandler client ev
      Home (Reply _) -> editKeyHandler client ev
      Notification -> tlviewHandler client ev

    editKeyHandler client ev =
      case ev of
        VtyEvent (EvKey (KChar ch) []) -> continue $ client & tweetbox . editContentsL %~ insertChar ch
        VtyEvent (EvKey (KChar 'q') [MCtrl]) -> continue $ client & currentTab . _Home .~ TLView
        VtyEvent (EvKey (KChar 'c') [MCtrl]) -> do
          let text = foldl1 (\x y -> x `T.append` "\n" `T.append` y) $ getEditContents $ client ^. tweetbox
          case client ^. currentTab of
            Home Tweet -> liftIO $ call ?twInfo ?mgr $ update text
            Home (Reply id) -> liftIO $ call ?twInfo ?mgr $ update text & inReplyToStatusId ?~ id

          continue $ client
            & tweetbox . editContentsL .~ textZipper [] Nothing
            & currentTab . _Home .~ TLView
        VtyEvent event -> handleEventLensed client tweetbox handleEditorEvent event >>= continue
        _ -> continue client

    -- Home, Notification共通
    -- Home限定の話を持ち込むときは注意
    tlviewHandler client ev =
      let i = client ^. currentTweetList ^. index
          ts = client ^. currentTweetList ^. items

          isHome = client ^. currentTab /= Notification
          isNotification = client ^. currentTab == Notification
          viewIsNonEmpty = length (client ^. currentTweetList ^. items) > 0
      in
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt client

        -- j,k移動はindexが端点の時はカーソルだけ動かす感じにしたい
        -- ページをまたぐときは上端と下端で固定する案
        VtyEvent (EvKey (KChar 'j') []) | i - 1 < length ts -> continue $ client & currentTweetList . index +~ 1
        VtyEvent (EvKey (KChar 'k') []) | i > 0 -> continue $ client & currentTweetList . index -~ 1
        VtyEvent (EvKey (KChar '<') []) -> continue $ client & currentTweetList . index .~ 0
        VtyEvent (EvKey (KChar 'u') []) | viewIsNonEmpty -> continue $ client & currentTweetList . items . ix i %~ \t ->
          case t of
            TStatusReply tw b tws -> TStatusReply tw (not b) tws
            z -> z

        -- tab間移動
        -- いずれリスト選択式にしたい
        VtyEvent (EvKey (KChar 'n') [MCtrl]) ->
          case client ^. currentTab of
            Home _ -> continue $ client & currentTab .~ Notification
            Notification -> continue $ client & currentTab .~ Home TLView

        -- Home Tabでのキーバインド
        VtyEvent (EvKey (KChar 't') []) | isHome -> continue $ client & currentTab . _Home .~ Tweet
        VtyEvent (EvKey (KChar 't') [MCtrl]) | isHome -> do
          let tw = (client ^. currentTweetList ^. items) !! (client ^. currentTweetList ^. index)
          continue $ client
            & tweetbox . editContentsL .~ textZipper ["@" `T.append` (tw ^. ofStatus ^. user ^. userScreenName)] Nothing
            & currentTab . _Home .~ Reply (tw ^. ofStatus ^. statusId)
        VtyEvent (EvKey (KChar 'f') []) | isHome && viewIsNonEmpty -> do
          let tw = (client ^. currentTweetList ^. items) !! (client ^. currentTweetList ^. index)
          liftIO $ call ?twInfo ?mgr $ favoritesCreate $ tw ^. ofStatus ^. statusId
          continue $ client
            & currentTweetList . items . ix (client ^. currentTweetList ^. index) . ofStatus . statusFavorited .~ Just True
        AppEvent tw ->
          let consTweet tab cl = cl
                & tweetLists . ix tab . items %~ (tw :)
                & tweetLists . ix tab . index %~ (\i -> if i == 0 then 0 else i + 1)
          in
          case tw of
            (TFavorite _ _) -> continue $ client & consTweet NotificationTab
            (TStatusRT tw) | tw ^. rsRetweetedStatus ^. statusUser ^. userId == client ^. meUser ^. userId -> continue $ client & consTweet HomeTab & consTweet NotificationTab
            _ -> continue $ client & consTweet HomeTab
        _ -> continue client

    widgets client = case client ^. currentTab of
      Home TLView -> [tweetList, minibuffer (Home TLView)]
      Home Tweet -> tweetBoxLayout
      Home (Reply _) -> tweetBoxLayout
      Notification -> [tweetList, minibuffer Notification]
      where
        tweetBoxLayout = [vLimit (client ^. screenSize ^. _2 - 6) tweetList, tweetEditBox, minibuffer (Home TLView)]
        
        tweetEditBox =
          let (w,h) = client ^. screenSize in
          translateBy (Location (0, h-8)) $ vLimit 6 $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) $ padRight Max $ txt ":tweetbox -- (C-c)send (C-q)quit",
            renderEditor True $ client ^. tweetbox]
        
        tweetList =
          let (w,h) = client ^. screenSize in
          cropBottomBy 2 $ vBox $ fmap (\(i,st) -> renderTweet (i == 0) st) $ zip [0..] $ drop (client ^. currentTweetList ^. index) $ client ^. currentTweetList ^. items

        renderTweet = go where
          scrName sn isMe =
            onAttr (resetStyle transparent) $ (onAttr (transparent `fore` (if isMe then blue else red) `style` underline) $ txt "@" <+> txt sn) <+> txt " "
          dspName n = txt n
          name user =
            scrName (user ^. userScreenName) (client ^. meUser ^. userId == user ^. userId)
            <+> txt "(" <+> dspName (user ^. userName) <+> txt ")"

          nameRT rtw =
            txt "RT "
            <+> name (rtw ^. rsRetweetedStatus ^. statusUser)
            <+> txt " by "
            <+> name (rtw ^. rsUser)
          tweetContent text =
            txt (textWrap (client ^. screenSize ^. _1 - 3) text)
          favRT tw =
            if tw ^. statusFavorited == Just True then txt " ★" else txt ""
            
          -- screenNameだけが反転しない
          -- 真面目にattrMap使わないとだめ？
          inverted =
            onAttr (transparent `fore` black `back` brightWhite) . padRight Max

          go b (TStatus tw) = padLeft (Pad 1) $
            (name $ tw ^. statusUser) <+> favRT tw
            <=> (txt " " <+> (tweetContent $ tw ^. statusText))
            <=> txt " "
          go b (TStatusRT rtw) = padLeft (Pad 1) $
            (nameRT rtw) <+> favRT (rtw ^. rsRetweetedStatus)
            <=> (txt " " <+> (tweetContent $ rtw ^. rsRetweetedStatus ^. statusText))
            <=> txt " "
          go b (TStatusReply tw unf tws) =
            if unf
            then vBox $ fmap (\(i,t) -> padLeft (Pad i) $ go (i == 0) $ TStatus t) $ zip [0..] $ tw:tws
            else go b (TStatus tw)
          go b (TFavorite suser tw) =
            txt "favorited by: " <+> (name suser)
            <=> (txt " " <+> tweetContent (tw ^. statusText))

        minibuffer (Home _) =
          let (w,h) = client ^. screenSize in
          translateBy (Location $ (0,h-2)) $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) (padRight Max (txt ":home -- (q)uit (u)nfold (t)weet (C-t)reply (f)avorite (C-n)next tab")),
            txt " "]
        minibuffer Notification =
          let (w,h) = client ^. screenSize in
          translateBy (Location $ (0,h-2)) $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) (padRight Max (txt ":notification -- (C-n)next tab")),
            txt " "]

