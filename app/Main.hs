{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
import Web.Twitter.Conduit hiding (map, index)
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
  deriving (Eq, Show)

getStatusReplies :: Status -> (StatusId -> IO Status) -> IO Timeline
getStatusReplies status getter = TStatusReply status False <$> go status
  where
    go s = do
      let mid = s ^. statusInReplyToStatusId
      case mid of
        Just i -> getter i >>= \s' -> (s':) <$> go s'
        Nothing -> return []

data ClientMode = TLView | Tweet
  deriving (Eq, Show)

data ListView = ListView {
  _items :: [Timeline],
  _index :: Int
  }

data Client = Client {
  _listView :: ListView,
  _screenSize :: (Int,Int),
  _tweetbox :: Editor T.Text String,
  _clientMode :: ClientMode
  }

makeLenses ''ListView
makeLenses ''Client

defClient :: Client
defClient = Client (ListView [] 0) (0,0) (editorText "tweetbox" (vBox . fmap txt) (Just 5) "") TLView

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
        _ -> return ()

textWrap :: Int -> T.Text -> T.Text
textWrap w text
  | T.length text == 0 = T.empty
  | otherwise = let (x,xs) = T.splitAt maxWidth text in x `T.append` "\n" `T.append` textWrap w xs
  where
    maxWidth = maximum [n | n <- [0..T.length text], textWidth (T.take n text) < w]

main = do
  mgr <- newManager tlsManagerSettings
  let request = call twInfo mgr . update :: T.Text -> IO Status
  
  channel <- newChan
  forkIO $ fetchTweetThread channel

  size <- displayBounds =<< outputForConfig =<< standardIOConfig

  customMain
    (standardIOConfig >>= mkVty)
    (Just channel)
    (app request)
    (defClient & screenSize .~ size)

  where
    app :: (T.Text -> IO r) -> App Client Timeline String
    app request = App widgets cursor (eventHandler request) return def

    transparent = Attr KeepCurrent KeepCurrent KeepCurrent
    fore = withForeColor
    back = withBackColor
    style = withStyle
    resetStyle (Attr _ x y) = Attr Default x y

    onAttr :: Attr -> Widget n -> Widget n
    onAttr tt = withAttr uid . updateAttrMap (applyAttrMappings [(uid, tt)])
      where uid = attrName "unique-attr-id"

    cursor client xs = case client ^. clientMode of
      TLView -> Just $ CursorLocation (Location (0,0)) Nothing
      Tweet -> Just $ head xs

    eventHandler request client ev = case client ^. clientMode of
      TLView -> tlviewHandler client ev
      Tweet -> editKeyHandler request client ev

    editKeyHandler request client ev =
      case ev of
        VtyEvent (EvKey (KChar ch) []) -> continue $ client & tweetbox . editContentsL %~ insertChar ch
        VtyEvent (EvKey (KChar 'q') ms) | MCtrl `elem` ms -> continue $ client & clientMode .~ TLView
        VtyEvent (EvKey (KChar 'c') ms) | MCtrl `elem` ms -> do
          liftIO $ request $ foldl1 (\x y -> x `T.append` "\n" `T.append` y) $ getEditContents $ client ^. tweetbox
          continue $ client & tweetbox . editContentsL .~ textZipper [] Nothing
        VtyEvent event -> handleEventLensed client tweetbox handleEditorEvent event >>= continue
        _ -> continue client

    tlviewHandler client ev =
      let i = client ^. listView ^. index;
          ts = client ^. listView ^. items in
      case ev of
        VtyEvent (EvKey (KChar 'q') _) -> halt client

        -- j,k移動はindexが端点の時はカーソルだけ動かす感じにしたい
        -- ページをまたぐときは上端と下端で固定する案
        VtyEvent (EvKey (KChar 'j') _) | i - 1 < length ts -> continue $ client & listView . index +~ 1
        VtyEvent (EvKey (KChar 'k') _) | i > 0 -> continue $ client & listView . index -~ 1
        VtyEvent (EvKey (KChar '<') _) -> continue $ client & listView . index .~ 0
        VtyEvent (EvKey (KChar 'r') _) -> continue $ client & listView . items . ix i %~ \t ->
          case t of
            TStatusReply tw b tws -> TStatusReply tw (not b) tws
            z -> z
        VtyEvent (EvKey (KChar 't') _) -> continue $ client & clientMode .~ Tweet
        AppEvent tw -> continue $ client
          & listView . items %~ (tw :)
          & listView . index %~ (\i -> if i == 0 then 0 else i + 1)
        _ -> continue client

    widgets client = case client ^. clientMode of
      TLView -> [tweetList, minibuffer]
      Tweet -> [tweetList, tweetEditBox, minibuffer]
      where
        tweetEditBox =
          let (w,h) = client ^. screenSize in
          translateBy (Location (0, h-8)) $ vLimit 6 $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) $ padRight Max $ txt ":tweetbox (C-c)update (C-q)quit",
            renderEditor True $ client ^. tweetbox]
        
        tweetList =
          let (w,h) = client ^. screenSize in
          cropBottomBy 2 $ vBox $ fmap (\(i,st) -> renderTweet (i == 0) st) $ zip [0..] $ drop (client ^. listView ^. index) $ client ^. listView ^. items

        renderTweet = go where
          scrName sn =
            onAttr (resetStyle transparent) $ (onAttr (transparent `fore` red `style` underline) $ txt "@" <+> txt sn) <+> txt " "
          dspName n = txt n
          name user =
            scrName (user ^. userScreenName)
            <+> txt "(" <+> dspName (user ^. userName) <+> txt ")"
          nameRT rtw =
            txt "RT "
            <+> name (rtw ^. rsRetweetedStatus ^. statusUser)
            <+> txt " by "
            <+> name (rtw ^. rsUser)
          tweetContent = textWrap (client ^. screenSize ^. _1 - 3)

          -- screenNameだけが反転しない
          -- 真面目にattrMap使わないとだめ？
          inverted =
            onAttr (transparent `fore` black `back` brightWhite) . padRight Max

          go b (TStatus tw) = padLeft (Pad 1) $
            (name $ tw ^. statusUser)
            <=> (txt " " <+> txt (tweetContent $ tw ^. statusText))
            <=> txt " "
          go b (TStatusRT rtw) = padLeft (Pad 1) $
            (nameRT rtw)
            <=> (txt " " <+> txt (tweetContent $ rtw ^. rsRetweetedStatus ^. statusText))
            <=> txt " "
          go b (TStatusReply tw unf tws) =
            if unf
            then vBox $ fmap (\(i,t) -> padLeft (Pad i) $ go (i == 0) $ TStatus t) $ zip [0..] $ tw:tws
            else go b (TStatus tw)

        minibuffer =
          let (w,h) = client ^. screenSize in
          translateBy (Location $ (0,h-2)) $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) (padRight Max (txt ":home")),
            txt "(q)uit (j)Next (k)Previous (<)Goto Top (r)eply (t)weet"]

