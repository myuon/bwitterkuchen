{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Lib
import Credential
import Brick
import Brick.Util
import Graphics.Vty (mkVty, outputForConfig)
import Graphics.Vty.Config
import Graphics.Vty.Output.Interface
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Web.Twitter.Conduit hiding (update, map, index)
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

data ListView = ListView {
  _items :: [StreamingAPI],
  _index :: Int
  }

data Client = Client {
  _listView :: ListView,
  _screenSize :: (Int,Int)
  }

makeLenses ''ListView
makeLenses ''Client

defClient :: Client
defClient = Client (ListView [] 0) (0,0)

fetchTweetThread :: Chan StreamingAPI -> IO ()
fetchTweetThread channel = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    src <- stream twInfo mgr $ statusesFilterByTrack "cat" --userstream
    src C.$$+- CL.mapM_ (liftIO . getStream)

  where
    getStream :: StreamingAPI -> IO ()
    getStream t = do
      case t of
        z@(SStatus tw) -> writeChan channel z
        z@(SRetweetedStatus tw) -> writeChan channel z
        _ -> return ()

textWrap :: Int -> T.Text -> T.Text
textWrap w text
  | T.length text == 0 = T.empty
  | otherwise = let (x,xs) = T.splitAt w text in x `T.append` "\n" `T.append` textWrap w xs

main = do
  channel <- newChan
  forkIO $ fetchTweetThread channel

  size <- displayBounds =<< outputForConfig =<< standardIOConfig

  customMain
    (standardIOConfig >>= mkVty)
    (Just channel)
    app
    (defClient & screenSize .~ size)

  where
    app :: App Client StreamingAPI String
    app = App widgets cursor eventHandler return def

    transparent = Attr KeepCurrent KeepCurrent KeepCurrent
    fore = withForeColor
    back = withBackColor
    style = withStyle
    resetStyle (Attr _ x y) = Attr Default x y

    onAttr :: Attr -> Widget n -> Widget n
    onAttr tt = withAttr uid . updateAttrMap (applyAttrMappings [(uid, tt)])
      where uid = attrName "unique-attr-id"

    cursor client xs = Just $ CursorLocation (Location (0,0)) Nothing

    eventHandler client ev =
      let i = client ^. listView ^. index;
          ts = client ^. listView ^. items in
      case ev of
        VtyEvent (EvKey (KChar 'q') _) -> halt client
        VtyEvent (EvKey (KChar 'j') _) | i - 1 < length ts -> continue $ client & listView . index +~ 1
        VtyEvent (EvKey (KChar 'k') _) | i > 0 -> continue $ client & listView . index -~ 1
        VtyEvent (EvKey (KChar '<') _) -> continue $ client & listView . index .~ 0
        AppEvent tw -> continue $ client
          & listView . items %~ (tw :)
          & listView . index %~ (\i -> if i == 0 then 0 else i + 1)
        _ -> continue client

    widgets client = [tweetList, minibuffer]
      where
        tweetList =
          let (w,h) = client ^. screenSize in
          cropBottomBy 2 $ vBox $ fmap (\(i,st) -> renderTweet (i == 0) st) $ zip [0..] $ drop (client ^. listView ^. index) $ client ^. listView ^. items

        renderTweet :: Bool -> StreamingAPI -> Widget String
        renderTweet = go where
          scrName sn =
            onAttr (resetStyle transparent) $ (onAttr (transparent `fore` red `style` underline) $ txt "@" <+> txt sn) <+> txt " "
          dspName n = txt n
          name user =
            scrName (user ^. userScreenName)
            <+> txt "(" <+> dspName (user ^. userName) <+> txt ")"
          nameRT rtw =
            name (rtw ^. rsUser)
            <+> txt " RT "
            <+> name (rtw ^. rsRetweetedStatus ^. statusUser)
          tweetContent = textWrap (client ^. screenSize ^. _1 - 3)

          -- screenNameだけが反転しない
          -- 真面目にattrMap使わないとだめ？
          inverted =
            onAttr (transparent `fore` black `back` brightWhite) . padRight Max

          go b (SStatus tw) = padLeft (Pad 1) $
            (name $ tw ^. statusUser)
            <=> (txt " " <+> txt (tweetContent $ tw ^. statusText))
            <=> txt " "
          go b (SRetweetedStatus rtw) = padLeft (Pad 1) $
            (nameRT rtw)
            <=> (txt " " <+> txt (tweetContent $ rtw ^. rsRetweetedStatus ^. statusText))
            <=> txt " "

        minibuffer =
          let (w,h) = client ^. screenSize in
          translateBy (Location $ (0,h-2)) $ vBox [
            onAttr (transparent `back` brightWhite `fore` black) (padRight Max (txt ":home")),
            txt "(j)Next (k)Previous (<)Goto topmost"]

