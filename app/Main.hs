{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Lib
import Brick
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
import qualified Data.Map as M

modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' ref f = modifyMVar_ ref $ return . f

----

data ListView = ListView {
  _items :: [StreamingAPI],
  _index :: Int
  }

data Client = Client {
  _listView :: ListView,
  _previousKey :: Maybe Key,
  _mainProcess :: IO (),
  _shutdownProcess :: [ThreadId] -> IO (),
  _threadIds :: [ThreadId]
  }

makeLenses ''ListView
makeLenses ''Client

defClient :: Client
defClient = Client (ListView [] 0) Nothing (return ()) (\_ -> return ()) []

fetchTweetThread :: MVar Client -> IO ()
fetchTweetThread ref = do
  mgr <- newManager tlsManagerSettings
  t2 <- myThreadId
  modifyMVar' ref $ threadIds %~ (t2 :)

  runResourceT $ do
    src <- stream twInfo mgr userstream
    src C.$$+- CL.mapM_ (liftIO . getStream)

  where
    getStream :: StreamingAPI -> IO ()
    getStream t = do
      case t of
        z@(SStatus tw) -> do
          modifyMVar' ref $ listView . items %~ (z :)
        z@(SRetweetedStatus tw) -> do
          modifyMVar' ref $ listView . items %~ (z :)
        _ -> return ()

      client <- readMVar ref
      client ^. mainProcess

fetchKeyEventThread :: Vty -> MVar Client -> IO ()
fetchKeyEventThread vty ref = do
  e <- nextEvent vty
  case e of
    (EvKey key _) -> do
      modifyMVar' ref $ previousKey .~ Just key

      client <- readMVar ref
      client ^. mainProcess
    _ -> return ()

  fetchKeyEventThread vty ref

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  ref <- newMVar $ defClient
  showCursor $ outputIface vty

  modifyMVar' ref $ mainProcess .~ mainloop vty ref
  modifyMVar' ref $ shutdownProcess .~ \tids -> do
    shutdown vty
    mapM_ killThread tids
  
  t1 <- forkIO $ fetchKeyEventThread vty ref
  modifyMVar' ref $ threadIds .~ [t1]
  fetchTweetThread ref

mainloop :: Vty -> MVar Client -> IO ()
mainloop vty ref = do
  listViewScroll

  (regionW, regionH) <- displayBounds =<< outputForConfig =<< standardIOConfig
  listImg <- getListImg

  let minibuffer = translateY (regionH-2) $
        charFill (defAttr `withBackColor` brightWhite) ' ' regionW 1
        <-> string defAttr ">"

  update vty (addToBottom listImg minibuffer)

  threadDelay 10
  checkQuit

  where
    checkQuit = do
      client <- readMVar ref
      when (client ^. previousKey == Just (KChar 'q')) $
        client ^. shutdownProcess $ client ^. threadIds
    
    listViewMaxItems = 10
    
    listViewScroll = do
      client <- readMVar ref
      
      case client ^. previousKey of
        Just (KChar 'j') | (client ^. listView ^. index + listViewMaxItems < length (client ^. listView ^. items)) -> modifyMVar' ref $ listView . index +~ 1
        Just (KChar 'k') | (client ^. listView ^. index > 0) -> modifyMVar' ref $ listView . index -~ 1
        _ -> return ()

    getListImg = do
      client <- readMVar ref
      let currentItms = drop (client^.listView^.index) $ take (client ^. listView ^. index + listViewMaxItems) $ client ^. listView . items
      return $ picForImage $ vertCat $ fmap statusImg currentItms

    statusImg (SStatus tw) = do
      string defAttr (T.unpack $ tw ^. statusUser ^. userName)
        <|> string defAttr " @"
        <|> string defAttr (T.unpack $ tw ^. statusUser ^. userScreenName)
      <-> string defAttr (" " ++ (T.unpack $ tw ^. statusText))
      <-> string defAttr "\n\n"
    statusImg (SRetweetedStatus tw) =
      string defAttr (T.unpack $ tw ^. rsUser ^. userScreenName)
        <|> string defAttr " RT "
        <|> statusImg (SStatus $ tw ^. rsRetweetedStatus)

