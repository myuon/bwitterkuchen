{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module Tw where

import Tweets
import Control.Lens
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import qualified Data.Text as T
import System.Environment

main = do
  mgr <- newManager tlsManagerSettings
  let ?mgr = mgr
  let showStatus s = T.unpack $ T.concat ["@", s^.user^.screen_name, ": [", T.pack (show (s^.status_id)), "] ", s^.text]

  getArgs >>= \xs -> case xs of
    ("fetch":[]) -> mapM_ putStrLn . fmap showStatus =<< runAuth (fetchTimeline 20)
    ("fetch":n:_) -> mapM_ putStrLn . fmap showStatus =<< runAuth (fetchTimeline (read n))
    ("favo":n:_) -> putStrLn . showStatus =<< runAuth (favo (read n))
    ("tweet":txt:_) -> putStrLn . showStatus =<< runAuth (tweet txt)
    ("replyTo":n:txt:_) -> putStrLn . showStatus =<< runAuth (replyTo (read n) txt)
    _ -> print xs

