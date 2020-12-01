module Binah.Foundation.App where

import Prelude
import Foundation.App ( Handler )
import Binah.Infrastructure (TaggedT, MonadTIO(..), runTIO, unTag, liftT)
import Yesod
import Control.Monad.Logger (MonadLogger(..))
import Binah.Core
import Binah.Model (User)

type THandler = TaggedT (Entity User) Handler

{-@ runTHandler :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ -> _ @-}
runTHandler :: THandler a -> Handler a
runTHandler = unTag

instance MonadIO m => MonadIO (TaggedT user m) where
  liftIO ma = liftT $ liftIO ma

instance MonadResource m => MonadResource (TaggedT user m) where
  liftResourceT = liftT . liftResourceT

instance MonadLogger m => MonadLogger (TaggedT user m) where
  monadLoggerLog a b c d = liftT $ monadLoggerLog a b c d

instance MonadHandler m => MonadHandler (TaggedT user m) where
  type HandlerSite (TaggedT user m) = HandlerSite m
  liftHandler act = liftT (liftHandler act)

instance MonadTIO Handler where
  liftTIO ma = liftIO $ runTIO ma

{-@ LIQUID "--compile-spec" @-}