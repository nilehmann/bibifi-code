module Binah.Test where

import Prelude
import Foundation.App (Handler)

import Binah.Model
import Binah.Core
import Binah.Infrastructure

{-@ ok :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok :: Monad m => TaggedT (Entity User) m Bool
ok = return True

{-@ err :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
err :: TaggedT (Entity User) Handler Bool
err = return True