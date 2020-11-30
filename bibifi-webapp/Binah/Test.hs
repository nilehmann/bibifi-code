module Binah.Test where

import Prelude
import Foundation.App (Handler)

import Binah.Model
import Binah.Core
import Binah.Infrastructure

{-@ ok1 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok1 :: Monad m => TaggedT (Entity User) m Bool
ok1 = return True

{-@ ok2 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok2 :: TaggedT user Handler Bool
ok2 = return True

{-@ err :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
err :: TaggedT (Entity User) Handler Bool
err = return True