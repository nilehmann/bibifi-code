module Binah.Test where

import Prelude
import Foundation.App (Handler, App)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Reader (ReaderT)
import qualified Yesod

import Binah.Model (User)
import Binah.Core
import Binah.Infrastructure

-- This is now fixed
{-@ ok0 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok0 :: TaggedT (Entity User) Handler Bool
ok0 = return True


-- Putting TaggedT on top of generic monad works
{-@ ok1 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok1 :: Monad m => TaggedT (Entity User) m Bool
ok1 = return True

-- Putting TaggedT on top of another monad seems to work
data H a = H

instance Functor H where
  fmap = undefined

instance Applicative H where
  pure = undefined

instance Monad H where
  (>>=) = undefined

{-@ ok2 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok2 :: TaggedT (Entity User) H Bool
ok2 = return True

-- Making user polymorphic seems to work. Weird!
{-@ ok3 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok3 :: TaggedT user Handler Bool
ok3 = return True

-- returnTagged is a normal haskell function which has the same refined type than
-- return. Using it here instead of return works.
{-@ ok4 :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
ok4 :: TaggedT (Entity User) Handler Bool
ok4 = returnTagged True

{-@ LIQUID "--no-pattern-inline" @-}