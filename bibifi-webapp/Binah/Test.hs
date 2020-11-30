module Binah.Test where

import Prelude
import Foundation.App (Handler)

import Binah.Model
import Binah.Core
import Binah.Infrastructure

-- I get a liquid type mismatch if I put TaggedT on top of Handler
{-@ err :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
err :: TaggedT (Entity User) Handler Bool
err = return True

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