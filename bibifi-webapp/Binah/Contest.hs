module Binah.Contest where

import Prelude
import Foundation.App (App, Handler)
import qualified Foundation.App as App
import Binah.Model
import Binah.Core
import Binah.Foundation.App (THandler, H)
import Binah.Actions
import Binah.Filters
import Binah.Infrastructure
import qualified Database.Persist as Persist
import Control.Monad (foldM)
import qualified Yesod
import Control.Monad.Reader (ReaderT)


{-@
ignore runDB
assume runDB :: forall <p:: user -> Bool, q :: user -> Bool>.
               TaggedT<p, q> user _ _ -> TaggedT<p, q> user _ _
@-}
runDB ::
  (Yesod.MonadHandler m, Yesod.MonadUnliftIO m, Yesod.HandlerSite m ~ App) =>
  (TaggedT user (ReaderT (Yesod.YesodPersistBackend App) m)) a
  -> TaggedT user m a
runDB f = do
  app <- liftT $ Yesod.getYesod
  liftT $ Persist.runPool
    (App.persistConfig app)
    (unTag f)
    (App.connPool app)


{-@
ignore foldT
assume foldT :: forall <p :: user -> Bool>.
  (_ -> _ -> TaggedT<p, {\_ -> False}> user _ _)
    -> _ -> _ -> TaggedT<p, {\_ -> False}> user _ _ @-}
foldT :: (Foldable t, Monad m) => (b -> a -> TaggedT user m b) -> b -> t a -> TaggedT user m b
foldT = foldM

{-@ checkTeams :: _ -> _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
checkTeams :: ContestId -> [Entity Team] -> THandler Bool
checkTeams contestId _ = returnTagged True
checkTeams contestId = foldT (\acc team -> do
        if acc then do
            teamId <- project teamId' team
            res <- runDB $ selectFirst (teamContestTeam' ==. teamId &&:
                                        teamContestContest' ==. contestId)
            return $ case res of
                -- Not signed up.
                Nothing ->
                    True
                -- Signed up.
                Just _ ->
                    False
        else
            return False
    ) True

{-@ LIQUID "--no-pattern-inline" @-}