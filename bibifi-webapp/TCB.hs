module TCB (raiseUserLabel, maybeAuth, maybeAuthId, requireAuth, requireAuthId, raiseTeamLabel, raiseJudgeLabel, raiseGroupLabel) where

import qualified Database.Esqueleto as E
import Database.LPersist
import LMonad.Label.DisjunctionCategory
import LMonad.TCB
import Prelude
import Yesod (Entity(..), MonadTrans(..))
import qualified Yesod.Auth as Yesod

import Foundation
import Foundation.App
import Model

raiseUserLabel :: LHandler ()
raiseUserLabel = maybeAuth >> return ()

raiseUserLabel' :: Entity User -> LHandler ()
raiseUserLabel' (Entity userId user) = 
    let label' = dcSingleton $ PrincipalUser userId in
    let label = if userAdmin user then
            lub label' $ dcSingleton PrincipalAdmin
          else
            label'
    in
    raiseClearanceTCB label

maybeAuth :: LHandler (Maybe (Entity User))
maybeAuth = do
    userM <- lLift Yesod.maybeAuth
    case userM of
        Nothing -> 
            return ()
        Just user -> do
            raiseUserLabel' user
    return userM

requireAuth :: LHandler (Entity User)
requireAuth = do
    user <- lLift Yesod.requireAuth
    raiseUserLabel' user
    return user

requireAuthId :: LHandler UserId
requireAuthId = 
    requireAuth >>= \(Entity uId _) -> return uId

maybeAuthId :: LHandler (Maybe (UserId))
maybeAuthId = 
    maybeAuth >>= (return . maybe Nothing (\(Entity uId _) -> Just uId))

raiseTeamLabel :: LHandler ()
raiseTeamLabel =
    maybeAuthId >>= ( maybe (return ()) $ \uId -> do
        res <- runDB $ E.select $ E.from $ \(tm `E.RightOuterJoin` t `E.InnerJoin` tc) -> do
            E.on ( t E.^. TeamId E.==. tc E.^. TeamContestTeam)
            E.on ( tm E.?. TeamMemberTeam E.==. E.just (t E.^. TeamId))
            E.where_ ( tm E.?. TeamMemberUser E.==. E.just (E.val uId) E.||. t E.^. TeamLeader E.==. E.val uId)
            return $ tc E.^. TeamContestId
        mapM_ (\(E.Value tcId) -> raiseClearanceTCB $ dcSingleton $ PrincipalTeam tcId) res
      )

raiseGroupLabel :: LHandler ()
raiseGroupLabel = 
    maybeAuthId >>= ( maybe (return ()) $ \uId -> do
        res <- runDB $ E.select $ E.from $ \(E.LeftOuterJoin t tm) -> do
            E.on ( E.just (t E.^. TeamId) E.==. tm E.?. TeamMemberTeam)
            E.where_ ( t E.^. TeamLeader E.==. E.val uId E.||. tm E.?. TeamMemberUser E.==. E.just (E.val uId))
            return $ t E.^. TeamId
        mapM_ (\(E.Value tId) -> raiseClearanceTCB $ dcSingleton $ PrincipalGroup tId) res
      )

raiseJudgeLabel :: LHandler ()
raiseJudgeLabel = 
    maybeAuthId >>= ( maybe (return ()) $ \uId -> do
        res <- runDB $ E.select $ E.from $ \ j -> do
            E.where_ ( j E.^. JudgeJudge E.==. E.val uId)
            return ( j E.^. JudgeId)
        mapM_ (\(E.Value jId) -> raiseClearanceTCB $ dcSingleton $ PrincipalJudge jId) res
      )

-- instance MonadTrans (LMonadT (DCLabel Principal)) where
--     -- lift :: forall m a. LMonad m => m a -> LMonadT (DCLabel Principal) m a
--     lift = LMonadT . lift

{-@ LIQUID "--compile-spec" @-}
