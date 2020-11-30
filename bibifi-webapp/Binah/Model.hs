{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--compile-spec" @-}

module Binah.Model
  ( migrateAll
  , mkUser
  , mkContest
  , mkTeam
  , mkTeamContest
  , mkStoredFile
  , User
  , Contest
  , Team
  , TeamContest
  , StoredFile
  , userId'
  , userIdent'
  , userPassword'
  , userSalt'
  , userEmail'
  , userCreated'
  , userAdmin'
  , userConsentForm'
  , userResume'
  , contestId'
  , contestUrl'
  , contestTitle'
  , contestBuildStart'
  , contestBuildEnd'
  , contestBreakFixStart'
  , contestBreakEnd'
  , contestFixEnd'
  , teamId'
  , teamName'
  , teamLeader'
  , teamContestId'
  , teamContestTeam'
  , teamContestContest'
  , teamContestGitUrl'
  , teamContestLanguages'
  , teamContestProfessional'
  , teamContestGithookNonce'
  , teamContestGitRepositoryIdentifier'
  , storedFileId'
  , storedFileOwner'
  , storedFileName'
  , storedFileContentType'
  , storedFileContent'
  , UserId
  , ContestId
  , TeamId
  , TeamContestId
  , StoredFileId
  )
where


import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import qualified Database.Persist              as Persist

import           Binah.Core

import Prelude
import Data.Text (Text)
import Data.Time (UTCTime) -- default=now() AT TIME ZONE ‘UTC’
import Data.ByteString (ByteString)
import Core.Git

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  ident Text
  password Text
  salt Text
  email Text
  created UTCTime
  admin Bool
  consentForm StoredFileId Maybe
  resume StoredFileId Maybe
  UniqueUser ident
 UniqueEmail email

Contest
  url Text
  title Text
  buildStart UTCTime
  buildEnd UTCTime
  breakFixStart UTCTime
  breakEnd UTCTime
  fixEnd UTCTime
  UniqueContest url

Team
  name Text
  leader UserId
  UniqueTeam name

TeamContest
  team TeamId
  contest ContestId
  gitUrl Text
  languages Text
  professional Bool
  githookNonce Text
  gitRepositoryIdentifier RepositoryIdentifier Maybe
  UniqueTeamContest team contest

StoredFile
  owner UserId
  name Text
  contentType Text
  content ByteString

|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: Text
     -> x_1: Text
     -> x_2: Text
     -> x_3: Text
     -> x_4: UTCTime
     -> x_5: Bool
     -> x_6: Maybe StoredFileId
     -> x_7: Maybe StoredFileId
     -> BinahRecord <{\row -> userIdent (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userSalt (entityVal row) == x_2 && userEmail (entityVal row) == x_3 && userCreated (entityVal row) == x_4 && userAdmin (entityVal row) == x_5 && userConsentForm (entityVal row) == x_6 && userResume (entityVal row) == x_7},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) User
  @-}
mkUser :: Text -> Text -> Text -> Text -> UTCTime -> Bool -> Maybe StoredFileId -> Maybe StoredFileId -> BinahRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) User UserId
  @-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userIdent :: User -> Text @-}

{-@ measure userIdentCap :: Entity User -> Bool @-}

{-@ assume userIdent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userIdent (entityVal row)},
                          {\field row -> field == userIdent (entityVal row)},
                          {\old -> userIdentCap old},
                          {\old _ _ -> userIdentCap old}>
                          (Entity User) User Text
  @-}
userIdent' :: EntityFieldWrapper (Entity User) User Text
userIdent' = EntityFieldWrapper UserIdent

{-@ measure userPassword :: User -> Text @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userPassword (entityVal row)},
                          {\field row -> field == userPassword (entityVal row)},
                          {\old -> userPasswordCap old},
                          {\old _ _ -> userPasswordCap old}>
                          (Entity User) User Text
  @-}
userPassword' :: EntityFieldWrapper (Entity User) User Text
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userSalt :: User -> Text @-}

{-@ measure userSaltCap :: Entity User -> Bool @-}

{-@ assume userSalt' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userSalt (entityVal row)},
                          {\field row -> field == userSalt (entityVal row)},
                          {\old -> userSaltCap old},
                          {\old _ _ -> userSaltCap old}>
                          (Entity User) User Text
  @-}
userSalt' :: EntityFieldWrapper (Entity User) User Text
userSalt' = EntityFieldWrapper UserSalt

{-@ measure userEmail :: User -> Text @-}

{-@ measure userEmailCap :: Entity User -> Bool @-}

{-@ assume userEmail' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userEmail (entityVal row)},
                          {\field row -> field == userEmail (entityVal row)},
                          {\old -> userEmailCap old},
                          {\old _ _ -> userEmailCap old}>
                          (Entity User) User Text
  @-}
userEmail' :: EntityFieldWrapper (Entity User) User Text
userEmail' = EntityFieldWrapper UserEmail

{-@ measure userCreated :: User -> UTCTime @-}

{-@ measure userCreatedCap :: Entity User -> Bool @-}

{-@ assume userCreated' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userCreated (entityVal row)},
                          {\field row -> field == userCreated (entityVal row)},
                          {\old -> userCreatedCap old},
                          {\old _ _ -> userCreatedCap old}>
                          (Entity User) User UTCTime
  @-}
userCreated' :: EntityFieldWrapper (Entity User) User UTCTime
userCreated' = EntityFieldWrapper UserCreated

{-@ measure userAdmin :: User -> Bool @-}

{-@ measure userAdminCap :: Entity User -> Bool @-}

{-@ assume userAdmin' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userAdmin (entityVal row)},
                          {\field row -> field == userAdmin (entityVal row)},
                          {\old -> userAdminCap old},
                          {\old _ _ -> userAdminCap old}>
                          (Entity User) User Bool
  @-}
userAdmin' :: EntityFieldWrapper (Entity User) User Bool
userAdmin' = EntityFieldWrapper UserAdmin

{-@ measure userConsentForm :: User -> (Maybe StoredFileId) @-}

{-@ measure userConsentFormCap :: Entity User -> Bool @-}

{-@ assume userConsentForm' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userConsentForm (entityVal row)},
                          {\field row -> field == userConsentForm (entityVal row)},
                          {\old -> userConsentFormCap old},
                          {\old _ _ -> userConsentFormCap old}>
                          (Entity User) User (Maybe StoredFileId)
  @-}
userConsentForm' :: EntityFieldWrapper (Entity User) User (Maybe StoredFileId)
userConsentForm' = EntityFieldWrapper UserConsentForm

{-@ measure userResume :: User -> (Maybe StoredFileId) @-}

{-@ measure userResumeCap :: Entity User -> Bool @-}

{-@ assume userResume' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userResume (entityVal row)},
                          {\field row -> field == userResume (entityVal row)},
                          {\old -> userResumeCap old},
                          {\old _ _ -> userResumeCap old}>
                          (Entity User) User (Maybe StoredFileId)
  @-}
userResume' :: EntityFieldWrapper (Entity User) User (Maybe StoredFileId)
userResume' = EntityFieldWrapper UserResume

-- * Contest
{-@ mkContest ::
        x_0: Text
     -> x_1: Text
     -> x_2: UTCTime
     -> x_3: UTCTime
     -> x_4: UTCTime
     -> x_5: UTCTime
     -> x_6: UTCTime
     -> BinahRecord <{\row -> contestUrl (entityVal row) == x_0 && contestTitle (entityVal row) == x_1 && contestBuildStart (entityVal row) == x_2 && contestBuildEnd (entityVal row) == x_3 && contestBreakFixStart (entityVal row) == x_4 && contestBreakEnd (entityVal row) == x_5 && contestFixEnd (entityVal row) == x_6},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Contest
  @-}
mkContest :: Text -> Text -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> BinahRecord (Entity User) Contest
mkContest x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (Contest x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity Contest | v == getJust (entityKey v)} @-}



{-@ assume contestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Contest ContestId
  @-}
contestId' :: EntityFieldWrapper (Entity User) Contest ContestId
contestId' = EntityFieldWrapper ContestId

{-@ measure contestUrl :: Contest -> Text @-}

{-@ measure contestUrlCap :: Entity Contest -> Bool @-}

{-@ assume contestUrl' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestUrl (entityVal row)},
                          {\field row -> field == contestUrl (entityVal row)},
                          {\old -> contestUrlCap old},
                          {\old _ _ -> contestUrlCap old}>
                          (Entity User) Contest Text
  @-}
contestUrl' :: EntityFieldWrapper (Entity User) Contest Text
contestUrl' = EntityFieldWrapper ContestUrl

{-@ measure contestTitle :: Contest -> Text @-}

{-@ measure contestTitleCap :: Entity Contest -> Bool @-}

{-@ assume contestTitle' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestTitle (entityVal row)},
                          {\field row -> field == contestTitle (entityVal row)},
                          {\old -> contestTitleCap old},
                          {\old _ _ -> contestTitleCap old}>
                          (Entity User) Contest Text
  @-}
contestTitle' :: EntityFieldWrapper (Entity User) Contest Text
contestTitle' = EntityFieldWrapper ContestTitle

{-@ measure contestBuildStart :: Contest -> UTCTime @-}

{-@ measure contestBuildStartCap :: Entity Contest -> Bool @-}

{-@ assume contestBuildStart' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestBuildStart (entityVal row)},
                          {\field row -> field == contestBuildStart (entityVal row)},
                          {\old -> contestBuildStartCap old},
                          {\old _ _ -> contestBuildStartCap old}>
                          (Entity User) Contest UTCTime
  @-}
contestBuildStart' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestBuildStart' = EntityFieldWrapper ContestBuildStart

{-@ measure contestBuildEnd :: Contest -> UTCTime @-}

{-@ measure contestBuildEndCap :: Entity Contest -> Bool @-}

{-@ assume contestBuildEnd' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestBuildEnd (entityVal row)},
                          {\field row -> field == contestBuildEnd (entityVal row)},
                          {\old -> contestBuildEndCap old},
                          {\old _ _ -> contestBuildEndCap old}>
                          (Entity User) Contest UTCTime
  @-}
contestBuildEnd' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestBuildEnd' = EntityFieldWrapper ContestBuildEnd

{-@ measure contestBreakFixStart :: Contest -> UTCTime @-}

{-@ measure contestBreakFixStartCap :: Entity Contest -> Bool @-}

{-@ assume contestBreakFixStart' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestBreakFixStart (entityVal row)},
                          {\field row -> field == contestBreakFixStart (entityVal row)},
                          {\old -> contestBreakFixStartCap old},
                          {\old _ _ -> contestBreakFixStartCap old}>
                          (Entity User) Contest UTCTime
  @-}
contestBreakFixStart' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestBreakFixStart' = EntityFieldWrapper ContestBreakFixStart

{-@ measure contestBreakEnd :: Contest -> UTCTime @-}

{-@ measure contestBreakEndCap :: Entity Contest -> Bool @-}

{-@ assume contestBreakEnd' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestBreakEnd (entityVal row)},
                          {\field row -> field == contestBreakEnd (entityVal row)},
                          {\old -> contestBreakEndCap old},
                          {\old _ _ -> contestBreakEndCap old}>
                          (Entity User) Contest UTCTime
  @-}
contestBreakEnd' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestBreakEnd' = EntityFieldWrapper ContestBreakEnd

{-@ measure contestFixEnd :: Contest -> UTCTime @-}

{-@ measure contestFixEndCap :: Entity Contest -> Bool @-}

{-@ assume contestFixEnd' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestFixEnd (entityVal row)},
                          {\field row -> field == contestFixEnd (entityVal row)},
                          {\old -> contestFixEndCap old},
                          {\old _ _ -> contestFixEndCap old}>
                          (Entity User) Contest UTCTime
  @-}
contestFixEnd' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestFixEnd' = EntityFieldWrapper ContestFixEnd

-- * Team
{-@ mkTeam ::
        x_0: Text
     -> x_1: UserId
     -> BinahRecord <{\row -> teamName (entityVal row) == x_0 && teamLeader (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Team
  @-}
mkTeam :: Text -> UserId -> BinahRecord (Entity User) Team
mkTeam x_0 x_1 = BinahRecord (Team x_0 x_1)

{-@ invariant {v: Entity Team | v == getJust (entityKey v)} @-}



{-@ assume teamId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Team TeamId
  @-}
teamId' :: EntityFieldWrapper (Entity User) Team TeamId
teamId' = EntityFieldWrapper TeamId

{-@ measure teamName :: Team -> Text @-}

{-@ measure teamNameCap :: Entity Team -> Bool @-}

{-@ assume teamName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamName (entityVal row)},
                          {\field row -> field == teamName (entityVal row)},
                          {\old -> teamNameCap old},
                          {\old _ _ -> teamNameCap old}>
                          (Entity User) Team Text
  @-}
teamName' :: EntityFieldWrapper (Entity User) Team Text
teamName' = EntityFieldWrapper TeamName

{-@ measure teamLeader :: Team -> UserId @-}

{-@ measure teamLeaderCap :: Entity Team -> Bool @-}

{-@ assume teamLeader' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamLeader (entityVal row)},
                          {\field row -> field == teamLeader (entityVal row)},
                          {\old -> teamLeaderCap old},
                          {\old _ _ -> teamLeaderCap old}>
                          (Entity User) Team UserId
  @-}
teamLeader' :: EntityFieldWrapper (Entity User) Team UserId
teamLeader' = EntityFieldWrapper TeamLeader

-- * TeamContest
{-@ mkTeamContest ::
        x_0: TeamId
     -> x_1: ContestId
     -> x_2: Text
     -> x_3: Text
     -> x_4: Bool
     -> x_5: Text
     -> x_6: Maybe RepositoryIdentifier
     -> BinahRecord <{\row -> teamContestTeam (entityVal row) == x_0 && teamContestContest (entityVal row) == x_1 && teamContestGitUrl (entityVal row) == x_2 && teamContestLanguages (entityVal row) == x_3 && teamContestProfessional (entityVal row) == x_4 && teamContestGithookNonce (entityVal row) == x_5 && teamContestGitRepositoryIdentifier (entityVal row) == x_6},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TeamContest
  @-}
mkTeamContest :: TeamId -> ContestId -> Text -> Text -> Bool -> Text -> Maybe RepositoryIdentifier -> BinahRecord (Entity User) TeamContest
mkTeamContest x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (TeamContest x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity TeamContest | v == getJust (entityKey v)} @-}



{-@ assume teamContestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TeamContest TeamContestId
  @-}
teamContestId' :: EntityFieldWrapper (Entity User) TeamContest TeamContestId
teamContestId' = EntityFieldWrapper TeamContestId

{-@ measure teamContestTeam :: TeamContest -> TeamId @-}

{-@ measure teamContestTeamCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestTeam (entityVal row)},
                          {\field row -> field == teamContestTeam (entityVal row)},
                          {\old -> teamContestTeamCap old},
                          {\old _ _ -> teamContestTeamCap old}>
                          (Entity User) TeamContest TeamId
  @-}
teamContestTeam' :: EntityFieldWrapper (Entity User) TeamContest TeamId
teamContestTeam' = EntityFieldWrapper TeamContestTeam

{-@ measure teamContestContest :: TeamContest -> ContestId @-}

{-@ measure teamContestContestCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestContest (entityVal row)},
                          {\field row -> field == teamContestContest (entityVal row)},
                          {\old -> teamContestContestCap old},
                          {\old _ _ -> teamContestContestCap old}>
                          (Entity User) TeamContest ContestId
  @-}
teamContestContest' :: EntityFieldWrapper (Entity User) TeamContest ContestId
teamContestContest' = EntityFieldWrapper TeamContestContest

{-@ measure teamContestGitUrl :: TeamContest -> Text @-}

{-@ measure teamContestGitUrlCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestGitUrl' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestGitUrl (entityVal row)},
                          {\field row -> field == teamContestGitUrl (entityVal row)},
                          {\old -> teamContestGitUrlCap old},
                          {\old _ _ -> teamContestGitUrlCap old}>
                          (Entity User) TeamContest Text
  @-}
teamContestGitUrl' :: EntityFieldWrapper (Entity User) TeamContest Text
teamContestGitUrl' = EntityFieldWrapper TeamContestGitUrl

{-@ measure teamContestLanguages :: TeamContest -> Text @-}

{-@ measure teamContestLanguagesCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestLanguages' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestLanguages (entityVal row)},
                          {\field row -> field == teamContestLanguages (entityVal row)},
                          {\old -> teamContestLanguagesCap old},
                          {\old _ _ -> teamContestLanguagesCap old}>
                          (Entity User) TeamContest Text
  @-}
teamContestLanguages' :: EntityFieldWrapper (Entity User) TeamContest Text
teamContestLanguages' = EntityFieldWrapper TeamContestLanguages

{-@ measure teamContestProfessional :: TeamContest -> Bool @-}

{-@ measure teamContestProfessionalCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestProfessional' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestProfessional (entityVal row)},
                          {\field row -> field == teamContestProfessional (entityVal row)},
                          {\old -> teamContestProfessionalCap old},
                          {\old _ _ -> teamContestProfessionalCap old}>
                          (Entity User) TeamContest Bool
  @-}
teamContestProfessional' :: EntityFieldWrapper (Entity User) TeamContest Bool
teamContestProfessional' = EntityFieldWrapper TeamContestProfessional

{-@ measure teamContestGithookNonce :: TeamContest -> Text @-}

{-@ measure teamContestGithookNonceCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestGithookNonce' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestGithookNonce (entityVal row)},
                          {\field row -> field == teamContestGithookNonce (entityVal row)},
                          {\old -> teamContestGithookNonceCap old},
                          {\old _ _ -> teamContestGithookNonceCap old}>
                          (Entity User) TeamContest Text
  @-}
teamContestGithookNonce' :: EntityFieldWrapper (Entity User) TeamContest Text
teamContestGithookNonce' = EntityFieldWrapper TeamContestGithookNonce

{-@ measure teamContestGitRepositoryIdentifier :: TeamContest -> (Maybe RepositoryIdentifier) @-}

{-@ measure teamContestGitRepositoryIdentifierCap :: Entity TeamContest -> Bool @-}

{-@ assume teamContestGitRepositoryIdentifier' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamContestGitRepositoryIdentifier (entityVal row)},
                          {\field row -> field == teamContestGitRepositoryIdentifier (entityVal row)},
                          {\old -> teamContestGitRepositoryIdentifierCap old},
                          {\old _ _ -> teamContestGitRepositoryIdentifierCap old}>
                          (Entity User) TeamContest (Maybe RepositoryIdentifier)
  @-}
teamContestGitRepositoryIdentifier' :: EntityFieldWrapper (Entity User) TeamContest (Maybe RepositoryIdentifier)
teamContestGitRepositoryIdentifier' = EntityFieldWrapper TeamContestGitRepositoryIdentifier

-- * StoredFile
{-@ mkStoredFile ::
        x_0: UserId
     -> x_1: Text
     -> x_2: Text
     -> x_3: ByteString
     -> BinahRecord <{\row -> storedFileOwner (entityVal row) == x_0 && storedFileName (entityVal row) == x_1 && storedFileContentType (entityVal row) == x_2 && storedFileContent (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) StoredFile
  @-}
mkStoredFile :: UserId -> Text -> Text -> ByteString -> BinahRecord (Entity User) StoredFile
mkStoredFile x_0 x_1 x_2 x_3 = BinahRecord (StoredFile x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity StoredFile | v == getJust (entityKey v)} @-}



{-@ assume storedFileId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) StoredFile StoredFileId
  @-}
storedFileId' :: EntityFieldWrapper (Entity User) StoredFile StoredFileId
storedFileId' = EntityFieldWrapper StoredFileId

{-@ measure storedFileOwner :: StoredFile -> UserId @-}

{-@ measure storedFileOwnerCap :: Entity StoredFile -> Bool @-}

{-@ assume storedFileOwner' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == storedFileOwner (entityVal row)},
                          {\field row -> field == storedFileOwner (entityVal row)},
                          {\old -> storedFileOwnerCap old},
                          {\old _ _ -> storedFileOwnerCap old}>
                          (Entity User) StoredFile UserId
  @-}
storedFileOwner' :: EntityFieldWrapper (Entity User) StoredFile UserId
storedFileOwner' = EntityFieldWrapper StoredFileOwner

{-@ measure storedFileName :: StoredFile -> Text @-}

{-@ measure storedFileNameCap :: Entity StoredFile -> Bool @-}

{-@ assume storedFileName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == storedFileName (entityVal row)},
                          {\field row -> field == storedFileName (entityVal row)},
                          {\old -> storedFileNameCap old},
                          {\old _ _ -> storedFileNameCap old}>
                          (Entity User) StoredFile Text
  @-}
storedFileName' :: EntityFieldWrapper (Entity User) StoredFile Text
storedFileName' = EntityFieldWrapper StoredFileName

{-@ measure storedFileContentType :: StoredFile -> Text @-}

{-@ measure storedFileContentTypeCap :: Entity StoredFile -> Bool @-}

{-@ assume storedFileContentType' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == storedFileContentType (entityVal row)},
                          {\field row -> field == storedFileContentType (entityVal row)},
                          {\old -> storedFileContentTypeCap old},
                          {\old _ _ -> storedFileContentTypeCap old}>
                          (Entity User) StoredFile Text
  @-}
storedFileContentType' :: EntityFieldWrapper (Entity User) StoredFile Text
storedFileContentType' = EntityFieldWrapper StoredFileContentType

{-@ measure storedFileContent :: StoredFile -> ByteString @-}

{-@ measure storedFileContentCap :: Entity StoredFile -> Bool @-}

{-@ assume storedFileContent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == storedFileContent (entityVal row)},
                          {\field row -> field == storedFileContent (entityVal row)},
                          {\old -> storedFileContentCap old},
                          {\old _ _ -> storedFileContentCap old}>
                          (Entity User) StoredFile ByteString
  @-}
storedFileContent' :: EntityFieldWrapper (Entity User) StoredFile ByteString
storedFileContent' = EntityFieldWrapper StoredFileContent
