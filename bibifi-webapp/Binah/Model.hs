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
  , mkCourseraUser
  , mkUserInformation
  , mkContest
  , mkCourseraContest
  , mkPost
  , mkPostDependency
  , mkJudge
  , mkJudgeConflict
  , mkBuildJudgement
  , mkBreakJudgement
  , mkFixJudgement
  , mkBreakDispute
  , mkTeam
  , mkTeamContest
  , mkTeamMember
  , mkTeamInvite
  , mkPasswordResetInvite
  , mkContestCoreTest
  , mkContestPerformanceTest
  , mkContestOptionalTest
  , mkTeamBreakScore
  , mkTeamBuildScore
  , mkOracleSubmission
  , mkBuildSubmission
  , mkBreakOracleSubmission
  , mkBreakSubmission
  , mkBreakFixSubmission
  , mkFixSubmission
  , mkBreakSubmissionFile
  , mkBuildSubmissionFile
  , mkFixSubmissionFile
  , mkBuildCoreResult
  , mkBuildPerformanceResult
  , mkBuildOptionalResult
  , mkFixCoreResult
  , mkFixPerformanceResult
  , mkFixOptionalResult
  , mkConfiguration
  , mkCacheExpiration
  , mkCacheBuildersCode
  , mkStoredFile
  , mkError
  , mkRateLimitLog
  , mkScorePending
  , User
  , CourseraUser
  , UserInformation
  , Contest
  , CourseraContest
  , Post
  , PostDependency
  , Judge
  , JudgeConflict
  , BuildJudgement
  , BreakJudgement
  , FixJudgement
  , BreakDispute
  , Team
  , TeamContest
  , TeamMember
  , TeamInvite
  , PasswordResetInvite
  , ContestCoreTest
  , ContestPerformanceTest
  , ContestOptionalTest
  , TeamBreakScore
  , TeamBuildScore
  , OracleSubmission
  , BuildSubmission
  , BreakOracleSubmission
  , BreakSubmission
  , BreakFixSubmission
  , FixSubmission
  , BreakSubmissionFile
  , BuildSubmissionFile
  , FixSubmissionFile
  , BuildCoreResult
  , BuildPerformanceResult
  , BuildOptionalResult
  , FixCoreResult
  , FixPerformanceResult
  , FixOptionalResult
  , Configuration
  , CacheExpiration
  , CacheBuildersCode
  , StoredFile
  , Error
  , RateLimitLog
  , ScorePending
  , userId'
  , userIdent'
  , userPassword'
  , userSalt'
  , userEmail'
  , userCreated'
  , userAdmin'
  , userConsentForm'
  , userResume'
  , courseraUserId'
  , courseraUserCourseraId'
  , courseraUserUser'
  , courseraUserToken'
  , userInformationId'
  , userInformationUser'
  , userInformationSchool'
  , userInformationMajor'
  , userInformationMinor'
  , userInformationDegreesHeld'
  , userInformationDegree'
  , userInformationYearsInProgram'
  , userInformationYearsOfExperience'
  , userInformationLanguages'
  , userInformationFavoriteLanguages'
  , userInformationYearsOfWork'
  , userInformationExperienceClass'
  , userInformationExperiencePersonal'
  , userInformationExperienceInternship'
  , userInformationExperienceJob'
  , userInformationSecurityTraining'
  , userInformationSecurityExperience'
  , userInformationSoftwareEngineering'
  , userInformationSecurityClass'
  , userInformationPreviousContest'
  , userInformationResumePermission'
  , userInformationAge'
  , userInformationNationality'
  , userInformationGender'
  , userInformationAgreeToParticipate'
  , userInformationGraduationYear'
  , userInformationProgrammerRating'
  , userInformationAttackerRating'
  , userInformationLanguage'
  , userInformationTimezone'
  , contestId'
  , contestUrl'
  , contestTitle'
  , contestBuildStart'
  , contestBuildEnd'
  , contestBreakFixStart'
  , contestBreakEnd'
  , contestFixEnd'
  , courseraContestId'
  , courseraContestContest'
  , courseraContestCourseId'
  , courseraContestSessionId'
  , postId'
  , postTitle'
  , postContest'
  , postTimestamp'
  , postDraft'
  , postContent'
  , postMarkdown'
  , postDependencyId'
  , postDependencyContest'
  , postDependencyPost'
  , postDependencyDependency'
  , judgeId'
  , judgeJudge'
  , judgeContest'
  , judgeAssignedCount'
  , judgeConflictId'
  , judgeConflictJudge'
  , judgeConflictTeam'
  , buildJudgementId'
  , buildJudgementJudge'
  , buildJudgementSubmission'
  , buildJudgementRuling'
  , buildJudgementComments'
  , breakJudgementId'
  , breakJudgementJudge'
  , breakJudgementSubmission'
  , breakJudgementRuling'
  , breakJudgementComments'
  , fixJudgementId'
  , fixJudgementJudge'
  , fixJudgementSubmission'
  , fixJudgementRuling'
  , fixJudgementComments'
  , breakDisputeId'
  , breakDisputeBreak'
  , breakDisputeJustification'
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
  , teamMemberId'
  , teamMemberTeam'
  , teamMemberUser'
  , teamInviteId'
  , teamInviteInvite'
  , teamInviteTeam'
  , teamInviteEmail'
  , passwordResetInviteId'
  , passwordResetInviteAccount'
  , passwordResetInviteInvite'
  , passwordResetInviteExpiration'
  , contestCoreTestId'
  , contestCoreTestContest'
  , contestCoreTestName'
  , contestCoreTestInputFile'
  , contestCoreTestOutputFile'
  , contestCoreTestTestScript'
  , contestPerformanceTestId'
  , contestPerformanceTestContest'
  , contestPerformanceTestName'
  , contestPerformanceTestInputFile'
  , contestPerformanceTestOutputFile'
  , contestPerformanceTestTestScript'
  , contestPerformanceTestOptional'
  , contestOptionalTestId'
  , contestOptionalTestContest'
  , contestOptionalTestName'
  , contestOptionalTestInputFile'
  , contestOptionalTestOutputFile'
  , contestOptionalTestTestScript'
  , teamBreakScoreId'
  , teamBreakScoreTeam'
  , teamBreakScoreBuildScore'
  , teamBreakScoreBreakScore'
  , teamBreakScoreFixScore'
  , teamBreakScoreTimestamp'
  , teamBuildScoreId'
  , teamBuildScoreTeam'
  , teamBuildScoreBuildScore'
  , teamBuildScoreBreakScore'
  , teamBuildScoreFixScore'
  , teamBuildScoreTimestamp'
  , oracleSubmissionId'
  , oracleSubmissionTeam'
  , oracleSubmissionTimestamp'
  , oracleSubmissionName'
  , oracleSubmissionInput'
  , oracleSubmissionOutput'
  , oracleSubmissionStatus'
  , buildSubmissionId'
  , buildSubmissionTeam'
  , buildSubmissionTimestamp'
  , buildSubmissionCommitHash'
  , buildSubmissionStatus'
  , buildSubmissionStdout'
  , buildSubmissionStderr'
  , breakOracleSubmissionId'
  , breakOracleSubmissionTeam'
  , breakOracleSubmissionTimestamp'
  , breakOracleSubmissionDescription'
  , breakOracleSubmissionValid'
  , breakSubmissionId'
  , breakSubmissionTeam'
  , breakSubmissionTargetTeam'
  , breakSubmissionTimestamp'
  , breakSubmissionCommitHash'
  , breakSubmissionName'
  , breakSubmissionStatus'
  , breakSubmissionBreakType'
  , breakSubmissionMessage'
  , breakSubmissionJson'
  , breakSubmissionStdout'
  , breakSubmissionStderr'
  , breakSubmissionValid'
  , breakSubmissionWithdrawn'
  , breakFixSubmissionId'
  , breakFixSubmissionBreak'
  , breakFixSubmissionFix'
  , breakFixSubmissionResult'
  , fixSubmissionId'
  , fixSubmissionTeam'
  , fixSubmissionTimestamp'
  , fixSubmissionCommitHash'
  , fixSubmissionStatus'
  , fixSubmissionResult'
  , fixSubmissionMessage'
  , fixSubmissionStdout'
  , fixSubmissionStderr'
  , breakSubmissionFileId'
  , breakSubmissionFileBreak'
  , breakSubmissionFileFile'
  , buildSubmissionFileId'
  , buildSubmissionFileTeam'
  , buildSubmissionFileFile'
  , fixSubmissionFileId'
  , fixSubmissionFileFix'
  , fixSubmissionFileFile'
  , buildCoreResultId'
  , buildCoreResultSubmission'
  , buildCoreResultTest'
  , buildCoreResultPass'
  , buildCoreResultMessage'
  , buildPerformanceResultId'
  , buildPerformanceResultSubmission'
  , buildPerformanceResultTest'
  , buildPerformanceResultTime'
  , buildPerformanceResultMessage'
  , buildOptionalResultId'
  , buildOptionalResultSubmission'
  , buildOptionalResultTest'
  , buildOptionalResultPass'
  , buildOptionalResultMessage'
  , fixCoreResultId'
  , fixCoreResultSubmission'
  , fixCoreResultTest'
  , fixCoreResultPass'
  , fixCoreResultMessage'
  , fixPerformanceResultId'
  , fixPerformanceResultSubmission'
  , fixPerformanceResultTest'
  , fixPerformanceResultTime'
  , fixPerformanceResultMessage'
  , fixOptionalResultId'
  , fixOptionalResultSubmission'
  , fixOptionalResultTest'
  , fixOptionalResultPass'
  , fixOptionalResultMessage'
  , configurationId'
  , configurationKey'
  , configurationValue'
  , cacheExpirationId'
  , cacheExpirationKey'
  , cacheExpirationExpiration'
  , cacheBuildersCodeId'
  , cacheBuildersCodeTeam'
  , cacheBuildersCodeTeamId'
  , cacheBuildersCodeLanguages'
  , cacheBuildersCodeContestId'
  , cacheBuildersCodeBuilderScore'
  , cacheBuildersCodeBugsFound'
  , cacheBuildersCodeVulnerabilitiesFound'
  , storedFileId'
  , storedFileOwner'
  , storedFileName'
  , storedFileContentType'
  , storedFileContent'
  , errorId'
  , errorHandlerName'
  , errorErrorText'
  , errorTime'
  , rateLimitLogId'
  , rateLimitLogAction'
  , rateLimitLogLimiter'
  , rateLimitLogTime'
  , scorePendingId'
  , scorePendingContest'
  , scorePendingRound'
  , UserId
  , CourseraUserId
  , UserInformationId
  , ContestId
  , CourseraContestId
  , PostId
  , PostDependencyId
  , JudgeId
  , JudgeConflictId
  , BuildJudgementId
  , BreakJudgementId
  , FixJudgementId
  , BreakDisputeId
  , TeamId
  , TeamContestId
  , TeamMemberId
  , TeamInviteId
  , PasswordResetInviteId
  , ContestCoreTestId
  , ContestPerformanceTestId
  , ContestOptionalTestId
  , TeamBreakScoreId
  , TeamBuildScoreId
  , OracleSubmissionId
  , BuildSubmissionId
  , BreakOracleSubmissionId
  , BreakSubmissionId
  , BreakFixSubmissionId
  , FixSubmissionId
  , BreakSubmissionFileId
  , BuildSubmissionFileId
  , FixSubmissionFileId
  , BuildCoreResultId
  , BuildPerformanceResultId
  , BuildOptionalResultId
  , FixCoreResultId
  , FixPerformanceResultId
  , FixOptionalResultId
  , ConfigurationId
  , CacheExpirationId
  , CacheBuildersCodeId
  , StoredFileId
  , ErrorId
  , RateLimitLogId
  , ScorePendingId
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
import Yesod (Textarea, Html)
import Core.Git
import PostDependencyType

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

CourseraUser
  courseraId Text
  user UserId
  token Text
  UniqueCourseraId courseraId
 UniqueCourseraUser user

UserInformation
  user UserId
  school Text Maybe
  major Text Maybe
  minor Text Maybe
  degreesHeld Text
  degree Text
  yearsInProgram Int Maybe
  yearsOfExperience Int Maybe
  languages Text Maybe
  favoriteLanguages Text Maybe
  yearsOfWork Int Maybe
  experienceClass Bool Maybe
  experiencePersonal Bool Maybe
  experienceInternship Bool Maybe
  experienceJob Bool Maybe
  securityTraining Bool Maybe
  securityExperience Bool Maybe
  softwareEngineering Bool Maybe
  securityClass Bool Maybe
  previousContest Bool Maybe
  resumePermission Bool
  age Int Maybe
  nationality Text Maybe
  gender Text Maybe
  agreeToParticipate Bool
  graduationYear Int Maybe
  programmerRating Int Maybe
  attackerRating Int Maybe
  language Text Maybe
  timezone Text Maybe
  UniqueUserInformation user

Contest
  url Text
  title Text
  buildStart UTCTime
  buildEnd UTCTime
  breakFixStart UTCTime
  breakEnd UTCTime
  fixEnd UTCTime
  UniqueContest url

CourseraContest
  contest ContestId
  courseId Int
  sessionId Int
  UniqueCourseraContest contest

Post
  title Text
  contest ContestId
  timestamp UTCTime
  draft Bool
  content Html
  markdown Text


PostDependency
  contest ContestId
  post PostId
  dependency PostDependencyType


Judge
  judge UserId
  contest ContestId
  assignedCount Int
  UniqueContestJudge judge contest

JudgeConflict
  judge JudgeId
  team TeamContestId
  UniqueJudgeConflict judge team

BuildJudgement
  judge JudgeId
  submission BuildSubmissionId
  ruling Bool Maybe
  comments Text Maybe
  UniqueBuildJudgement submission

BreakJudgement
  judge JudgeId
  submission BreakSubmissionId
  ruling Bool Maybe
  comments Text Maybe
  UniqueBreakJudgement submission

FixJudgement
  judge JudgeId
  submission FixSubmissionId
  ruling Bool Maybe
  comments Text Maybe
  UniqueFixJudgement submission

BreakDispute
  break BreakSubmissionId
  justification Text
  UniqueBreakDispute break

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

TeamMember
  team TeamId
  user UserId
  UniqueTeamMember team user

TeamInvite
  invite Text
  team TeamId
  email Text
  UniqueTeamInvite invite

PasswordResetInvite
  account UserId
  invite Text
  expiration UTCTime
  UniquePasswordResetInvite invite

ContestCoreTest
  contest ContestId
  name Text
  inputFile Text
  outputFile Text
  testScript Text


ContestPerformanceTest
  contest ContestId
  name Text
  inputFile Text
  outputFile Text
  testScript Text
  optional Bool


ContestOptionalTest
  contest ContestId
  name Text
  inputFile Text
  outputFile Text
  testScript Text


TeamBreakScore
  team TeamContestId
  buildScore Double Maybe
  breakScore Double Maybe
  fixScore Double Maybe
  timestamp UTCTime


TeamBuildScore
  team TeamContestId
  buildScore Double Maybe
  breakScore Double Maybe
  fixScore Double Maybe
  timestamp UTCTime


OracleSubmission
  team TeamContestId
  timestamp UTCTime
  name Text
  input Text
  output Text Maybe
  status OracleSubmissionStatus


BuildSubmission
  team TeamContestId
  timestamp UTCTime
  commitHash Text
  status BuildSubmissionStatus
  stdout Textarea Maybe
  stderr Textarea Maybe


BreakOracleSubmission
  team TeamContestId
  timestamp UTCTime
  description Text
  valid Bool


BreakSubmission
  team TeamContestId
  targetTeam TeamContestId Maybe
  timestamp UTCTime
  commitHash Text
  name Text
  status BreakSubmissionStatus
  breakType BreakType Maybe
  message String Maybe
  json Text Maybe
  stdout Textarea Maybe
  stderr Textarea Maybe
  valid Bool Maybe
  withdrawn Bool


BreakFixSubmission
  break BreakSubmissionId
  fix FixSubmissionId Maybe
  result BreakSubmissionResult


FixSubmission
  team TeamContestId
  timestamp UTCTime
  commitHash Text
  status FixSubmissionStatus
  result FixSubmissionResult Maybe
  message String Maybe
  stdout Textarea Maybe
  stderr Textarea Maybe


BreakSubmissionFile
  break BreakSubmissionId
  file ByteString
  UniqueBreakSubmissionFile break

BuildSubmissionFile
  team TeamContestId
  file ByteString
  UniqueBuildSubmissionFile team

FixSubmissionFile
  fix FixSubmissionId
  file ByteString
  UniqueFixSubmissionFile fix

BuildCoreResult
  submission BuildSubmissionId
  test ContestCoreTestId
  pass Bool
  message Text Maybe
  UniqueBuildCoreSubmission submission test

BuildPerformanceResult
  submission BuildSubmissionId
  test ContestPerformanceTestId
  time Double Maybe
  message Text Maybe
  UniqueBuildPerformanceSubmission submission test

BuildOptionalResult
  submission BuildSubmissionId
  test ContestOptionalTestId
  pass Bool
  message Text Maybe
  UniqueBuildOptionalSubmission submission test

FixCoreResult
  submission FixSubmissionId
  test ContestCoreTestId
  pass Bool
  message Text Maybe
  UniqueFixCoreSubmission submission test

FixPerformanceResult
  submission FixSubmissionId
  test ContestPerformanceTestId
  time Double Maybe
  message Text Maybe
  UniqueFixPerformanceSubmission submission test

FixOptionalResult
  submission FixSubmissionId
  test ContestOptionalTestId
  pass Bool
  message Text Maybe
  UniqueFixOptionalSubmission submission test

Configuration
  key Text
  value Text
  UniqueKey key

CacheExpiration
  key Text
  expiration UTCTime
  UniqueCache key

CacheBuildersCode
  team Text
  teamId TeamContestId
  languages Text
  contestId ContestId
  builderScore Double
  bugsFound Int
  vulnerabilitiesFound Int
  UniqueCacheBuilderCodeTeam teamId

StoredFile
  owner UserId
  name Text
  contentType Text
  content ByteString


Error
  handlerName Text
  errorText Text
  time UTCTime


RateLimitLog
  action Int
  limiter Int
  time UTCTime


ScorePending
  contest ContestId
  round ContestRound
  UniqueScorePending contest round
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
                          {\x_0 x_1 x_2 -> ((False)) => (userIdentCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userSaltCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userEmailCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userCreatedCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userAdminCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userConsentFormCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (userResumeCap x_0)}>
                          (Entity User) User (Maybe StoredFileId)
  @-}
userResume' :: EntityFieldWrapper (Entity User) User (Maybe StoredFileId)
userResume' = EntityFieldWrapper UserResume

-- * CourseraUser
{-@ mkCourseraUser ::
        x_0: Text
     -> x_1: UserId
     -> x_2: Text
     -> BinahRecord <{\row -> courseraUserCourseraId (entityVal row) == x_0 && courseraUserUser (entityVal row) == x_1 && courseraUserToken (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CourseraUser
  @-}
mkCourseraUser :: Text -> UserId -> Text -> BinahRecord (Entity User) CourseraUser
mkCourseraUser x_0 x_1 x_2 = BinahRecord (CourseraUser x_0 x_1 x_2)

{-@ invariant {v: Entity CourseraUser | v == getJust (entityKey v)} @-}



{-@ assume courseraUserId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CourseraUser CourseraUserId
  @-}
courseraUserId' :: EntityFieldWrapper (Entity User) CourseraUser CourseraUserId
courseraUserId' = EntityFieldWrapper CourseraUserId

{-@ measure courseraUserCourseraId :: CourseraUser -> Text @-}

{-@ measure courseraUserCourseraIdCap :: Entity CourseraUser -> Bool @-}

{-@ assume courseraUserCourseraId' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraUserCourseraId (entityVal row)},
                          {\field row -> field == courseraUserCourseraId (entityVal row)},
                          {\old -> courseraUserCourseraIdCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (courseraUserCourseraIdCap x_0)}>
                          (Entity User) CourseraUser Text
  @-}
courseraUserCourseraId' :: EntityFieldWrapper (Entity User) CourseraUser Text
courseraUserCourseraId' = EntityFieldWrapper CourseraUserCourseraId

{-@ measure courseraUserUser :: CourseraUser -> UserId @-}

{-@ measure courseraUserUserCap :: Entity CourseraUser -> Bool @-}

{-@ assume courseraUserUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraUserUser (entityVal row)},
                          {\field row -> field == courseraUserUser (entityVal row)},
                          {\old -> courseraUserUserCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (courseraUserUserCap x_0)}>
                          (Entity User) CourseraUser UserId
  @-}
courseraUserUser' :: EntityFieldWrapper (Entity User) CourseraUser UserId
courseraUserUser' = EntityFieldWrapper CourseraUserUser

{-@ measure courseraUserToken :: CourseraUser -> Text @-}

{-@ measure courseraUserTokenCap :: Entity CourseraUser -> Bool @-}

{-@ assume courseraUserToken' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraUserToken (entityVal row)},
                          {\field row -> field == courseraUserToken (entityVal row)},
                          {\old -> courseraUserTokenCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (courseraUserTokenCap x_0)}>
                          (Entity User) CourseraUser Text
  @-}
courseraUserToken' :: EntityFieldWrapper (Entity User) CourseraUser Text
courseraUserToken' = EntityFieldWrapper CourseraUserToken

-- * UserInformation
{-@ mkUserInformation ::
        x_0: UserId
     -> x_1: Maybe Text
     -> x_2: Maybe Text
     -> x_3: Maybe Text
     -> x_4: Text
     -> x_5: Text
     -> x_6: Maybe Int
     -> x_7: Maybe Int
     -> x_8: Maybe Text
     -> x_9: Maybe Text
     -> x_10: Maybe Int
     -> x_11: Maybe Bool
     -> x_12: Maybe Bool
     -> x_13: Maybe Bool
     -> x_14: Maybe Bool
     -> x_15: Maybe Bool
     -> x_16: Maybe Bool
     -> x_17: Maybe Bool
     -> x_18: Maybe Bool
     -> x_19: Maybe Bool
     -> x_20: Bool
     -> x_21: Maybe Int
     -> x_22: Maybe Text
     -> x_23: Maybe Text
     -> x_24: Bool
     -> x_25: Maybe Int
     -> x_26: Maybe Int
     -> x_27: Maybe Int
     -> x_28: Maybe Text
     -> x_29: Maybe Text
     -> BinahRecord <{\row -> userInformationUser (entityVal row) == x_0 && userInformationSchool (entityVal row) == x_1 && userInformationMajor (entityVal row) == x_2 && userInformationMinor (entityVal row) == x_3 && userInformationDegreesHeld (entityVal row) == x_4 && userInformationDegree (entityVal row) == x_5 && userInformationYearsInProgram (entityVal row) == x_6 && userInformationYearsOfExperience (entityVal row) == x_7 && userInformationLanguages (entityVal row) == x_8 && userInformationFavoriteLanguages (entityVal row) == x_9 && userInformationYearsOfWork (entityVal row) == x_10 && userInformationExperienceClass (entityVal row) == x_11 && userInformationExperiencePersonal (entityVal row) == x_12 && userInformationExperienceInternship (entityVal row) == x_13 && userInformationExperienceJob (entityVal row) == x_14 && userInformationSecurityTraining (entityVal row) == x_15 && userInformationSecurityExperience (entityVal row) == x_16 && userInformationSoftwareEngineering (entityVal row) == x_17 && userInformationSecurityClass (entityVal row) == x_18 && userInformationPreviousContest (entityVal row) == x_19 && userInformationResumePermission (entityVal row) == x_20 && userInformationAge (entityVal row) == x_21 && userInformationNationality (entityVal row) == x_22 && userInformationGender (entityVal row) == x_23 && userInformationAgreeToParticipate (entityVal row) == x_24 && userInformationGraduationYear (entityVal row) == x_25 && userInformationProgrammerRating (entityVal row) == x_26 && userInformationAttackerRating (entityVal row) == x_27 && userInformationLanguage (entityVal row) == x_28 && userInformationTimezone (entityVal row) == x_29},
                     {\info viewer -> userInformationUser (entityVal info) == entityKey viewer},
                     {\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))}>
                     (Entity User) UserInformation
  @-}
mkUserInformation :: UserId -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool -> Maybe Int -> Maybe Text -> Maybe Text -> Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> BinahRecord (Entity User) UserInformation
mkUserInformation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12 x_13 x_14 x_15 x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 = BinahRecord (UserInformation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12 x_13 x_14 x_15 x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29)

{-@ invariant {v: Entity UserInformation | v == getJust (entityKey v)} @-}



{-@ assume userInformationId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) UserInformation UserInformationId
  @-}
userInformationId' :: EntityFieldWrapper (Entity User) UserInformation UserInformationId
userInformationId' = EntityFieldWrapper UserInformationId

{-@ measure userInformationUser :: UserInformation -> UserId @-}

{-@ measure userInformationUserCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userInformationUser (entityVal row)},
                          {\field row -> field == userInformationUser (entityVal row)},
                          {\old -> userInformationUserCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationUserCap x_0)}>
                          (Entity User) UserInformation UserId
  @-}
userInformationUser' :: EntityFieldWrapper (Entity User) UserInformation UserId
userInformationUser' = EntityFieldWrapper UserInformationUser

{-@ measure userInformationSchool :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationSchoolCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationSchool' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationSchool (entityVal row)},
                          {\field row -> field == userInformationSchool (entityVal row)},
                          {\old -> userInformationSchoolCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationSchoolCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationSchool' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationSchool' = EntityFieldWrapper UserInformationSchool

{-@ measure userInformationMajor :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationMajorCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationMajor' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationMajor (entityVal row)},
                          {\field row -> field == userInformationMajor (entityVal row)},
                          {\old -> userInformationMajorCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationMajorCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationMajor' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationMajor' = EntityFieldWrapper UserInformationMajor

{-@ measure userInformationMinor :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationMinorCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationMinor' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationMinor (entityVal row)},
                          {\field row -> field == userInformationMinor (entityVal row)},
                          {\old -> userInformationMinorCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationMinorCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationMinor' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationMinor' = EntityFieldWrapper UserInformationMinor

{-@ measure userInformationDegreesHeld :: UserInformation -> Text @-}

{-@ measure userInformationDegreesHeldCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationDegreesHeld' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationDegreesHeld (entityVal row)},
                          {\field row -> field == userInformationDegreesHeld (entityVal row)},
                          {\old -> userInformationDegreesHeldCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationDegreesHeldCap x_0)}>
                          (Entity User) UserInformation Text
  @-}
userInformationDegreesHeld' :: EntityFieldWrapper (Entity User) UserInformation Text
userInformationDegreesHeld' = EntityFieldWrapper UserInformationDegreesHeld

{-@ measure userInformationDegree :: UserInformation -> Text @-}

{-@ measure userInformationDegreeCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationDegree' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationDegree (entityVal row)},
                          {\field row -> field == userInformationDegree (entityVal row)},
                          {\old -> userInformationDegreeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationDegreeCap x_0)}>
                          (Entity User) UserInformation Text
  @-}
userInformationDegree' :: EntityFieldWrapper (Entity User) UserInformation Text
userInformationDegree' = EntityFieldWrapper UserInformationDegree

{-@ measure userInformationYearsInProgram :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationYearsInProgramCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationYearsInProgram' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationYearsInProgram (entityVal row)},
                          {\field row -> field == userInformationYearsInProgram (entityVal row)},
                          {\old -> userInformationYearsInProgramCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationYearsInProgramCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationYearsInProgram' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationYearsInProgram' = EntityFieldWrapper UserInformationYearsInProgram

{-@ measure userInformationYearsOfExperience :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationYearsOfExperienceCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationYearsOfExperience' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationYearsOfExperience (entityVal row)},
                          {\field row -> field == userInformationYearsOfExperience (entityVal row)},
                          {\old -> userInformationYearsOfExperienceCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationYearsOfExperienceCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationYearsOfExperience' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationYearsOfExperience' = EntityFieldWrapper UserInformationYearsOfExperience

{-@ measure userInformationLanguages :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationLanguagesCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationLanguages' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationLanguages (entityVal row)},
                          {\field row -> field == userInformationLanguages (entityVal row)},
                          {\old -> userInformationLanguagesCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationLanguagesCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationLanguages' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationLanguages' = EntityFieldWrapper UserInformationLanguages

{-@ measure userInformationFavoriteLanguages :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationFavoriteLanguagesCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationFavoriteLanguages' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationFavoriteLanguages (entityVal row)},
                          {\field row -> field == userInformationFavoriteLanguages (entityVal row)},
                          {\old -> userInformationFavoriteLanguagesCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationFavoriteLanguagesCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationFavoriteLanguages' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationFavoriteLanguages' = EntityFieldWrapper UserInformationFavoriteLanguages

{-@ measure userInformationYearsOfWork :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationYearsOfWorkCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationYearsOfWork' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationYearsOfWork (entityVal row)},
                          {\field row -> field == userInformationYearsOfWork (entityVal row)},
                          {\old -> userInformationYearsOfWorkCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationYearsOfWorkCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationYearsOfWork' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationYearsOfWork' = EntityFieldWrapper UserInformationYearsOfWork

{-@ measure userInformationExperienceClass :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationExperienceClassCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationExperienceClass' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationExperienceClass (entityVal row)},
                          {\field row -> field == userInformationExperienceClass (entityVal row)},
                          {\old -> userInformationExperienceClassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationExperienceClassCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationExperienceClass' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationExperienceClass' = EntityFieldWrapper UserInformationExperienceClass

{-@ measure userInformationExperiencePersonal :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationExperiencePersonalCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationExperiencePersonal' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationExperiencePersonal (entityVal row)},
                          {\field row -> field == userInformationExperiencePersonal (entityVal row)},
                          {\old -> userInformationExperiencePersonalCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationExperiencePersonalCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationExperiencePersonal' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationExperiencePersonal' = EntityFieldWrapper UserInformationExperiencePersonal

{-@ measure userInformationExperienceInternship :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationExperienceInternshipCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationExperienceInternship' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userInformationExperienceInternship (entityVal row)},
                          {\field row -> field == userInformationExperienceInternship (entityVal row)},
                          {\old -> userInformationExperienceInternshipCap old},
                          {\old _ _ -> userInformationExperienceInternshipCap old}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationExperienceInternship' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationExperienceInternship' = EntityFieldWrapper UserInformationExperienceInternship

{-@ measure userInformationExperienceJob :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationExperienceJobCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationExperienceJob' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationExperienceJob (entityVal row)},
                          {\field row -> field == userInformationExperienceJob (entityVal row)},
                          {\old -> userInformationExperienceJobCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationExperienceJobCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationExperienceJob' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationExperienceJob' = EntityFieldWrapper UserInformationExperienceJob

{-@ measure userInformationSecurityTraining :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationSecurityTrainingCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationSecurityTraining' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationSecurityTraining (entityVal row)},
                          {\field row -> field == userInformationSecurityTraining (entityVal row)},
                          {\old -> userInformationSecurityTrainingCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationSecurityTrainingCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationSecurityTraining' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationSecurityTraining' = EntityFieldWrapper UserInformationSecurityTraining

{-@ measure userInformationSecurityExperience :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationSecurityExperienceCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationSecurityExperience' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationSecurityExperience (entityVal row)},
                          {\field row -> field == userInformationSecurityExperience (entityVal row)},
                          {\old -> userInformationSecurityExperienceCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationSecurityExperienceCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationSecurityExperience' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationSecurityExperience' = EntityFieldWrapper UserInformationSecurityExperience

{-@ measure userInformationSoftwareEngineering :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationSoftwareEngineeringCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationSoftwareEngineering' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationSoftwareEngineering (entityVal row)},
                          {\field row -> field == userInformationSoftwareEngineering (entityVal row)},
                          {\old -> userInformationSoftwareEngineeringCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationSoftwareEngineeringCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationSoftwareEngineering' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationSoftwareEngineering' = EntityFieldWrapper UserInformationSoftwareEngineering

{-@ measure userInformationSecurityClass :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationSecurityClassCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationSecurityClass' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationSecurityClass (entityVal row)},
                          {\field row -> field == userInformationSecurityClass (entityVal row)},
                          {\old -> userInformationSecurityClassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationSecurityClassCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationSecurityClass' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationSecurityClass' = EntityFieldWrapper UserInformationSecurityClass

{-@ measure userInformationPreviousContest :: UserInformation -> (Maybe Bool) @-}

{-@ measure userInformationPreviousContestCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationPreviousContest' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationPreviousContest (entityVal row)},
                          {\field row -> field == userInformationPreviousContest (entityVal row)},
                          {\old -> userInformationPreviousContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationPreviousContestCap x_0)}>
                          (Entity User) UserInformation (Maybe Bool)
  @-}
userInformationPreviousContest' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Bool)
userInformationPreviousContest' = EntityFieldWrapper UserInformationPreviousContest

{-@ measure userInformationResumePermission :: UserInformation -> Bool @-}

{-@ measure userInformationResumePermissionCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationResumePermission' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationResumePermission (entityVal row)},
                          {\field row -> field == userInformationResumePermission (entityVal row)},
                          {\old -> userInformationResumePermissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationResumePermissionCap x_0)}>
                          (Entity User) UserInformation Bool
  @-}
userInformationResumePermission' :: EntityFieldWrapper (Entity User) UserInformation Bool
userInformationResumePermission' = EntityFieldWrapper UserInformationResumePermission

{-@ measure userInformationAge :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationAgeCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationAge' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationAge (entityVal row)},
                          {\field row -> field == userInformationAge (entityVal row)},
                          {\old -> userInformationAgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationAgeCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationAge' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationAge' = EntityFieldWrapper UserInformationAge

{-@ measure userInformationNationality :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationNationalityCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationNationality' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationNationality (entityVal row)},
                          {\field row -> field == userInformationNationality (entityVal row)},
                          {\old -> userInformationNationalityCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationNationalityCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationNationality' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationNationality' = EntityFieldWrapper UserInformationNationality

{-@ measure userInformationGender :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationGenderCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationGender' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationGender (entityVal row)},
                          {\field row -> field == userInformationGender (entityVal row)},
                          {\old -> userInformationGenderCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationGenderCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationGender' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationGender' = EntityFieldWrapper UserInformationGender

{-@ measure userInformationAgreeToParticipate :: UserInformation -> Bool @-}

{-@ measure userInformationAgreeToParticipateCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationAgreeToParticipate' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationAgreeToParticipate (entityVal row)},
                          {\field row -> field == userInformationAgreeToParticipate (entityVal row)},
                          {\old -> userInformationAgreeToParticipateCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationAgreeToParticipateCap x_0)}>
                          (Entity User) UserInformation Bool
  @-}
userInformationAgreeToParticipate' :: EntityFieldWrapper (Entity User) UserInformation Bool
userInformationAgreeToParticipate' = EntityFieldWrapper UserInformationAgreeToParticipate

{-@ measure userInformationGraduationYear :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationGraduationYearCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationGraduationYear' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationGraduationYear (entityVal row)},
                          {\field row -> field == userInformationGraduationYear (entityVal row)},
                          {\old -> userInformationGraduationYearCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationGraduationYearCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationGraduationYear' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationGraduationYear' = EntityFieldWrapper UserInformationGraduationYear

{-@ measure userInformationProgrammerRating :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationProgrammerRatingCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationProgrammerRating' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationProgrammerRating (entityVal row)},
                          {\field row -> field == userInformationProgrammerRating (entityVal row)},
                          {\old -> userInformationProgrammerRatingCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationProgrammerRatingCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationProgrammerRating' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationProgrammerRating' = EntityFieldWrapper UserInformationProgrammerRating

{-@ measure userInformationAttackerRating :: UserInformation -> (Maybe Int) @-}

{-@ measure userInformationAttackerRatingCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationAttackerRating' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationAttackerRating (entityVal row)},
                          {\field row -> field == userInformationAttackerRating (entityVal row)},
                          {\old -> userInformationAttackerRatingCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationAttackerRatingCap x_0)}>
                          (Entity User) UserInformation (Maybe Int)
  @-}
userInformationAttackerRating' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Int)
userInformationAttackerRating' = EntityFieldWrapper UserInformationAttackerRating

{-@ measure userInformationLanguage :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationLanguageCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationLanguage' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationLanguage (entityVal row)},
                          {\field row -> field == userInformationLanguage (entityVal row)},
                          {\old -> userInformationLanguageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationLanguageCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationLanguage' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationLanguage' = EntityFieldWrapper UserInformationLanguage

{-@ measure userInformationTimezone :: UserInformation -> (Maybe Text) @-}

{-@ measure userInformationTimezoneCap :: Entity UserInformation -> Bool @-}

{-@ assume userInformationTimezone' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userInformationUser (entityVal x_0) == entityKey x_1 || userAdmin (entityVal x_1))},
                          {\row field -> field == userInformationTimezone (entityVal row)},
                          {\field row -> field == userInformationTimezone (entityVal row)},
                          {\old -> userInformationTimezoneCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userInformationTimezoneCap x_0)}>
                          (Entity User) UserInformation (Maybe Text)
  @-}
userInformationTimezone' :: EntityFieldWrapper (Entity User) UserInformation (Maybe Text)
userInformationTimezone' = EntityFieldWrapper UserInformationTimezone

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
                          {\x_0 x_1 x_2 -> ((False)) => (contestUrlCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestTitleCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestBuildStartCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestBuildEndCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestBreakFixStartCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestBreakEndCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (contestFixEndCap x_0)}>
                          (Entity User) Contest UTCTime
  @-}
contestFixEnd' :: EntityFieldWrapper (Entity User) Contest UTCTime
contestFixEnd' = EntityFieldWrapper ContestFixEnd

-- * CourseraContest
{-@ mkCourseraContest ::
        x_0: ContestId
     -> x_1: Int
     -> x_2: Int
     -> BinahRecord <{\row -> courseraContestContest (entityVal row) == x_0 && courseraContestCourseId (entityVal row) == x_1 && courseraContestSessionId (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CourseraContest
  @-}
mkCourseraContest :: ContestId -> Int -> Int -> BinahRecord (Entity User) CourseraContest
mkCourseraContest x_0 x_1 x_2 = BinahRecord (CourseraContest x_0 x_1 x_2)

{-@ invariant {v: Entity CourseraContest | v == getJust (entityKey v)} @-}



{-@ assume courseraContestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CourseraContest CourseraContestId
  @-}
courseraContestId' :: EntityFieldWrapper (Entity User) CourseraContest CourseraContestId
courseraContestId' = EntityFieldWrapper CourseraContestId

{-@ measure courseraContestContest :: CourseraContest -> ContestId @-}

{-@ measure courseraContestContestCap :: Entity CourseraContest -> Bool @-}

{-@ assume courseraContestContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraContestContest (entityVal row)},
                          {\field row -> field == courseraContestContest (entityVal row)},
                          {\old -> courseraContestContestCap old},
                          {\old _ _ -> courseraContestContestCap old}>
                          (Entity User) CourseraContest ContestId
  @-}
courseraContestContest' :: EntityFieldWrapper (Entity User) CourseraContest ContestId
courseraContestContest' = EntityFieldWrapper CourseraContestContest

{-@ measure courseraContestCourseId :: CourseraContest -> Int @-}

{-@ measure courseraContestCourseIdCap :: Entity CourseraContest -> Bool @-}

{-@ assume courseraContestCourseId' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraContestCourseId (entityVal row)},
                          {\field row -> field == courseraContestCourseId (entityVal row)},
                          {\old -> courseraContestCourseIdCap old},
                          {\old _ _ -> courseraContestCourseIdCap old}>
                          (Entity User) CourseraContest Int
  @-}
courseraContestCourseId' :: EntityFieldWrapper (Entity User) CourseraContest Int
courseraContestCourseId' = EntityFieldWrapper CourseraContestCourseId

{-@ measure courseraContestSessionId :: CourseraContest -> Int @-}

{-@ measure courseraContestSessionIdCap :: Entity CourseraContest -> Bool @-}

{-@ assume courseraContestSessionId' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseraContestSessionId (entityVal row)},
                          {\field row -> field == courseraContestSessionId (entityVal row)},
                          {\old -> courseraContestSessionIdCap old},
                          {\old _ _ -> courseraContestSessionIdCap old}>
                          (Entity User) CourseraContest Int
  @-}
courseraContestSessionId' :: EntityFieldWrapper (Entity User) CourseraContest Int
courseraContestSessionId' = EntityFieldWrapper CourseraContestSessionId

-- * Post
{-@ mkPost ::
        x_0: Text
     -> x_1: ContestId
     -> x_2: UTCTime
     -> x_3: Bool
     -> x_4: Html
     -> x_5: Text
     -> BinahRecord <{\row -> postTitle (entityVal row) == x_0 && postContest (entityVal row) == x_1 && postTimestamp (entityVal row) == x_2 && postDraft (entityVal row) == x_3 && postContent (entityVal row) == x_4 && postMarkdown (entityVal row) == x_5},
                     {\post viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) Post
  @-}
mkPost :: Text -> ContestId -> UTCTime -> Bool -> Html -> Text -> BinahRecord (Entity User) Post
mkPost x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (Post x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity Post | v == getJust (entityKey v)} @-}



{-@ assume postId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Post PostId
  @-}
postId' :: EntityFieldWrapper (Entity User) Post PostId
postId' = EntityFieldWrapper PostId

{-@ measure postTitle :: Post -> Text @-}

{-@ measure postTitleCap :: Entity Post -> Bool @-}

{-@ assume postTitle' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postTitle (entityVal row)},
                          {\field row -> field == postTitle (entityVal row)},
                          {\old -> postTitleCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (postTitleCap x_0)}>
                          (Entity User) Post Text
  @-}
postTitle' :: EntityFieldWrapper (Entity User) Post Text
postTitle' = EntityFieldWrapper PostTitle

{-@ measure postContest :: Post -> ContestId @-}

{-@ measure postContestCap :: Entity Post -> Bool @-}

{-@ assume postContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postContest (entityVal row)},
                          {\field row -> field == postContest (entityVal row)},
                          {\old -> postContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postContestCap x_0)}>
                          (Entity User) Post ContestId
  @-}
postContest' :: EntityFieldWrapper (Entity User) Post ContestId
postContest' = EntityFieldWrapper PostContest

{-@ measure postTimestamp :: Post -> UTCTime @-}

{-@ measure postTimestampCap :: Entity Post -> Bool @-}

{-@ assume postTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postTimestamp (entityVal row)},
                          {\field row -> field == postTimestamp (entityVal row)},
                          {\old -> postTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postTimestampCap x_0)}>
                          (Entity User) Post UTCTime
  @-}
postTimestamp' :: EntityFieldWrapper (Entity User) Post UTCTime
postTimestamp' = EntityFieldWrapper PostTimestamp

{-@ measure postDraft :: Post -> Bool @-}

{-@ measure postDraftCap :: Entity Post -> Bool @-}

{-@ assume postDraft' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postDraft (entityVal row)},
                          {\field row -> field == postDraft (entityVal row)},
                          {\old -> postDraftCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postDraftCap x_0)}>
                          (Entity User) Post Bool
  @-}
postDraft' :: EntityFieldWrapper (Entity User) Post Bool
postDraft' = EntityFieldWrapper PostDraft

{-@ measure postContent :: Post -> Html @-}

{-@ measure postContentCap :: Entity Post -> Bool @-}

{-@ assume postContent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postContent (entityVal row)},
                          {\field row -> field == postContent (entityVal row)},
                          {\old -> postContentCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (postContentCap x_0)}>
                          (Entity User) Post Html
  @-}
postContent' :: EntityFieldWrapper (Entity User) Post Html
postContent' = EntityFieldWrapper PostContent

{-@ measure postMarkdown :: Post -> Text @-}

{-@ measure postMarkdownCap :: Entity Post -> Bool @-}

{-@ assume postMarkdown' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postMarkdown (entityVal row)},
                          {\field row -> field == postMarkdown (entityVal row)},
                          {\old -> postMarkdownCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (postMarkdownCap x_0)}>
                          (Entity User) Post Text
  @-}
postMarkdown' :: EntityFieldWrapper (Entity User) Post Text
postMarkdown' = EntityFieldWrapper PostMarkdown

-- * PostDependency
{-@ mkPostDependency ::
        x_0: ContestId
     -> x_1: PostId
     -> x_2: PostDependencyType
     -> BinahRecord <{\row -> postDependencyContest (entityVal row) == x_0 && postDependencyPost (entityVal row) == x_1 && postDependencyDependency (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) PostDependency
  @-}
mkPostDependency :: ContestId -> PostId -> PostDependencyType -> BinahRecord (Entity User) PostDependency
mkPostDependency x_0 x_1 x_2 = BinahRecord (PostDependency x_0 x_1 x_2)

{-@ invariant {v: Entity PostDependency | v == getJust (entityKey v)} @-}



{-@ assume postDependencyId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) PostDependency PostDependencyId
  @-}
postDependencyId' :: EntityFieldWrapper (Entity User) PostDependency PostDependencyId
postDependencyId' = EntityFieldWrapper PostDependencyId

{-@ measure postDependencyContest :: PostDependency -> ContestId @-}

{-@ measure postDependencyContestCap :: Entity PostDependency -> Bool @-}

{-@ assume postDependencyContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postDependencyContest (entityVal row)},
                          {\field row -> field == postDependencyContest (entityVal row)},
                          {\old -> postDependencyContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postDependencyContestCap x_0)}>
                          (Entity User) PostDependency ContestId
  @-}
postDependencyContest' :: EntityFieldWrapper (Entity User) PostDependency ContestId
postDependencyContest' = EntityFieldWrapper PostDependencyContest

{-@ measure postDependencyPost :: PostDependency -> PostId @-}

{-@ measure postDependencyPostCap :: Entity PostDependency -> Bool @-}

{-@ assume postDependencyPost' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postDependencyPost (entityVal row)},
                          {\field row -> field == postDependencyPost (entityVal row)},
                          {\old -> postDependencyPostCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postDependencyPostCap x_0)}>
                          (Entity User) PostDependency PostId
  @-}
postDependencyPost' :: EntityFieldWrapper (Entity User) PostDependency PostId
postDependencyPost' = EntityFieldWrapper PostDependencyPost

{-@ measure postDependencyDependency :: PostDependency -> PostDependencyType @-}

{-@ measure postDependencyDependencyCap :: Entity PostDependency -> Bool @-}

{-@ assume postDependencyDependency' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == postDependencyDependency (entityVal row)},
                          {\field row -> field == postDependencyDependency (entityVal row)},
                          {\old -> postDependencyDependencyCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (postDependencyDependencyCap x_0)}>
                          (Entity User) PostDependency PostDependencyType
  @-}
postDependencyDependency' :: EntityFieldWrapper (Entity User) PostDependency PostDependencyType
postDependencyDependency' = EntityFieldWrapper PostDependencyDependency

-- * Judge
{-@ mkJudge ::
        x_0: UserId
     -> x_1: ContestId
     -> x_2: Int
     -> BinahRecord <{\row -> judgeJudge (entityVal row) == x_0 && judgeContest (entityVal row) == x_1 && judgeAssignedCount (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Judge
  @-}
mkJudge :: UserId -> ContestId -> Int -> BinahRecord (Entity User) Judge
mkJudge x_0 x_1 x_2 = BinahRecord (Judge x_0 x_1 x_2)

{-@ invariant {v: Entity Judge | v == getJust (entityKey v)} @-}



{-@ assume judgeId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Judge JudgeId
  @-}
judgeId' :: EntityFieldWrapper (Entity User) Judge JudgeId
judgeId' = EntityFieldWrapper JudgeId

{-@ measure judgeJudge :: Judge -> UserId @-}

{-@ measure judgeJudgeCap :: Entity Judge -> Bool @-}

{-@ assume judgeJudge' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == judgeJudge (entityVal row)},
                          {\field row -> field == judgeJudge (entityVal row)},
                          {\old -> judgeJudgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (judgeJudgeCap x_0)}>
                          (Entity User) Judge UserId
  @-}
judgeJudge' :: EntityFieldWrapper (Entity User) Judge UserId
judgeJudge' = EntityFieldWrapper JudgeJudge

{-@ measure judgeContest :: Judge -> ContestId @-}

{-@ measure judgeContestCap :: Entity Judge -> Bool @-}

{-@ assume judgeContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == judgeContest (entityVal row)},
                          {\field row -> field == judgeContest (entityVal row)},
                          {\old -> judgeContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (judgeContestCap x_0)}>
                          (Entity User) Judge ContestId
  @-}
judgeContest' :: EntityFieldWrapper (Entity User) Judge ContestId
judgeContest' = EntityFieldWrapper JudgeContest

{-@ measure judgeAssignedCount :: Judge -> Int @-}

{-@ measure judgeAssignedCountCap :: Entity Judge -> Bool @-}

{-@ assume judgeAssignedCount' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == judgeAssignedCount (entityVal row)},
                          {\field row -> field == judgeAssignedCount (entityVal row)},
                          {\old -> judgeAssignedCountCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (judgeAssignedCountCap x_0)}>
                          (Entity User) Judge Int
  @-}
judgeAssignedCount' :: EntityFieldWrapper (Entity User) Judge Int
judgeAssignedCount' = EntityFieldWrapper JudgeAssignedCount

-- * JudgeConflict
{-@ mkJudgeConflict ::
        x_0: JudgeId
     -> x_1: TeamContestId
     -> BinahRecord <{\row -> judgeConflictJudge (entityVal row) == x_0 && judgeConflictTeam (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) JudgeConflict
  @-}
mkJudgeConflict :: JudgeId -> TeamContestId -> BinahRecord (Entity User) JudgeConflict
mkJudgeConflict x_0 x_1 = BinahRecord (JudgeConflict x_0 x_1)

{-@ invariant {v: Entity JudgeConflict | v == getJust (entityKey v)} @-}



{-@ assume judgeConflictId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) JudgeConflict JudgeConflictId
  @-}
judgeConflictId' :: EntityFieldWrapper (Entity User) JudgeConflict JudgeConflictId
judgeConflictId' = EntityFieldWrapper JudgeConflictId

{-@ measure judgeConflictJudge :: JudgeConflict -> JudgeId @-}

{-@ measure judgeConflictJudgeCap :: Entity JudgeConflict -> Bool @-}

{-@ assume judgeConflictJudge' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == judgeConflictJudge (entityVal row)},
                          {\field row -> field == judgeConflictJudge (entityVal row)},
                          {\old -> judgeConflictJudgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (judgeConflictJudgeCap x_0)}>
                          (Entity User) JudgeConflict JudgeId
  @-}
judgeConflictJudge' :: EntityFieldWrapper (Entity User) JudgeConflict JudgeId
judgeConflictJudge' = EntityFieldWrapper JudgeConflictJudge

{-@ measure judgeConflictTeam :: JudgeConflict -> TeamContestId @-}

{-@ measure judgeConflictTeamCap :: Entity JudgeConflict -> Bool @-}

{-@ assume judgeConflictTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == judgeConflictTeam (entityVal row)},
                          {\field row -> field == judgeConflictTeam (entityVal row)},
                          {\old -> judgeConflictTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (judgeConflictTeamCap x_0)}>
                          (Entity User) JudgeConflict TeamContestId
  @-}
judgeConflictTeam' :: EntityFieldWrapper (Entity User) JudgeConflict TeamContestId
judgeConflictTeam' = EntityFieldWrapper JudgeConflictTeam

-- * BuildJudgement
{-@ mkBuildJudgement ::
        x_0: JudgeId
     -> x_1: BuildSubmissionId
     -> x_2: Maybe Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> buildJudgementJudge (entityVal row) == x_0 && buildJudgementSubmission (entityVal row) == x_1 && buildJudgementRuling (entityVal row) == x_2 && buildJudgementComments (entityVal row) == x_3},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildJudgement
  @-}
mkBuildJudgement :: JudgeId -> BuildSubmissionId -> Maybe Bool -> Maybe Text -> BinahRecord (Entity User) BuildJudgement
mkBuildJudgement x_0 x_1 x_2 x_3 = BinahRecord (BuildJudgement x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BuildJudgement | v == getJust (entityKey v)} @-}



{-@ assume buildJudgementId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildJudgement BuildJudgementId
  @-}
buildJudgementId' :: EntityFieldWrapper (Entity User) BuildJudgement BuildJudgementId
buildJudgementId' = EntityFieldWrapper BuildJudgementId

{-@ measure buildJudgementJudge :: BuildJudgement -> JudgeId @-}

{-@ measure buildJudgementJudgeCap :: Entity BuildJudgement -> Bool @-}

{-@ assume buildJudgementJudge' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildJudgementJudge (entityVal row)},
                          {\field row -> field == buildJudgementJudge (entityVal row)},
                          {\old -> buildJudgementJudgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildJudgementJudgeCap x_0)}>
                          (Entity User) BuildJudgement JudgeId
  @-}
buildJudgementJudge' :: EntityFieldWrapper (Entity User) BuildJudgement JudgeId
buildJudgementJudge' = EntityFieldWrapper BuildJudgementJudge

{-@ measure buildJudgementSubmission :: BuildJudgement -> BuildSubmissionId @-}

{-@ measure buildJudgementSubmissionCap :: Entity BuildJudgement -> Bool @-}

{-@ assume buildJudgementSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildJudgementSubmission (entityVal row)},
                          {\field row -> field == buildJudgementSubmission (entityVal row)},
                          {\old -> buildJudgementSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildJudgementSubmissionCap x_0)}>
                          (Entity User) BuildJudgement BuildSubmissionId
  @-}
buildJudgementSubmission' :: EntityFieldWrapper (Entity User) BuildJudgement BuildSubmissionId
buildJudgementSubmission' = EntityFieldWrapper BuildJudgementSubmission

{-@ measure buildJudgementRuling :: BuildJudgement -> (Maybe Bool) @-}

{-@ measure buildJudgementRulingCap :: Entity BuildJudgement -> Bool @-}

{-@ assume buildJudgementRuling' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildJudgementRuling (entityVal row)},
                          {\field row -> field == buildJudgementRuling (entityVal row)},
                          {\old -> buildJudgementRulingCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (buildJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (buildJudgementRulingCap x_0)}>
                          (Entity User) BuildJudgement (Maybe Bool)
  @-}
buildJudgementRuling' :: EntityFieldWrapper (Entity User) BuildJudgement (Maybe Bool)
buildJudgementRuling' = EntityFieldWrapper BuildJudgementRuling

{-@ measure buildJudgementComments :: BuildJudgement -> (Maybe Text) @-}

{-@ measure buildJudgementCommentsCap :: Entity BuildJudgement -> Bool @-}

{-@ assume buildJudgementComments' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildJudgementComments (entityVal row)},
                          {\field row -> field == buildJudgementComments (entityVal row)},
                          {\old -> buildJudgementCommentsCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (buildJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (buildJudgementCommentsCap x_0)}>
                          (Entity User) BuildJudgement (Maybe Text)
  @-}
buildJudgementComments' :: EntityFieldWrapper (Entity User) BuildJudgement (Maybe Text)
buildJudgementComments' = EntityFieldWrapper BuildJudgementComments

-- * BreakJudgement
{-@ mkBreakJudgement ::
        x_0: JudgeId
     -> x_1: BreakSubmissionId
     -> x_2: Maybe Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> breakJudgementJudge (entityVal row) == x_0 && breakJudgementSubmission (entityVal row) == x_1 && breakJudgementRuling (entityVal row) == x_2 && breakJudgementComments (entityVal row) == x_3},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakJudgement
  @-}
mkBreakJudgement :: JudgeId -> BreakSubmissionId -> Maybe Bool -> Maybe Text -> BinahRecord (Entity User) BreakJudgement
mkBreakJudgement x_0 x_1 x_2 x_3 = BinahRecord (BreakJudgement x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BreakJudgement | v == getJust (entityKey v)} @-}



{-@ assume breakJudgementId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakJudgement BreakJudgementId
  @-}
breakJudgementId' :: EntityFieldWrapper (Entity User) BreakJudgement BreakJudgementId
breakJudgementId' = EntityFieldWrapper BreakJudgementId

{-@ measure breakJudgementJudge :: BreakJudgement -> JudgeId @-}

{-@ measure breakJudgementJudgeCap :: Entity BreakJudgement -> Bool @-}

{-@ assume breakJudgementJudge' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakJudgementJudge (entityVal row)},
                          {\field row -> field == breakJudgementJudge (entityVal row)},
                          {\old -> breakJudgementJudgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakJudgementJudgeCap x_0)}>
                          (Entity User) BreakJudgement JudgeId
  @-}
breakJudgementJudge' :: EntityFieldWrapper (Entity User) BreakJudgement JudgeId
breakJudgementJudge' = EntityFieldWrapper BreakJudgementJudge

{-@ measure breakJudgementSubmission :: BreakJudgement -> BreakSubmissionId @-}

{-@ measure breakJudgementSubmissionCap :: Entity BreakJudgement -> Bool @-}

{-@ assume breakJudgementSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakJudgementSubmission (entityVal row)},
                          {\field row -> field == breakJudgementSubmission (entityVal row)},
                          {\old -> breakJudgementSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakJudgementSubmissionCap x_0)}>
                          (Entity User) BreakJudgement BreakSubmissionId
  @-}
breakJudgementSubmission' :: EntityFieldWrapper (Entity User) BreakJudgement BreakSubmissionId
breakJudgementSubmission' = EntityFieldWrapper BreakJudgementSubmission

{-@ measure breakJudgementRuling :: BreakJudgement -> (Maybe Bool) @-}

{-@ measure breakJudgementRulingCap :: Entity BreakJudgement -> Bool @-}

{-@ assume breakJudgementRuling' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakJudgementRuling (entityVal row)},
                          {\field row -> field == breakJudgementRuling (entityVal row)},
                          {\old -> breakJudgementRulingCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (breakJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (breakJudgementRulingCap x_0)}>
                          (Entity User) BreakJudgement (Maybe Bool)
  @-}
breakJudgementRuling' :: EntityFieldWrapper (Entity User) BreakJudgement (Maybe Bool)
breakJudgementRuling' = EntityFieldWrapper BreakJudgementRuling

{-@ measure breakJudgementComments :: BreakJudgement -> (Maybe Text) @-}

{-@ measure breakJudgementCommentsCap :: Entity BreakJudgement -> Bool @-}

{-@ assume breakJudgementComments' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakJudgementComments (entityVal row)},
                          {\field row -> field == breakJudgementComments (entityVal row)},
                          {\old -> breakJudgementCommentsCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (breakJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (breakJudgementCommentsCap x_0)}>
                          (Entity User) BreakJudgement (Maybe Text)
  @-}
breakJudgementComments' :: EntityFieldWrapper (Entity User) BreakJudgement (Maybe Text)
breakJudgementComments' = EntityFieldWrapper BreakJudgementComments

-- * FixJudgement
{-@ mkFixJudgement ::
        x_0: JudgeId
     -> x_1: FixSubmissionId
     -> x_2: Maybe Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> fixJudgementJudge (entityVal row) == x_0 && fixJudgementSubmission (entityVal row) == x_1 && fixJudgementRuling (entityVal row) == x_2 && fixJudgementComments (entityVal row) == x_3},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixJudgement
  @-}
mkFixJudgement :: JudgeId -> FixSubmissionId -> Maybe Bool -> Maybe Text -> BinahRecord (Entity User) FixJudgement
mkFixJudgement x_0 x_1 x_2 x_3 = BinahRecord (FixJudgement x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity FixJudgement | v == getJust (entityKey v)} @-}



{-@ assume fixJudgementId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixJudgement FixJudgementId
  @-}
fixJudgementId' :: EntityFieldWrapper (Entity User) FixJudgement FixJudgementId
fixJudgementId' = EntityFieldWrapper FixJudgementId

{-@ measure fixJudgementJudge :: FixJudgement -> JudgeId @-}

{-@ measure fixJudgementJudgeCap :: Entity FixJudgement -> Bool @-}

{-@ assume fixJudgementJudge' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixJudgementJudge (entityVal row)},
                          {\field row -> field == fixJudgementJudge (entityVal row)},
                          {\old -> fixJudgementJudgeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixJudgementJudgeCap x_0)}>
                          (Entity User) FixJudgement JudgeId
  @-}
fixJudgementJudge' :: EntityFieldWrapper (Entity User) FixJudgement JudgeId
fixJudgementJudge' = EntityFieldWrapper FixJudgementJudge

{-@ measure fixJudgementSubmission :: FixJudgement -> FixSubmissionId @-}

{-@ measure fixJudgementSubmissionCap :: Entity FixJudgement -> Bool @-}

{-@ assume fixJudgementSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixJudgementSubmission (entityVal row)},
                          {\field row -> field == fixJudgementSubmission (entityVal row)},
                          {\old -> fixJudgementSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixJudgementSubmissionCap x_0)}>
                          (Entity User) FixJudgement FixSubmissionId
  @-}
fixJudgementSubmission' :: EntityFieldWrapper (Entity User) FixJudgement FixSubmissionId
fixJudgementSubmission' = EntityFieldWrapper FixJudgementSubmission

{-@ measure fixJudgementRuling :: FixJudgement -> (Maybe Bool) @-}

{-@ measure fixJudgementRulingCap :: Entity FixJudgement -> Bool @-}

{-@ assume fixJudgementRuling' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixJudgementRuling (entityVal row)},
                          {\field row -> field == fixJudgementRuling (entityVal row)},
                          {\old -> fixJudgementRulingCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (fixJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (fixJudgementRulingCap x_0)}>
                          (Entity User) FixJudgement (Maybe Bool)
  @-}
fixJudgementRuling' :: EntityFieldWrapper (Entity User) FixJudgement (Maybe Bool)
fixJudgementRuling' = EntityFieldWrapper FixJudgementRuling

{-@ measure fixJudgementComments :: FixJudgement -> (Maybe Text) @-}

{-@ measure fixJudgementCommentsCap :: Entity FixJudgement -> Bool @-}

{-@ assume fixJudgementComments' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixJudgementComments (entityVal row)},
                          {\field row -> field == fixJudgementComments (entityVal row)},
                          {\old -> fixJudgementCommentsCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2) || judgeJudge (entityVal (getJust (fixJudgementJudge (entityVal x_0)))) == entityKey x_2)) => (fixJudgementCommentsCap x_0)}>
                          (Entity User) FixJudgement (Maybe Text)
  @-}
fixJudgementComments' :: EntityFieldWrapper (Entity User) FixJudgement (Maybe Text)
fixJudgementComments' = EntityFieldWrapper FixJudgementComments

-- * BreakDispute
{-@ mkBreakDispute ::
        x_0: BreakSubmissionId
     -> x_1: Text
     -> BinahRecord <{\row -> breakDisputeBreak (entityVal row) == x_0 && breakDisputeJustification (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakDispute
  @-}
mkBreakDispute :: BreakSubmissionId -> Text -> BinahRecord (Entity User) BreakDispute
mkBreakDispute x_0 x_1 = BinahRecord (BreakDispute x_0 x_1)

{-@ invariant {v: Entity BreakDispute | v == getJust (entityKey v)} @-}



{-@ assume breakDisputeId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakDispute BreakDisputeId
  @-}
breakDisputeId' :: EntityFieldWrapper (Entity User) BreakDispute BreakDisputeId
breakDisputeId' = EntityFieldWrapper BreakDisputeId

{-@ measure breakDisputeBreak :: BreakDispute -> BreakSubmissionId @-}

{-@ measure breakDisputeBreakCap :: Entity BreakDispute -> Bool @-}

{-@ assume breakDisputeBreak' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakDisputeBreak (entityVal row)},
                          {\field row -> field == breakDisputeBreak (entityVal row)},
                          {\old -> breakDisputeBreakCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakDisputeBreakCap x_0)}>
                          (Entity User) BreakDispute BreakSubmissionId
  @-}
breakDisputeBreak' :: EntityFieldWrapper (Entity User) BreakDispute BreakSubmissionId
breakDisputeBreak' = EntityFieldWrapper BreakDisputeBreak

{-@ measure breakDisputeJustification :: BreakDispute -> Text @-}

{-@ measure breakDisputeJustificationCap :: Entity BreakDispute -> Bool @-}

{-@ assume breakDisputeJustification' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakDisputeJustification (entityVal row)},
                          {\field row -> field == breakDisputeJustification (entityVal row)},
                          {\old -> breakDisputeJustificationCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakDisputeJustificationCap x_0)}>
                          (Entity User) BreakDispute Text
  @-}
breakDisputeJustification' :: EntityFieldWrapper (Entity User) BreakDispute Text
breakDisputeJustification' = EntityFieldWrapper BreakDisputeJustification

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
                          {\x_0 x_1 x_2 -> ((False)) => (teamNameCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamLeaderCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestTeamCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestContestCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestGitUrlCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestLanguagesCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestProfessionalCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestGithookNonceCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (teamContestGitRepositoryIdentifierCap x_0)}>
                          (Entity User) TeamContest (Maybe RepositoryIdentifier)
  @-}
teamContestGitRepositoryIdentifier' :: EntityFieldWrapper (Entity User) TeamContest (Maybe RepositoryIdentifier)
teamContestGitRepositoryIdentifier' = EntityFieldWrapper TeamContestGitRepositoryIdentifier

-- * TeamMember
{-@ mkTeamMember ::
        x_0: TeamId
     -> x_1: UserId
     -> BinahRecord <{\row -> teamMemberTeam (entityVal row) == x_0 && teamMemberUser (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TeamMember
  @-}
mkTeamMember :: TeamId -> UserId -> BinahRecord (Entity User) TeamMember
mkTeamMember x_0 x_1 = BinahRecord (TeamMember x_0 x_1)

{-@ invariant {v: Entity TeamMember | v == getJust (entityKey v)} @-}



{-@ assume teamMemberId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TeamMember TeamMemberId
  @-}
teamMemberId' :: EntityFieldWrapper (Entity User) TeamMember TeamMemberId
teamMemberId' = EntityFieldWrapper TeamMemberId

{-@ measure teamMemberTeam :: TeamMember -> TeamId @-}

{-@ measure teamMemberTeamCap :: Entity TeamMember -> Bool @-}

{-@ assume teamMemberTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamMemberTeam (entityVal row)},
                          {\field row -> field == teamMemberTeam (entityVal row)},
                          {\old -> teamMemberTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamMemberTeamCap x_0)}>
                          (Entity User) TeamMember TeamId
  @-}
teamMemberTeam' :: EntityFieldWrapper (Entity User) TeamMember TeamId
teamMemberTeam' = EntityFieldWrapper TeamMemberTeam

{-@ measure teamMemberUser :: TeamMember -> UserId @-}

{-@ measure teamMemberUserCap :: Entity TeamMember -> Bool @-}

{-@ assume teamMemberUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamMemberUser (entityVal row)},
                          {\field row -> field == teamMemberUser (entityVal row)},
                          {\old -> teamMemberUserCap old},
                          {\old _ _ -> teamMemberUserCap old}>
                          (Entity User) TeamMember UserId
  @-}
teamMemberUser' :: EntityFieldWrapper (Entity User) TeamMember UserId
teamMemberUser' = EntityFieldWrapper TeamMemberUser

-- * TeamInvite
{-@ mkTeamInvite ::
        x_0: Text
     -> x_1: TeamId
     -> x_2: Text
     -> BinahRecord <{\row -> teamInviteInvite (entityVal row) == x_0 && teamInviteTeam (entityVal row) == x_1 && teamInviteEmail (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TeamInvite
  @-}
mkTeamInvite :: Text -> TeamId -> Text -> BinahRecord (Entity User) TeamInvite
mkTeamInvite x_0 x_1 x_2 = BinahRecord (TeamInvite x_0 x_1 x_2)

{-@ invariant {v: Entity TeamInvite | v == getJust (entityKey v)} @-}



{-@ assume teamInviteId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TeamInvite TeamInviteId
  @-}
teamInviteId' :: EntityFieldWrapper (Entity User) TeamInvite TeamInviteId
teamInviteId' = EntityFieldWrapper TeamInviteId

{-@ measure teamInviteInvite :: TeamInvite -> Text @-}

{-@ measure teamInviteInviteCap :: Entity TeamInvite -> Bool @-}

{-@ assume teamInviteInvite' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamInviteInvite (entityVal row)},
                          {\field row -> field == teamInviteInvite (entityVal row)},
                          {\old -> teamInviteInviteCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamInviteInviteCap x_0)}>
                          (Entity User) TeamInvite Text
  @-}
teamInviteInvite' :: EntityFieldWrapper (Entity User) TeamInvite Text
teamInviteInvite' = EntityFieldWrapper TeamInviteInvite

{-@ measure teamInviteTeam :: TeamInvite -> TeamId @-}

{-@ measure teamInviteTeamCap :: Entity TeamInvite -> Bool @-}

{-@ assume teamInviteTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamInviteTeam (entityVal row)},
                          {\field row -> field == teamInviteTeam (entityVal row)},
                          {\old -> teamInviteTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamInviteTeamCap x_0)}>
                          (Entity User) TeamInvite TeamId
  @-}
teamInviteTeam' :: EntityFieldWrapper (Entity User) TeamInvite TeamId
teamInviteTeam' = EntityFieldWrapper TeamInviteTeam

{-@ measure teamInviteEmail :: TeamInvite -> Text @-}

{-@ measure teamInviteEmailCap :: Entity TeamInvite -> Bool @-}

{-@ assume teamInviteEmail' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamInviteEmail (entityVal row)},
                          {\field row -> field == teamInviteEmail (entityVal row)},
                          {\old -> teamInviteEmailCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamInviteEmailCap x_0)}>
                          (Entity User) TeamInvite Text
  @-}
teamInviteEmail' :: EntityFieldWrapper (Entity User) TeamInvite Text
teamInviteEmail' = EntityFieldWrapper TeamInviteEmail

-- * PasswordResetInvite
{-@ mkPasswordResetInvite ::
        x_0: UserId
     -> x_1: Text
     -> x_2: UTCTime
     -> BinahRecord <{\row -> passwordResetInviteAccount (entityVal row) == x_0 && passwordResetInviteInvite (entityVal row) == x_1 && passwordResetInviteExpiration (entityVal row) == x_2},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) PasswordResetInvite
  @-}
mkPasswordResetInvite :: UserId -> Text -> UTCTime -> BinahRecord (Entity User) PasswordResetInvite
mkPasswordResetInvite x_0 x_1 x_2 = BinahRecord (PasswordResetInvite x_0 x_1 x_2)

{-@ invariant {v: Entity PasswordResetInvite | v == getJust (entityKey v)} @-}



{-@ assume passwordResetInviteId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) PasswordResetInvite PasswordResetInviteId
  @-}
passwordResetInviteId' :: EntityFieldWrapper (Entity User) PasswordResetInvite PasswordResetInviteId
passwordResetInviteId' = EntityFieldWrapper PasswordResetInviteId

{-@ measure passwordResetInviteAccount :: PasswordResetInvite -> UserId @-}

{-@ measure passwordResetInviteAccountCap :: Entity PasswordResetInvite -> Bool @-}

{-@ assume passwordResetInviteAccount' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == passwordResetInviteAccount (entityVal row)},
                          {\field row -> field == passwordResetInviteAccount (entityVal row)},
                          {\old -> passwordResetInviteAccountCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (passwordResetInviteAccountCap x_0)}>
                          (Entity User) PasswordResetInvite UserId
  @-}
passwordResetInviteAccount' :: EntityFieldWrapper (Entity User) PasswordResetInvite UserId
passwordResetInviteAccount' = EntityFieldWrapper PasswordResetInviteAccount

{-@ measure passwordResetInviteInvite :: PasswordResetInvite -> Text @-}

{-@ measure passwordResetInviteInviteCap :: Entity PasswordResetInvite -> Bool @-}

{-@ assume passwordResetInviteInvite' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == passwordResetInviteInvite (entityVal row)},
                          {\field row -> field == passwordResetInviteInvite (entityVal row)},
                          {\old -> passwordResetInviteInviteCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (passwordResetInviteInviteCap x_0)}>
                          (Entity User) PasswordResetInvite Text
  @-}
passwordResetInviteInvite' :: EntityFieldWrapper (Entity User) PasswordResetInvite Text
passwordResetInviteInvite' = EntityFieldWrapper PasswordResetInviteInvite

{-@ measure passwordResetInviteExpiration :: PasswordResetInvite -> UTCTime @-}

{-@ measure passwordResetInviteExpirationCap :: Entity PasswordResetInvite -> Bool @-}

{-@ assume passwordResetInviteExpiration' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == passwordResetInviteExpiration (entityVal row)},
                          {\field row -> field == passwordResetInviteExpiration (entityVal row)},
                          {\old -> passwordResetInviteExpirationCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (passwordResetInviteExpirationCap x_0)}>
                          (Entity User) PasswordResetInvite UTCTime
  @-}
passwordResetInviteExpiration' :: EntityFieldWrapper (Entity User) PasswordResetInvite UTCTime
passwordResetInviteExpiration' = EntityFieldWrapper PasswordResetInviteExpiration

-- * ContestCoreTest
{-@ mkContestCoreTest ::
        x_0: ContestId
     -> x_1: Text
     -> x_2: Text
     -> x_3: Text
     -> x_4: Text
     -> BinahRecord <{\row -> contestCoreTestContest (entityVal row) == x_0 && contestCoreTestName (entityVal row) == x_1 && contestCoreTestInputFile (entityVal row) == x_2 && contestCoreTestOutputFile (entityVal row) == x_3 && contestCoreTestTestScript (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) ContestCoreTest
  @-}
mkContestCoreTest :: ContestId -> Text -> Text -> Text -> Text -> BinahRecord (Entity User) ContestCoreTest
mkContestCoreTest x_0 x_1 x_2 x_3 x_4 = BinahRecord (ContestCoreTest x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity ContestCoreTest | v == getJust (entityKey v)} @-}



{-@ assume contestCoreTestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) ContestCoreTest ContestCoreTestId
  @-}
contestCoreTestId' :: EntityFieldWrapper (Entity User) ContestCoreTest ContestCoreTestId
contestCoreTestId' = EntityFieldWrapper ContestCoreTestId

{-@ measure contestCoreTestContest :: ContestCoreTest -> ContestId @-}

{-@ measure contestCoreTestContestCap :: Entity ContestCoreTest -> Bool @-}

{-@ assume contestCoreTestContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestCoreTestContest (entityVal row)},
                          {\field row -> field == contestCoreTestContest (entityVal row)},
                          {\old -> contestCoreTestContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestCoreTestContestCap x_0)}>
                          (Entity User) ContestCoreTest ContestId
  @-}
contestCoreTestContest' :: EntityFieldWrapper (Entity User) ContestCoreTest ContestId
contestCoreTestContest' = EntityFieldWrapper ContestCoreTestContest

{-@ measure contestCoreTestName :: ContestCoreTest -> Text @-}

{-@ measure contestCoreTestNameCap :: Entity ContestCoreTest -> Bool @-}

{-@ assume contestCoreTestName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestCoreTestName (entityVal row)},
                          {\field row -> field == contestCoreTestName (entityVal row)},
                          {\old -> contestCoreTestNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestCoreTestNameCap x_0)}>
                          (Entity User) ContestCoreTest Text
  @-}
contestCoreTestName' :: EntityFieldWrapper (Entity User) ContestCoreTest Text
contestCoreTestName' = EntityFieldWrapper ContestCoreTestName

{-@ measure contestCoreTestInputFile :: ContestCoreTest -> Text @-}

{-@ measure contestCoreTestInputFileCap :: Entity ContestCoreTest -> Bool @-}

{-@ assume contestCoreTestInputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestCoreTestInputFile (entityVal row)},
                          {\field row -> field == contestCoreTestInputFile (entityVal row)},
                          {\old -> contestCoreTestInputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestCoreTestInputFileCap x_0)}>
                          (Entity User) ContestCoreTest Text
  @-}
contestCoreTestInputFile' :: EntityFieldWrapper (Entity User) ContestCoreTest Text
contestCoreTestInputFile' = EntityFieldWrapper ContestCoreTestInputFile

{-@ measure contestCoreTestOutputFile :: ContestCoreTest -> Text @-}

{-@ measure contestCoreTestOutputFileCap :: Entity ContestCoreTest -> Bool @-}

{-@ assume contestCoreTestOutputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestCoreTestOutputFile (entityVal row)},
                          {\field row -> field == contestCoreTestOutputFile (entityVal row)},
                          {\old -> contestCoreTestOutputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestCoreTestOutputFileCap x_0)}>
                          (Entity User) ContestCoreTest Text
  @-}
contestCoreTestOutputFile' :: EntityFieldWrapper (Entity User) ContestCoreTest Text
contestCoreTestOutputFile' = EntityFieldWrapper ContestCoreTestOutputFile

{-@ measure contestCoreTestTestScript :: ContestCoreTest -> Text @-}

{-@ measure contestCoreTestTestScriptCap :: Entity ContestCoreTest -> Bool @-}

{-@ assume contestCoreTestTestScript' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestCoreTestTestScript (entityVal row)},
                          {\field row -> field == contestCoreTestTestScript (entityVal row)},
                          {\old -> contestCoreTestTestScriptCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestCoreTestTestScriptCap x_0)}>
                          (Entity User) ContestCoreTest Text
  @-}
contestCoreTestTestScript' :: EntityFieldWrapper (Entity User) ContestCoreTest Text
contestCoreTestTestScript' = EntityFieldWrapper ContestCoreTestTestScript

-- * ContestPerformanceTest
{-@ mkContestPerformanceTest ::
        x_0: ContestId
     -> x_1: Text
     -> x_2: Text
     -> x_3: Text
     -> x_4: Text
     -> x_5: Bool
     -> BinahRecord <{\row -> contestPerformanceTestContest (entityVal row) == x_0 && contestPerformanceTestName (entityVal row) == x_1 && contestPerformanceTestInputFile (entityVal row) == x_2 && contestPerformanceTestOutputFile (entityVal row) == x_3 && contestPerformanceTestTestScript (entityVal row) == x_4 && contestPerformanceTestOptional (entityVal row) == x_5},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) ContestPerformanceTest
  @-}
mkContestPerformanceTest :: ContestId -> Text -> Text -> Text -> Text -> Bool -> BinahRecord (Entity User) ContestPerformanceTest
mkContestPerformanceTest x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (ContestPerformanceTest x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity ContestPerformanceTest | v == getJust (entityKey v)} @-}



{-@ assume contestPerformanceTestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) ContestPerformanceTest ContestPerformanceTestId
  @-}
contestPerformanceTestId' :: EntityFieldWrapper (Entity User) ContestPerformanceTest ContestPerformanceTestId
contestPerformanceTestId' = EntityFieldWrapper ContestPerformanceTestId

{-@ measure contestPerformanceTestContest :: ContestPerformanceTest -> ContestId @-}

{-@ measure contestPerformanceTestContestCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestContest (entityVal row)},
                          {\field row -> field == contestPerformanceTestContest (entityVal row)},
                          {\old -> contestPerformanceTestContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestContestCap x_0)}>
                          (Entity User) ContestPerformanceTest ContestId
  @-}
contestPerformanceTestContest' :: EntityFieldWrapper (Entity User) ContestPerformanceTest ContestId
contestPerformanceTestContest' = EntityFieldWrapper ContestPerformanceTestContest

{-@ measure contestPerformanceTestName :: ContestPerformanceTest -> Text @-}

{-@ measure contestPerformanceTestNameCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestName (entityVal row)},
                          {\field row -> field == contestPerformanceTestName (entityVal row)},
                          {\old -> contestPerformanceTestNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestNameCap x_0)}>
                          (Entity User) ContestPerformanceTest Text
  @-}
contestPerformanceTestName' :: EntityFieldWrapper (Entity User) ContestPerformanceTest Text
contestPerformanceTestName' = EntityFieldWrapper ContestPerformanceTestName

{-@ measure contestPerformanceTestInputFile :: ContestPerformanceTest -> Text @-}

{-@ measure contestPerformanceTestInputFileCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestInputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestInputFile (entityVal row)},
                          {\field row -> field == contestPerformanceTestInputFile (entityVal row)},
                          {\old -> contestPerformanceTestInputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestInputFileCap x_0)}>
                          (Entity User) ContestPerformanceTest Text
  @-}
contestPerformanceTestInputFile' :: EntityFieldWrapper (Entity User) ContestPerformanceTest Text
contestPerformanceTestInputFile' = EntityFieldWrapper ContestPerformanceTestInputFile

{-@ measure contestPerformanceTestOutputFile :: ContestPerformanceTest -> Text @-}

{-@ measure contestPerformanceTestOutputFileCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestOutputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestOutputFile (entityVal row)},
                          {\field row -> field == contestPerformanceTestOutputFile (entityVal row)},
                          {\old -> contestPerformanceTestOutputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestOutputFileCap x_0)}>
                          (Entity User) ContestPerformanceTest Text
  @-}
contestPerformanceTestOutputFile' :: EntityFieldWrapper (Entity User) ContestPerformanceTest Text
contestPerformanceTestOutputFile' = EntityFieldWrapper ContestPerformanceTestOutputFile

{-@ measure contestPerformanceTestTestScript :: ContestPerformanceTest -> Text @-}

{-@ measure contestPerformanceTestTestScriptCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestTestScript' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestTestScript (entityVal row)},
                          {\field row -> field == contestPerformanceTestTestScript (entityVal row)},
                          {\old -> contestPerformanceTestTestScriptCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestTestScriptCap x_0)}>
                          (Entity User) ContestPerformanceTest Text
  @-}
contestPerformanceTestTestScript' :: EntityFieldWrapper (Entity User) ContestPerformanceTest Text
contestPerformanceTestTestScript' = EntityFieldWrapper ContestPerformanceTestTestScript

{-@ measure contestPerformanceTestOptional :: ContestPerformanceTest -> Bool @-}

{-@ measure contestPerformanceTestOptionalCap :: Entity ContestPerformanceTest -> Bool @-}

{-@ assume contestPerformanceTestOptional' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestPerformanceTestOptional (entityVal row)},
                          {\field row -> field == contestPerformanceTestOptional (entityVal row)},
                          {\old -> contestPerformanceTestOptionalCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestPerformanceTestOptionalCap x_0)}>
                          (Entity User) ContestPerformanceTest Bool
  @-}
contestPerformanceTestOptional' :: EntityFieldWrapper (Entity User) ContestPerformanceTest Bool
contestPerformanceTestOptional' = EntityFieldWrapper ContestPerformanceTestOptional

-- * ContestOptionalTest
{-@ mkContestOptionalTest ::
        x_0: ContestId
     -> x_1: Text
     -> x_2: Text
     -> x_3: Text
     -> x_4: Text
     -> BinahRecord <{\row -> contestOptionalTestContest (entityVal row) == x_0 && contestOptionalTestName (entityVal row) == x_1 && contestOptionalTestInputFile (entityVal row) == x_2 && contestOptionalTestOutputFile (entityVal row) == x_3 && contestOptionalTestTestScript (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) ContestOptionalTest
  @-}
mkContestOptionalTest :: ContestId -> Text -> Text -> Text -> Text -> BinahRecord (Entity User) ContestOptionalTest
mkContestOptionalTest x_0 x_1 x_2 x_3 x_4 = BinahRecord (ContestOptionalTest x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity ContestOptionalTest | v == getJust (entityKey v)} @-}



{-@ assume contestOptionalTestId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) ContestOptionalTest ContestOptionalTestId
  @-}
contestOptionalTestId' :: EntityFieldWrapper (Entity User) ContestOptionalTest ContestOptionalTestId
contestOptionalTestId' = EntityFieldWrapper ContestOptionalTestId

{-@ measure contestOptionalTestContest :: ContestOptionalTest -> ContestId @-}

{-@ measure contestOptionalTestContestCap :: Entity ContestOptionalTest -> Bool @-}

{-@ assume contestOptionalTestContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestOptionalTestContest (entityVal row)},
                          {\field row -> field == contestOptionalTestContest (entityVal row)},
                          {\old -> contestOptionalTestContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestOptionalTestContestCap x_0)}>
                          (Entity User) ContestOptionalTest ContestId
  @-}
contestOptionalTestContest' :: EntityFieldWrapper (Entity User) ContestOptionalTest ContestId
contestOptionalTestContest' = EntityFieldWrapper ContestOptionalTestContest

{-@ measure contestOptionalTestName :: ContestOptionalTest -> Text @-}

{-@ measure contestOptionalTestNameCap :: Entity ContestOptionalTest -> Bool @-}

{-@ assume contestOptionalTestName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestOptionalTestName (entityVal row)},
                          {\field row -> field == contestOptionalTestName (entityVal row)},
                          {\old -> contestOptionalTestNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestOptionalTestNameCap x_0)}>
                          (Entity User) ContestOptionalTest Text
  @-}
contestOptionalTestName' :: EntityFieldWrapper (Entity User) ContestOptionalTest Text
contestOptionalTestName' = EntityFieldWrapper ContestOptionalTestName

{-@ measure contestOptionalTestInputFile :: ContestOptionalTest -> Text @-}

{-@ measure contestOptionalTestInputFileCap :: Entity ContestOptionalTest -> Bool @-}

{-@ assume contestOptionalTestInputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestOptionalTestInputFile (entityVal row)},
                          {\field row -> field == contestOptionalTestInputFile (entityVal row)},
                          {\old -> contestOptionalTestInputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestOptionalTestInputFileCap x_0)}>
                          (Entity User) ContestOptionalTest Text
  @-}
contestOptionalTestInputFile' :: EntityFieldWrapper (Entity User) ContestOptionalTest Text
contestOptionalTestInputFile' = EntityFieldWrapper ContestOptionalTestInputFile

{-@ measure contestOptionalTestOutputFile :: ContestOptionalTest -> Text @-}

{-@ measure contestOptionalTestOutputFileCap :: Entity ContestOptionalTest -> Bool @-}

{-@ assume contestOptionalTestOutputFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestOptionalTestOutputFile (entityVal row)},
                          {\field row -> field == contestOptionalTestOutputFile (entityVal row)},
                          {\old -> contestOptionalTestOutputFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestOptionalTestOutputFileCap x_0)}>
                          (Entity User) ContestOptionalTest Text
  @-}
contestOptionalTestOutputFile' :: EntityFieldWrapper (Entity User) ContestOptionalTest Text
contestOptionalTestOutputFile' = EntityFieldWrapper ContestOptionalTestOutputFile

{-@ measure contestOptionalTestTestScript :: ContestOptionalTest -> Text @-}

{-@ measure contestOptionalTestTestScriptCap :: Entity ContestOptionalTest -> Bool @-}

{-@ assume contestOptionalTestTestScript' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == contestOptionalTestTestScript (entityVal row)},
                          {\field row -> field == contestOptionalTestTestScript (entityVal row)},
                          {\old -> contestOptionalTestTestScriptCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (contestOptionalTestTestScriptCap x_0)}>
                          (Entity User) ContestOptionalTest Text
  @-}
contestOptionalTestTestScript' :: EntityFieldWrapper (Entity User) ContestOptionalTest Text
contestOptionalTestTestScript' = EntityFieldWrapper ContestOptionalTestTestScript

-- * TeamBreakScore
{-@ mkTeamBreakScore ::
        x_0: TeamContestId
     -> x_1: Maybe Double
     -> x_2: Maybe Double
     -> x_3: Maybe Double
     -> x_4: UTCTime
     -> BinahRecord <{\row -> teamBreakScoreTeam (entityVal row) == x_0 && teamBreakScoreBuildScore (entityVal row) == x_1 && teamBreakScoreBreakScore (entityVal row) == x_2 && teamBreakScoreFixScore (entityVal row) == x_3 && teamBreakScoreTimestamp (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TeamBreakScore
  @-}
mkTeamBreakScore :: TeamContestId -> Maybe Double -> Maybe Double -> Maybe Double -> UTCTime -> BinahRecord (Entity User) TeamBreakScore
mkTeamBreakScore x_0 x_1 x_2 x_3 x_4 = BinahRecord (TeamBreakScore x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity TeamBreakScore | v == getJust (entityKey v)} @-}



{-@ assume teamBreakScoreId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TeamBreakScore TeamBreakScoreId
  @-}
teamBreakScoreId' :: EntityFieldWrapper (Entity User) TeamBreakScore TeamBreakScoreId
teamBreakScoreId' = EntityFieldWrapper TeamBreakScoreId

{-@ measure teamBreakScoreTeam :: TeamBreakScore -> TeamContestId @-}

{-@ measure teamBreakScoreTeamCap :: Entity TeamBreakScore -> Bool @-}

{-@ assume teamBreakScoreTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBreakScoreTeam (entityVal row)},
                          {\field row -> field == teamBreakScoreTeam (entityVal row)},
                          {\old -> teamBreakScoreTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBreakScoreTeamCap x_0)}>
                          (Entity User) TeamBreakScore TeamContestId
  @-}
teamBreakScoreTeam' :: EntityFieldWrapper (Entity User) TeamBreakScore TeamContestId
teamBreakScoreTeam' = EntityFieldWrapper TeamBreakScoreTeam

{-@ measure teamBreakScoreBuildScore :: TeamBreakScore -> (Maybe Double) @-}

{-@ measure teamBreakScoreBuildScoreCap :: Entity TeamBreakScore -> Bool @-}

{-@ assume teamBreakScoreBuildScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBreakScoreBuildScore (entityVal row)},
                          {\field row -> field == teamBreakScoreBuildScore (entityVal row)},
                          {\old -> teamBreakScoreBuildScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBreakScoreBuildScoreCap x_0)}>
                          (Entity User) TeamBreakScore (Maybe Double)
  @-}
teamBreakScoreBuildScore' :: EntityFieldWrapper (Entity User) TeamBreakScore (Maybe Double)
teamBreakScoreBuildScore' = EntityFieldWrapper TeamBreakScoreBuildScore

{-@ measure teamBreakScoreBreakScore :: TeamBreakScore -> (Maybe Double) @-}

{-@ measure teamBreakScoreBreakScoreCap :: Entity TeamBreakScore -> Bool @-}

{-@ assume teamBreakScoreBreakScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBreakScoreBreakScore (entityVal row)},
                          {\field row -> field == teamBreakScoreBreakScore (entityVal row)},
                          {\old -> teamBreakScoreBreakScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBreakScoreBreakScoreCap x_0)}>
                          (Entity User) TeamBreakScore (Maybe Double)
  @-}
teamBreakScoreBreakScore' :: EntityFieldWrapper (Entity User) TeamBreakScore (Maybe Double)
teamBreakScoreBreakScore' = EntityFieldWrapper TeamBreakScoreBreakScore

{-@ measure teamBreakScoreFixScore :: TeamBreakScore -> (Maybe Double) @-}

{-@ measure teamBreakScoreFixScoreCap :: Entity TeamBreakScore -> Bool @-}

{-@ assume teamBreakScoreFixScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBreakScoreFixScore (entityVal row)},
                          {\field row -> field == teamBreakScoreFixScore (entityVal row)},
                          {\old -> teamBreakScoreFixScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBreakScoreFixScoreCap x_0)}>
                          (Entity User) TeamBreakScore (Maybe Double)
  @-}
teamBreakScoreFixScore' :: EntityFieldWrapper (Entity User) TeamBreakScore (Maybe Double)
teamBreakScoreFixScore' = EntityFieldWrapper TeamBreakScoreFixScore

{-@ measure teamBreakScoreTimestamp :: TeamBreakScore -> UTCTime @-}

{-@ measure teamBreakScoreTimestampCap :: Entity TeamBreakScore -> Bool @-}

{-@ assume teamBreakScoreTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBreakScoreTimestamp (entityVal row)},
                          {\field row -> field == teamBreakScoreTimestamp (entityVal row)},
                          {\old -> teamBreakScoreTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBreakScoreTimestampCap x_0)}>
                          (Entity User) TeamBreakScore UTCTime
  @-}
teamBreakScoreTimestamp' :: EntityFieldWrapper (Entity User) TeamBreakScore UTCTime
teamBreakScoreTimestamp' = EntityFieldWrapper TeamBreakScoreTimestamp

-- * TeamBuildScore
{-@ mkTeamBuildScore ::
        x_0: TeamContestId
     -> x_1: Maybe Double
     -> x_2: Maybe Double
     -> x_3: Maybe Double
     -> x_4: UTCTime
     -> BinahRecord <{\row -> teamBuildScoreTeam (entityVal row) == x_0 && teamBuildScoreBuildScore (entityVal row) == x_1 && teamBuildScoreBreakScore (entityVal row) == x_2 && teamBuildScoreFixScore (entityVal row) == x_3 && teamBuildScoreTimestamp (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TeamBuildScore
  @-}
mkTeamBuildScore :: TeamContestId -> Maybe Double -> Maybe Double -> Maybe Double -> UTCTime -> BinahRecord (Entity User) TeamBuildScore
mkTeamBuildScore x_0 x_1 x_2 x_3 x_4 = BinahRecord (TeamBuildScore x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity TeamBuildScore | v == getJust (entityKey v)} @-}



{-@ assume teamBuildScoreId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TeamBuildScore TeamBuildScoreId
  @-}
teamBuildScoreId' :: EntityFieldWrapper (Entity User) TeamBuildScore TeamBuildScoreId
teamBuildScoreId' = EntityFieldWrapper TeamBuildScoreId

{-@ measure teamBuildScoreTeam :: TeamBuildScore -> TeamContestId @-}

{-@ measure teamBuildScoreTeamCap :: Entity TeamBuildScore -> Bool @-}

{-@ assume teamBuildScoreTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBuildScoreTeam (entityVal row)},
                          {\field row -> field == teamBuildScoreTeam (entityVal row)},
                          {\old -> teamBuildScoreTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBuildScoreTeamCap x_0)}>
                          (Entity User) TeamBuildScore TeamContestId
  @-}
teamBuildScoreTeam' :: EntityFieldWrapper (Entity User) TeamBuildScore TeamContestId
teamBuildScoreTeam' = EntityFieldWrapper TeamBuildScoreTeam

{-@ measure teamBuildScoreBuildScore :: TeamBuildScore -> (Maybe Double) @-}

{-@ measure teamBuildScoreBuildScoreCap :: Entity TeamBuildScore -> Bool @-}

{-@ assume teamBuildScoreBuildScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBuildScoreBuildScore (entityVal row)},
                          {\field row -> field == teamBuildScoreBuildScore (entityVal row)},
                          {\old -> teamBuildScoreBuildScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBuildScoreBuildScoreCap x_0)}>
                          (Entity User) TeamBuildScore (Maybe Double)
  @-}
teamBuildScoreBuildScore' :: EntityFieldWrapper (Entity User) TeamBuildScore (Maybe Double)
teamBuildScoreBuildScore' = EntityFieldWrapper TeamBuildScoreBuildScore

{-@ measure teamBuildScoreBreakScore :: TeamBuildScore -> (Maybe Double) @-}

{-@ measure teamBuildScoreBreakScoreCap :: Entity TeamBuildScore -> Bool @-}

{-@ assume teamBuildScoreBreakScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBuildScoreBreakScore (entityVal row)},
                          {\field row -> field == teamBuildScoreBreakScore (entityVal row)},
                          {\old -> teamBuildScoreBreakScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBuildScoreBreakScoreCap x_0)}>
                          (Entity User) TeamBuildScore (Maybe Double)
  @-}
teamBuildScoreBreakScore' :: EntityFieldWrapper (Entity User) TeamBuildScore (Maybe Double)
teamBuildScoreBreakScore' = EntityFieldWrapper TeamBuildScoreBreakScore

{-@ measure teamBuildScoreFixScore :: TeamBuildScore -> (Maybe Double) @-}

{-@ measure teamBuildScoreFixScoreCap :: Entity TeamBuildScore -> Bool @-}

{-@ assume teamBuildScoreFixScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBuildScoreFixScore (entityVal row)},
                          {\field row -> field == teamBuildScoreFixScore (entityVal row)},
                          {\old -> teamBuildScoreFixScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBuildScoreFixScoreCap x_0)}>
                          (Entity User) TeamBuildScore (Maybe Double)
  @-}
teamBuildScoreFixScore' :: EntityFieldWrapper (Entity User) TeamBuildScore (Maybe Double)
teamBuildScoreFixScore' = EntityFieldWrapper TeamBuildScoreFixScore

{-@ measure teamBuildScoreTimestamp :: TeamBuildScore -> UTCTime @-}

{-@ measure teamBuildScoreTimestampCap :: Entity TeamBuildScore -> Bool @-}

{-@ assume teamBuildScoreTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == teamBuildScoreTimestamp (entityVal row)},
                          {\field row -> field == teamBuildScoreTimestamp (entityVal row)},
                          {\old -> teamBuildScoreTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (teamBuildScoreTimestampCap x_0)}>
                          (Entity User) TeamBuildScore UTCTime
  @-}
teamBuildScoreTimestamp' :: EntityFieldWrapper (Entity User) TeamBuildScore UTCTime
teamBuildScoreTimestamp' = EntityFieldWrapper TeamBuildScoreTimestamp

-- * OracleSubmission
{-@ mkOracleSubmission ::
        x_0: TeamContestId
     -> x_1: UTCTime
     -> x_2: Text
     -> x_3: Text
     -> x_4: Maybe Text
     -> x_5: OracleSubmissionStatus
     -> BinahRecord <{\row -> oracleSubmissionTeam (entityVal row) == x_0 && oracleSubmissionTimestamp (entityVal row) == x_1 && oracleSubmissionName (entityVal row) == x_2 && oracleSubmissionInput (entityVal row) == x_3 && oracleSubmissionOutput (entityVal row) == x_4 && oracleSubmissionStatus (entityVal row) == x_5},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) OracleSubmission
  @-}
mkOracleSubmission :: TeamContestId -> UTCTime -> Text -> Text -> Maybe Text -> OracleSubmissionStatus -> BinahRecord (Entity User) OracleSubmission
mkOracleSubmission x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (OracleSubmission x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity OracleSubmission | v == getJust (entityKey v)} @-}



{-@ assume oracleSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) OracleSubmission OracleSubmissionId
  @-}
oracleSubmissionId' :: EntityFieldWrapper (Entity User) OracleSubmission OracleSubmissionId
oracleSubmissionId' = EntityFieldWrapper OracleSubmissionId

{-@ measure oracleSubmissionTeam :: OracleSubmission -> TeamContestId @-}

{-@ measure oracleSubmissionTeamCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionTeam (entityVal row)},
                          {\field row -> field == oracleSubmissionTeam (entityVal row)},
                          {\old -> oracleSubmissionTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionTeamCap x_0)}>
                          (Entity User) OracleSubmission TeamContestId
  @-}
oracleSubmissionTeam' :: EntityFieldWrapper (Entity User) OracleSubmission TeamContestId
oracleSubmissionTeam' = EntityFieldWrapper OracleSubmissionTeam

{-@ measure oracleSubmissionTimestamp :: OracleSubmission -> UTCTime @-}

{-@ measure oracleSubmissionTimestampCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionTimestamp (entityVal row)},
                          {\field row -> field == oracleSubmissionTimestamp (entityVal row)},
                          {\old -> oracleSubmissionTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionTimestampCap x_0)}>
                          (Entity User) OracleSubmission UTCTime
  @-}
oracleSubmissionTimestamp' :: EntityFieldWrapper (Entity User) OracleSubmission UTCTime
oracleSubmissionTimestamp' = EntityFieldWrapper OracleSubmissionTimestamp

{-@ measure oracleSubmissionName :: OracleSubmission -> Text @-}

{-@ measure oracleSubmissionNameCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionName (entityVal row)},
                          {\field row -> field == oracleSubmissionName (entityVal row)},
                          {\old -> oracleSubmissionNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionNameCap x_0)}>
                          (Entity User) OracleSubmission Text
  @-}
oracleSubmissionName' :: EntityFieldWrapper (Entity User) OracleSubmission Text
oracleSubmissionName' = EntityFieldWrapper OracleSubmissionName

{-@ measure oracleSubmissionInput :: OracleSubmission -> Text @-}

{-@ measure oracleSubmissionInputCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionInput' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionInput (entityVal row)},
                          {\field row -> field == oracleSubmissionInput (entityVal row)},
                          {\old -> oracleSubmissionInputCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionInputCap x_0)}>
                          (Entity User) OracleSubmission Text
  @-}
oracleSubmissionInput' :: EntityFieldWrapper (Entity User) OracleSubmission Text
oracleSubmissionInput' = EntityFieldWrapper OracleSubmissionInput

{-@ measure oracleSubmissionOutput :: OracleSubmission -> (Maybe Text) @-}

{-@ measure oracleSubmissionOutputCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionOutput' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionOutput (entityVal row)},
                          {\field row -> field == oracleSubmissionOutput (entityVal row)},
                          {\old -> oracleSubmissionOutputCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionOutputCap x_0)}>
                          (Entity User) OracleSubmission (Maybe Text)
  @-}
oracleSubmissionOutput' :: EntityFieldWrapper (Entity User) OracleSubmission (Maybe Text)
oracleSubmissionOutput' = EntityFieldWrapper OracleSubmissionOutput

{-@ measure oracleSubmissionStatus :: OracleSubmission -> OracleSubmissionStatus @-}

{-@ measure oracleSubmissionStatusCap :: Entity OracleSubmission -> Bool @-}

{-@ assume oracleSubmissionStatus' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == oracleSubmissionStatus (entityVal row)},
                          {\field row -> field == oracleSubmissionStatus (entityVal row)},
                          {\old -> oracleSubmissionStatusCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (oracleSubmissionStatusCap x_0)}>
                          (Entity User) OracleSubmission OracleSubmissionStatus
  @-}
oracleSubmissionStatus' :: EntityFieldWrapper (Entity User) OracleSubmission OracleSubmissionStatus
oracleSubmissionStatus' = EntityFieldWrapper OracleSubmissionStatus

-- * BuildSubmission
{-@ mkBuildSubmission ::
        x_0: TeamContestId
     -> x_1: UTCTime
     -> x_2: Text
     -> x_3: BuildSubmissionStatus
     -> x_4: Maybe Textarea
     -> x_5: Maybe Textarea
     -> BinahRecord <{\row -> buildSubmissionTeam (entityVal row) == x_0 && buildSubmissionTimestamp (entityVal row) == x_1 && buildSubmissionCommitHash (entityVal row) == x_2 && buildSubmissionStatus (entityVal row) == x_3 && buildSubmissionStdout (entityVal row) == x_4 && buildSubmissionStderr (entityVal row) == x_5},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildSubmission
  @-}
mkBuildSubmission :: TeamContestId -> UTCTime -> Text -> BuildSubmissionStatus -> Maybe Textarea -> Maybe Textarea -> BinahRecord (Entity User) BuildSubmission
mkBuildSubmission x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (BuildSubmission x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity BuildSubmission | v == getJust (entityKey v)} @-}



{-@ assume buildSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildSubmission BuildSubmissionId
  @-}
buildSubmissionId' :: EntityFieldWrapper (Entity User) BuildSubmission BuildSubmissionId
buildSubmissionId' = EntityFieldWrapper BuildSubmissionId

{-@ measure buildSubmissionTeam :: BuildSubmission -> TeamContestId @-}

{-@ measure buildSubmissionTeamCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionTeam (entityVal row)},
                          {\field row -> field == buildSubmissionTeam (entityVal row)},
                          {\old -> buildSubmissionTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionTeamCap x_0)}>
                          (Entity User) BuildSubmission TeamContestId
  @-}
buildSubmissionTeam' :: EntityFieldWrapper (Entity User) BuildSubmission TeamContestId
buildSubmissionTeam' = EntityFieldWrapper BuildSubmissionTeam

{-@ measure buildSubmissionTimestamp :: BuildSubmission -> UTCTime @-}

{-@ measure buildSubmissionTimestampCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionTimestamp (entityVal row)},
                          {\field row -> field == buildSubmissionTimestamp (entityVal row)},
                          {\old -> buildSubmissionTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionTimestampCap x_0)}>
                          (Entity User) BuildSubmission UTCTime
  @-}
buildSubmissionTimestamp' :: EntityFieldWrapper (Entity User) BuildSubmission UTCTime
buildSubmissionTimestamp' = EntityFieldWrapper BuildSubmissionTimestamp

{-@ measure buildSubmissionCommitHash :: BuildSubmission -> Text @-}

{-@ measure buildSubmissionCommitHashCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionCommitHash' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionCommitHash (entityVal row)},
                          {\field row -> field == buildSubmissionCommitHash (entityVal row)},
                          {\old -> buildSubmissionCommitHashCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionCommitHashCap x_0)}>
                          (Entity User) BuildSubmission Text
  @-}
buildSubmissionCommitHash' :: EntityFieldWrapper (Entity User) BuildSubmission Text
buildSubmissionCommitHash' = EntityFieldWrapper BuildSubmissionCommitHash

{-@ measure buildSubmissionStatus :: BuildSubmission -> BuildSubmissionStatus @-}

{-@ measure buildSubmissionStatusCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionStatus' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionStatus (entityVal row)},
                          {\field row -> field == buildSubmissionStatus (entityVal row)},
                          {\old -> buildSubmissionStatusCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionStatusCap x_0)}>
                          (Entity User) BuildSubmission BuildSubmissionStatus
  @-}
buildSubmissionStatus' :: EntityFieldWrapper (Entity User) BuildSubmission BuildSubmissionStatus
buildSubmissionStatus' = EntityFieldWrapper BuildSubmissionStatus

{-@ measure buildSubmissionStdout :: BuildSubmission -> (Maybe Textarea) @-}

{-@ measure buildSubmissionStdoutCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionStdout' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionStdout (entityVal row)},
                          {\field row -> field == buildSubmissionStdout (entityVal row)},
                          {\old -> buildSubmissionStdoutCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionStdoutCap x_0)}>
                          (Entity User) BuildSubmission (Maybe Textarea)
  @-}
buildSubmissionStdout' :: EntityFieldWrapper (Entity User) BuildSubmission (Maybe Textarea)
buildSubmissionStdout' = EntityFieldWrapper BuildSubmissionStdout

{-@ measure buildSubmissionStderr :: BuildSubmission -> (Maybe Textarea) @-}

{-@ measure buildSubmissionStderrCap :: Entity BuildSubmission -> Bool @-}

{-@ assume buildSubmissionStderr' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionStderr (entityVal row)},
                          {\field row -> field == buildSubmissionStderr (entityVal row)},
                          {\old -> buildSubmissionStderrCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionStderrCap x_0)}>
                          (Entity User) BuildSubmission (Maybe Textarea)
  @-}
buildSubmissionStderr' :: EntityFieldWrapper (Entity User) BuildSubmission (Maybe Textarea)
buildSubmissionStderr' = EntityFieldWrapper BuildSubmissionStderr

-- * BreakOracleSubmission
{-@ mkBreakOracleSubmission ::
        x_0: TeamContestId
     -> x_1: UTCTime
     -> x_2: Text
     -> x_3: Bool
     -> BinahRecord <{\row -> breakOracleSubmissionTeam (entityVal row) == x_0 && breakOracleSubmissionTimestamp (entityVal row) == x_1 && breakOracleSubmissionDescription (entityVal row) == x_2 && breakOracleSubmissionValid (entityVal row) == x_3},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakOracleSubmission
  @-}
mkBreakOracleSubmission :: TeamContestId -> UTCTime -> Text -> Bool -> BinahRecord (Entity User) BreakOracleSubmission
mkBreakOracleSubmission x_0 x_1 x_2 x_3 = BinahRecord (BreakOracleSubmission x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BreakOracleSubmission | v == getJust (entityKey v)} @-}



{-@ assume breakOracleSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakOracleSubmission BreakOracleSubmissionId
  @-}
breakOracleSubmissionId' :: EntityFieldWrapper (Entity User) BreakOracleSubmission BreakOracleSubmissionId
breakOracleSubmissionId' = EntityFieldWrapper BreakOracleSubmissionId

{-@ measure breakOracleSubmissionTeam :: BreakOracleSubmission -> TeamContestId @-}

{-@ measure breakOracleSubmissionTeamCap :: Entity BreakOracleSubmission -> Bool @-}

{-@ assume breakOracleSubmissionTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakOracleSubmissionTeam (entityVal row)},
                          {\field row -> field == breakOracleSubmissionTeam (entityVal row)},
                          {\old -> breakOracleSubmissionTeamCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (breakOracleSubmissionTeamCap x_0)}>
                          (Entity User) BreakOracleSubmission TeamContestId
  @-}
breakOracleSubmissionTeam' :: EntityFieldWrapper (Entity User) BreakOracleSubmission TeamContestId
breakOracleSubmissionTeam' = EntityFieldWrapper BreakOracleSubmissionTeam

{-@ measure breakOracleSubmissionTimestamp :: BreakOracleSubmission -> UTCTime @-}

{-@ measure breakOracleSubmissionTimestampCap :: Entity BreakOracleSubmission -> Bool @-}

{-@ assume breakOracleSubmissionTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakOracleSubmissionTimestamp (entityVal row)},
                          {\field row -> field == breakOracleSubmissionTimestamp (entityVal row)},
                          {\old -> breakOracleSubmissionTimestampCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (breakOracleSubmissionTimestampCap x_0)}>
                          (Entity User) BreakOracleSubmission UTCTime
  @-}
breakOracleSubmissionTimestamp' :: EntityFieldWrapper (Entity User) BreakOracleSubmission UTCTime
breakOracleSubmissionTimestamp' = EntityFieldWrapper BreakOracleSubmissionTimestamp

{-@ measure breakOracleSubmissionDescription :: BreakOracleSubmission -> Text @-}

{-@ measure breakOracleSubmissionDescriptionCap :: Entity BreakOracleSubmission -> Bool @-}

{-@ assume breakOracleSubmissionDescription' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakOracleSubmissionDescription (entityVal row)},
                          {\field row -> field == breakOracleSubmissionDescription (entityVal row)},
                          {\old -> breakOracleSubmissionDescriptionCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (breakOracleSubmissionDescriptionCap x_0)}>
                          (Entity User) BreakOracleSubmission Text
  @-}
breakOracleSubmissionDescription' :: EntityFieldWrapper (Entity User) BreakOracleSubmission Text
breakOracleSubmissionDescription' = EntityFieldWrapper BreakOracleSubmissionDescription

{-@ measure breakOracleSubmissionValid :: BreakOracleSubmission -> Bool @-}

{-@ measure breakOracleSubmissionValidCap :: Entity BreakOracleSubmission -> Bool @-}

{-@ assume breakOracleSubmissionValid' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakOracleSubmissionValid (entityVal row)},
                          {\field row -> field == breakOracleSubmissionValid (entityVal row)},
                          {\old -> breakOracleSubmissionValidCap old},
                          {\old _ _ -> breakOracleSubmissionValidCap old}>
                          (Entity User) BreakOracleSubmission Bool
  @-}
breakOracleSubmissionValid' :: EntityFieldWrapper (Entity User) BreakOracleSubmission Bool
breakOracleSubmissionValid' = EntityFieldWrapper BreakOracleSubmissionValid

-- * BreakSubmission
{-@ mkBreakSubmission ::
        x_0: TeamContestId
     -> x_1: Maybe TeamContestId
     -> x_2: UTCTime
     -> x_3: Text
     -> x_4: Text
     -> x_5: BreakSubmissionStatus
     -> x_6: Maybe BreakType
     -> x_7: Maybe String
     -> x_8: Maybe Text
     -> x_9: Maybe Textarea
     -> x_10: Maybe Textarea
     -> x_11: Maybe Bool
     -> x_12: Bool
     -> BinahRecord <{\row -> breakSubmissionTeam (entityVal row) == x_0 && breakSubmissionTargetTeam (entityVal row) == x_1 && breakSubmissionTimestamp (entityVal row) == x_2 && breakSubmissionCommitHash (entityVal row) == x_3 && breakSubmissionName (entityVal row) == x_4 && breakSubmissionStatus (entityVal row) == x_5 && breakSubmissionBreakType (entityVal row) == x_6 && breakSubmissionMessage (entityVal row) == x_7 && breakSubmissionJson (entityVal row) == x_8 && breakSubmissionStdout (entityVal row) == x_9 && breakSubmissionStderr (entityVal row) == x_10 && breakSubmissionValid (entityVal row) == x_11 && breakSubmissionWithdrawn (entityVal row) == x_12},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakSubmission
  @-}
mkBreakSubmission :: TeamContestId -> Maybe TeamContestId -> UTCTime -> Text -> Text -> BreakSubmissionStatus -> Maybe BreakType -> Maybe String -> Maybe Text -> Maybe Textarea -> Maybe Textarea -> Maybe Bool -> Bool -> BinahRecord (Entity User) BreakSubmission
mkBreakSubmission x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12 = BinahRecord (BreakSubmission x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12)

{-@ invariant {v: Entity BreakSubmission | v == getJust (entityKey v)} @-}



{-@ assume breakSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakSubmission BreakSubmissionId
  @-}
breakSubmissionId' :: EntityFieldWrapper (Entity User) BreakSubmission BreakSubmissionId
breakSubmissionId' = EntityFieldWrapper BreakSubmissionId

{-@ measure breakSubmissionTeam :: BreakSubmission -> TeamContestId @-}

{-@ measure breakSubmissionTeamCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionTeam (entityVal row)},
                          {\field row -> field == breakSubmissionTeam (entityVal row)},
                          {\old -> breakSubmissionTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionTeamCap x_0)}>
                          (Entity User) BreakSubmission TeamContestId
  @-}
breakSubmissionTeam' :: EntityFieldWrapper (Entity User) BreakSubmission TeamContestId
breakSubmissionTeam' = EntityFieldWrapper BreakSubmissionTeam

{-@ measure breakSubmissionTargetTeam :: BreakSubmission -> (Maybe TeamContestId) @-}

{-@ measure breakSubmissionTargetTeamCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionTargetTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionTargetTeam (entityVal row)},
                          {\field row -> field == breakSubmissionTargetTeam (entityVal row)},
                          {\old -> breakSubmissionTargetTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionTargetTeamCap x_0)}>
                          (Entity User) BreakSubmission (Maybe TeamContestId)
  @-}
breakSubmissionTargetTeam' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe TeamContestId)
breakSubmissionTargetTeam' = EntityFieldWrapper BreakSubmissionTargetTeam

{-@ measure breakSubmissionTimestamp :: BreakSubmission -> UTCTime @-}

{-@ measure breakSubmissionTimestampCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionTimestamp (entityVal row)},
                          {\field row -> field == breakSubmissionTimestamp (entityVal row)},
                          {\old -> breakSubmissionTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionTimestampCap x_0)}>
                          (Entity User) BreakSubmission UTCTime
  @-}
breakSubmissionTimestamp' :: EntityFieldWrapper (Entity User) BreakSubmission UTCTime
breakSubmissionTimestamp' = EntityFieldWrapper BreakSubmissionTimestamp

{-@ measure breakSubmissionCommitHash :: BreakSubmission -> Text @-}

{-@ measure breakSubmissionCommitHashCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionCommitHash' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionCommitHash (entityVal row)},
                          {\field row -> field == breakSubmissionCommitHash (entityVal row)},
                          {\old -> breakSubmissionCommitHashCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionCommitHashCap x_0)}>
                          (Entity User) BreakSubmission Text
  @-}
breakSubmissionCommitHash' :: EntityFieldWrapper (Entity User) BreakSubmission Text
breakSubmissionCommitHash' = EntityFieldWrapper BreakSubmissionCommitHash

{-@ measure breakSubmissionName :: BreakSubmission -> Text @-}

{-@ measure breakSubmissionNameCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionName (entityVal row)},
                          {\field row -> field == breakSubmissionName (entityVal row)},
                          {\old -> breakSubmissionNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionNameCap x_0)}>
                          (Entity User) BreakSubmission Text
  @-}
breakSubmissionName' :: EntityFieldWrapper (Entity User) BreakSubmission Text
breakSubmissionName' = EntityFieldWrapper BreakSubmissionName

{-@ measure breakSubmissionStatus :: BreakSubmission -> BreakSubmissionStatus @-}

{-@ measure breakSubmissionStatusCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionStatus' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionStatus (entityVal row)},
                          {\field row -> field == breakSubmissionStatus (entityVal row)},
                          {\old -> breakSubmissionStatusCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionStatusCap x_0)}>
                          (Entity User) BreakSubmission BreakSubmissionStatus
  @-}
breakSubmissionStatus' :: EntityFieldWrapper (Entity User) BreakSubmission BreakSubmissionStatus
breakSubmissionStatus' = EntityFieldWrapper BreakSubmissionStatus

{-@ measure breakSubmissionBreakType :: BreakSubmission -> (Maybe BreakType) @-}

{-@ measure breakSubmissionBreakTypeCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionBreakType' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionBreakType (entityVal row)},
                          {\field row -> field == breakSubmissionBreakType (entityVal row)},
                          {\old -> breakSubmissionBreakTypeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionBreakTypeCap x_0)}>
                          (Entity User) BreakSubmission (Maybe BreakType)
  @-}
breakSubmissionBreakType' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe BreakType)
breakSubmissionBreakType' = EntityFieldWrapper BreakSubmissionBreakType

{-@ measure breakSubmissionMessage :: BreakSubmission -> (Maybe String) @-}

{-@ measure breakSubmissionMessageCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionMessage (entityVal row)},
                          {\field row -> field == breakSubmissionMessage (entityVal row)},
                          {\old -> breakSubmissionMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionMessageCap x_0)}>
                          (Entity User) BreakSubmission (Maybe String)
  @-}
breakSubmissionMessage' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe String)
breakSubmissionMessage' = EntityFieldWrapper BreakSubmissionMessage

{-@ measure breakSubmissionJson :: BreakSubmission -> (Maybe Text) @-}

{-@ measure breakSubmissionJsonCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionJson' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionJson (entityVal row)},
                          {\field row -> field == breakSubmissionJson (entityVal row)},
                          {\old -> breakSubmissionJsonCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionJsonCap x_0)}>
                          (Entity User) BreakSubmission (Maybe Text)
  @-}
breakSubmissionJson' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe Text)
breakSubmissionJson' = EntityFieldWrapper BreakSubmissionJson

{-@ measure breakSubmissionStdout :: BreakSubmission -> (Maybe Textarea) @-}

{-@ measure breakSubmissionStdoutCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionStdout' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionStdout (entityVal row)},
                          {\field row -> field == breakSubmissionStdout (entityVal row)},
                          {\old -> breakSubmissionStdoutCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionStdoutCap x_0)}>
                          (Entity User) BreakSubmission (Maybe Textarea)
  @-}
breakSubmissionStdout' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe Textarea)
breakSubmissionStdout' = EntityFieldWrapper BreakSubmissionStdout

{-@ measure breakSubmissionStderr :: BreakSubmission -> (Maybe Textarea) @-}

{-@ measure breakSubmissionStderrCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionStderr' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionStderr (entityVal row)},
                          {\field row -> field == breakSubmissionStderr (entityVal row)},
                          {\old -> breakSubmissionStderrCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionStderrCap x_0)}>
                          (Entity User) BreakSubmission (Maybe Textarea)
  @-}
breakSubmissionStderr' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe Textarea)
breakSubmissionStderr' = EntityFieldWrapper BreakSubmissionStderr

{-@ measure breakSubmissionValid :: BreakSubmission -> (Maybe Bool) @-}

{-@ measure breakSubmissionValidCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionValid' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionValid (entityVal row)},
                          {\field row -> field == breakSubmissionValid (entityVal row)},
                          {\old -> breakSubmissionValidCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionValidCap x_0)}>
                          (Entity User) BreakSubmission (Maybe Bool)
  @-}
breakSubmissionValid' :: EntityFieldWrapper (Entity User) BreakSubmission (Maybe Bool)
breakSubmissionValid' = EntityFieldWrapper BreakSubmissionValid

{-@ measure breakSubmissionWithdrawn :: BreakSubmission -> Bool @-}

{-@ measure breakSubmissionWithdrawnCap :: Entity BreakSubmission -> Bool @-}

{-@ assume breakSubmissionWithdrawn' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionWithdrawn (entityVal row)},
                          {\field row -> field == breakSubmissionWithdrawn (entityVal row)},
                          {\old -> breakSubmissionWithdrawnCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionWithdrawnCap x_0)}>
                          (Entity User) BreakSubmission Bool
  @-}
breakSubmissionWithdrawn' :: EntityFieldWrapper (Entity User) BreakSubmission Bool
breakSubmissionWithdrawn' = EntityFieldWrapper BreakSubmissionWithdrawn

-- * BreakFixSubmission
{-@ mkBreakFixSubmission ::
        x_0: BreakSubmissionId
     -> x_1: Maybe FixSubmissionId
     -> x_2: BreakSubmissionResult
     -> BinahRecord <{\row -> breakFixSubmissionBreak (entityVal row) == x_0 && breakFixSubmissionFix (entityVal row) == x_1 && breakFixSubmissionResult (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakFixSubmission
  @-}
mkBreakFixSubmission :: BreakSubmissionId -> Maybe FixSubmissionId -> BreakSubmissionResult -> BinahRecord (Entity User) BreakFixSubmission
mkBreakFixSubmission x_0 x_1 x_2 = BinahRecord (BreakFixSubmission x_0 x_1 x_2)

{-@ invariant {v: Entity BreakFixSubmission | v == getJust (entityKey v)} @-}



{-@ assume breakFixSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakFixSubmission BreakFixSubmissionId
  @-}
breakFixSubmissionId' :: EntityFieldWrapper (Entity User) BreakFixSubmission BreakFixSubmissionId
breakFixSubmissionId' = EntityFieldWrapper BreakFixSubmissionId

{-@ measure breakFixSubmissionBreak :: BreakFixSubmission -> BreakSubmissionId @-}

{-@ measure breakFixSubmissionBreakCap :: Entity BreakFixSubmission -> Bool @-}

{-@ assume breakFixSubmissionBreak' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakFixSubmissionBreak (entityVal row)},
                          {\field row -> field == breakFixSubmissionBreak (entityVal row)},
                          {\old -> breakFixSubmissionBreakCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakFixSubmissionBreakCap x_0)}>
                          (Entity User) BreakFixSubmission BreakSubmissionId
  @-}
breakFixSubmissionBreak' :: EntityFieldWrapper (Entity User) BreakFixSubmission BreakSubmissionId
breakFixSubmissionBreak' = EntityFieldWrapper BreakFixSubmissionBreak

{-@ measure breakFixSubmissionFix :: BreakFixSubmission -> (Maybe FixSubmissionId) @-}

{-@ measure breakFixSubmissionFixCap :: Entity BreakFixSubmission -> Bool @-}

{-@ assume breakFixSubmissionFix' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakFixSubmissionFix (entityVal row)},
                          {\field row -> field == breakFixSubmissionFix (entityVal row)},
                          {\old -> breakFixSubmissionFixCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakFixSubmissionFixCap x_0)}>
                          (Entity User) BreakFixSubmission (Maybe FixSubmissionId)
  @-}
breakFixSubmissionFix' :: EntityFieldWrapper (Entity User) BreakFixSubmission (Maybe FixSubmissionId)
breakFixSubmissionFix' = EntityFieldWrapper BreakFixSubmissionFix

{-@ measure breakFixSubmissionResult :: BreakFixSubmission -> BreakSubmissionResult @-}

{-@ measure breakFixSubmissionResultCap :: Entity BreakFixSubmission -> Bool @-}

{-@ assume breakFixSubmissionResult' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakFixSubmissionResult (entityVal row)},
                          {\field row -> field == breakFixSubmissionResult (entityVal row)},
                          {\old -> breakFixSubmissionResultCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakFixSubmissionResultCap x_0)}>
                          (Entity User) BreakFixSubmission BreakSubmissionResult
  @-}
breakFixSubmissionResult' :: EntityFieldWrapper (Entity User) BreakFixSubmission BreakSubmissionResult
breakFixSubmissionResult' = EntityFieldWrapper BreakFixSubmissionResult

-- * FixSubmission
{-@ mkFixSubmission ::
        x_0: TeamContestId
     -> x_1: UTCTime
     -> x_2: Text
     -> x_3: FixSubmissionStatus
     -> x_4: Maybe FixSubmissionResult
     -> x_5: Maybe String
     -> x_6: Maybe Textarea
     -> x_7: Maybe Textarea
     -> BinahRecord <{\row -> fixSubmissionTeam (entityVal row) == x_0 && fixSubmissionTimestamp (entityVal row) == x_1 && fixSubmissionCommitHash (entityVal row) == x_2 && fixSubmissionStatus (entityVal row) == x_3 && fixSubmissionResult (entityVal row) == x_4 && fixSubmissionMessage (entityVal row) == x_5 && fixSubmissionStdout (entityVal row) == x_6 && fixSubmissionStderr (entityVal row) == x_7},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixSubmission
  @-}
mkFixSubmission :: TeamContestId -> UTCTime -> Text -> FixSubmissionStatus -> Maybe FixSubmissionResult -> Maybe String -> Maybe Textarea -> Maybe Textarea -> BinahRecord (Entity User) FixSubmission
mkFixSubmission x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 = BinahRecord (FixSubmission x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7)

{-@ invariant {v: Entity FixSubmission | v == getJust (entityKey v)} @-}



{-@ assume fixSubmissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixSubmission FixSubmissionId
  @-}
fixSubmissionId' :: EntityFieldWrapper (Entity User) FixSubmission FixSubmissionId
fixSubmissionId' = EntityFieldWrapper FixSubmissionId

{-@ measure fixSubmissionTeam :: FixSubmission -> TeamContestId @-}

{-@ measure fixSubmissionTeamCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionTeam (entityVal row)},
                          {\field row -> field == fixSubmissionTeam (entityVal row)},
                          {\old -> fixSubmissionTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionTeamCap x_0)}>
                          (Entity User) FixSubmission TeamContestId
  @-}
fixSubmissionTeam' :: EntityFieldWrapper (Entity User) FixSubmission TeamContestId
fixSubmissionTeam' = EntityFieldWrapper FixSubmissionTeam

{-@ measure fixSubmissionTimestamp :: FixSubmission -> UTCTime @-}

{-@ measure fixSubmissionTimestampCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionTimestamp (entityVal row)},
                          {\field row -> field == fixSubmissionTimestamp (entityVal row)},
                          {\old -> fixSubmissionTimestampCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionTimestampCap x_0)}>
                          (Entity User) FixSubmission UTCTime
  @-}
fixSubmissionTimestamp' :: EntityFieldWrapper (Entity User) FixSubmission UTCTime
fixSubmissionTimestamp' = EntityFieldWrapper FixSubmissionTimestamp

{-@ measure fixSubmissionCommitHash :: FixSubmission -> Text @-}

{-@ measure fixSubmissionCommitHashCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionCommitHash' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionCommitHash (entityVal row)},
                          {\field row -> field == fixSubmissionCommitHash (entityVal row)},
                          {\old -> fixSubmissionCommitHashCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionCommitHashCap x_0)}>
                          (Entity User) FixSubmission Text
  @-}
fixSubmissionCommitHash' :: EntityFieldWrapper (Entity User) FixSubmission Text
fixSubmissionCommitHash' = EntityFieldWrapper FixSubmissionCommitHash

{-@ measure fixSubmissionStatus :: FixSubmission -> FixSubmissionStatus @-}

{-@ measure fixSubmissionStatusCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionStatus' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionStatus (entityVal row)},
                          {\field row -> field == fixSubmissionStatus (entityVal row)},
                          {\old -> fixSubmissionStatusCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionStatusCap x_0)}>
                          (Entity User) FixSubmission FixSubmissionStatus
  @-}
fixSubmissionStatus' :: EntityFieldWrapper (Entity User) FixSubmission FixSubmissionStatus
fixSubmissionStatus' = EntityFieldWrapper FixSubmissionStatus

{-@ measure fixSubmissionResult :: FixSubmission -> (Maybe FixSubmissionResult) @-}

{-@ measure fixSubmissionResultCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionResult' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionResult (entityVal row)},
                          {\field row -> field == fixSubmissionResult (entityVal row)},
                          {\old -> fixSubmissionResultCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionResultCap x_0)}>
                          (Entity User) FixSubmission (Maybe FixSubmissionResult)
  @-}
fixSubmissionResult' :: EntityFieldWrapper (Entity User) FixSubmission (Maybe FixSubmissionResult)
fixSubmissionResult' = EntityFieldWrapper FixSubmissionResult

{-@ measure fixSubmissionMessage :: FixSubmission -> (Maybe String) @-}

{-@ measure fixSubmissionMessageCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionMessage (entityVal row)},
                          {\field row -> field == fixSubmissionMessage (entityVal row)},
                          {\old -> fixSubmissionMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionMessageCap x_0)}>
                          (Entity User) FixSubmission (Maybe String)
  @-}
fixSubmissionMessage' :: EntityFieldWrapper (Entity User) FixSubmission (Maybe String)
fixSubmissionMessage' = EntityFieldWrapper FixSubmissionMessage

{-@ measure fixSubmissionStdout :: FixSubmission -> (Maybe Textarea) @-}

{-@ measure fixSubmissionStdoutCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionStdout' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionStdout (entityVal row)},
                          {\field row -> field == fixSubmissionStdout (entityVal row)},
                          {\old -> fixSubmissionStdoutCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionStdoutCap x_0)}>
                          (Entity User) FixSubmission (Maybe Textarea)
  @-}
fixSubmissionStdout' :: EntityFieldWrapper (Entity User) FixSubmission (Maybe Textarea)
fixSubmissionStdout' = EntityFieldWrapper FixSubmissionStdout

{-@ measure fixSubmissionStderr :: FixSubmission -> (Maybe Textarea) @-}

{-@ measure fixSubmissionStderrCap :: Entity FixSubmission -> Bool @-}

{-@ assume fixSubmissionStderr' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionStderr (entityVal row)},
                          {\field row -> field == fixSubmissionStderr (entityVal row)},
                          {\old -> fixSubmissionStderrCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionStderrCap x_0)}>
                          (Entity User) FixSubmission (Maybe Textarea)
  @-}
fixSubmissionStderr' :: EntityFieldWrapper (Entity User) FixSubmission (Maybe Textarea)
fixSubmissionStderr' = EntityFieldWrapper FixSubmissionStderr

-- * BreakSubmissionFile
{-@ mkBreakSubmissionFile ::
        x_0: BreakSubmissionId
     -> x_1: ByteString
     -> BinahRecord <{\row -> breakSubmissionFileBreak (entityVal row) == x_0 && breakSubmissionFileFile (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BreakSubmissionFile
  @-}
mkBreakSubmissionFile :: BreakSubmissionId -> ByteString -> BinahRecord (Entity User) BreakSubmissionFile
mkBreakSubmissionFile x_0 x_1 = BinahRecord (BreakSubmissionFile x_0 x_1)

{-@ invariant {v: Entity BreakSubmissionFile | v == getJust (entityKey v)} @-}



{-@ assume breakSubmissionFileId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BreakSubmissionFile BreakSubmissionFileId
  @-}
breakSubmissionFileId' :: EntityFieldWrapper (Entity User) BreakSubmissionFile BreakSubmissionFileId
breakSubmissionFileId' = EntityFieldWrapper BreakSubmissionFileId

{-@ measure breakSubmissionFileBreak :: BreakSubmissionFile -> BreakSubmissionId @-}

{-@ measure breakSubmissionFileBreakCap :: Entity BreakSubmissionFile -> Bool @-}

{-@ assume breakSubmissionFileBreak' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionFileBreak (entityVal row)},
                          {\field row -> field == breakSubmissionFileBreak (entityVal row)},
                          {\old -> breakSubmissionFileBreakCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionFileBreakCap x_0)}>
                          (Entity User) BreakSubmissionFile BreakSubmissionId
  @-}
breakSubmissionFileBreak' :: EntityFieldWrapper (Entity User) BreakSubmissionFile BreakSubmissionId
breakSubmissionFileBreak' = EntityFieldWrapper BreakSubmissionFileBreak

{-@ measure breakSubmissionFileFile :: BreakSubmissionFile -> ByteString @-}

{-@ measure breakSubmissionFileFileCap :: Entity BreakSubmissionFile -> Bool @-}

{-@ assume breakSubmissionFileFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == breakSubmissionFileFile (entityVal row)},
                          {\field row -> field == breakSubmissionFileFile (entityVal row)},
                          {\old -> breakSubmissionFileFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (breakSubmissionFileFileCap x_0)}>
                          (Entity User) BreakSubmissionFile ByteString
  @-}
breakSubmissionFileFile' :: EntityFieldWrapper (Entity User) BreakSubmissionFile ByteString
breakSubmissionFileFile' = EntityFieldWrapper BreakSubmissionFileFile

-- * BuildSubmissionFile
{-@ mkBuildSubmissionFile ::
        x_0: TeamContestId
     -> x_1: ByteString
     -> BinahRecord <{\row -> buildSubmissionFileTeam (entityVal row) == x_0 && buildSubmissionFileFile (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildSubmissionFile
  @-}
mkBuildSubmissionFile :: TeamContestId -> ByteString -> BinahRecord (Entity User) BuildSubmissionFile
mkBuildSubmissionFile x_0 x_1 = BinahRecord (BuildSubmissionFile x_0 x_1)

{-@ invariant {v: Entity BuildSubmissionFile | v == getJust (entityKey v)} @-}



{-@ assume buildSubmissionFileId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildSubmissionFile BuildSubmissionFileId
  @-}
buildSubmissionFileId' :: EntityFieldWrapper (Entity User) BuildSubmissionFile BuildSubmissionFileId
buildSubmissionFileId' = EntityFieldWrapper BuildSubmissionFileId

{-@ measure buildSubmissionFileTeam :: BuildSubmissionFile -> TeamContestId @-}

{-@ measure buildSubmissionFileTeamCap :: Entity BuildSubmissionFile -> Bool @-}

{-@ assume buildSubmissionFileTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionFileTeam (entityVal row)},
                          {\field row -> field == buildSubmissionFileTeam (entityVal row)},
                          {\old -> buildSubmissionFileTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionFileTeamCap x_0)}>
                          (Entity User) BuildSubmissionFile TeamContestId
  @-}
buildSubmissionFileTeam' :: EntityFieldWrapper (Entity User) BuildSubmissionFile TeamContestId
buildSubmissionFileTeam' = EntityFieldWrapper BuildSubmissionFileTeam

{-@ measure buildSubmissionFileFile :: BuildSubmissionFile -> ByteString @-}

{-@ measure buildSubmissionFileFileCap :: Entity BuildSubmissionFile -> Bool @-}

{-@ assume buildSubmissionFileFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildSubmissionFileFile (entityVal row)},
                          {\field row -> field == buildSubmissionFileFile (entityVal row)},
                          {\old -> buildSubmissionFileFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildSubmissionFileFileCap x_0)}>
                          (Entity User) BuildSubmissionFile ByteString
  @-}
buildSubmissionFileFile' :: EntityFieldWrapper (Entity User) BuildSubmissionFile ByteString
buildSubmissionFileFile' = EntityFieldWrapper BuildSubmissionFileFile

-- * FixSubmissionFile
{-@ mkFixSubmissionFile ::
        x_0: FixSubmissionId
     -> x_1: ByteString
     -> BinahRecord <{\row -> fixSubmissionFileFix (entityVal row) == x_0 && fixSubmissionFileFile (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixSubmissionFile
  @-}
mkFixSubmissionFile :: FixSubmissionId -> ByteString -> BinahRecord (Entity User) FixSubmissionFile
mkFixSubmissionFile x_0 x_1 = BinahRecord (FixSubmissionFile x_0 x_1)

{-@ invariant {v: Entity FixSubmissionFile | v == getJust (entityKey v)} @-}



{-@ assume fixSubmissionFileId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixSubmissionFile FixSubmissionFileId
  @-}
fixSubmissionFileId' :: EntityFieldWrapper (Entity User) FixSubmissionFile FixSubmissionFileId
fixSubmissionFileId' = EntityFieldWrapper FixSubmissionFileId

{-@ measure fixSubmissionFileFix :: FixSubmissionFile -> FixSubmissionId @-}

{-@ measure fixSubmissionFileFixCap :: Entity FixSubmissionFile -> Bool @-}

{-@ assume fixSubmissionFileFix' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionFileFix (entityVal row)},
                          {\field row -> field == fixSubmissionFileFix (entityVal row)},
                          {\old -> fixSubmissionFileFixCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionFileFixCap x_0)}>
                          (Entity User) FixSubmissionFile FixSubmissionId
  @-}
fixSubmissionFileFix' :: EntityFieldWrapper (Entity User) FixSubmissionFile FixSubmissionId
fixSubmissionFileFix' = EntityFieldWrapper FixSubmissionFileFix

{-@ measure fixSubmissionFileFile :: FixSubmissionFile -> ByteString @-}

{-@ measure fixSubmissionFileFileCap :: Entity FixSubmissionFile -> Bool @-}

{-@ assume fixSubmissionFileFile' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixSubmissionFileFile (entityVal row)},
                          {\field row -> field == fixSubmissionFileFile (entityVal row)},
                          {\old -> fixSubmissionFileFileCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixSubmissionFileFileCap x_0)}>
                          (Entity User) FixSubmissionFile ByteString
  @-}
fixSubmissionFileFile' :: EntityFieldWrapper (Entity User) FixSubmissionFile ByteString
fixSubmissionFileFile' = EntityFieldWrapper FixSubmissionFileFile

-- * BuildCoreResult
{-@ mkBuildCoreResult ::
        x_0: BuildSubmissionId
     -> x_1: ContestCoreTestId
     -> x_2: Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> buildCoreResultSubmission (entityVal row) == x_0 && buildCoreResultTest (entityVal row) == x_1 && buildCoreResultPass (entityVal row) == x_2 && buildCoreResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildCoreResult
  @-}
mkBuildCoreResult :: BuildSubmissionId -> ContestCoreTestId -> Bool -> Maybe Text -> BinahRecord (Entity User) BuildCoreResult
mkBuildCoreResult x_0 x_1 x_2 x_3 = BinahRecord (BuildCoreResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BuildCoreResult | v == getJust (entityKey v)} @-}



{-@ assume buildCoreResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildCoreResult BuildCoreResultId
  @-}
buildCoreResultId' :: EntityFieldWrapper (Entity User) BuildCoreResult BuildCoreResultId
buildCoreResultId' = EntityFieldWrapper BuildCoreResultId

{-@ measure buildCoreResultSubmission :: BuildCoreResult -> BuildSubmissionId @-}

{-@ measure buildCoreResultSubmissionCap :: Entity BuildCoreResult -> Bool @-}

{-@ assume buildCoreResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildCoreResultSubmission (entityVal row)},
                          {\field row -> field == buildCoreResultSubmission (entityVal row)},
                          {\old -> buildCoreResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildCoreResultSubmissionCap x_0)}>
                          (Entity User) BuildCoreResult BuildSubmissionId
  @-}
buildCoreResultSubmission' :: EntityFieldWrapper (Entity User) BuildCoreResult BuildSubmissionId
buildCoreResultSubmission' = EntityFieldWrapper BuildCoreResultSubmission

{-@ measure buildCoreResultTest :: BuildCoreResult -> ContestCoreTestId @-}

{-@ measure buildCoreResultTestCap :: Entity BuildCoreResult -> Bool @-}

{-@ assume buildCoreResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildCoreResultTest (entityVal row)},
                          {\field row -> field == buildCoreResultTest (entityVal row)},
                          {\old -> buildCoreResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildCoreResultTestCap x_0)}>
                          (Entity User) BuildCoreResult ContestCoreTestId
  @-}
buildCoreResultTest' :: EntityFieldWrapper (Entity User) BuildCoreResult ContestCoreTestId
buildCoreResultTest' = EntityFieldWrapper BuildCoreResultTest

{-@ measure buildCoreResultPass :: BuildCoreResult -> Bool @-}

{-@ measure buildCoreResultPassCap :: Entity BuildCoreResult -> Bool @-}

{-@ assume buildCoreResultPass' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildCoreResultPass (entityVal row)},
                          {\field row -> field == buildCoreResultPass (entityVal row)},
                          {\old -> buildCoreResultPassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildCoreResultPassCap x_0)}>
                          (Entity User) BuildCoreResult Bool
  @-}
buildCoreResultPass' :: EntityFieldWrapper (Entity User) BuildCoreResult Bool
buildCoreResultPass' = EntityFieldWrapper BuildCoreResultPass

{-@ measure buildCoreResultMessage :: BuildCoreResult -> (Maybe Text) @-}

{-@ measure buildCoreResultMessageCap :: Entity BuildCoreResult -> Bool @-}

{-@ assume buildCoreResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildCoreResultMessage (entityVal row)},
                          {\field row -> field == buildCoreResultMessage (entityVal row)},
                          {\old -> buildCoreResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildCoreResultMessageCap x_0)}>
                          (Entity User) BuildCoreResult (Maybe Text)
  @-}
buildCoreResultMessage' :: EntityFieldWrapper (Entity User) BuildCoreResult (Maybe Text)
buildCoreResultMessage' = EntityFieldWrapper BuildCoreResultMessage

-- * BuildPerformanceResult
{-@ mkBuildPerformanceResult ::
        x_0: BuildSubmissionId
     -> x_1: ContestPerformanceTestId
     -> x_2: Maybe Double
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> buildPerformanceResultSubmission (entityVal row) == x_0 && buildPerformanceResultTest (entityVal row) == x_1 && buildPerformanceResultTime (entityVal row) == x_2 && buildPerformanceResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildPerformanceResult
  @-}
mkBuildPerformanceResult :: BuildSubmissionId -> ContestPerformanceTestId -> Maybe Double -> Maybe Text -> BinahRecord (Entity User) BuildPerformanceResult
mkBuildPerformanceResult x_0 x_1 x_2 x_3 = BinahRecord (BuildPerformanceResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BuildPerformanceResult | v == getJust (entityKey v)} @-}



{-@ assume buildPerformanceResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildPerformanceResult BuildPerformanceResultId
  @-}
buildPerformanceResultId' :: EntityFieldWrapper (Entity User) BuildPerformanceResult BuildPerformanceResultId
buildPerformanceResultId' = EntityFieldWrapper BuildPerformanceResultId

{-@ measure buildPerformanceResultSubmission :: BuildPerformanceResult -> BuildSubmissionId @-}

{-@ measure buildPerformanceResultSubmissionCap :: Entity BuildPerformanceResult -> Bool @-}

{-@ assume buildPerformanceResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildPerformanceResultSubmission (entityVal row)},
                          {\field row -> field == buildPerformanceResultSubmission (entityVal row)},
                          {\old -> buildPerformanceResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildPerformanceResultSubmissionCap x_0)}>
                          (Entity User) BuildPerformanceResult BuildSubmissionId
  @-}
buildPerformanceResultSubmission' :: EntityFieldWrapper (Entity User) BuildPerformanceResult BuildSubmissionId
buildPerformanceResultSubmission' = EntityFieldWrapper BuildPerformanceResultSubmission

{-@ measure buildPerformanceResultTest :: BuildPerformanceResult -> ContestPerformanceTestId @-}

{-@ measure buildPerformanceResultTestCap :: Entity BuildPerformanceResult -> Bool @-}

{-@ assume buildPerformanceResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildPerformanceResultTest (entityVal row)},
                          {\field row -> field == buildPerformanceResultTest (entityVal row)},
                          {\old -> buildPerformanceResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildPerformanceResultTestCap x_0)}>
                          (Entity User) BuildPerformanceResult ContestPerformanceTestId
  @-}
buildPerformanceResultTest' :: EntityFieldWrapper (Entity User) BuildPerformanceResult ContestPerformanceTestId
buildPerformanceResultTest' = EntityFieldWrapper BuildPerformanceResultTest

{-@ measure buildPerformanceResultTime :: BuildPerformanceResult -> (Maybe Double) @-}

{-@ measure buildPerformanceResultTimeCap :: Entity BuildPerformanceResult -> Bool @-}

{-@ assume buildPerformanceResultTime' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildPerformanceResultTime (entityVal row)},
                          {\field row -> field == buildPerformanceResultTime (entityVal row)},
                          {\old -> buildPerformanceResultTimeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildPerformanceResultTimeCap x_0)}>
                          (Entity User) BuildPerformanceResult (Maybe Double)
  @-}
buildPerformanceResultTime' :: EntityFieldWrapper (Entity User) BuildPerformanceResult (Maybe Double)
buildPerformanceResultTime' = EntityFieldWrapper BuildPerformanceResultTime

{-@ measure buildPerformanceResultMessage :: BuildPerformanceResult -> (Maybe Text) @-}

{-@ measure buildPerformanceResultMessageCap :: Entity BuildPerformanceResult -> Bool @-}

{-@ assume buildPerformanceResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildPerformanceResultMessage (entityVal row)},
                          {\field row -> field == buildPerformanceResultMessage (entityVal row)},
                          {\old -> buildPerformanceResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildPerformanceResultMessageCap x_0)}>
                          (Entity User) BuildPerformanceResult (Maybe Text)
  @-}
buildPerformanceResultMessage' :: EntityFieldWrapper (Entity User) BuildPerformanceResult (Maybe Text)
buildPerformanceResultMessage' = EntityFieldWrapper BuildPerformanceResultMessage

-- * BuildOptionalResult
{-@ mkBuildOptionalResult ::
        x_0: BuildSubmissionId
     -> x_1: ContestOptionalTestId
     -> x_2: Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> buildOptionalResultSubmission (entityVal row) == x_0 && buildOptionalResultTest (entityVal row) == x_1 && buildOptionalResultPass (entityVal row) == x_2 && buildOptionalResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BuildOptionalResult
  @-}
mkBuildOptionalResult :: BuildSubmissionId -> ContestOptionalTestId -> Bool -> Maybe Text -> BinahRecord (Entity User) BuildOptionalResult
mkBuildOptionalResult x_0 x_1 x_2 x_3 = BinahRecord (BuildOptionalResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BuildOptionalResult | v == getJust (entityKey v)} @-}



{-@ assume buildOptionalResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BuildOptionalResult BuildOptionalResultId
  @-}
buildOptionalResultId' :: EntityFieldWrapper (Entity User) BuildOptionalResult BuildOptionalResultId
buildOptionalResultId' = EntityFieldWrapper BuildOptionalResultId

{-@ measure buildOptionalResultSubmission :: BuildOptionalResult -> BuildSubmissionId @-}

{-@ measure buildOptionalResultSubmissionCap :: Entity BuildOptionalResult -> Bool @-}

{-@ assume buildOptionalResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildOptionalResultSubmission (entityVal row)},
                          {\field row -> field == buildOptionalResultSubmission (entityVal row)},
                          {\old -> buildOptionalResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildOptionalResultSubmissionCap x_0)}>
                          (Entity User) BuildOptionalResult BuildSubmissionId
  @-}
buildOptionalResultSubmission' :: EntityFieldWrapper (Entity User) BuildOptionalResult BuildSubmissionId
buildOptionalResultSubmission' = EntityFieldWrapper BuildOptionalResultSubmission

{-@ measure buildOptionalResultTest :: BuildOptionalResult -> ContestOptionalTestId @-}

{-@ measure buildOptionalResultTestCap :: Entity BuildOptionalResult -> Bool @-}

{-@ assume buildOptionalResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildOptionalResultTest (entityVal row)},
                          {\field row -> field == buildOptionalResultTest (entityVal row)},
                          {\old -> buildOptionalResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildOptionalResultTestCap x_0)}>
                          (Entity User) BuildOptionalResult ContestOptionalTestId
  @-}
buildOptionalResultTest' :: EntityFieldWrapper (Entity User) BuildOptionalResult ContestOptionalTestId
buildOptionalResultTest' = EntityFieldWrapper BuildOptionalResultTest

{-@ measure buildOptionalResultPass :: BuildOptionalResult -> Bool @-}

{-@ measure buildOptionalResultPassCap :: Entity BuildOptionalResult -> Bool @-}

{-@ assume buildOptionalResultPass' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildOptionalResultPass (entityVal row)},
                          {\field row -> field == buildOptionalResultPass (entityVal row)},
                          {\old -> buildOptionalResultPassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildOptionalResultPassCap x_0)}>
                          (Entity User) BuildOptionalResult Bool
  @-}
buildOptionalResultPass' :: EntityFieldWrapper (Entity User) BuildOptionalResult Bool
buildOptionalResultPass' = EntityFieldWrapper BuildOptionalResultPass

{-@ measure buildOptionalResultMessage :: BuildOptionalResult -> (Maybe Text) @-}

{-@ measure buildOptionalResultMessageCap :: Entity BuildOptionalResult -> Bool @-}

{-@ assume buildOptionalResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == buildOptionalResultMessage (entityVal row)},
                          {\field row -> field == buildOptionalResultMessage (entityVal row)},
                          {\old -> buildOptionalResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (buildOptionalResultMessageCap x_0)}>
                          (Entity User) BuildOptionalResult (Maybe Text)
  @-}
buildOptionalResultMessage' :: EntityFieldWrapper (Entity User) BuildOptionalResult (Maybe Text)
buildOptionalResultMessage' = EntityFieldWrapper BuildOptionalResultMessage

-- * FixCoreResult
{-@ mkFixCoreResult ::
        x_0: FixSubmissionId
     -> x_1: ContestCoreTestId
     -> x_2: Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> fixCoreResultSubmission (entityVal row) == x_0 && fixCoreResultTest (entityVal row) == x_1 && fixCoreResultPass (entityVal row) == x_2 && fixCoreResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixCoreResult
  @-}
mkFixCoreResult :: FixSubmissionId -> ContestCoreTestId -> Bool -> Maybe Text -> BinahRecord (Entity User) FixCoreResult
mkFixCoreResult x_0 x_1 x_2 x_3 = BinahRecord (FixCoreResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity FixCoreResult | v == getJust (entityKey v)} @-}



{-@ assume fixCoreResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixCoreResult FixCoreResultId
  @-}
fixCoreResultId' :: EntityFieldWrapper (Entity User) FixCoreResult FixCoreResultId
fixCoreResultId' = EntityFieldWrapper FixCoreResultId

{-@ measure fixCoreResultSubmission :: FixCoreResult -> FixSubmissionId @-}

{-@ measure fixCoreResultSubmissionCap :: Entity FixCoreResult -> Bool @-}

{-@ assume fixCoreResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixCoreResultSubmission (entityVal row)},
                          {\field row -> field == fixCoreResultSubmission (entityVal row)},
                          {\old -> fixCoreResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixCoreResultSubmissionCap x_0)}>
                          (Entity User) FixCoreResult FixSubmissionId
  @-}
fixCoreResultSubmission' :: EntityFieldWrapper (Entity User) FixCoreResult FixSubmissionId
fixCoreResultSubmission' = EntityFieldWrapper FixCoreResultSubmission

{-@ measure fixCoreResultTest :: FixCoreResult -> ContestCoreTestId @-}

{-@ measure fixCoreResultTestCap :: Entity FixCoreResult -> Bool @-}

{-@ assume fixCoreResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixCoreResultTest (entityVal row)},
                          {\field row -> field == fixCoreResultTest (entityVal row)},
                          {\old -> fixCoreResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixCoreResultTestCap x_0)}>
                          (Entity User) FixCoreResult ContestCoreTestId
  @-}
fixCoreResultTest' :: EntityFieldWrapper (Entity User) FixCoreResult ContestCoreTestId
fixCoreResultTest' = EntityFieldWrapper FixCoreResultTest

{-@ measure fixCoreResultPass :: FixCoreResult -> Bool @-}

{-@ measure fixCoreResultPassCap :: Entity FixCoreResult -> Bool @-}

{-@ assume fixCoreResultPass' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixCoreResultPass (entityVal row)},
                          {\field row -> field == fixCoreResultPass (entityVal row)},
                          {\old -> fixCoreResultPassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixCoreResultPassCap x_0)}>
                          (Entity User) FixCoreResult Bool
  @-}
fixCoreResultPass' :: EntityFieldWrapper (Entity User) FixCoreResult Bool
fixCoreResultPass' = EntityFieldWrapper FixCoreResultPass

{-@ measure fixCoreResultMessage :: FixCoreResult -> (Maybe Text) @-}

{-@ measure fixCoreResultMessageCap :: Entity FixCoreResult -> Bool @-}

{-@ assume fixCoreResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixCoreResultMessage (entityVal row)},
                          {\field row -> field == fixCoreResultMessage (entityVal row)},
                          {\old -> fixCoreResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixCoreResultMessageCap x_0)}>
                          (Entity User) FixCoreResult (Maybe Text)
  @-}
fixCoreResultMessage' :: EntityFieldWrapper (Entity User) FixCoreResult (Maybe Text)
fixCoreResultMessage' = EntityFieldWrapper FixCoreResultMessage

-- * FixPerformanceResult
{-@ mkFixPerformanceResult ::
        x_0: FixSubmissionId
     -> x_1: ContestPerformanceTestId
     -> x_2: Maybe Double
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> fixPerformanceResultSubmission (entityVal row) == x_0 && fixPerformanceResultTest (entityVal row) == x_1 && fixPerformanceResultTime (entityVal row) == x_2 && fixPerformanceResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixPerformanceResult
  @-}
mkFixPerformanceResult :: FixSubmissionId -> ContestPerformanceTestId -> Maybe Double -> Maybe Text -> BinahRecord (Entity User) FixPerformanceResult
mkFixPerformanceResult x_0 x_1 x_2 x_3 = BinahRecord (FixPerformanceResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity FixPerformanceResult | v == getJust (entityKey v)} @-}



{-@ assume fixPerformanceResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixPerformanceResult FixPerformanceResultId
  @-}
fixPerformanceResultId' :: EntityFieldWrapper (Entity User) FixPerformanceResult FixPerformanceResultId
fixPerformanceResultId' = EntityFieldWrapper FixPerformanceResultId

{-@ measure fixPerformanceResultSubmission :: FixPerformanceResult -> FixSubmissionId @-}

{-@ measure fixPerformanceResultSubmissionCap :: Entity FixPerformanceResult -> Bool @-}

{-@ assume fixPerformanceResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixPerformanceResultSubmission (entityVal row)},
                          {\field row -> field == fixPerformanceResultSubmission (entityVal row)},
                          {\old -> fixPerformanceResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixPerformanceResultSubmissionCap x_0)}>
                          (Entity User) FixPerformanceResult FixSubmissionId
  @-}
fixPerformanceResultSubmission' :: EntityFieldWrapper (Entity User) FixPerformanceResult FixSubmissionId
fixPerformanceResultSubmission' = EntityFieldWrapper FixPerformanceResultSubmission

{-@ measure fixPerformanceResultTest :: FixPerformanceResult -> ContestPerformanceTestId @-}

{-@ measure fixPerformanceResultTestCap :: Entity FixPerformanceResult -> Bool @-}

{-@ assume fixPerformanceResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixPerformanceResultTest (entityVal row)},
                          {\field row -> field == fixPerformanceResultTest (entityVal row)},
                          {\old -> fixPerformanceResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixPerformanceResultTestCap x_0)}>
                          (Entity User) FixPerformanceResult ContestPerformanceTestId
  @-}
fixPerformanceResultTest' :: EntityFieldWrapper (Entity User) FixPerformanceResult ContestPerformanceTestId
fixPerformanceResultTest' = EntityFieldWrapper FixPerformanceResultTest

{-@ measure fixPerformanceResultTime :: FixPerformanceResult -> (Maybe Double) @-}

{-@ measure fixPerformanceResultTimeCap :: Entity FixPerformanceResult -> Bool @-}

{-@ assume fixPerformanceResultTime' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixPerformanceResultTime (entityVal row)},
                          {\field row -> field == fixPerformanceResultTime (entityVal row)},
                          {\old -> fixPerformanceResultTimeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixPerformanceResultTimeCap x_0)}>
                          (Entity User) FixPerformanceResult (Maybe Double)
  @-}
fixPerformanceResultTime' :: EntityFieldWrapper (Entity User) FixPerformanceResult (Maybe Double)
fixPerformanceResultTime' = EntityFieldWrapper FixPerformanceResultTime

{-@ measure fixPerformanceResultMessage :: FixPerformanceResult -> (Maybe Text) @-}

{-@ measure fixPerformanceResultMessageCap :: Entity FixPerformanceResult -> Bool @-}

{-@ assume fixPerformanceResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixPerformanceResultMessage (entityVal row)},
                          {\field row -> field == fixPerformanceResultMessage (entityVal row)},
                          {\old -> fixPerformanceResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixPerformanceResultMessageCap x_0)}>
                          (Entity User) FixPerformanceResult (Maybe Text)
  @-}
fixPerformanceResultMessage' :: EntityFieldWrapper (Entity User) FixPerformanceResult (Maybe Text)
fixPerformanceResultMessage' = EntityFieldWrapper FixPerformanceResultMessage

-- * FixOptionalResult
{-@ mkFixOptionalResult ::
        x_0: FixSubmissionId
     -> x_1: ContestOptionalTestId
     -> x_2: Bool
     -> x_3: Maybe Text
     -> BinahRecord <{\row -> fixOptionalResultSubmission (entityVal row) == x_0 && fixOptionalResultTest (entityVal row) == x_1 && fixOptionalResultPass (entityVal row) == x_2 && fixOptionalResultMessage (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) FixOptionalResult
  @-}
mkFixOptionalResult :: FixSubmissionId -> ContestOptionalTestId -> Bool -> Maybe Text -> BinahRecord (Entity User) FixOptionalResult
mkFixOptionalResult x_0 x_1 x_2 x_3 = BinahRecord (FixOptionalResult x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity FixOptionalResult | v == getJust (entityKey v)} @-}



{-@ assume fixOptionalResultId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) FixOptionalResult FixOptionalResultId
  @-}
fixOptionalResultId' :: EntityFieldWrapper (Entity User) FixOptionalResult FixOptionalResultId
fixOptionalResultId' = EntityFieldWrapper FixOptionalResultId

{-@ measure fixOptionalResultSubmission :: FixOptionalResult -> FixSubmissionId @-}

{-@ measure fixOptionalResultSubmissionCap :: Entity FixOptionalResult -> Bool @-}

{-@ assume fixOptionalResultSubmission' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixOptionalResultSubmission (entityVal row)},
                          {\field row -> field == fixOptionalResultSubmission (entityVal row)},
                          {\old -> fixOptionalResultSubmissionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixOptionalResultSubmissionCap x_0)}>
                          (Entity User) FixOptionalResult FixSubmissionId
  @-}
fixOptionalResultSubmission' :: EntityFieldWrapper (Entity User) FixOptionalResult FixSubmissionId
fixOptionalResultSubmission' = EntityFieldWrapper FixOptionalResultSubmission

{-@ measure fixOptionalResultTest :: FixOptionalResult -> ContestOptionalTestId @-}

{-@ measure fixOptionalResultTestCap :: Entity FixOptionalResult -> Bool @-}

{-@ assume fixOptionalResultTest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixOptionalResultTest (entityVal row)},
                          {\field row -> field == fixOptionalResultTest (entityVal row)},
                          {\old -> fixOptionalResultTestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixOptionalResultTestCap x_0)}>
                          (Entity User) FixOptionalResult ContestOptionalTestId
  @-}
fixOptionalResultTest' :: EntityFieldWrapper (Entity User) FixOptionalResult ContestOptionalTestId
fixOptionalResultTest' = EntityFieldWrapper FixOptionalResultTest

{-@ measure fixOptionalResultPass :: FixOptionalResult -> Bool @-}

{-@ measure fixOptionalResultPassCap :: Entity FixOptionalResult -> Bool @-}

{-@ assume fixOptionalResultPass' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixOptionalResultPass (entityVal row)},
                          {\field row -> field == fixOptionalResultPass (entityVal row)},
                          {\old -> fixOptionalResultPassCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixOptionalResultPassCap x_0)}>
                          (Entity User) FixOptionalResult Bool
  @-}
fixOptionalResultPass' :: EntityFieldWrapper (Entity User) FixOptionalResult Bool
fixOptionalResultPass' = EntityFieldWrapper FixOptionalResultPass

{-@ measure fixOptionalResultMessage :: FixOptionalResult -> (Maybe Text) @-}

{-@ measure fixOptionalResultMessageCap :: Entity FixOptionalResult -> Bool @-}

{-@ assume fixOptionalResultMessage' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == fixOptionalResultMessage (entityVal row)},
                          {\field row -> field == fixOptionalResultMessage (entityVal row)},
                          {\old -> fixOptionalResultMessageCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (fixOptionalResultMessageCap x_0)}>
                          (Entity User) FixOptionalResult (Maybe Text)
  @-}
fixOptionalResultMessage' :: EntityFieldWrapper (Entity User) FixOptionalResult (Maybe Text)
fixOptionalResultMessage' = EntityFieldWrapper FixOptionalResultMessage

-- * Configuration
{-@ mkConfiguration ::
        x_0: Text
     -> x_1: Text
     -> BinahRecord <{\row -> configurationKey (entityVal row) == x_0 && configurationValue (entityVal row) == x_1},
                     {\_ viewer -> userAdmin (entityVal viewer)},
                     {\x_0 x_1 -> False}>
                     (Entity User) Configuration
  @-}
mkConfiguration :: Text -> Text -> BinahRecord (Entity User) Configuration
mkConfiguration x_0 x_1 = BinahRecord (Configuration x_0 x_1)

{-@ invariant {v: Entity Configuration | v == getJust (entityKey v)} @-}



{-@ assume configurationId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Configuration ConfigurationId
  @-}
configurationId' :: EntityFieldWrapper (Entity User) Configuration ConfigurationId
configurationId' = EntityFieldWrapper ConfigurationId

{-@ measure configurationKey :: Configuration -> Text @-}

{-@ measure configurationKeyCap :: Entity Configuration -> Bool @-}

{-@ assume configurationKey' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == configurationKey (entityVal row)},
                          {\field row -> field == configurationKey (entityVal row)},
                          {\old -> configurationKeyCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (configurationKeyCap x_0)}>
                          (Entity User) Configuration Text
  @-}
configurationKey' :: EntityFieldWrapper (Entity User) Configuration Text
configurationKey' = EntityFieldWrapper ConfigurationKey

{-@ measure configurationValue :: Configuration -> Text @-}

{-@ measure configurationValueCap :: Entity Configuration -> Bool @-}

{-@ assume configurationValue' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == configurationValue (entityVal row)},
                          {\field row -> field == configurationValue (entityVal row)},
                          {\old -> configurationValueCap old},
                          {\x_0 x_1 x_2 -> ((userAdmin (entityVal x_2))) => (configurationValueCap x_0)}>
                          (Entity User) Configuration Text
  @-}
configurationValue' :: EntityFieldWrapper (Entity User) Configuration Text
configurationValue' = EntityFieldWrapper ConfigurationValue

-- * CacheExpiration
{-@ mkCacheExpiration ::
        x_0: Text
     -> x_1: UTCTime
     -> BinahRecord <{\row -> cacheExpirationKey (entityVal row) == x_0 && cacheExpirationExpiration (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CacheExpiration
  @-}
mkCacheExpiration :: Text -> UTCTime -> BinahRecord (Entity User) CacheExpiration
mkCacheExpiration x_0 x_1 = BinahRecord (CacheExpiration x_0 x_1)

{-@ invariant {v: Entity CacheExpiration | v == getJust (entityKey v)} @-}



{-@ assume cacheExpirationId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CacheExpiration CacheExpirationId
  @-}
cacheExpirationId' :: EntityFieldWrapper (Entity User) CacheExpiration CacheExpirationId
cacheExpirationId' = EntityFieldWrapper CacheExpirationId

{-@ measure cacheExpirationKey :: CacheExpiration -> Text @-}

{-@ measure cacheExpirationKeyCap :: Entity CacheExpiration -> Bool @-}

{-@ assume cacheExpirationKey' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheExpirationKey (entityVal row)},
                          {\field row -> field == cacheExpirationKey (entityVal row)},
                          {\old -> cacheExpirationKeyCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheExpirationKeyCap x_0)}>
                          (Entity User) CacheExpiration Text
  @-}
cacheExpirationKey' :: EntityFieldWrapper (Entity User) CacheExpiration Text
cacheExpirationKey' = EntityFieldWrapper CacheExpirationKey

{-@ measure cacheExpirationExpiration :: CacheExpiration -> UTCTime @-}

{-@ measure cacheExpirationExpirationCap :: Entity CacheExpiration -> Bool @-}

{-@ assume cacheExpirationExpiration' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheExpirationExpiration (entityVal row)},
                          {\field row -> field == cacheExpirationExpiration (entityVal row)},
                          {\old -> cacheExpirationExpirationCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheExpirationExpirationCap x_0)}>
                          (Entity User) CacheExpiration UTCTime
  @-}
cacheExpirationExpiration' :: EntityFieldWrapper (Entity User) CacheExpiration UTCTime
cacheExpirationExpiration' = EntityFieldWrapper CacheExpirationExpiration

-- * CacheBuildersCode
{-@ mkCacheBuildersCode ::
        x_0: Text
     -> x_1: TeamContestId
     -> x_2: Text
     -> x_3: ContestId
     -> x_4: Double
     -> x_5: Int
     -> x_6: Int
     -> BinahRecord <{\row -> cacheBuildersCodeTeam (entityVal row) == x_0 && cacheBuildersCodeTeamId (entityVal row) == x_1 && cacheBuildersCodeLanguages (entityVal row) == x_2 && cacheBuildersCodeContestId (entityVal row) == x_3 && cacheBuildersCodeBuilderScore (entityVal row) == x_4 && cacheBuildersCodeBugsFound (entityVal row) == x_5 && cacheBuildersCodeVulnerabilitiesFound (entityVal row) == x_6},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CacheBuildersCode
  @-}
mkCacheBuildersCode :: Text -> TeamContestId -> Text -> ContestId -> Double -> Int -> Int -> BinahRecord (Entity User) CacheBuildersCode
mkCacheBuildersCode x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (CacheBuildersCode x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity CacheBuildersCode | v == getJust (entityKey v)} @-}



{-@ assume cacheBuildersCodeId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CacheBuildersCode CacheBuildersCodeId
  @-}
cacheBuildersCodeId' :: EntityFieldWrapper (Entity User) CacheBuildersCode CacheBuildersCodeId
cacheBuildersCodeId' = EntityFieldWrapper CacheBuildersCodeId

{-@ measure cacheBuildersCodeTeam :: CacheBuildersCode -> Text @-}

{-@ measure cacheBuildersCodeTeamCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeTeam' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeTeam (entityVal row)},
                          {\field row -> field == cacheBuildersCodeTeam (entityVal row)},
                          {\old -> cacheBuildersCodeTeamCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeTeamCap x_0)}>
                          (Entity User) CacheBuildersCode Text
  @-}
cacheBuildersCodeTeam' :: EntityFieldWrapper (Entity User) CacheBuildersCode Text
cacheBuildersCodeTeam' = EntityFieldWrapper CacheBuildersCodeTeam

{-@ measure cacheBuildersCodeTeamId :: CacheBuildersCode -> TeamContestId @-}

{-@ measure cacheBuildersCodeTeamIdCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeTeamId' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeTeamId (entityVal row)},
                          {\field row -> field == cacheBuildersCodeTeamId (entityVal row)},
                          {\old -> cacheBuildersCodeTeamIdCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeTeamIdCap x_0)}>
                          (Entity User) CacheBuildersCode TeamContestId
  @-}
cacheBuildersCodeTeamId' :: EntityFieldWrapper (Entity User) CacheBuildersCode TeamContestId
cacheBuildersCodeTeamId' = EntityFieldWrapper CacheBuildersCodeTeamId

{-@ measure cacheBuildersCodeLanguages :: CacheBuildersCode -> Text @-}

{-@ measure cacheBuildersCodeLanguagesCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeLanguages' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeLanguages (entityVal row)},
                          {\field row -> field == cacheBuildersCodeLanguages (entityVal row)},
                          {\old -> cacheBuildersCodeLanguagesCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeLanguagesCap x_0)}>
                          (Entity User) CacheBuildersCode Text
  @-}
cacheBuildersCodeLanguages' :: EntityFieldWrapper (Entity User) CacheBuildersCode Text
cacheBuildersCodeLanguages' = EntityFieldWrapper CacheBuildersCodeLanguages

{-@ measure cacheBuildersCodeContestId :: CacheBuildersCode -> ContestId @-}

{-@ measure cacheBuildersCodeContestIdCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeContestId' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeContestId (entityVal row)},
                          {\field row -> field == cacheBuildersCodeContestId (entityVal row)},
                          {\old -> cacheBuildersCodeContestIdCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeContestIdCap x_0)}>
                          (Entity User) CacheBuildersCode ContestId
  @-}
cacheBuildersCodeContestId' :: EntityFieldWrapper (Entity User) CacheBuildersCode ContestId
cacheBuildersCodeContestId' = EntityFieldWrapper CacheBuildersCodeContestId

{-@ measure cacheBuildersCodeBuilderScore :: CacheBuildersCode -> Double @-}

{-@ measure cacheBuildersCodeBuilderScoreCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeBuilderScore' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeBuilderScore (entityVal row)},
                          {\field row -> field == cacheBuildersCodeBuilderScore (entityVal row)},
                          {\old -> cacheBuildersCodeBuilderScoreCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeBuilderScoreCap x_0)}>
                          (Entity User) CacheBuildersCode Double
  @-}
cacheBuildersCodeBuilderScore' :: EntityFieldWrapper (Entity User) CacheBuildersCode Double
cacheBuildersCodeBuilderScore' = EntityFieldWrapper CacheBuildersCodeBuilderScore

{-@ measure cacheBuildersCodeBugsFound :: CacheBuildersCode -> Int @-}

{-@ measure cacheBuildersCodeBugsFoundCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeBugsFound' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeBugsFound (entityVal row)},
                          {\field row -> field == cacheBuildersCodeBugsFound (entityVal row)},
                          {\old -> cacheBuildersCodeBugsFoundCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeBugsFoundCap x_0)}>
                          (Entity User) CacheBuildersCode Int
  @-}
cacheBuildersCodeBugsFound' :: EntityFieldWrapper (Entity User) CacheBuildersCode Int
cacheBuildersCodeBugsFound' = EntityFieldWrapper CacheBuildersCodeBugsFound

{-@ measure cacheBuildersCodeVulnerabilitiesFound :: CacheBuildersCode -> Int @-}

{-@ measure cacheBuildersCodeVulnerabilitiesFoundCap :: Entity CacheBuildersCode -> Bool @-}

{-@ assume cacheBuildersCodeVulnerabilitiesFound' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == cacheBuildersCodeVulnerabilitiesFound (entityVal row)},
                          {\field row -> field == cacheBuildersCodeVulnerabilitiesFound (entityVal row)},
                          {\old -> cacheBuildersCodeVulnerabilitiesFoundCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (cacheBuildersCodeVulnerabilitiesFoundCap x_0)}>
                          (Entity User) CacheBuildersCode Int
  @-}
cacheBuildersCodeVulnerabilitiesFound' :: EntityFieldWrapper (Entity User) CacheBuildersCode Int
cacheBuildersCodeVulnerabilitiesFound' = EntityFieldWrapper CacheBuildersCodeVulnerabilitiesFound

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
                          {\x_0 x_1 x_2 -> ((False)) => (storedFileOwnerCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (storedFileNameCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (storedFileContentTypeCap x_0)}>
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
                          {\x_0 x_1 x_2 -> ((False)) => (storedFileContentCap x_0)}>
                          (Entity User) StoredFile ByteString
  @-}
storedFileContent' :: EntityFieldWrapper (Entity User) StoredFile ByteString
storedFileContent' = EntityFieldWrapper StoredFileContent

-- * Error
{-@ mkError ::
        x_0: Text
     -> x_1: Text
     -> x_2: UTCTime
     -> BinahRecord <{\row -> errorHandlerName (entityVal row) == x_0 && errorErrorText (entityVal row) == x_1 && errorTime (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Error
  @-}
mkError :: Text -> Text -> UTCTime -> BinahRecord (Entity User) Error
mkError x_0 x_1 x_2 = BinahRecord (Error x_0 x_1 x_2)

{-@ invariant {v: Entity Error | v == getJust (entityKey v)} @-}



{-@ assume errorId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Error ErrorId
  @-}
errorId' :: EntityFieldWrapper (Entity User) Error ErrorId
errorId' = EntityFieldWrapper ErrorId

{-@ measure errorHandlerName :: Error -> Text @-}

{-@ measure errorHandlerNameCap :: Entity Error -> Bool @-}

{-@ assume errorHandlerName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == errorHandlerName (entityVal row)},
                          {\field row -> field == errorHandlerName (entityVal row)},
                          {\old -> errorHandlerNameCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (errorHandlerNameCap x_0)}>
                          (Entity User) Error Text
  @-}
errorHandlerName' :: EntityFieldWrapper (Entity User) Error Text
errorHandlerName' = EntityFieldWrapper ErrorHandlerName

{-@ measure errorErrorText :: Error -> Text @-}

{-@ measure errorErrorTextCap :: Entity Error -> Bool @-}

{-@ assume errorErrorText' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == errorErrorText (entityVal row)},
                          {\field row -> field == errorErrorText (entityVal row)},
                          {\old -> errorErrorTextCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (errorErrorTextCap x_0)}>
                          (Entity User) Error Text
  @-}
errorErrorText' :: EntityFieldWrapper (Entity User) Error Text
errorErrorText' = EntityFieldWrapper ErrorErrorText

{-@ measure errorTime :: Error -> UTCTime @-}

{-@ measure errorTimeCap :: Entity Error -> Bool @-}

{-@ assume errorTime' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == errorTime (entityVal row)},
                          {\field row -> field == errorTime (entityVal row)},
                          {\old -> errorTimeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (errorTimeCap x_0)}>
                          (Entity User) Error UTCTime
  @-}
errorTime' :: EntityFieldWrapper (Entity User) Error UTCTime
errorTime' = EntityFieldWrapper ErrorTime

-- * RateLimitLog
{-@ mkRateLimitLog ::
        x_0: Int
     -> x_1: Int
     -> x_2: UTCTime
     -> BinahRecord <{\row -> rateLimitLogAction (entityVal row) == x_0 && rateLimitLogLimiter (entityVal row) == x_1 && rateLimitLogTime (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) RateLimitLog
  @-}
mkRateLimitLog :: Int -> Int -> UTCTime -> BinahRecord (Entity User) RateLimitLog
mkRateLimitLog x_0 x_1 x_2 = BinahRecord (RateLimitLog x_0 x_1 x_2)

{-@ invariant {v: Entity RateLimitLog | v == getJust (entityKey v)} @-}



{-@ assume rateLimitLogId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) RateLimitLog RateLimitLogId
  @-}
rateLimitLogId' :: EntityFieldWrapper (Entity User) RateLimitLog RateLimitLogId
rateLimitLogId' = EntityFieldWrapper RateLimitLogId

{-@ measure rateLimitLogAction :: RateLimitLog -> Int @-}

{-@ measure rateLimitLogActionCap :: Entity RateLimitLog -> Bool @-}

{-@ assume rateLimitLogAction' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == rateLimitLogAction (entityVal row)},
                          {\field row -> field == rateLimitLogAction (entityVal row)},
                          {\old -> rateLimitLogActionCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (rateLimitLogActionCap x_0)}>
                          (Entity User) RateLimitLog Int
  @-}
rateLimitLogAction' :: EntityFieldWrapper (Entity User) RateLimitLog Int
rateLimitLogAction' = EntityFieldWrapper RateLimitLogAction

{-@ measure rateLimitLogLimiter :: RateLimitLog -> Int @-}

{-@ measure rateLimitLogLimiterCap :: Entity RateLimitLog -> Bool @-}

{-@ assume rateLimitLogLimiter' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == rateLimitLogLimiter (entityVal row)},
                          {\field row -> field == rateLimitLogLimiter (entityVal row)},
                          {\old -> rateLimitLogLimiterCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (rateLimitLogLimiterCap x_0)}>
                          (Entity User) RateLimitLog Int
  @-}
rateLimitLogLimiter' :: EntityFieldWrapper (Entity User) RateLimitLog Int
rateLimitLogLimiter' = EntityFieldWrapper RateLimitLogLimiter

{-@ measure rateLimitLogTime :: RateLimitLog -> UTCTime @-}

{-@ measure rateLimitLogTimeCap :: Entity RateLimitLog -> Bool @-}

{-@ assume rateLimitLogTime' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == rateLimitLogTime (entityVal row)},
                          {\field row -> field == rateLimitLogTime (entityVal row)},
                          {\old -> rateLimitLogTimeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (rateLimitLogTimeCap x_0)}>
                          (Entity User) RateLimitLog UTCTime
  @-}
rateLimitLogTime' :: EntityFieldWrapper (Entity User) RateLimitLog UTCTime
rateLimitLogTime' = EntityFieldWrapper RateLimitLogTime

-- * ScorePending
{-@ mkScorePending ::
        x_0: ContestId
     -> x_1: ContestRound
     -> BinahRecord <{\row -> scorePendingContest (entityVal row) == x_0 && scorePendingRound (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) ScorePending
  @-}
mkScorePending :: ContestId -> ContestRound -> BinahRecord (Entity User) ScorePending
mkScorePending x_0 x_1 = BinahRecord (ScorePending x_0 x_1)

{-@ invariant {v: Entity ScorePending | v == getJust (entityKey v)} @-}



{-@ assume scorePendingId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) ScorePending ScorePendingId
  @-}
scorePendingId' :: EntityFieldWrapper (Entity User) ScorePending ScorePendingId
scorePendingId' = EntityFieldWrapper ScorePendingId

{-@ measure scorePendingContest :: ScorePending -> ContestId @-}

{-@ measure scorePendingContestCap :: Entity ScorePending -> Bool @-}

{-@ assume scorePendingContest' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == scorePendingContest (entityVal row)},
                          {\field row -> field == scorePendingContest (entityVal row)},
                          {\old -> scorePendingContestCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (scorePendingContestCap x_0)}>
                          (Entity User) ScorePending ContestId
  @-}
scorePendingContest' :: EntityFieldWrapper (Entity User) ScorePending ContestId
scorePendingContest' = EntityFieldWrapper ScorePendingContest

{-@ measure scorePendingRound :: ScorePending -> ContestRound @-}

{-@ measure scorePendingRoundCap :: Entity ScorePending -> Bool @-}

{-@ assume scorePendingRound' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == scorePendingRound (entityVal row)},
                          {\field row -> field == scorePendingRound (entityVal row)},
                          {\old -> scorePendingRoundCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (scorePendingRoundCap x_0)}>
                          (Entity User) ScorePending ContestRound
  @-}
scorePendingRound' :: EntityFieldWrapper (Entity User) ScorePending ContestRound
scorePendingRound' = EntityFieldWrapper ScorePendingRound
