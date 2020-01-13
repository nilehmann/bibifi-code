module Problem.API where

import Codec.Archive.Tar (Entries(..))
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.Error
import Data.Aeson ((.=), FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.IORef.Lifted as IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Database.Esqueleto as E
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
-- import qualified System.Posix.Files as Files
import Yesod.Form.Fields (Textarea(..))

import Cloud
import Common
-- import Core (keyToInt)
import Core.Score
import Core.SSH
import Problem.Class
import Problem.Shared hiding (grader, runBuildTest, runTestAt)
import Scorer.Class

import BuildSubmissions (getLatestBuildOrFix)

newtype APIProblem = APIProblem (Entity Contest)

instance ExtractContest APIProblem where
    extractContest (APIProblem c) = c

instance ScorerClass APIProblem where
    scoreContestBuild p _ = defaultScoreBuildRound $ extractContestId p
    scoreContestBreak p _ = defaultScoreBreakRound $ extractContestId p
    scoreContestFix p _ = defaultScoreFixRound $ extractContestId p

instance ProblemRunnerClass APIProblem where
    runOracleSubmission (APIProblem _) opts (Entity submissionId submission) = 
        -- Parse oracle input.
        let inputObjectM = Aeson.decodeStrict' $ Text.encodeUtf8 $ oracleSubmissionInput submission in
        case inputObjectM of
            Nothing -> do
                -- Invalid input so store as error.
                runDB $ update submissionId [OracleSubmissionStatus =. OracleError, OracleSubmissionOutput =. Just "Invalid input"]

                -- Return success.
                return $ Just True
            Just inputObject -> do
                
                -- Start instance.
                let conf = runnerCloudConfiguration opts
                let manager = runnerHttpManager opts
                resultE <- runErrorT $ launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do

                    -- setupFirewall session

                    -- Send oracle.
                    putLog "Sending oracle files."
                    uploadFolder session (runnerProblemDirectory opts) "/problem"

                    -- Run oracle.
                    runOracle session inputObject

                -- Return result.
                case resultE of
                    Left (OracleErr err) -> do
                        putLog err
                        return (Just False)
                    Left OracleErrTimeout -> do
                        return Nothing
                    Right res -> do
                        let (status, output) = case res of
                              Nothing ->
                                (OracleError, Nothing)
                              Just (OracleOutputError err) ->
                                (OracleError, Just err)
                              Just (OracleOutputSuccess out) ->
                                (OracleFinished, Just out)
                        runDB $ update submissionId [OracleSubmissionOutput =. output, OracleSubmissionStatus =. status]
                        return $ Just True

        where
            runOracle :: (BackendError e, MonadIO m) => Session -> Aeson.Value -> ErrorT e m (Maybe OracleOutput)
            runOracle session input = do
                let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
                        "type" .= ("oracle" :: String)
                      , "input" .= input
                      ]

                -- Upload json input.
                uploadString session json destJson

                -- Launch oracle.
                (Result resOut' resErr exitCode) <- executioner' session testUser "/problem/grader" [destJson]
                when ( exitCode /= ExitSuccess) $ do
                    putLog $ BS8.unpack resOut'
                    putLog $ BS8.unpack resErr

                -- Drop newline.
                let (resOut, _) = BS.breakSubstring "\n" resOut'
                -- putLog $ show resOut

                -- Parse resOut.
                return $ Aeson.decodeStrict' resOut

    runBuildSubmission (APIProblem (Entity _contestId _contest)) opts (Entity submissionId submission) = do
        -- Extract tests.
        let BuildTests coreTests performanceTests optionalTests = runnerBuildTests opts

        coreDoneRef <- IO.newIORef False
        resultsE <- runErrorT $ do
            -- Delete any previous stored results.
            lift $ runDB $ deleteWhere [BuildCoreResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildPerformanceResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildOptionalResultSubmission ==. submissionId]

            -- Extract build submission.
            submissionTarGz <- extractAndCompressBuildSubmission submission opts

            -- Run instance.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Upload, extract, and build submission.
                uploadAndCompileBuilderSubmissionBuild session submissionTarGz

                -- Upload problem files.
                putLog "Sending problem files."
                uploadFolder session (runnerProblemDirectory opts) "/problem"

                -- Map over tests.
                let (requiredTests, optTests) = List.partition (isBuildTestRequired . fst) (coreTests <> performanceTests <> optionalTests)
                portRef <- initialPort
                requiredResults <- mapM (runBuildTest session portRef) requiredTests
                mapM_ (recordBuildResult submissionId) requiredResults

                -- Store submission in db.
                _ <- lift $ lift $ runDB $ do
                    -- Delete old one if it exists.
                    deleteBy $ UniqueBuildSubmissionFile tcId
                    
                    -- Insert new one.
                    insertUnique $ BuildSubmissionFile tcId submissionTarGz

                -- Indicate core tests passed. 
                coreDoneRef `IO.writeIORef` True

                -- Continue running optional tests.
                mapM_ (\test -> do
                    result <- runBuildTest session portRef test 
                    (recordBuildResult submissionId) result
                  ) optTests



        case (resultsE :: Either BuildError ()) of
            Left (BuildFail stdout' stderr') -> do
                let stdout = decodeUtf8 stdout'
                let stderr = decodeUtf8 stderr'
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuildFail, BuildSubmissionStdout =. stdout, BuildSubmissionStderr =. stderr]
                putLog "Build failed"
                return $ Just (True, False)
            Left (BuildError err) -> do
                putLog err
                return $ Just (False, False)
            Left BuildErrorTimeout -> do
                -- Timeout.
                -- Check if core completed.
                coreDone <- IO.readIORef coreDoneRef
                if coreDone then 
                    buildBuilt
                else
                    return Nothing
            Right () ->
                buildBuilt
        
        where
            buildBuilt = do
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuilt]
                return $ Just (True, True)

            tcId = buildSubmissionTeam submission


    runBreakSubmission (APIProblem (Entity contestId _contest)) opts bsE@(Entity bsId bs) = do
        -- Delete any previous break fix submissions.
        runDB $ deleteWhere [BreakFixSubmissionBreak ==. bsId]

        -- Hack so that we can get the target submission id.
        targetSubmissionIdRef <- IO.newIORef Nothing

        resultE <- runErrorT $ do
            targetTeamId <- checkSubmissionRound2 contestId bsE

            -- let breakArchiveLocation = teamSubmissionLocation opts submitTeamId $ breakSubmissionCommitHash bs

            -- Extract and compress break submission. 
            breakSubmissionTarGz <- extractAndCompressBreakSubmission bs opts

            -- Store break submission in database.
            _ <- lift $ runDB $ do
                -- Delete old one if it exists.
                deleteBy $ UniqueBreakSubmissionFile bsId

                -- Insert new one.
                insertUnique $ BreakSubmissionFile bsId breakSubmissionTarGz

            -- Get latest build or fix submission.
            (targetSubmissionId, targetSubmissionTarGz) <- getLatestBuildOrFixTarGz targetTeamId
            IO.writeIORef targetSubmissionIdRef targetSubmissionId
            

        --     (breakTest :: JSONBreakTest) <- loadBreakSubmissionJSON submissionId breakJSONFile
            (breakTest :: JSONBreakTest, _) <- breakTestToJSONBreakTest bsE

            -- Start instance.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 30 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Upload problem files.
                putLog "Sending problem files."
                uploadFolder session (runnerProblemDirectory opts) "/problem"

                -- Upload target submission.
                uploadAndCompileBuilderSubmissionBreak session targetSubmissionTarGz

                -- Retrieve list of passed build tests.
                passedOptionalTests <- retrievePassedOptionalTests targetTeamId

                portRef <- initialPort

                -- Upload and compile break folder.
                (remoteUsername, remoteBreakDir) <- uploadAndCompileBreakerSubmissionBreak session portRef bs breakSubmissionTarGz

                -- Make sure break submission description exists.
                descriptionExists <- remoteFileExists session remoteUsername $ remoteBreakDir <> "description.txt"
                unless descriptionExists $
                    fail "description.txt does not exist."

                runBreakTest session passedOptionalTests portRef remoteUsername remoteBreakDir breakTest

        -- Record result.
        targetSubmissionId <- IO.readIORef targetSubmissionIdRef
        case resultE of
            Left (BreakErrorSystem err) -> do
                systemFail err

            Left BreakErrorTimeout ->
                return Nothing

            Left (BreakErrorBuildFail stdout' stderr') -> runDB $ do
                let stdout = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stdout'
                let stderr = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stderr'
                insert_ $ BreakFixSubmission bsId targetSubmissionId BreakFailed
                runIfStillValid $ 
                    update bsId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionMessage =. Just "Running make failed", BreakSubmissionValid =. Nothing, BreakSubmissionStdout =. stdout, BreakSubmissionStderr =. stderr]
                userFail "Build failed"

            Left (BreakErrorRejected msg) -> runDB $ do
                insert_ $ BreakFixSubmission bsId targetSubmissionId BreakFailed
                runIfStillValid $
                    update bsId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionMessage =. Just msg, BreakSubmissionValid =. Nothing, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                userFail msg

            Right (BreakResult False msgM) -> runDB $ do
                insert_ $ BreakFixSubmission bsId targetSubmissionId BreakFailed
                runIfStillValid $ 
                    update bsId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionMessage =. fmap Text.unpack msgM, BreakSubmissionValid =. Nothing, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                userFail $ maybe "Test failed" Text.unpack msgM

            -- Right (BreakResult Nothing _) -> runDB $ 
            --     runUnlessNewFix targetSubmissionId $ do
            --         -- JP: We mark as valid so that users can fix. If they appeal, manually judge as invalid.
            --         -- TODO: Email target team.
            --         insert_ $ BreakFixSubmission bsId targetSubmissionId BreakSucceeded
            --         runIfStillValid $
            --             update bsId [BreakSubmissionStatus =. BreakTested, BreakSubmissionMessage =. Nothing, BreakSubmissionValid =. Just True, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
            --         return $ Just ( True, True)

            --         -- runIfStillValid $
            --         --     update bsId [BreakSubmissionStatus =. BreakJudging, BreakSubmissionMessage =. Nothing, BreakSubmissionValid =. Nothing]
            --         -- return $ Just ( True, False)

            Right (BreakResult True _) -> runDB $ do
                runUnlessNewFix targetSubmissionId $ do
                    -- TODO: Email target team.
                    insert_ $ BreakFixSubmission bsId targetSubmissionId BreakSucceeded
                    runIfStillValid $
                        update bsId [BreakSubmissionStatus =. BreakTested, BreakSubmissionMessage =. Nothing, BreakSubmissionValid =. Just True, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                    return $ Just ( True, True)

        where
            runUnlessNewFix oldTargetSubmissionId m = do
                needRerun <- do
                    case breakSubmissionTargetTeam bs of
                        Nothing ->
                            return True
                        Just targetTeamId -> do
                            newTargetSubmissionIdE <- getLatestBuildOrFix targetTeamId (breakSubmissionTimestamp bs)
                            case newTargetSubmissionIdE of
                                Left _ ->
                                    return True
                                Right e ->
                                    let newTargetSubmissionId = either (const Nothing) Just e in
                                    return $ newTargetSubmissionId == oldTargetSubmissionId

                if needRerun then
                    m
                else 
                    systemFail "New fix detected after testing a break."

            getLatestBuildOrFixTarGz targetTeamId = do
                resE <- lift $ runDB $ getLatestBuildOrFix targetTeamId $ breakSubmissionTimestamp bs -- can't fail due to prior checks
                case resE of
                    Left _err ->
                        fail "Could not retrieve target submission."
                    Right (Left _buildId) -> do
                        f <- lift $ runDB $ getBy $ UniqueBuildSubmissionFile targetTeamId
                        case f of
                            Nothing ->
                                fail "Target submission does not exist."
                            Just (Entity _ f) ->
                                return (Nothing, buildSubmissionFileFile f)

                    Right (Right fixId) -> do
                        f <- lift $ runDB $ getBy $ UniqueFixSubmissionFile fixId
                        case f of
                            Nothing ->
                                fail "Target submission does not exist."
                            Just (Entity _ f) ->
                                return (Just fixId, fixSubmissionFileFile f)

        --     basePath = runnerRepositoryPath opts
        --     breakJSONFile = FilePath.addExtension (FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName, "test"]) "json"
        --     breakDir = FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName]
        --     breakMakefile = FilePath.joinPath [breakDir, "Makefile"]
        --     targetTeamIdS = show $ keyToInt targetTeamId
        --     submitTeamId = breakSubmissionTeam bs
        --     submitTeamIdS = show $ keyToInt submitTeamId
        --     breakName = Text.unpack $ breakSubmissionName submission

            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False)

        --     targetId = breakFixSubmissionFix bfs

            runIfStillValid m = do
                bsM <- get bsId
                case bsM of
                    Nothing ->
                        return ()
                    Just bs | breakSubmissionValid bs == Just False -> 
                        return ()
                    _ ->
                        m

        --     saveIfStillValid msg stdout stderr stat res = runDB $ do
        --         Just latestBreakSubmission <- get submissionId
        --         unless (breakSubmissionValid latestBreakSubmission == Just False) $ do
        --             update bfsId [ BreakFixSubmissionStdout =. stdout
        --                          , BreakFixSubmissionStderr =. stderr
        --                          , BreakFixSubmissionStatus =. stat
        --                          , BreakFixSubmissionResult =. res ]
        --             update submissionId [BreakSubmissionMessage =. msg, BreakSubmissionValid =. Just True]


    runFixSubmission (APIProblem (Entity _contestId _contest)) opts (Entity submissionId submission) = do
        
        -- Extract tests.
        let BuildTests coreTests performanceTests _optionalTests = runnerBuildTests opts

        -- Delete any previous results.
        runDB $ deleteWhere [BreakFixSubmissionFix ==. Just submissionId]

        -- -- Get all valid and active breaks against this team
        breaks' <- runDB $ getValidActiveBreaksAgainst submission
        -- breaks'' <- runDB $ selectList [ BreakSubmissionTargetTeam ==. teamId
        --                                , BreakSubmissionValid !=. Just False ]
        --                                []

        -- let archiveLocation = teamSubmissionLocation opts teamId hash

        resultsE <- runErrorT $ do

            -- Extract fix submission.
            submissionTarGz <- extractAndCompressFixSubmission submission opts




        --     -- Verify breaks fixed and filter them (only include automatically tested ones).
        --     let breaks' = breaks'' -- FIXME
        --     --breaks' <- verifyAndFilterBreaksForFix breaks'' $ \bs -> breakSubmissionStatus bs == BreakTested
        --     

            -- Convert breaks to JSON breaks.
            breaks <- mapM breakTestToJSONBreakTest breaks'

            -- Start instance.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 30 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Upload, extract, and build submission.
                uploadAndCompileBuilderSubmissionFix session submissionTarGz

                -- Upload problem files.
                putLog "Sending problem files."
                uploadFolder session (runnerProblemDirectory opts) "/problem"

                -- Run core tests.
                let (requiredTests, _) = List.partition (isBuildTestRequired . fst) (coreTests <> performanceTests)
                portRef <- initialPort
                mapM_ (\t -> do
                    (test, BuildResult pass msgM _) <- runBuildTest session portRef t
                    when ( not pass) $ 
                        let msg = maybe "" Text.unpack msgM in
                        throwError $ FixErrorRejected $ "Failed required test: " <> Text.unpack (buildTestName test) <> ". " <> msg
                  ) requiredTests

                -- Retrieve list of passed build tests.
                passedOptionalTests <- retrievePassedOptionalTests teamId

                -- Run each break test.
                mapM_ (\(breakTest, (Entity bsId bs)) -> do
                    -- Retrieve break folder.
                    breakSubmissionTarGz <- do
                        resM <- lift $ lift $ runDB $ getBy $ UniqueBreakSubmissionFile bsId
                        case resM of
                            Nothing -> 
                                fail $ "Break submission tar.gz missing for " <> Text.unpack (breakSubmissionName bs)
                            Just bsf ->
                                return $ breakSubmissionFileFile $ entityVal bsf

                    -- Upload and compile break folder.
                    (remoteUsername, remoteBreakDir) <- uploadAndCompileBreakerSubmissionFix session portRef bs breakSubmissionTarGz

                    -- Run break test.
                    res <- runBreakTest session passedOptionalTests portRef remoteUsername remoteBreakDir breakTest
                    lift $ lift $ runDB $ case res of
                        BreakResult False _ ->
                            insert_ $ BreakFixSubmission bsId (Just submissionId) BreakFailed

                        -- BreakResult Nothing _ ->
                        --     insert_ $ BreakFixSubmission bsId (Just submissionId) BreakSucceeded

                        BreakResult True _ ->
                            insert_ $ BreakFixSubmission bsId (Just submissionId) BreakSucceeded
                        
                  ) breaks

        --             -- Delete break directory.
        --             (Result _ _ exit) <- runSSH (FixErrorSystem "Could not delete break directory.") $ execCommand session "sudo rm -rf /break"
        --             when (exit /= ExitSuccess) $
        --                 fail "Could not delete break directory."

        --             -- Upload break.
        --             let breakName = Text.unpack $ breakSubmissionName bs
        --             let submitTeamIdS = show $ keyToInt $ breakSubmissionTeam bs
        --             let breakDir = FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName]
        --             let breakMakefile = FilePath.joinPath [breakDir, "Makefile"]
        --             uploadFolder session breakDir "/break"

        --             -- Build break if there's a Makefile.
        --             makefileExists <- liftIO $ Directory.doesFileExist breakMakefile
        --             when makefileExists $ do
        --                 putLog "Building break submission."
        --                 (Result _ _ exit) <- runSSH (FixErrorSystem "Could not build break") $ execCommand session $ "sudo -i -u breaker make -B -C /break"
        --                 when (exit /= ExitSuccess) $
        --                     fail "Could not build break"

        --             -- Run break test.
        --             res <- runBreakTest session portRef t
        --             case res of
        --                 BreakResult (Just False) _ ->
        --                     return ()
        --                 BreakResult (Just True) _ ->
        --                     throwError $ FixErrorRejected $ "Failed test: " ++ Text.unpack (breakSubmissionName bs)
        --                 BreakResult Nothing _ ->
        --                     throwError $ FixErrorRejected $ "Failed test: " ++ Text.unpack (breakSubmissionName bs)


        -- Record result.
        case resultsE of
            Left (FixErrorSystem err) -> 
                systemFail err
            Left FixErrorTimeout ->
                return Nothing
            Left (FixErrorBuildFail stdout' stderr') -> do
                let stdout = decodeUtf8 stdout'
                let stderr = decodeUtf8 stderr'
                runDB $ updateFix FixRejected (Just "Running make failed") stdout stderr
                userFail "Build failed"
            Left (FixErrorRejected msg) -> do
                runDB $ updateFix FixRejected (Just msg) Nothing Nothing
                userFail msg
            Right () -> runDB $ do
                runUnlessNewBreak breaks' $ do
                    updateFix FixJudging Nothing Nothing Nothing
                    return $ Just (True, True)

        where
            -- Rerun if there's a new break.
            runUnlessNewBreak origBreaks' m = do
                let origBreaks = Set.fromList $ map entityKey origBreaks'

                latestBreaks' <- getValidActiveBreaksAgainst submission
                let latestBreaks = Set.fromList $ map entityKey latestBreaks'

                if origBreaks == latestBreaks then
                    m
                else
                    -- Mark as pending so that we try again.
                    systemFail "New break detected after testing a fix."
                


            teamId = fixSubmissionTeam submission
        --     hash = fixSubmissionCommitHash submission
        --     basePath = runnerRepositoryPath opts

            updateFix status msg stdout stderr = 
                update submissionId [FixSubmissionStatus =. status, FixSubmissionMessage =. msg, FixSubmissionStdout =. stdout, FixSubmissionStderr =. stderr]

            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False)

-- Could use a state transformer...
initialPort :: MonadIO m => m (IO.IORef Int)
initialPort = liftIO $ IO.newIORef 6300

getNextPort :: MonadIO m => IO.IORef Int -> m Int
getNextPort portRef = liftIO $ do
    port <- IO.readIORef portRef
    IO.writeIORef portRef (port + 5)
    return port

runBreakTest session passedTests portRef user breakDir breakTest = do
    port <- getNextPort portRef
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "test" .= jsonTest
          , "port" .= port
          , "type" .= ("break" :: String)
          , "classification" .= testType
          , "passed_tests" .= passedTests -- Passed optional tests. -- JP: New...
          , "user" .= user -- JP: New...
          , "break_directory" .= breakDir -- JP: New...
          ]

    -- Upload json input.
    uploadString session json destJson

    runTestAt session $ Text.pack destJson

    where
        testType = case breakTest of
          JSONBreakCorrectnessTest _ -> "correctness" :: Text
          JSONBreakIntegrityTest _ -> "integrity"
          JSONBreakConfidentialityTest _ -> "confidentiality"
          JSONBreakCrashTest _ -> "crash"
          JSONBreakSecurityTest _ -> "security"
          JSONBreakAvailabilityTest _ -> "availability"
            
        jsonTest = case breakTest of
          JSONBreakCorrectnessTest v -> v
          JSONBreakIntegrityTest v -> v
          JSONBreakConfidentialityTest v -> v
          JSONBreakCrashTest v -> v
          JSONBreakSecurityTest v -> v
          JSONBreakAvailabilityTest v -> v
            

runBuildTest session portRef (test, input) = do
    port <- getNextPort portRef
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "type" .= ("build" :: String)
          , "port" .= port
          , "input" .= input
          ]

    -- Upload json input.
    uploadString session json destJson

    res <- runTestAt session $ Text.pack destJson

    return ( test, res)

runTestAt :: (BackendError e, MonadIO m, FromJSON a) => Session -> Text -> ErrorT e m a
runTestAt session location = do
    -- Launch grader.
    (Result resOut' resErr _) <- executioner' session testUser "/problem/grader" [Text.unpack location]
    putLog "Output received."

    -- Drop newline. 
    let (resOut, _) = BS.breakSubstring "\n" resOut'
    putLog $ show resErr
    putLog $ show resOut

    -- Parse resOut.
    output <- ErrorT $ case Aeson.decodeStrict' resOut of
        Nothing ->
            return $ Left $ strMsg $ "Could not decodeUtf8 test output: " <> BS8.unpack resOut
        Just r ->
            return $ Right r

    return output


uploadFolder session localDir destDir' = do
    -- Check destDir.
    when ( FilePath.isRelative destDir || destDir == "/") $ 
        throwError $ strMsg $ "Destination directory is not absolute: " <> destDir

    -- TODO: Delete dest directory first XXX

    -- Make target directory.
    (Result _ _ exit) <- runSSH (strMsg ("Could not make directory `" <> destDir <> "`.")) $ 
        execCommand session ( "sudo -i mkdir -p " <> destDir)
    when (exit /= ExitSuccess) $
        fail ( "Could not make directory `" <> destDir <> "`.")

    -- Update permissions.
    (Result _ _ exit) <- runSSH (strMsg ( "Could not update directory `" <> destDir <> "` permissions.")) $ 
        execCommand session ( "sudo -i chmod -R 0777 " <> destDir)
    when (exit /= ExitSuccess) $
        fail ( "Could not update directory `" <> destDir <> "` permissions.")

    -- Get contents.
    contents' <- liftIO $ listDirectory localDir
    let contents = fmap (\f -> (localDir </> f, destDir </> f)) contents'

    mapM_ (\( local, dest) -> do
        -- Check if it's a file or directory.
        isFile <- liftIO $ Directory.doesFileExist local  
        if isFile then do
        
            -- Upload file.
            _ <- runSSH (strMsg $ "Could not send file: " ++ local) $ sendFile session 0o777 local dest
            return ()

        else

            -- Recursively upload directory.
            uploadFolder session local dest
      
      ) contents
    
    where
        destDir = FilePath.normalise destDir'

        listDirectory path = filter f <$> Directory.getDirectoryContents path
            where f filename = filename /= "." && filename /= ".."

decodeUtf8 :: BS8.ByteString -> Maybe Textarea
decodeUtf8 = Just . Textarea . Text.decodeUtf8With Text.lenientDecode

{-
retrieveTeamSubmission :: TeamContestId -> Text -> DatabaseM FilePath.FilePath
-- Return the path the submission is saved at, possibly downloading it the first time it's requested
retrieveTeamSubmission tcId hash = do
    let archiveLoc = 
    exists <- liftIO $ Files.fileExist archiveLoc
    unless exists $ do
        teamName <- runDB $ do
            Just tc <- get tcId
            Just (Team name _) <- get (teamContestTeam tc)
            return name
        liftIO $ downloadFromGitLab hash (urlFromName teamName) archiveLoc
    return archiveLoc
  where -- TODO fill these out
    root = undefined
    urlFromName = undefined

downloadFromGitLab :: Text -> Text -> FilePath.FilePath -> IO ()
-- Download the file at given commit hash and url and save to the specified location
downloadFromGitLab hash url dst = undefined
-}

uploadAndCompileBuilderSubmissionBuild = uploadAndCompileBuilderSubmission $ \stderr stdout -> throwError $ BuildFail stderr stdout
uploadAndCompileBuilderSubmissionBreak = uploadAndCompileBuilderSubmission $ \_ _ -> fail "Building target failed"
uploadAndCompileBuilderSubmissionFix = uploadAndCompileBuilderSubmission $ \stderr stdout -> throwError $ FixErrorBuildFail stderr stdout

uploadAndCompileBuilderSubmission buildFail session tarSubmission = do
    putLog "Sending builder submission."
    let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
    uploadString session tarSubmission destArchiveLocation
    -- _ <- runSSH (strMsg "Could not send submission") $ sendFile session 0o666 archiveLocation destArchiveLocation

    -- Setup directory.
    (Result _ _ exit) <- runSSH (strMsg "Could not make test directory.") $ execCommand session "sudo -i -u builder mkdir /home/builder/submission"
    when (exit /= ExitSuccess) $
        fail "Could not make test directory."

    -- Extract submission.
    putLog "Extracting builder submission."
    (Result _ _ exit) <- runSSH (strMsg "Could not extract submission") $ execCommand session ("cd /home/builder/submission; sudo -u builder tar -xf " <> destArchiveLocation <> " --strip-components 1")
    when (exit /= ExitSuccess) $ 
        fail "Could not extract submission"

    -- Build submission. 
    putLog "Building builder submission."
    (Result stderr stdout exit) <- runSSH (strMsg "Could not run make") $ execCommand session "sudo -i -u builder make -B -C /home/builder/submission/build"
    when (exit /= ExitSuccess) $
        buildFail stderr stdout

    -- return "/home/builder/submission/build/"

uploadAndCompileBreakerSubmissionBreak = uploadAndCompileBreakerSubmission $ \stderr stdout -> throwError $ BreakErrorBuildFail stderr stdout
uploadAndCompileBreakerSubmissionFix = uploadAndCompileBreakerSubmission $ \_ _ -> fail "Could not build break"

uploadAndCompileBreakerSubmission buildFail session portRef bs tarSubmission = do
    -- Create user.
    username <- (\p -> "break" <> show p) <$> IO.readIORef portRef
    putLog $ "Creating user " <> username
    createNewUser session username

    -- Upload break submission.
    putLog "Sending breaker submission."
    let destArchiveLocation = "/home/ubuntu/" <> username <> ".tar.gz"
    uploadString session tarSubmission destArchiveLocation

    -- Setup directory.
    (Result _ _ exit) <- runSSH (strMsg "Could not make break directory.") $ execCommand session $ "sudo -i -u " <> username <> " mkdir /home/" <> username <> "/submission"
    when (exit /= ExitSuccess) $
        fail "Could not make break directory."

    -- Extract submission.
    putLog "Extracting breaker submission."
    (Result _ _ exit) <- runSSH (strMsg "Could not extract submission") $ execCommand session ("cd /home/" <> username <> "/submission; sudo -u " <> username <> " tar -xf " <> destArchiveLocation <> " --strip-components 1")
    when (exit /= ExitSuccess) $ 
        fail "Could not extract submission"

    let remoteBreakDir = "/home/" <> username <> "/submission/break/" <> Text.unpack (breakSubmissionName bs) <> "/"
    makefileExists <- remoteFileExists session username $ remoteBreakDir <> "Makefile"

    when makefileExists $ do
        putLog "Building break submission."
        (Result stderr stdout exit) <- runSSH (strMsg "Could not run make") $ execCommand session $ "sudo -i -u " <> username <> " make -B -C " <> remoteBreakDir -- /home/builder/submission/build"
        when (exit /= ExitSuccess) $
            buildFail stderr stdout

    return (username, remoteBreakDir)

remoteFileExists session username f = do
    (Result _ _ exit) <- runSSH (strMsg "Could not determine if file exists.") $ execCommand session $ "sudo -i -u " <> username <> " test -f " <> f
    return $ exit == ExitSuccess

extractAndCompressBuildSubmission bs opts = do
    archiveLocation <- getArchiveLocation tcId hash opts

    extractAndFilterSubmission archiveLocation filter
  
  where
    tcId = buildSubmissionTeam bs
    hash = buildSubmissionCommitHash bs
    filter fp = case FilePath.splitDirectories fp of
      _:"build":_ -> True
      _           -> False

extractAndCompressFixSubmission bs opts = do
    archiveLocation <- getArchiveLocation tcId hash opts

    extractAndFilterSubmission archiveLocation filter
  
  where
    tcId = fixSubmissionTeam bs
    hash = fixSubmissionCommitHash bs
    filter fp = case FilePath.splitDirectories fp of
      _:"build":_ -> True
      _           -> False


-- Extract and then compress break files from a break submission.
extractAndCompressBreakSubmission bs opts = do
    archiveLocation <- getArchiveLocation tcId hash opts

    extractAndFilterSubmission archiveLocation filter

  where
    tcId = breakSubmissionTeam bs
    hash = breakSubmissionCommitHash bs
    name = Text.unpack $ breakSubmissionName bs
    filter fp = case FilePath.splitDirectories fp of
      _:"break":name':_ -> name == name'
      _                 -> False

extractAndFilterSubmission filename filterF = catchAny (do
    -- Load break submission tar.gz.
    bs <- liftIO $ BSL.readFile filename

    -- Decompress tar.
    let entries' = Tar.checkSecurity $ Tar.read $ GZip.decompress bs

    -- Filter out entries that aren't related to this break.
    entries <- filterEntries entries'

    -- Recompress tar.gz.
    return $ BSL.toStrict $ GZip.compress $ Tar.write entries
  ) $ \e -> 
    fail $ "Error recompressing break submission" <> show e

  where
    filterEntries Done = return []
    filterEntries (Next e es) 
      | filterF (Tar.entryPath e) = filterEntries es >>= return . (e:)
      | otherwise             = filterEntries es
    filterEntries (Fail e) = fail $ "Error decompressing break submission" <> show e

retrievePassedOptionalTests targetId = do
    -- Get latest submission.
    submissionM <- lift $ lift $ runDB $ selectFirst [BuildSubmissionTeam ==. targetId] [Desc BuildSubmissionTimestamp]
    case submissionM of
        Nothing ->
            throwError $ strMsg $ "Could not find build submission for team: " ++ show targetId
        Just (Entity submissionId _) -> lift $ lift $ do

            -- Get passed optional tests.
            optionalTests <- runDB $ E.select $ E.from $ \(t `E.InnerJoin` r) -> do
                E.on (r E.^. BuildOptionalResultTest E.==. t E.^. ContestOptionalTestId)
                E.where_ (
                        (r E.^. BuildOptionalResultPass E.==. E.val True)
                  E.&&. (r E.^. BuildOptionalResultSubmission E.==. E.val submissionId)
                  )
                return (t E.^. ContestOptionalTestName)
                
            -- Get passed optional performance tests.
            optionalPerformanceTests <- runDB $ E.select $ E.from $ \(t `E.InnerJoin` r) -> do
                E.on (r E.^. BuildPerformanceResultTest E.==. t E.^. ContestPerformanceTestId)
                E.where_ (
                        (E.not_ (E.isNothing (r E.^. BuildPerformanceResultTime)))
                  E.&&. (r E.^. BuildPerformanceResultSubmission E.==. E.val submissionId)
                  E.&&. (t E.^. ContestPerformanceTestOptional E.==. E.val True)
                  )
                return (t E.^. ContestPerformanceTestName)

            return $ map E.unValue $ optionalTests <> optionalPerformanceTests

-- Get the valid, active breaks against the fixing team before the fix timestamp.
getValidActiveBreaksAgainst fs = do
    -- Get valid breaks.
    breaks <- getValidBreaks teamId time

    -- Filter out inactive breaks.
    filterM (isActiveBreak . entityKey) breaks

    -- Get break, latest break fix submission against teamId.
    --
    --
    --
    -- Get latest breaks.

    
    -- Filter out valid == False || break/fix = fail || fixed result = fixed

  where
    teamId = fixSubmissionTeam fs
    time = fixSubmissionTimestamp fs

    isActiveBreak bsId = do
        n <- E.select $ E.from $ \(E.InnerJoin bfs fs) -> do
            E.on (bfs E.^. BreakFixSubmissionFix E.==. E.just (fs E.^. FixSubmissionId))
            E.where_ (
                    bfs E.^. BreakFixSubmissionBreak E.==. E.val bsId
              E.&&. fs E.^. FixSubmissionResult E.==. E.just (E.val FixFixed)
              E.&&. bfs E.^. BreakFixSubmissionResult E.==. E.val BreakFailed
              )
            return $ fs E.^. FixSubmissionId

        -- Fixed if there exists an accepted fix and the break failed.
        return $ length n > 0

    getValidBreaks teamId time = selectList [
        BreakSubmissionValid ==. Just True
      , BreakSubmissionTargetTeam ==. Just teamId
      , BreakSubmissionTimestamp ==. time
      ] [Asc BreakSubmissionTimestamp]

-- getLatestBreakFixSubmissions :: (MonadIO m, SqlSelect a b) => TeamContestId -> E.SqlPersistT m [(Entity BreakSubmission, Entity BreakFixSubmission)]
-- getLatestBreakFixSubmissions tcId = do
--     E.select $ E.from $ \(E.InnerJoin bs (E.LeftOuterJoin bfs bfs')) -> do
--         -- Now assumes that BreakFixSubmissionId is monotonically increasing. 
--         E.on ( E.just ( bfs E.^. BreakFixSubmissionId) E.<. bfs' E.?. BreakFixSubmissionId E.&&. E.just ( bfs E.^. BreakFixSubmissionFix) E.==. bfs' E.?. BreakFixSubmissionFix)
--         E.on ( bs E.^. BreakSubmissionId E.==. bfs E.^. BreakFixSubmissionBreak)
-- 
--         E.where_ (
--                 E.isNothing (bfs' E.?. BreakFixSubmissionId)
--           -- E.&&. 
--           )
--         return 
        
    

-- getLatestBuildSubmissions :: (MonadIO m, SqlSelect a b) => ContestId -> (E.SqlExpr (Entity TeamContest) -> E.SqlExpr (Entity BuildSubmission) -> E.SqlQuery a) -> E.SqlPersistT m [b]
-- getLatestBuildSubmissions cId f = do
--     E.select $ E.from $ \(E.InnerJoin tc (E.LeftOuterJoin bs bs')) -> do
--         -- E.on ( E.just ( bs E.^. BuildSubmissionTimestamp) E.<. bs' E.?. BuildSubmissionTimestamp E.&&. E.just ( bs E.^. BuildSubmissionTeam) E.==. bs' E.?. BuildSubmissionTeam)
--         -- Now assumes that BuildSubmissionId is monotonically increasing. 
--         E.on ( E.just ( bs E.^. BuildSubmissionId) E.<. bs' E.?. BuildSubmissionId E.&&. E.just ( bs E.^. BuildSubmissionTeam) E.==. bs' E.?. BuildSubmissionTeam)
--         E.on ( tc E.^. TeamContestId E.==. bs E.^. BuildSubmissionTeam)
--         E.where_ ( tc E.^. TeamContestContest E.==. E.val cId E.&&. E.isNothing (bs' E.?. BuildSubmissionTeam) E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
--         f tc bs

