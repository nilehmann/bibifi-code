module Contest.Edit where

import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Char (isAlphaNum)
import qualified Data.Text as Text

import Import
import Forms (utcField)

data FormData = FormData {
    formContestName :: Text
  , formContestUrl :: Text
  , formBuildStart :: UTCTime
  , formBuildEnd :: UTCTime
  , formBreakFixStart :: UTCTime
  , formBreakEnd :: UTCTime
  , formFixEnd :: UTCTime
  -- , formMakeDefault :: Bool
  -- TODO: Coursera corse and session? XXX
}

-- contestForm :: TimeZone -> Form FormData
contestForm tz contestM = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField nameSettings ( fmap contestTitle contestM)
    <*> areq urlField urlSettings (fmap contestUrl contestM)
    <*> areq (utcField tz) buildStartSettings (fmap contestBuildStart contestM)
    <*> areq (utcField tz) buildEndSettings (fmap contestBuildEnd contestM)
    <*> areq (utcField tz) breakStartSettings (fmap contestBreakFixStart contestM)
    <*> areq (utcField tz) breakEndSettings (fmap contestBreakEnd contestM)
    <*> areq (utcField tz) fixEndSettings (fmap contestFixEnd contestM)
    -- <* bootstrapSubmit (BootstrapSubmit ("Create Contest"::Text) "btn-primary" [])

    where
        nameSettings = withPlaceholder "Contest Name" $ bfs ("Contest Name" :: Text)
        urlSettings = withPlaceholder "Unique Contest Identifier" $ bfs ("Contest Identifier" :: Text) -- TODO: Verify placeholder is unique. XXX
        buildStartSettings = withPlaceholder "Build-it Start Date" $ bfs ("Build-it Start Date" :: Text)
        buildEndSettings = withPlaceholder "Build-it End Date" $ bfs ("Build-it End Date" :: Text)
        breakStartSettings = withPlaceholder "Break-it Start Date" $ bfs ("Break-it Start Date" :: Text)
        breakEndSettings = withPlaceholder "Break-it End Date" $ bfs ("Break-it End Date" :: Text)
        fixStartSettings = withPlaceholder "Fix-it Start Date" $ bfs ("Fix-it Start Date" :: Text)
        fixEndSettings = withPlaceholder "Fix-it End Date" $ bfs ("Fix-it End Date" :: Text)

        -- urlField = checkM (\url -> do
        --     contestM <- runDB $ getBy $ UniqueContest url  
        --     return $ maybe (Left ("This URL is already taken" :: Text)) (const $ Right url) contestM
        --   ) textField
        urlField = check (\u -> if Text.all (\c -> isAlphaNum c || c == '_') u then Right u else Left ("Identifiers can only be made of underscores and alphanumerics." :: Text)) textField


convertContest FormData{..} = Contest
    formContestUrl
    formContestName
    formBuildStart
    formBuildEnd
    formBreakFixStart
    formBreakEnd
    formFixEnd

validateContest FormData{..} contestIdM = runExceptT $ do
    -- Check that url is unique. 
    do
        contestM <- lift $ handlerToWidget $ runDB $ getBy $ UniqueContest formContestUrl
        case contestM of
            Just (Entity contestId _) | Just contestId /= contestIdM -> 
                throwE "This URL is already taken."
            _ ->
                return ()

    -- Check that all dates are sequential.
    when (formBuildStart >= formBuildEnd) $
        throwE "Build-it end date must be after the start date."

    when (formBuildEnd >= formBreakFixStart) $
        throwE "Break-it start date must be after the build-it end date."

    when (formBreakFixStart >= formBreakEnd) $
        throwE "Break-it end date must be after the start date."

    return ()

{-@ LIQUID "--compile-spec" @-}
