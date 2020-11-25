module Handler.Admin.Contest.Tests.CreatePerformance where

import qualified Admin
import Import
import Test

generateHtml :: Text -> Widget -> Enctype -> LWidget
generateHtml url form enctype = do
    [whamlet|
        <a href="@{AdminContestTestsR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Create performance test
        <form method=post action="@{AdminContestTestsCreatePerformanceR url}" enctype=#{enctype} roles="form">
            ^{form}
            <div .form-group .optional>
                <button .btn .btn-primary type="submit">
                    Create test
    |]

getAdminContestTestsCreatePerformanceR :: Text -> Handler Html
getAdminContestTestsCreatePerformanceR url = runLHandler $ Admin.layoutContest url $ \(Entity _ _) -> do
    Admin.setTitle "Create test"
    (form, enctype) <- handlerToWidget $ generateFormPost $ performanceTestForm Nothing
    generateHtml url form enctype

postAdminContestTestsCreatePerformanceR :: Text -> Handler Html
postAdminContestTestsCreatePerformanceR url = runLHandler $ Admin.layoutContest url $ \(Entity cId _) -> do
    Admin.setTitle "Create test"
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ performanceTestForm Nothing
    case res of
        FormMissing ->
            errorHandler form enctype
        FormFailure _ ->
            errorHandler form enctype
        FormSuccess PerformanceFormData{..} -> do
            -- Insert test into database.
            handlerToWidget $ runDB $ insert_ $ ContestPerformanceTest cId performanceFormDataName "" "" (unTextarea performanceFormDataTest) (not performanceFormDataRequired)

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Created test!
            |]
            
            -- Redirect.
            redirect $ AdminContestTestsCreatePerformanceR url

    where
        errorHandler form enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not create test.
            |]
            
            generateHtml url form enctype


{-@ LIQUID "--compile-spec" @-}
