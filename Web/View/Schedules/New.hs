module Web.View.Schedules.New where

import Web.View.Prelude

data NewView = NewView { schedule :: Schedule , ymd :: Text }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Schedule</h1>
        {renderForm schedule ymd}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schedules" SchedulesAction
                , breadcrumbText "New Schedule"
                ]

renderForm :: Schedule -> Text -> Html
renderForm schedule ymd = formFor schedule [hsx|
    {(textField #filledDate) {fieldValue=ymd}}
    {(textField #filledTime)}
    {(textField #scheduleType)}
    {(textField #description)}
    {submitButton}

|]
