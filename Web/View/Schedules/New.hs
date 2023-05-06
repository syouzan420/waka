module Web.View.Schedules.New where
import Web.View.Prelude

data NewView = NewView { schedule :: Schedule }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Schedule</h1>
        {renderForm schedule}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schedules" SchedulesAction
                , breadcrumbText "New Schedule"
                ]

renderForm :: Schedule -> Html
renderForm schedule = formFor schedule [hsx|
    {(textField #userId)}
    {(textField #filledDate)}
    {(textField #filledTime)}
    {(textField #scheduleType)}
    {(textField #description)}
    {submitButton}

|]