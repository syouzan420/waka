module Web.View.Durations.New where
import Web.View.Prelude

data NewView = NewView { duration :: Duration }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Duration</h1>
        {renderForm duration}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Durations" DurationsAction
                , breadcrumbText "New Duration"
                ]

renderForm :: Duration -> Html
renderForm duration = formFor duration [hsx|
    {(textField #timeDuration)}
    {submitButton}

|]