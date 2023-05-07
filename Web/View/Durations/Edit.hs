module Web.View.Durations.Edit where
import Web.View.Prelude

data EditView = EditView { duration :: Duration }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Duration</h1>
        {renderForm duration}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Durations" DurationsAction
                , breadcrumbText "Edit Duration"
                ]

renderForm :: Duration -> Html
renderForm duration = formFor duration [hsx|
    {(textField #timeDuration)}
    {submitButton}

|]