module Web.View.Schtypes.New where
import Web.View.Prelude

data NewView = NewView { schtype :: Schtype }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Schtype</h1>
        {renderForm schtype}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schtypes" SchtypesAction
                , breadcrumbText "New Schtype"
                ]

renderForm :: Schtype -> Html
renderForm schtype = formFor schtype [hsx|
    {(textField #scheduleType)}
    {submitButton}

|]