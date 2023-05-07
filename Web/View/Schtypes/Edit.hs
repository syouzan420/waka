module Web.View.Schtypes.Edit where
import Web.View.Prelude

data EditView = EditView { schtype :: Schtype }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Schtype</h1>
        {renderForm schtype}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schtypes" SchtypesAction
                , breadcrumbText "Edit Schtype"
                ]

renderForm :: Schtype -> Html
renderForm schtype = formFor schtype [hsx|
    {(textField #scheduleType)}
    {submitButton}

|]