module Web.View.Schtypes.Show where
import Web.View.Prelude

data ShowView = ShowView { schtype :: Schtype }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Schtype</h1>
        <p>{schtype}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Schtypes" SchtypesAction
                            , breadcrumbText "Show Schtype"
                            ]