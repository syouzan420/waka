module Web.View.Durations.Show where
import Web.View.Prelude

data ShowView = ShowView { duration :: Duration }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Duration</h1>
        <p>{duration}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Durations" DurationsAction
                            , breadcrumbText "Show Duration"
                            ]