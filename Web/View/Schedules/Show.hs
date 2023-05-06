module Web.View.Schedules.Show where
import Web.View.Prelude

data ShowView = ShowView { schedule :: Schedule }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{schedule.filledDate}</h1>
        <p>{schedule}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Schedules" SchedulesAction
                            , breadcrumbText "Show Schedule"
                            ]
