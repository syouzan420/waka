module Web.View.Durations.Index where
import Web.View.Prelude

data IndexView = IndexView { durations :: [Duration]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>時間帯の設定<a href={pathTo NewDurationAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>時間帯</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach durations renderDuration}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "よてい" SchedulesAction
                ]

renderDuration :: Duration -> Html
renderDuration duration = [hsx|
    <tr>
        <td><a>{duration.timeDuration}</a></td>
        <td><a href={EditDurationAction duration.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteDurationAction duration.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
