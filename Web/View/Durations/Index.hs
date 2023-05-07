module Web.View.Durations.Index where
import Web.View.Prelude

data IndexView = IndexView { durations :: [Duration]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewDurationAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Duration</th>
                        <th></th>
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
                [ breadcrumbLink "Durations" DurationsAction
                ]

renderDuration :: Duration -> Html
renderDuration duration = [hsx|
    <tr>
        <td>{duration}</td>
        <td><a href={ShowDurationAction duration.id}>Show</a></td>
        <td><a href={EditDurationAction duration.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteDurationAction duration.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]