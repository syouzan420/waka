module Web.View.Schtypes.Index where
import Web.View.Prelude

data IndexView = IndexView { schtypes :: [Schtype]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewSchtypeAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Schtype</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach schtypes renderSchtype}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schtypes" SchtypesAction
                ]

renderSchtype :: Schtype -> Html
renderSchtype schtype = [hsx|
    <tr>
        <td>{schtype}</td>
        <td><a href={ShowSchtypeAction schtype.id}>Show</a></td>
        <td><a href={EditSchtypeAction schtype.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteSchtypeAction schtype.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]