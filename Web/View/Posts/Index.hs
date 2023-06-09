module Web.View.Posts.Index where
import Web.View.Prelude

data IndexView = IndexView { posts :: [Post]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}
        <h1>ことのはくばり<a href={pathTo NewPostAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                        <th>created</th>
                        <th>updated</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

renderPost :: Post -> Html
renderPost post = [hsx|
    <tr>
        <td><a href={ShowPostAction post.id}>{post.title}</a></td>
        <td><a>{post.createdAt |> dateTime}</a></td>
        <td><a>{post.updatedAt |> dateTime}</a></td>
        <td><a href={EditPostAction post.id} class="text-muted">Edit</a></td>
        <td><a href={DeletePostAction post.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]

