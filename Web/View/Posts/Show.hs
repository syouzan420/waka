module Web.View.Posts.Show where
import Web.View.Prelude
import qualified Text.MMark as MMark

data ShowView = ShowView { post :: Include "comments" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div style="background-color: #657b83; padding: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
        <div style="writing-mode: vertical-rl; overflow-x:scroll; max-width:100%;  max-height: 480px; margin-left: auto">
        <h1>{post.title}</h1>
        <p >{post.createdAt |> dateTime}</p>
        <div style="font-size: 1.2rem; font-weight: 300">{post.body |> renderMarkdown}</div>
        </div>
        </div>

        <a href={NewCommentAction post.id}>Add Comment</a>

        <div>{forEach post.comments renderComment}</div>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]
renderMarkdown text = 
  case text |> MMark.parse "" of
    Left error -> "Something went wrong"
    Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml

renderComment comment = [hsx|
      <div class="mt-4">
        <h5>{comment.author}</h5>
        <p>{comment.body}</p>
      </div>
    |]

