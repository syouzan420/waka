module Web.View.Posts.Show where
import Web.View.Prelude
import Data.Text (unpack) 
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.MathJax as MMath

data ShowView = ShowView { post :: Include "comments" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        {scriptMath}
        <div id={currentViewId}>
        <div style="background-color: #657b83; padding: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
        {renderTate post}
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

renderMain post = [hsx|
        <h1>{post.title}</h1>
        <p >{post.createdAt |> dateTime}</p>
        <div style="font-size: 1.2rem; font-weight: 300">{post.body |> renderMarkdown}</div>
   |]

--renderMarkdown text = 
--  case text |> MMark.parse "" of
--    Left error -> preEscapedToHtml text 
--    Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml

renderMarkdown :: Text -> Html
renderMarkdown text =
  let text2 = map oneLineMarkdown (lines text)
   in unlines text2 |> preEscapedToHtml 

oneLineMarkdown :: Text -> Text
oneLineMarkdown text =
  case text |> MMark.parse "" of
    Left error -> text 
    Right markdown -> MMark.render markdown |> tshow

renderComment comment = [hsx|
      <div class="mt-4">
        <h5>{comment.author}</h5>
        <p>{comment.body}</p>
      </div>
    |]

-- this is the test for MathJax Markdown
renderMarkdown2 :: Text -> Html
renderMarkdown2 text =
  let text2 = map useExtendMarkdown (lines text)
   in unlines text2 |> preEscapedToHtml 

useExtendMarkdown :: Text -> Text 
useExtendMarkdown text = 
  case text |> MMark.parse "" of
    Left error -> "something goes wrong" 
    Right markdown -> MMark.useExtension (MMath.mathJax (Just '\\')) markdown
                        |> MMark.render |> tshow
-----------

renderTate :: (Include "comments" Post) -> Html
renderTate post = if post.tate then [hsx|
        <div style="writing-mode: vertical-rl; overflow-x:scroll; max-width:100%;  max-height: 480px; margin-left: auto">
        {renderMain post}
        </div>
   |]
                               else [hsx|
        <div style="max-width:100%; ">
        {renderMain post}
        </div>
   |]

scriptMath :: Html
scriptMath = [hsx|
  <script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML">
  </script>
  <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      tex2jax: {
        inlineMath: [['$', '$'] ],
        displayMath: [ ['$$','$$'], ["\\[","\\]"] ]
      }
    });
  </script>
  |]

