module Web.View.Posts.New where
import Web.View.Prelude

data NewView = NewView { post :: Post }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>新規</h1>
        {renderForm post}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                , breadcrumbText "New Post"
                ]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {(textField #title)}
    {(checkboxField #tate){fieldLabel = "縦書き"}}
    {(textareaField #body){helpText = "You can use Markdown here"
                          ,additionalAttributes = [("rows","10")]}}
    {submitButton}

|]
