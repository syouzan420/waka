module Web.View.Posts.Edit where
import Web.View.Prelude

data EditView = EditView { post :: Post }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>編集</h1>
        {renderForm post}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                , breadcrumbText "Edit Post"
                ]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {(textField #title)}
    {(checkboxField #tate){fieldLabel = "縦書き"}}
    {(textareaField #body){helpText = "You can use Markdown here"}}
    {submitButton}

|]
