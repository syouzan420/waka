module Web.View.Schedules.Edit where
import Web.View.Prelude

data EditView = EditView { schedule :: Schedule }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Schedule</h1>
        {renderForm schedule}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schedules" SchedulesAction
                , breadcrumbText "Edit Schedule"
                ]

renderForm :: Schedule -> Html
renderForm schedule = formFor schedule [hsx|
    {(textField #userId) {fieldValue = show currentUser.id}}
    {(textField #filledDate)}
    {(textField #filledTime)}
    {(textField #scheduleType)}
    {(textField #description)}
    {(checkboxField #booked)}
    {submitButton}

|]
