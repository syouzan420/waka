module Web.View.Schedules.New where

import Web.View.Prelude

data NewView = NewView { schedule :: Schedule , ymd :: Text , schtypes :: [Schtype]}

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>よていをきめる</h1>
        <h3> 日付 : {ymd} </h3>
        {renderForm schedule ymd schtypes}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schedules" SchedulesAction
                , breadcrumbText "New Schedule"
                ]

instance CanSelect Schtype where
  type SelectValue Schtype = Text 
  selectValue schtype = schtype.scheduleType
  selectLabel schtype = schtype.scheduleType

renderForm :: Schedule -> Text -> [Schtype] -> Html
renderForm schedule ymd schtypes = formFor schedule [hsx|
    {(textField #filledTime)}
    {(selectField #scheduleType schtypes)}
    {(textField #description)}
    {submitButton}

|]
