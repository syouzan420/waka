module Web.View.Schedules.New where

import Web.View.Prelude

data NewView = NewView { schedule :: Schedule , ymd :: Text 
                       , schtypes :: [Schtype], durations :: [Duration]}

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>よていをきめる</h1>
        <h3> 日付 : {ymd} </h3>
        {renderForm schedule ymd schtypes durations}
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

instance CanSelect Duration where
  type SelectValue Duration = Text 
  selectValue duration = duration.timeDuration
  selectLabel duration = duration.timeDuration

renderForm :: Schedule -> Text -> [Schtype] -> [Duration] -> Html
renderForm schedule ymd schtypes durations = formFor schedule [hsx|
    {(textField #filledDate) {fieldValue = ymd}}
    {(selectField #filledTime durations)}
    {(selectField #scheduleType schtypes)}
    {(textField #description)}
    {submitButton}
|]
