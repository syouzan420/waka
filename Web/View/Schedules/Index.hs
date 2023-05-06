module Web.View.Schedules.Index where
import Web.View.Prelude

data IndexView = IndexView { schedules :: [Schedule], ymd :: Text  }

instance View IndexView where
  html IndexView { .. } = [hsx|
    {breadcrumb}
    <h1>よてい</h1>
    {generateCalenderHtml newSchedulePath (yearNow ymd) (monthNow ymd)}
    <div class="table-responsive">
      <table class="table">
        <thead>
          <tr>
            <th>Schedule</th>
            <th></th>
            <th></th>
          </tr>
        </thead>
        <tbody>{forEach schedules renderSchedule}</tbody>
      </table>
    </div>
  |]
    where
      newSchedulePath dy = pathTo (NewScheduleAction ("202305"++digitShow dy))
      breadcrumb = renderBreadcrumb
          [ breadcrumbLink "Schedules" SchedulesAction
          ]

digitShow :: Text -> Text
digitShow dy = if dy `elem` ["1","2","3","4","5","6","7","8","9"] then "0"<>dy else dy

yearNow :: Text -> Int
yearNow ymd = let (y,_,_) = splitYearMonthDay ymd in y

monthNow :: Text -> Int
monthNow ymd = let (_,m,_) = splitYearMonthDay ymd in m

dayNow :: Text -> Int
dayNow ymd = let (_,_,d) = splitYearMonthDay ymd in d

renderSchedule :: Schedule -> Html
renderSchedule schedule = [hsx|
  <tr>
    <td><a href={ShowScheduleAction schedule.id}>{schedule.filledDate}</a></td>
    <td><a href={EditScheduleAction schedule.id} class="text-muted">Edit</a></td>
    <td><a href={DeleteScheduleAction schedule.id} class="js-delete text-muted">Delete</a></td>
  </tr>
  |]

