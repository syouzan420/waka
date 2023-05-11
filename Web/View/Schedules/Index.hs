module Web.View.Schedules.Index where
import Web.View.Prelude

data IndexView = IndexView { schedules :: [Schedule], ymd :: Text }

instance View IndexView where
  html IndexView { .. } = [hsx|
    {breadcrumb}
    <h1>よてい</h1>
    <a href={OtherSchedulesAction (subMonth ymd)}>←←</a>
    {showYearMonth ymd}
    <a href={OtherSchedulesAction (addMonth ymd)}>→→</a>
    {makeCalender ymd schedules}
    <div class="table-responsive">
      <table class="table">
        <thead>
          <tr>
            <th>Schedule</th>
            <th>時間</th>
            <th>種類</th>
            <th>予約</th>
            <th></th>
            <th></th>
          </tr>
        </thead>
        <tbody>{forEach schedules renderSchedule}</tbody>
      </table>
    </div>
  |]
    where
      breadcrumb = renderBreadcrumb
          [ breadcrumbLink "Schedules" SchedulesAction
          ]

getBookableDates :: [Schedule] -> Text -> [Text]
getBookableDates schs uid = nub$map filledDate
            (filter (\d -> (show d.userId ==uid || d.booked==False)) schs)

getFilledDates :: [Schedule] -> [Text]
getFilledDates schs = nub$map filledDate schs

makeCalender :: Text -> [Schedule] -> Html
makeCalender ymd schedules = 
  generateCalenderHtml (newSchedulePath ymd) (yearNow ymd) (monthNow ymd) 
    where
      newSchedulePath ymd dy = 
          case currentUserOrNothing of
              Just currentUser -> do
                let cid = show$currentUser.id
                    targetDate = yearMonth ymd ++ digitShow dy
                if cid == userTeru || cid == userTeruOverThere
                  then if targetDate `elem` getFilledDates schedules
                    then " style=\"color:green\"; href="<>pathTo (NewScheduleAction targetDate)
                    else " style=\"color:blue\"; href="<>pathTo (NewScheduleAction targetDate)
                  else if targetDate `elem` getBookableDates schedules cid
                    then " href="<>pathTo (NewBookingAction targetDate)
                    else ""
              Nothing -> ""

digitShow :: Text -> Text
digitShow dy = if dy `elem` ["1","2","3","4","5","6","7","8","9"] then "0"<>dy else dy

yearNow :: Text -> Int
yearNow ymd = let (y,_,_) = splitYearMonthDay ymd in y

monthNow :: Text -> Int
monthNow ymd = let (_,m,_) = splitYearMonthDay ymd in m

dayNow :: Text -> Int
dayNow ymd = let (_,_,d) = splitYearMonthDay ymd in d

addMonth :: Text -> Text
addMonth ymd = let (y,m,d) = splitYearMonthDay ymd
                   nm = if m==12 then 1 else m+1
                   ny = if m==12 then y+1 else y
                in show ny ++ digitShow (show nm) ++ digitShow (show d)

subMonth :: Text -> Text
subMonth ymd = let (y,m,d) = splitYearMonthDay ymd
                   nm = if m==1 then 12 else m-1
                   ny = if m==1 then y-1 else y
                in show ny ++ digitShow (show nm) ++ digitShow (show d)

yearMonth :: Text -> Text
yearMonth ymd = let (y,m,_) = splitYearMonthDay ymd
                 in show y ++ digitShow (show m)

showYearMonth :: Text -> Html 
showYearMonth ymd = let (y,m,_) = splitYearMonthDay ymd
                     in [hsx| <a> {y} 年 {m} 月 </a> |]

renderSchedule :: Schedule -> Html
renderSchedule schedule = [hsx|
  <tr>
    <td><a href={ShowScheduleAction schedule.id}>{schedule.filledDate}</a></td>
    <td><a>{schedule.filledTime}</a></td>
    <td><a>{schedule.scheduleType}</a></td>
    <td><a>{schedule.booked}</a></td>
    <td><a href={EditScheduleAction schedule.id} class="text-muted">Edit</a></td>
    <td><a href={DeleteScheduleAction schedule.id} class="js-delete text-muted">Delete</a></td>
  </tr>
  |]

