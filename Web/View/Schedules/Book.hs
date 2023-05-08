module Web.View.Schedules.Book where

import Web.View.Prelude

data BookView = BookView { schedules :: [Schedule] }

instance View BookView where
    html BookView { .. } = [hsx|
        {breadcrumb}
        <h1>予約する</h1>
        <h3> 日付 : {head$nub$map (\s -> s.filledDate) schedules} </h3>
    
        <div class="table-responsive">
        <table class="table">
          <thead>
            <tr>
              <th>時間</th>
              <th>種類</th>
            </tr>
          </thead>
          <tbody>{forEach schedules renderSchedule}</tbody>
        </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Schedules" SchedulesAction
                , breadcrumbText "Book Schedule"
                ]

renderSchedule :: Schedule -> Html
renderSchedule schedule = [hsx|
  <tr>
    <td><a href={EditUserBookAction schedule.id}>{schedule.filledTime}</a></td>
    <td><a>{schedule.scheduleType}</a></td>
  </tr>
  |]
