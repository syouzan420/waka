module Web.Controller.Schedules where

import Web.Controller.Prelude
import Web.View.Schedules.Index
import Web.View.Schedules.New
import Web.View.Schedules.Edit
import Web.View.Schedules.Show
import Web.View.Schedules.Book
import IHP.SEO.Sitemap.ControllerFunctions
import Data.Text.Encoding
import qualified Data.Text as T

instance Controller SchedulesController where
  action SchedulesAction = do
      schedules <- query @Schedule
          |> orderByDesc #createdAt
          |> fetch
      currentTime <- getCurrentTime
      let ymd = T.concat (T.splitOn "-" (formatUTCTime currentTime))
      let tdy = ymd
      render IndexView { .. }

  action OtherSchedulesAction { ymd, tdy } = do
      schedules <- query @Schedule
          |> orderByDesc #createdAt
          |> fetch
      render IndexView { .. }
  
  action NewScheduleAction { ymd } = do
      ensureIsUser
      let schedule = newRecord
            |> set #filledDate ymd
            |> set #userId currentUser.id
      schtypes <- query @Schtype |> fetch
      durations <- query @Duration |> fetch
      render NewView { .. }

  action NewBookingAction { ymd } = do
      ensureIsUser
      schedules <- query @Schedule
          |> filterWhere (#filledDate, ymd)
          |> queryOr
              (filterWhere (#booked, False))
              (filterWhere (#userId, currentUserId))
          |> fetch
      render BookView { .. }

  action ShowScheduleAction { scheduleId } = do
      schedule <- fetch scheduleId
      render ShowView { .. }

  action EditScheduleAction { scheduleId } = do
      ensureIsTeru
      schedule <- fetch scheduleId
      render EditView { .. }

  action EditUserBookAction { scheduleId } = do
      ensureIsUser
      schedule <- fetch scheduleId
      render EditView { .. }

  action UpdateScheduleAction { scheduleId } = do
      schedule <- fetch scheduleId
      schedule
          |> buildSchedule
          |> ifValid \case
              Left schedule -> render EditView { .. }
              Right schedule -> do
                  schedule <- schedule |> updateRecord
                  setSuccessMessage "Schedule updated"
                  redirectTo EditScheduleAction { .. }

  action CreateScheduleAction = do
    let schedule = newRecord @Schedule
            |> set #userId currentUser.id
        ymd = filledDate schedule
    schtypes <- query @Schtype |> fetch
    durations <- query @Duration |> fetch
    schedule
        |> buildSchedule
        |> ifValid \case
            Left schedule -> render NewView { .. }
            Right schedule -> do
                schedule <- schedule |> createRecord
                setSuccessMessage "Schedule created"
                redirectTo SchedulesAction

  action DeleteScheduleAction { scheduleId } = do
      ensureIsTeru
      schedule <- fetch scheduleId
      deleteRecord schedule
      setSuccessMessage "Schedule deleted"
      redirectTo SchedulesAction

buildSchedule schedule = schedule
    |> fill @["userId","filledDate","filledTime", "scheduleType", "description", "booked"]
    |> validateField #filledDate nonEmpty 
    |> validateField #filledTime nonEmpty 
    |> validateField #scheduleType nonEmpty
  
