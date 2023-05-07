module Web.Controller.Schedules where

import Web.Controller.Prelude
import Web.View.Schedules.Index
import Web.View.Schedules.New
import Web.View.Schedules.Edit
import Web.View.Schedules.Show
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
      setSuccessMessage ymd 
      render IndexView { .. }

  action OtherSchedulesAction { ymd } = do
      schedules <- query @Schedule
          |> orderByDesc #createdAt
          |> fetch
      setSuccessMessage ymd
      render IndexView { .. }
  
  action NewScheduleAction { ymd } = do
      ensureIsUser
      let schedule = newRecord
            |> set #filledDate ymd
            |> set #userId currentUser.id
            |> set #booked False
      schtypes <- query @Schtype |> fetch
      durations <- query @Duration |> fetch
      setSuccessMessage ymd 
      render NewView { .. }

  action ShowScheduleAction { scheduleId } = do
      schedule <- fetch scheduleId
      render ShowView { .. }

  action EditScheduleAction { scheduleId } = do
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
            |> set #booked False
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
      schedule <- fetch scheduleId
      deleteRecord schedule
      setSuccessMessage "Schedule deleted"
      redirectTo SchedulesAction

buildSchedule schedule = schedule
    |> fill @["filledDate","filledTime", "scheduleType", "description"]
    |> validateField #filledDate nonEmpty 
    |> validateField #filledTime nonEmpty 
    |> validateField #scheduleType nonEmpty
  
