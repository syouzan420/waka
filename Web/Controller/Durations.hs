module Web.Controller.Durations where

import Web.Controller.Prelude
import Web.View.Durations.Index
import Web.View.Durations.New
import Web.View.Durations.Edit
import Web.View.Durations.Show

instance Controller DurationsController where
    action DurationsAction = do
        durations <- query @Duration |> fetch
        render IndexView { .. }

    action NewDurationAction = do
        let duration = newRecord
        render NewView { .. }

    action ShowDurationAction { durationId } = do
        duration <- fetch durationId
        render ShowView { .. }

    action EditDurationAction { durationId } = do
        duration <- fetch durationId
        render EditView { .. }

    action UpdateDurationAction { durationId } = do
        duration <- fetch durationId
        duration
            |> buildDuration
            |> ifValid \case
                Left duration -> render EditView { .. }
                Right duration -> do
                    duration <- duration |> updateRecord
                    setSuccessMessage "Duration updated"
                    redirectTo EditDurationAction { .. }

    action CreateDurationAction = do
        let duration = newRecord @Duration
        duration
            |> buildDuration
            |> ifValid \case
                Left duration -> render NewView { .. } 
                Right duration -> do
                    duration <- duration |> createRecord
                    setSuccessMessage "Duration created"
                    redirectTo DurationsAction

    action DeleteDurationAction { durationId } = do
        duration <- fetch durationId
        deleteRecord duration
        setSuccessMessage "Duration deleted"
        redirectTo DurationsAction

buildDuration duration = duration
    |> fill @'["timeDuration"]
    |> validateField #timeDuration isDuration
