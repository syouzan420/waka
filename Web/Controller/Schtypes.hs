module Web.Controller.Schtypes where

import Web.Controller.Prelude
import Web.View.Schtypes.Index
import Web.View.Schtypes.New
import Web.View.Schtypes.Edit
import Web.View.Schtypes.Show

instance Controller SchtypesController where
    action SchtypesAction = do
        schtypes <- query @Schtype |> fetch
        render IndexView { .. }

    action NewSchtypeAction = do
        let schtype = newRecord
        render NewView { .. }

    action ShowSchtypeAction { schtypeId } = do
        schtype <- fetch schtypeId
        render ShowView { .. }

    action EditSchtypeAction { schtypeId } = do
        schtype <- fetch schtypeId
        render EditView { .. }

    action UpdateSchtypeAction { schtypeId } = do
        schtype <- fetch schtypeId
        schtype
            |> buildSchtype
            |> ifValid \case
                Left schtype -> render EditView { .. }
                Right schtype -> do
                    schtype <- schtype |> updateRecord
                    setSuccessMessage "Schtype updated"
                    redirectTo EditSchtypeAction { .. }

    action CreateSchtypeAction = do
        let schtype = newRecord @Schtype
        schtype
            |> buildSchtype
            |> ifValid \case
                Left schtype -> render NewView { .. } 
                Right schtype -> do
                    schtype <- schtype |> createRecord
                    setSuccessMessage "Schtype created"
                    redirectTo SchtypesAction

    action DeleteSchtypeAction { schtypeId } = do
        schtype <- fetch schtypeId
        deleteRecord schtype
        setSuccessMessage "Schtype deleted"
        redirectTo SchtypesAction

buildSchtype schtype = schtype
    |> fill @'["scheduleType"]
