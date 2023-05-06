module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import IHP.AuthSupport.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
  action NewSessionAction = Sessions.newSessionAction @User
  action CreateSessionAction = Sessions.createSessionAction @User
  action DeleteSessionAction = Sessions.deleteSessionAction @User
  action CreateUserAction = do
    let user = newRecord @User
    user
        |> fill @["name","email","passwordHash"]
        |> validateField #name nonEmpty
        |> validateField #email isEmail
        |> validateField #passwordHash nonEmpty
        |> ifValid \case
            Left user -> render NewView { .. }
            Right user -> do
              hashed <- hashPassword user.passwordHash
              user <- user
                  |> set #passwordHash hashed
                  |> createRecord
              setSuccessMessage "You have registered successfully"
              redirectTo NewSessionAction

instance Sessions.SessionsControllerConfig User
