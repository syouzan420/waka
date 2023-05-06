module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.New

instance Controller UsersController where
  action UsersAction = do
    let user = newRecord
    render NewView { .. }

  action NewUserAction = do
    let user = newRecord
    render NewView { .. }

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

