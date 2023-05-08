module Web.Controller.Prelude
( module Web.Types
, module Application.Helper.Controller
, module IHP.ControllerPrelude
, module Generated.Types
, ensureIsTeru
)
where

import Web.Types
import Application.Helper.Controller
import IHP.ControllerPrelude
import Generated.Types
import Web.Routes

ensureIsTeru :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user,
                      Typeable user, user ~ CurrentUserRecord) => IO () 
ensureIsTeru = case currentUserOrNothing of
                  Just currentUser -> do
                    let cid = show$currentUser.id
                    if cid==userTeru || cid==userTeruOverThere
                       then pure ()
                       else redirectTo NewSessionAction 
                  Nothing -> redirectTo NewSessionAction 
