module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome
import Web.View.Static.Code

instance Controller StaticController where
    action WelcomeAction = render WelcomeView
    action CodeAction {arg} = render CodeView {arg}
