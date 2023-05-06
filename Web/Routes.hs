module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

instance AutoRoute SessionsController

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute PostsController
instance AutoRoute SchedulesController
instance AutoRoute CommentsController
