module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)

data SchedulesController
    = SchedulesAction
    | OtherSchedulesAction { ymd :: !Text }
    | NewScheduleAction { ymd :: !Text }
    | ShowScheduleAction { scheduleId :: !(Id Schedule) }
    | CreateScheduleAction
    | EditScheduleAction { scheduleId :: !(Id Schedule) }
    | UpdateScheduleAction { scheduleId :: !(Id Schedule) }
    | DeleteScheduleAction { scheduleId :: !(Id Schedule) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    | CreateUserAction
    deriving (Eq, Show, Data)


instance HasNewSessionUrl User where
  newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data CommentsController
    = CommentsAction
    | NewCommentAction { postId :: !(Id Post) }
    | ShowCommentAction { commentId :: !(Id Comment) }
    | CreateCommentAction
    | EditCommentAction { commentId :: !(Id Comment) }
    | UpdateCommentAction { commentId :: !(Id Comment) }
    | DeleteCommentAction { commentId :: !(Id Comment) }
    deriving (Eq, Show, Data)

