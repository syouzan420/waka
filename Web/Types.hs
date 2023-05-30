{-# LANGUAGE OverloadedStrings #-}
module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types
import Data.Text

userTeru :: Text
userTeru = "f05a9b35-7df1-42d1-a1b5-dc420af273c2"

userTeruOverThere :: Text
userTeruOverThere = "6a03be6b-32c7-41b8-975e-e19d40032069"

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController 
    = WelcomeAction
    | CodeAction {arg :: !Text}
    deriving (Eq, Show, Data)

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
    | OtherSchedulesAction { ymd :: !Text , tdy :: !Text}
    | NewScheduleAction { ymd :: !Text }
    | NewBookingAction  { ymd :: !Text }
    | ShowScheduleAction { scheduleId :: !(Id Schedule) }
    | CreateScheduleAction 
    | EditScheduleAction { scheduleId :: !(Id Schedule) }
    | EditUserBookAction { scheduleId :: !(Id Schedule) }
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

data SchtypesController
    = SchtypesAction
    | NewSchtypeAction
    | ShowSchtypeAction { schtypeId :: !(Id Schtype) }
    | CreateSchtypeAction
    | EditSchtypeAction { schtypeId :: !(Id Schtype) }
    | UpdateSchtypeAction { schtypeId :: !(Id Schtype) }
    | DeleteSchtypeAction { schtypeId :: !(Id Schtype) }
    deriving (Eq, Show, Data)


data DurationsController
    = DurationsAction
    | NewDurationAction
    | ShowDurationAction { durationId :: !(Id Duration) }
    | CreateDurationAction
    | EditDurationAction { durationId :: !(Id Duration) }
    | UpdateDurationAction { durationId :: !(Id Duration) }
    | DeleteDurationAction { durationId :: !(Id Duration) }
    deriving (Eq, Show, Data)
