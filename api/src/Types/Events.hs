module Types.Events
    ( Event(..)
    ) where

import Data.Text (Text)
import Types

data Event
    = UserRegistered SubmittedUser
    | SingleSubmitted UserId PuzzleId SubmittedSingle
    | SingleDeleted Single
    | UserPasswordReseted Email Text ClearPassword
    deriving (Show)
