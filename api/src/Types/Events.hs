module Types.Events
    ( Event(..)
    ) where

import Types

data Event = UserRegistered SubmittedUser | SingleSubmitted UserId SubmittedSingle | SingleDeleted Single deriving (Show)
