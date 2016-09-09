module Types.Events
    ( Event(..)
    ) where

import Types

data Event = SingleSubmitted UserId SubmittedSingle | SingleDeleted Single deriving (Show)
