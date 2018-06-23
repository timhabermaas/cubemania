{-# LANGUAGE OverloadedStrings #-}

module Frontend.PuzzleNavigation
    ( puzzleNavigation
    ) where

import Data.Monoid ((<>))
import Types
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

puzzleImage :: Kind -> Puzzle -> (PuzzleSlug -> T.Text) -> Puzzle -> Html
puzzleImage k selectedPuzzle linkFn puzzle =
    li ! class_ (if selectedPuzzle == puzzle then "checked" else "") $
        a ! href (toValue $ linkFn (puzzleSlug puzzle)) $ do
            H.span ! class_ (toValue $ "puzzle pos" <> show (puzzleCssPosition puzzle)) $
                H.span ! class_ (toValue $ "kind pos" <> show (kindCssPosition k)) $ mempty
            H.span ! class_ "name" $ toHtml $ puzzleName puzzle

kindSection :: Int -> Puzzle -> (PuzzleSlug -> T.Text) -> Kind -> [Puzzle] -> Html
kindSection kindCount selectedPuzzle linkFn kind puzzles =
    li ! A.style (toValue $ "width: " <> show (100 `Prelude.div` kindCount) <> "%") $
        ul ! class_ "puzzles" $
            mapM_ (puzzleImage kind selectedPuzzle linkFn) $ reverse puzzles

puzzleNavigation :: Map.Map Kind [Puzzle] -> (Puzzle, Kind) -> (PuzzleSlug -> T.Text) -> Html
puzzleNavigation puzzles (selectedPuzzle, selectedKind) linkFn =
    nav ! A.id "subnavigation" $ do
        H.div ! A.id "puzzles" $
            ul ! A.style (toValue $ "width: " <> show (kindCount * 100) <> "%; left: " <> show (kindIndex * (-100)) <> "%") $
                sequence_ $ Map.mapWithKey (kindSection kindCount selectedPuzzle linkFn) puzzles
        H.div ! A.id "kinds" $ ul ! class_ "center" $
            mapM_ (bottomBarItem selectedKind) kinds
  where
    kindCount = length kinds
    kinds = Map.keys puzzles
    kindIndex = fromMaybe 0 $ elemIndex selectedKind kinds
    bottomBarItem selected k =
        li ! A.style "width: 25%" ! class_ (if selected == k then "checked" else "") $
            a ! href "#" $ toHtml $ kindName k
