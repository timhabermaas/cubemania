{-# LANGUAGE OverloadedStrings #-}

module Frontend.PuzzleNavigation
    ( puzzleNavigation
    ) where

import Data.Monoid ((<>))
import Types
import Routes
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

puzzleImage :: Kind -> Puzzle -> (PuzzleSlug -> T.Text) -> Puzzle -> Html
puzzleImage k selectedPuzzle link p =
    li ! class_ (if selectedPuzzle == p then "checked" else "") $
        a ! href (toValue $ link (puzzleSlug p)) $ do
            H.span ! class_ (toValue $ "puzzle pos" <> show (puzzleCssPosition p)) $
                H.span ! class_ (toValue $ "kind pos" <> show (kindCssPosition k)) $ mempty
            H.span ! class_ "name" $ toHtml $ puzzleName p

kindSection :: Int -> Puzzle -> (PuzzleSlug -> T.Text) -> Kind -> [Puzzle] -> Html
kindSection kindCount selectedPuzzle link kind puzzles =
    li ! A.style (toValue $ "width: " <> show (100 `Prelude.div` kindCount) <> "%") $
        ul ! class_ "puzzles" $
            mapM_ (puzzleImage kind selectedPuzzle link) $ reverse puzzles

puzzleNavigation :: Map.Map Kind [Puzzle] -> (Puzzle, Kind) -> (PuzzleSlug -> T.Text) -> Html
puzzleNavigation puzzles (selectedPuzzle, selectedKind) link =
    nav ! A.id "subnavigation" $ do
        H.div ! A.id "puzzles" $
            ul ! A.style (toValue $ "width: " <> show (kindCount * 100) <> "%; left: " <> show (kindIndex * (-100)) <> "%") $
                sequence_ $ Map.mapWithKey (kindSection kindCount selectedPuzzle link) puzzles
        H.div ! A.id "kinds" $ ul ! class_ "center" $
            mapM_ (bottomBarItem selectedKind) kinds
  where
    kindCount = length kinds
    kinds = Map.keys puzzles
    kindIndex = fromMaybe 0 $ elemIndex selectedKind kinds
    bottomBarItem selected k =
        li ! A.style "width: 25%" ! class_ (if selected == k then "checked" else "") $
            a ! href "#" $ toHtml $ kindName k
