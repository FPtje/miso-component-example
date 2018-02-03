{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Button

import Control.Lens hiding (view)
import Miso
import Miso.String

-- The two Button components' Models are embedded in this (the parent's) Model
data Model
   = Model
     { _mLeftButton  :: !Button.Model
     , _mValue       :: !Int
     , _mRightButton :: !Button.Model
     }
     deriving ( Eq )

makeLenses ''Model

data Action
  = LeftButtonAction  !Button.Action
  | RightButtonAction !Button.Action
  | SubtractOne
  | AddOne
  | ManyClicksWarning !Int
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main =
    startApp App
      { initialAction = NoOp
      , model         = initialModel
      , update        = fromTransition . updateModel
      , view          = viewModel
      , events        = defaultEvents
      , subs          = []
      , mountPoint    = Nothing
      }

initialModel :: Model
initialModel =
    Model
    { _mLeftButton  = Button.initialModel "-"
    , _mValue       = 0
    , _mRightButton = Button.initialModel "+"
    }

updateModel :: Action -> Transition Action Model ()
updateModel action = case action of
    LeftButtonAction act -> do
        -- Update the component's model, with whatever side effects it may have
        zoom mLeftButton $ Button.updateModel leftButtonPa act

    RightButtonAction act -> do
        zoom mRightButton $ Button.updateModel rightButtonPa act

    SubtractOne -> do
      mValue -= 1

    AddOne ->
      mValue += 1

    ManyClicksWarning i -> scheduleIO $ do
      putStrLn "Ouch! You're clicking over too much!"
      putStrLn $ show i ++ " is way too much for me to handle!"

      pure NoOp

    NoOp -> pure ()


-- Call the component's `viewModel` where you want it to be drawn
viewModel :: Model -> View Action
viewModel m =
    div_ []
    [ Button.viewModel leftButtonPa $ m ^. mLeftButton
    , text $ m ^. mValue . to show . to ms
    , Button.viewModel rightButtonPa $ m ^. mRightButton
    ]

-- Filling in the PublicActions values for both buttons
leftButtonPa :: Button.PublicActions Action
leftButtonPa =
    Button.PublicActions
    { Button.toParent   = LeftButtonAction
    , Button.click      = SubtractOne
    , Button.manyClicks = ManyClicksWarning
    }

rightButtonPa :: Button.PublicActions Action
rightButtonPa =
    Button.PublicActions
    { Button.toParent   = RightButtonAction
    , Button.click      = AddOne
    , Button.manyClicks = ManyClicksWarning
    }
