{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Button

import Control.Lens ( (^.), (+=), (-=), makeLenses, to, zoom )
import Miso ( App(..), Transition )
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso

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
    Miso.startApp App
      { initialAction = NoOp
      , model         = initialModel
      , update        = Miso.fromTransition . updateModel
      , view          = viewModel
      , events        = Miso.defaultEvents
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
        zoom mLeftButton $ Button.updateModel iLeftButton act

    RightButtonAction act -> do
        zoom mRightButton $ Button.updateModel iRightButton act
        -- When not using the lens library, the above function call can be
        -- implemented as follows:

        -- m <- State.get
        -- rightButton <-
        --   lift $ State.execStateT (Button.updateModel iRightButton act) $ _mRightButton m
        -- State.put $ m { _mRightButton = rightButton }

    SubtractOne -> do
      mValue -= 1

    AddOne ->
      mValue += 1

    ManyClicksWarning i -> Miso.scheduleIO $ do
      putStrLn "Ouch! You're clicking over too much!"
      putStrLn $ show i ++ " is way too much for me to handle!"

      pure NoOp

    NoOp -> pure ()


-- Call the component's `viewModel` where you want it to be drawn
viewModel :: Model -> View Action
viewModel m =
    div_ []
    [ Button.viewModel iLeftButton $ m ^. mLeftButton
    , text $ m ^. mValue . to show . to Miso.ms
    , Button.viewModel iRightButton $ m ^. mRightButton
    ]

-- Filling in the Interface values for both buttons
iLeftButton :: Button.Interface Action
iLeftButton =
    Button.Interface
    { Button.passAction = LeftButtonAction
    , Button.click      = SubtractOne
    , Button.manyClicks = ManyClicksWarning
    }

iRightButton :: Button.Interface Action
iRightButton =
    Button.Interface
    { Button.passAction = RightButtonAction
    , Button.click      = AddOne
    , Button.manyClicks = ManyClicksWarning
    }
