{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Button
  ( -- Note: No constructors
    Model
  , Action

  , initialModel
  , PublicActions(..)
  , updateModel
  , viewModel
  )
  where

import Control.Lens hiding ( view )

import Miso
import Miso.String
import Miso.Transition
import Control.Monad ( when )

-- Internal state
data Model
   = Model
     { _mDownState  :: !Bool
     , _mText       :: !MisoString
     , _mEnterCount :: !Int
     }
     deriving (Eq)

-- Some lenses
makeLenses ''Model

-- Demand a button text from above,
-- use defaults for the rest
initialModel :: MisoString -> Model
initialModel txt =
    Model
    { _mDownState = False
    , _mText       = txt
    , _mEnterCount = 0
    }

-- Actions interface
-- These actions are interesting for the parent
data PublicActions action
   = PublicActions
     { -- toParent to channel Actions back to this component
       toParent   :: Action -> action

       -- Two events that the parent should do something with
     , click      :: action
     , manyClicks :: Int -> action
     }

data Action
   = MouseDown
   | MouseUp
   deriving (Show, Eq)


-- Note the polymorphism in `action`
-- This `action` will be filled in to become the parent's `Action`
-- Also note that this is the Transition monad, rather than the Effect monad
updateModel
    :: PublicActions action
    -> Action
    -> Transition Model action
updateModel pa action = case action of
    MouseDown -> do
      mDownState .= True
      mEnterCount += 1

      enterCount <- use mEnterCount

      when (enterCount == 10) $
        scheduleIO $ pure $ manyClicks pa enterCount

    MouseUp ->
      mDownState .= False

-- Same pattern here
viewModel :: PublicActions action -> Model -> View action
viewModel pa m =
    button_
      [ onClick $ click pa
      , onMouseDown $ toParent pa MouseDown
      , onMouseUp $ toParent pa MouseUp
      ]
      [ if m ^. mDownState
        then text $ "~" <> m ^. mText <> "~"
        else text $ m ^. mText
      ]

