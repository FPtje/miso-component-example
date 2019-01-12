{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Button
  ( -- Note: No constructors are exported
    Model
  , Action

  , initialModel
  , Interface(..)
  , updateModel
  , viewModel
  )
  where

import Control.Lens ( (^.), (+=), (.=), makeLenses, use )

import Data.Monoid ( (<>) )
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso
import Control.Monad ( when )

-- Internal state
data Model
   = Model
     { _mDownState  :: !Bool
     , _mText       :: !Miso.MisoString
     , _mEnterCount :: !Int
     }
     deriving (Eq)

-- Some lenses
makeLenses ''Model

-- Demand a button text from above,
-- use defaults for the rest
initialModel :: Miso.MisoString -> Model
initialModel txt =
    Model
    { _mDownState  = False
    , _mText       = txt
    , _mEnterCount = 0
    }

-- Actions interface
-- These actions are interesting for the parent
data Interface action
   = Interface
     { -- passAction to channel Actions back to this component
       passAction :: Action -> action

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
-- See the documentation for the Transition monad in miso's Haddock.
updateModel
    :: Interface action
    -> Action
    -> Miso.Transition action Model ()
updateModel iface action = case action of
    MouseDown -> do
      mDownState .= True
      mEnterCount += 1

      enterCount <- use mEnterCount

      when (enterCount == 10) $
        Miso.scheduleIO $ pure $ manyClicks iface enterCount

    MouseUp ->
      mDownState .= False

-- Same pattern as the `update` function
viewModel :: Interface action -> Model -> Miso.View action
viewModel iface m =
    button_
      [ onClick $ click iface
      , onMouseDown $ passAction iface MouseDown
      , onMouseUp $ passAction iface MouseUp
      ]
      [ if m ^. mDownState
        then text $ "~" <> m ^. mText <> "~"
        else text $ m ^. mText
      ]

