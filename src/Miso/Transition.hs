module Miso.Transition
    ( Transition
    , TransitionM
    , fromTransition
    , toTransition
    , scheduleIO
    ) where

import Control.Monad.Writer ( tell )
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict hiding ( tell )
import Miso (Effect(..))

type Transition  model action = TransitionM model action ()
type TransitionM model action = StateT model (Writer [IO action])

fromTransition
    :: Transition model action
    -> (model -> Effect action model)
fromTransition act = uncurry Effect . runWriter . execStateT act

-- Not used in this example
toTransition
    :: (model -> Effect action model)
    -> Transition model action
toTransition f = StateT $ \s ->
                   let Effect s' ios = f s
                   in WriterT $ pure (((), s'), ios)

scheduleIO :: IO action -> Transition model action
scheduleIO ioAction = tell [ ioAction ]
