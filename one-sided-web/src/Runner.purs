module Runner where 

import Prelude (bind)
import Data.Unit (Unit)
import Effect (Effect)

import Halogen (Component,mkComponent,mkEval, defaultEval)
import Halogen.Aff (awaitBody,runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Halogen.Query.HalogenQ (HalogenQ)
import Halogen.Query.HalogenM (HalogenM)
import Effect.Aff.Class (class MonadAff)

import Definitions (Input(..),State, initialState)
import Layout (render)
import Events (handleAction)

component :: forall output m t. MonadAff m => Component t Input output m
component = mkComponent { initialState:initialState, render:render, eval:eval}

eval :: forall slots output m a t t2. MonadAff m => HalogenQ t Input t2 a -> HalogenM State Input slots output m a
eval = mkEval defaultEval {handleAction=handleAction}

uiRunner :: Effect Unit
uiRunner = runHalogenAff do
  body <- awaitBody
  runUI component (RunProg) body
