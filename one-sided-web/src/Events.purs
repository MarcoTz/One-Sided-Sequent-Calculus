module Events (runSrc, selectExample, getSrc, handleAction) where 

import Definitions (Input(..),State, runProg)
import StandardLib (libMap) 
import Common (Modulename(..))

import Halogen (modify_)

import Prelude (bind)
import Data.Unit (Unit)
import Data.Maybe (Maybe(..))
import Data.Map (lookup)
import Control.Monad.State (class MonadState, gets)

getSrc :: String -> Input 
getSrc newSrc = ProgramInput newSrc 

runSrc::forall ev. ev->Input
runSrc = \_ -> RunProg

selectExample :: String -> Input
selectExample nm = case lookup (Modulename nm) libMap of 
  Just src -> ProgramInput src 
  Nothing -> ProgramInput ""

handleAction :: forall m. MonadState State m => Input -> m Unit
handleAction inp = case inp of 
  ProgramInput src -> modify_ (\st -> st {progSrc=src})
  RunProg -> do
    src <- gets (\st -> st.progSrc)
    let res = runProg src
    modify_ (\st -> st {runRes=res})
