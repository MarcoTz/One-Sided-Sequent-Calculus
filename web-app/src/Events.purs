module Events (runSrc, selectExample, getSrc, handleAction) where 

import Definitions (Input(..),State, getExSource, runProg, indexToEx)

import Halogen (modify_)

import Prelude (bind,($))
import Data.Unit (Unit)
import Control.Monad.State (class MonadState, gets)

getSrc :: String -> Input 
getSrc newSrc = ProgramInput newSrc 

runSrc::forall ev. ev->Input
runSrc = \_ -> RunProg

selectExample :: Int -> Input
selectExample = \i  -> ExampleSelect $ indexToEx i

handleAction :: forall m. MonadState State m => Input -> m Unit
handleAction inp = case inp of 
  ExampleSelect ex -> modify_ (\st -> st {progSrc=getExSource ex})                      
  ProgramInput src -> modify_ (\st -> st {progSrc=src})
  RunProg -> do
    src <- gets (\st -> st.progSrc)
    let res = runProg src
    modify_ (\st -> st {runRes=res})
