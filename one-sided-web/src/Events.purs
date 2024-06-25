module Events (runSrc, selectExample, getSrc, handleAction) where 

import Definitions (Input(..),State, runProg)
import StandardLib (libMap) 
import Common (Modulename(..))

import Halogen (modify_, liftAff)

import Prelude (bind)
import Data.Unit (Unit)
import Data.Maybe (Maybe(..))
import Data.Map (lookup)
import Control.Monad.State (class MonadState, gets)
import Effect.Aff.Class (class MonadAff)
import Effect.Unsafe (unsafePerformEffect)
import EditorMod (create, readEditorValue, setEditorValue)

getSrc :: String -> Input 
getSrc newSrc = ProgramInput newSrc 

runSrc::forall ev. ev->Input
runSrc = \_ -> RunProg

selectExample :: String -> Input
selectExample nm = case lookup (Modulename nm) libMap of 
  Just src -> ProgramInput src 
  Nothing -> ProgramInput ""

handleAction :: forall m. MonadAff m => MonadState State m => Input -> m Unit
handleAction inp = case inp of 
  ProgramInput src -> do
    -- dont know how to call the setEditorValue function by itself (without using let or something)
    -- setEditorValue has no return and only needs to be called for the editor value to change
    let _ = setEditorValue src 
    modify_ (\st -> st {progSrc=src})
  RunProg -> do
    -- read directly from the editor and write it in the state after
    let src = unsafePerformEffect readEditorValue
    let res = runProg src
    modify_ (\st -> st {runRes=res, progSrc=src})
  InitEditor -> do
    src <- gets (\st -> st.progSrc)
    let editorInst = create src "container"
    x <- liftAff editorInst
    modify_ (\st -> st {monEditor = Just x})
