module Layout (render) where 

import Definitions (RunResult(..),Input, State)
import Events (runSrc, selectExample,getSrc)
import StandardLib (libMap)

import Prelude (($),(<>),(<$>),show)
import Data.Map (toUnfoldable)
--import Data.String (Pattern(..), split)
--import Data.Array(concatMap)
import Data.Tuple (Tuple(..))
import Web.HTML.Common (ClassName(..))
import Halogen.HTML (HTML,text)
import Halogen.HTML.Elements (body,div,div_,textarea,button,h1_, h2_,br_, select,option_)
import Halogen.HTML.Properties (class_,id, readOnly,value)
import Halogen.HTML.Events (onClick,onValueChange)

--strToText :: forall w. String -> Array (HTML w Input)
--strToText str = do
--  let lines = split (Pattern "\n") str
--  concatMap (\l -> [text l,br_]) lines

render :: forall w. State -> HTML w Input
render {progSrc:src,runRes:res} = layout src res

progDiv :: forall w. String -> HTML w Input
progDiv src = div 
  [ class_ $ ClassName "prog" ]
  [
    textarea [id "progInput", value src, onValueChange getSrc],
    br_,
    button [id "runButton", onClick runSrc] [text "Run"]
  ]

resDiv :: forall w.RunResult -> HTML w Input 
resDiv (ResErr {errMsg:err, errDebug:debug, errTypes:tys}) = div [ class_ $ ClassName "results"]
  [
    h1_ [text "Results"],
    h2_ [text "Output"],
    textarea [class_ $ ClassName "evalError", id "evalRes", readOnly true, value ("Error: " <> err)],
    br_,
    h2_ [text "Inferred Types"],
    textarea [id "typesStr", readOnly true, value tys],
    h2_ [text "Debug Trace"],
    textarea [id "debugStr", readOnly true, value debug]
  ]
resDiv (ResSucc{succCmd:cmd,succTrace:tr,succDebug:debug, succTypes:tys}) = div 
  [ class_ $ ClassName "results" ]
  [ 
    h1_ [text "Results"],
    h2_ [text "Output"],
    textarea [class_ $ ClassName "evalSucc", id "evalRes", readOnly true, id "resultStr", value cmd],
    br_,
    h2_ [text "Inferred Types"],
    textarea [id "typesStr", readOnly true, value tys],
    h2_ [text "Evaluation Trace"],
    br_,
    textarea [id "traceStr", readOnly true, value tr],
    h2_ [text "Debug Trace"],
    textarea [id "debugStr", readOnly true, value debug]
  ]

exSelect :: forall w. HTML w Input
exSelect = div_ [
  text "Choose Example ",
  select [id "exampleSelect", onValueChange selectExample] 
    ((\(Tuple mn _) -> option_ [text (show mn)]) <$>  toUnfoldable libMap),
  br_
  ]

layout :: forall w. String -> RunResult -> HTML w Input
layout src res = body  []
  [
    h1_ [text "One Sided Sequent Calculus"],
    exSelect,
    progDiv src,
    resDiv res
  ]
