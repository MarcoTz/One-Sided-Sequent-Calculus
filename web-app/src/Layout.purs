module Layout (render) where 

import Definitions (RunResult(..),Input, State)
import Events (runSrc, selectExample,getSrc)

import Prelude (($),(<>))
--import Data.String (Pattern(..), split)
--import Data.Array(concatMap)
import Web.HTML.Common (ClassName(..))
import Halogen.HTML (HTML,text)
import Halogen.HTML.Elements (body,div,div_,textarea,button,h1_, h2_,br_, select,option)
import Halogen.HTML.Properties (class_,id, readOnly,value)
import Halogen.HTML.Events (onClick,onValueChange,onSelectedIndexChange)

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
resDiv (ResErr err debug) = div [ class_ $ ClassName "results"]
  [
    h1_ [text "Results"],
    h2_ [text "Output"],
    textarea [class_ $ ClassName "evalError", value ("Error: " <> err)],
    br_,
    h2_ [text "Debug Trace"],
    textarea [id "traceStr", readOnly true, value debug]
  ]
resDiv (ResSucc cmd tr _st) = div 
  [ class_ $ ClassName "results" ]
  [ 
    h1_ [text "Results"],
    h2_ [text "Output"],
    textarea [class_ $ ClassName "evalSucc", id "resultStr", value cmd],
    br_,
    h2_ [text "Debug Trace"],
    br_, 
    textarea [id "traceStr", readOnly true, value tr]
  ]

exSelect :: forall w. HTML w Input
exSelect = div_ [
  text "Choose Example ",
  select [id "exampleSelect", onSelectedIndexChange selectExample] 
    [
    option [value "tuple"] [text "Tuples"],
    option [value "list"] [text "Lists"],
    option [value "nat"] [text "Natural Numbers"],
    option [value "fun"] [text "Functions"]
    ],
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
