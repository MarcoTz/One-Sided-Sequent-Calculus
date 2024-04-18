module Layout (render) where 

import Definitions (RunResult(..),Input, State,getExSource,Examples(..))
import Events (runSrc, selectExample,getSrc)

import Prelude (($))
import Web.HTML.Common (ClassName(..))
import Halogen.HTML (HTML,text)
import Halogen.HTML.Elements (body,div,div_,textarea,button,h1_, h2_, br_, select,option)
import Halogen.HTML.Properties (class_,id, readOnly,value)
import Halogen.HTML.Events (onClick,onValueChange,onSelectedIndexChange)


render :: forall w. State -> HTML w Input
render {progSrc:src,runRes:res} = layout src res

progDiv :: forall w. String -> HTML w Input
progDiv src = div 
  [ class_ $ ClassName "prog" ]
  [
    textarea [id "progInput", value src, onValueChange getSrc],
    button [id "runButton", onClick runSrc] [text "Run"]
  ]

resDiv :: forall w.RunResult -> HTML w Input 
resDiv (ResErr err) = div [ class_ $ ClassName "results"]
  [
    h2_ [text "Results"],
    div [class_ $ ClassName "evalError"] [text err]
  ]
resDiv (ResSucc cmd tr st) = div 
  [ class_ $ ClassName "results" ]
  [ 
    h1_ [text "Results"],
    div [class_ $ ClassName "code", id "resultStr"] [text cmd],
    br_,
    div_ [text "Types in Program"],
    br_, 
    textarea [id "typeStr", readOnly true, value st] ,
    br_, 
    div_ [text "Evaluation Trace"],
    br_, 
    textarea [id "traceStr", readOnly true, value tr]
  ]

exSelect :: forall w. HTML w Input
exSelect = div_ [
  text "Choose Example",
  select [id "exampleSelect", onSelectedIndexChange selectExample] 
    [
    option [value "tuple"] [text "Tuples"],
    option [value "list"] [text "Lists"],
    option [value "nat"] [text "Natural Numbers"],
    option [value "fun"] [text "Functions"]
    ]
  ]

exampleDiv :: forall w. HTML w Input 
exampleDiv = div
  [id "examples"]
  [
    div [id "tuple"] [text $ getExSource ExTuple],
    div [id "list"] [text $ getExSource ExList],
    div [id "nat"] [text $ getExSource ExNat],
    div [id "fun"] [text $ getExSource ExFun]
  ]

debugDiv :: forall w. String -> HTML w Input 
debugDiv src = div_ [text $ src]

layout :: forall w. String -> RunResult -> HTML w Input
layout src res = body  []
  [
    h1_ [text "One Sided Sequent Calculus"],
    debugDiv src,
    exSelect,
    progDiv src,
    resDiv res,
    exampleDiv
  ]
