module Main (main) where 

import Effect (Effect)
import Data.Unit (Unit)

import Runner (uiRunner)

main :: Effect Unit 
main = uiRunner 
