module EditorMod
  ( create
  , readEditorValue
  , setEditorValue
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Types (Editor, defaultConstuctorOptions)

foreign import createImpl
  :: Foreign
  -> String
  -> Effect (Promise Editor)

create
  :: String
  -> String
  -> Aff Editor
create textValue elementIdString = do
  let opts = defaultConstuctorOptions 
                  { value = Just textValue 
                      , language = Just "one-sided-sequent-calculus"
                      , theme = Just "custom-vs-dark"
                  }
  let effProm = createImpl (unsafeToForeign opts) elementIdString
  toAffE effProm

foreign import readEditorValue :: Effect String

foreign import setEditorValue :: String -> Unit

