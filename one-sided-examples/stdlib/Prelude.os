module Prelude

printT :: forall a. a
printT := mu x. Print x

exitSucc :: forall a. a
exitSucc := mu x. Done
