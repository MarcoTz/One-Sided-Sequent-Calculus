module Parser.Keywords (
  Keyword (..),
  allKws
) where 

import Prelude (class Show)
import Data.List (List(..))

data Keyword = 
 KwModule
  | KwData
  | KwCodata
  | KwCBV
  | KwCBN
  | KwCBA
  | Kwmu
  | KwMu
  | KwCase
  | Kwcase 
  | KwDone
  | KwForall
  | Kwforall 
  | KwCo
  | Kwco
  | KwImport
  | KwMain
  | Kwmain
  | KwError 
  | KwRec 
  | KwPrint
  | Kwprint
  | KwIf
  | Kwif
  | KwThen
  | Kwthen
  | KwElse
  | Kwelse
  | KwOf 
  | Kwof
  | KwReturn
  | Kwreturn

instance Show Keyword where 
  show KwModule = "module"
  show KwData   = "data" 
  show KwCodata = "codata"
  show KwCBV    = "CBV"
  show KwCBN    = "CBN"
  show KwCBA    = "CBA"
  show KwMu     = "Mu"
  show Kwmu     = "mu"
  show KwCase   = "Case"
  show Kwcase   = "case"
  show KwDone   = "Done"
  show KwForall = "Forall"
  show Kwforall = "forall"
  show KwCo     = "Co"
  show Kwco     = "co"
  show KwImport = "import"
  show KwMain   = "Main"
  show Kwmain   = "main"
  show KwError  = "error"
  show KwRec    = "rec"
  show KwPrint  = "Print"
  show Kwprint  = "print"
  show KwIf     = "If"
  show Kwif     = "if"
  show KwThen   = "Then"
  show Kwthen   = "then"
  show KwElse   = "Else"
  show Kwelse   = "else"
  show KwOf     = "Of"
  show Kwof     = "of"
  show KwReturn = "Return"
  show Kwreturn = "return"

allKws :: List Keyword
allKws = Cons KwModule 
  (Cons KwData 
  (Cons Kwmu
  (Cons KwMu 
  (Cons KwCase 
  (Cons Kwcase
  (Cons KwDone
  (Cons KwForall
  (Cons Kwforall
  (Cons KwCo
  (Cons Kwco
  (Cons KwImport
  (Cons KwMain
  (Cons Kwmain
  (Cons KwError
  (Cons KwRec
  (Cons KwCBV
  (Cons KwCBN
  (Cons KwCBA
  (Cons KwCodata 
  (Cons KwIf
  (Cons Kwif
  (Cons KwThen
  (Cons Kwthen
  (Cons KwElse
  (Cons Kwelse 
  (Cons KwOf
  (Cons Kwof 
  (Cons KwReturn
  (Cons Kwreturn Nil)))))))))))))))))))))))))))))
