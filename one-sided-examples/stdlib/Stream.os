module Stream

import Bool

codata Stream(a:-){
  Head(a),
  Tail(Stream(a))
}

constTrue :: Stream(Bool)
rec constTrue := mu a. 
  <case { Head(b) => <True | CBV |b>, Tail(str) => < constTrue | CBV | str >  } | CBV | a>

