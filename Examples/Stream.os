module Stream

import Bool;

codata Stream(a:-){
  Head(a),
  Tail(Stream(a))
}

constTrue :: Stream(Bool):CBV;
constTrue := mu a. <case { Head(a) => <True | CBV | a>, Tail(str) => < constTrue | CBV | a >  } | CBV | a>;
