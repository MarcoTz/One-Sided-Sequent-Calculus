module Stream

import Bool;

data Stream(a:-):-{
  Head(a),
  Tail(Stream(a))
}

-- recursive
--constTrue :: Stream(Bool):+;
--constTrue := mu a. <case { Head(a) => <True | + | a>, Tail(str) => < constTrue | + | a >  } | + | a>;
