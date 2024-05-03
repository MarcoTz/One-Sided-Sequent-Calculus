(() => {
  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map119 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map119(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map119 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map119($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applyFirst = function(dictApply) {
    var apply1 = apply(dictApply);
    var map60 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map60($$const)(a2))(b2);
      };
    };
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map60 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map60($$const(identity2))(a2))(b2);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map60 = map(dictApply.Functor0());
    return function(f) {
      return function(a2) {
        return function(b2) {
          return apply1(map60(f)(a2))(b2);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure111 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply3(pure111(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind113 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind113(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };
  var ifM = function(dictBind) {
    var bind113 = bind(dictBind);
    return function(cond) {
      return function(t) {
        return function(f) {
          return bind113(cond)(function(cond$prime) {
            if (cond$prime) {
              return t;
            }
            ;
            return f;
          });
        };
      };
    };
  };

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqRowNil = {
    eqRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return true;
        };
      };
    }
  };
  var eqRecord = function(dict) {
    return dict.eqRecord;
  };
  var eqRec = function() {
    return function(dictEqRecord) {
      return {
        eq: eqRecord(dictEqRecord)($$Proxy.value)
      };
    };
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq1 = function(dict) {
    return dict.eq1;
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var eqRowCons = function(dictEqRecord) {
    var eqRecord1 = eqRecord(dictEqRecord);
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictEq) {
          var eq37 = eq(dictEq);
          return {
            eqRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = eqRecord1($$Proxy.value)(ra)(rb);
                  var key = reflectSymbol2($$Proxy.value);
                  var get4 = unsafeGet(key);
                  return eq37(get4(ra))(get4(rb)) && tail2;
                };
              };
            }
          };
        };
      };
    };
  };
  var notEq = function(dictEq) {
    var eq37 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq37(x)(y))(false);
      };
    };
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq31) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq31 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showCharImpl = function(c) {
    var code2 = c.charCodeAt(0);
    if (code2 < 32 || code2 === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code2.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty8;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i2 = 0, l = xs.length; i2 < l; i2++) {
        ss[i2] = f(xs[i2]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var showMaybe = function(dictShow) {
    var show60 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Just) {
          return "(Just " + (show60(v.value0) + ")");
        }
        ;
        if (v instanceof Nothing) {
          return "Nothing";
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 223, column 1 - line 225, column 28): " + [v.constructor.name]);
      }
    };
  };
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    var eq31 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq31(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var ordMaybe = function(dictOrd) {
    var compare5 = compare(dictOrd);
    var eqMaybe1 = eqMaybe(dictOrd.Eq0());
    return {
      compare: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return EQ.value;
          }
          ;
          if (x instanceof Nothing) {
            return LT.value;
          }
          ;
          if (y instanceof Nothing) {
            return GT.value;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return compare5(x.value0)(y.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Maybe (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
        };
      },
      Eq0: function() {
        return eqMaybe1;
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Control.Lazy/index.js
  var defer = function(dict) {
    return dict.defer;
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Bifunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity4);
    };
  };
  var bifunctorTuple = {
    bimap: function(f) {
      return function(g) {
        return function(v) {
          return new Tuple(f(v.value0), g(v.value1));
        };
      };
    }
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x) {
    return x;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond5 = applySecond(dictApplicative.Apply0());
    var pure39 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond5(f($454));
        })(pure39(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(v1) {
              if (v.init) {
                return {
                  init: false,
                  acc: v1
                };
              }
              ;
              return {
                init: false,
                acc: append8(v.acc)(append8(sep)(v1))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty2
          })(xs).acc;
        };
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append8(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var foldM = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonad) {
      var bind36 = bind(dictMonad.Bind1());
      var pure39 = pure(dictMonad.Applicative0());
      return function(f) {
        return function(b0) {
          return foldl22(function(b2) {
            return function(a2) {
              return bind36(b2)(flip(f)(a2));
            };
          })(pure39(b0));
        };
      };
    };
  };
  var find = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(p2) {
      var go2 = function(v) {
        return function(v1) {
          if (v instanceof Nothing && p2(v1)) {
            return new Just(v1);
          }
          ;
          return v;
        };
      };
      return foldl22(go2)(Nothing.value);
    };
  };
  var any = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap22(monoidDisj(dictHeytingAlgebra)));
    };
  };
  var elem = function(dictFoldable) {
    var any1 = any(dictFoldable)(heytingAlgebraBoolean);
    return function(dictEq) {
      var $462 = eq(dictEq);
      return function($463) {
        return any1($462($463));
      };
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.FoldableWithIndex/index.js
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };

  // output/Data.Traversable/index.js
  var traverse = function(dict) {
    return dict.traverse;
  };
  var traversableMaybe = {
    traverse: function(dictApplicative) {
      var pure39 = pure(dictApplicative);
      var map60 = map(dictApplicative.Apply0().Functor0());
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return pure39(Nothing.value);
          }
          ;
          if (v1 instanceof Just) {
            return map60(Just.create)(v(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    },
    sequence: function(dictApplicative) {
      var pure39 = pure(dictApplicative);
      var map60 = map(dictApplicative.Apply0().Functor0());
      return function(v) {
        if (v instanceof Nothing) {
          return pure39(Nothing.value);
        }
        ;
        if (v instanceof Just) {
          return map60(Just.create)(v.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    },
    Foldable1: function() {
      return foldableMaybe;
    }
  };
  var $$for = function(dictApplicative) {
    return function(dictTraversable) {
      var traverse2 = traverse(dictTraversable)(dictApplicative);
      return function(x) {
        return function(f) {
          return traverse2(f)(x);
        };
      };
    };
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value14 = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };

  // output/Data.List.Types/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map3 = /* @__PURE__ */ map(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var intercalate2 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var showList = function(dictShow) {
    var show60 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Nil) {
          return "Nil";
        }
        ;
        return "(" + (intercalate2(" : ")(map3(show60)(v)) + " : Nil)");
      }
    };
  };
  var traversableList = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      var map119 = map(Apply0.Functor0());
      var lift22 = lift2(Apply0);
      var pure111 = pure(dictApplicative);
      return function(f) {
        var $301 = map119(foldl2(flip(Cons.create))(Nil.value));
        var $302 = foldl2(function(acc) {
          var $304 = lift22(flip(Cons.create))(acc);
          return function($305) {
            return $304(f($305));
          };
        })(pure111(Nil.value));
        return function($303) {
          return $301($302($303));
        };
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableList)(dictApplicative)(identity5);
    },
    Functor0: function() {
      return functorList;
    },
    Foldable1: function() {
      return foldableList;
    }
  };
  var unfoldable1List = {
    unfoldr1: function(f) {
      return function(b2) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source3, memo) {
              var v = f(source3);
              if (v.value1 instanceof Just) {
                $tco_var_source = v.value1.value0;
                $copy_memo = new Cons(v.value0, memo);
                return;
              }
              ;
              if (v.value1 instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b2)(Nil.value);
      };
    }
  };
  var unfoldableList = {
    unfoldr: function(f) {
      return function(b2) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source3, memo) {
              var v = f(source3);
              if (v instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(memo);
              }
              ;
              if (v instanceof Just) {
                $tco_var_source = v.value0.value1;
                $copy_memo = new Cons(v.value0.value0, memo);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b2)(Nil.value);
      };
    },
    Unfoldable10: function() {
      return unfoldable1List;
    }
  };
  var eq1List = {
    eq1: function(dictEq) {
      var eq31 = eq(dictEq);
      return function(xs) {
        return function(ys) {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              return function($copy_v2) {
                var $tco_var_v = $copy_v;
                var $tco_var_v1 = $copy_v1;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v, v1, v2) {
                  if (!v2) {
                    $tco_done = true;
                    return false;
                  }
                  ;
                  if (v instanceof Nil && v1 instanceof Nil) {
                    $tco_done = true;
                    return v2;
                  }
                  ;
                  if (v instanceof Cons && v1 instanceof Cons) {
                    $tco_var_v = v.value1;
                    $tco_var_v1 = v1.value1;
                    $copy_v2 = v2 && eq31(v1.value0)(v.value0);
                    return;
                  }
                  ;
                  $tco_done = true;
                  return false;
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                }
                ;
                return $tco_result;
              };
            };
          };
          return go2(xs)(ys)(true);
        };
      };
    }
  };
  var eq12 = /* @__PURE__ */ eq1(eq1List);
  var eqList = function(dictEq) {
    return {
      eq: eq12(dictEq)
    };
  };
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map3(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var bindList = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(v1(v.value0))(bind(bindList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 164, column 1 - line 166, column 37): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind36 = bind(dictMonad.Bind1());
    var unless5 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind36(mb)(function(b2) {
          return unless5(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind36 = bind(dictMonad.Bind1());
    var pure39 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind36(f)(function(f$prime) {
          return bind36(a2)(function(a$prime) {
            return pure39(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done6(value0) {
      this.value0 = value0;
    }
    ;
    Done6.create = function(value0) {
      return new Done6(value0);
    };
    return Done6;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go2(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map4(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.List/index.js
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var eq3 = /* @__PURE__ */ eq(eqOrdering);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
  var bind2 = /* @__PURE__ */ bind(bindList);
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var uncons = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map5(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons(xs));
    });
  };
  var snoc = function(xs) {
    return function(x) {
      return foldr3(Cons.create)(new Cons(x, Nil.value))(xs);
    };
  };
  var singleton3 = function(a2) {
    return new Cons(a2, Nil.value);
  };
  var sortBy = function(cmp) {
    var merge = function(v) {
      return function(v1) {
        if (v instanceof Cons && v1 instanceof Cons) {
          if (eq3(cmp(v.value0)(v1.value0))(GT.value)) {
            return new Cons(v1.value0, merge(v)(v1.value1));
          }
          ;
          if (otherwise) {
            return new Cons(v.value0, merge(v.value1)(v1));
          }
          ;
        }
        ;
        if (v instanceof Nil) {
          return v1;
        }
        ;
        if (v1 instanceof Nil) {
          return v;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 466, column 3 - line 466, column 38): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    var mergePairs = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
      }
      ;
      return v;
    };
    var mergeAll = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
          $tco_done = true;
          return v.value0;
        }
        ;
        $copy_v = mergePairs(v);
        return;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    var sequences = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        if (eq3(cmp(v.value0)(v.value1.value0))(GT.value)) {
          return descending(v.value1.value0)(singleton3(v.value0))(v.value1.value1);
        }
        ;
        if (otherwise) {
          return ascending(v.value1.value0)(function(v1) {
            return new Cons(v.value0, v1);
          })(v.value1.value1);
        }
        ;
      }
      ;
      return singleton3(v);
    };
    var descending = function($copy_v) {
      return function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v = $copy_v;
          var $tco_var_v1 = $copy_v1;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(v, v1, v2) {
            if (v2 instanceof Cons && eq3(cmp(v)(v2.value0))(GT.value)) {
              $tco_var_v = v2.value0;
              $tco_var_v1 = new Cons(v, v1);
              $copy_v2 = v2.value1;
              return;
            }
            ;
            $tco_done1 = true;
            return new Cons(new Cons(v, v1), sequences(v2));
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
    };
    var ascending = function($copy_v) {
      return function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v = $copy_v;
          var $tco_var_v1 = $copy_v1;
          var $tco_done2 = false;
          var $tco_result;
          function $tco_loop(v, v1, v2) {
            if (v2 instanceof Cons && notEq2(cmp(v)(v2.value0))(GT.value)) {
              $tco_var_v = v2.value0;
              $tco_var_v1 = function(ys) {
                return v1(new Cons(v, ys));
              };
              $copy_v2 = v2.value1;
              return;
            }
            ;
            $tco_done2 = true;
            return new Cons(v1(singleton3(v)), sequences(v2));
          }
          ;
          while (!$tco_done2) {
            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
    };
    return function($444) {
      return mergeAll(sequences($444));
    };
  };
  var reverse = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var take = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v = $copy_v;
          var $tco_var_v1 = $copy_v1;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v, v1, v2) {
            if (v1 < 1) {
              $tco_done = true;
              return reverse(v);
            }
            ;
            if (v2 instanceof Nil) {
              $tco_done = true;
              return reverse(v);
            }
            ;
            if (v2 instanceof Cons) {
              $tco_var_v = new Cons(v2.value0, v);
              $tco_var_v1 = v1 - 1 | 0;
              $copy_v2 = v2.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List (line 513, column 3 - line 513, column 35): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
    };
    return go2(Nil.value);
  }();
  var zipWith = function(f) {
    return function(xs) {
      return function(ys) {
        var go2 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return v2;
                }
                ;
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v2;
                }
                ;
                if (v instanceof Cons && v1 instanceof Cons) {
                  $tco_var_v = v.value1;
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new Cons(f(v.value0)(v1.value0), v2);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List (line 779, column 3 - line 779, column 21): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        return reverse(go2(xs)(ys)(Nil.value));
      };
    };
  };
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var manyRec = function(dictMonadRec) {
    var bind113 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt11 = alt(Alt0);
      var map119 = map(Alt0.Functor0());
      var pure39 = pure(dictAlternative.Applicative0());
      return function(p2) {
        var go2 = function(acc) {
          return bind113(alt11(map119(Loop.create)(p2))(pure39(new Done(unit))))(function(aa) {
            return pure39(bimap2(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse(acc);
            })(aa));
          });
        };
        return tailRecM5(go2)(Nil.value);
      };
    };
  };
  var findIndex = function(fn) {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons) {
            if (fn(v1.value0)) {
              $tco_done = true;
              return new Just(v);
            }
            ;
            if (otherwise) {
              $tco_var_v = v + 1 | 0;
              $copy_v1 = v1.value1;
              return;
            }
            ;
          }
          ;
          if (v1 instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 301, column 3 - line 301, column 35): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(0);
  };
  var filter = function(p2) {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return reverse(v);
          }
          ;
          if (v1 instanceof Cons) {
            if (p2(v1.value0)) {
              $tco_var_v = new Cons(v1.value0, v);
              $copy_v1 = v1.value1;
              return;
            }
            ;
            if (otherwise) {
              $tco_var_v = v;
              $copy_v1 = v1.value1;
              return;
            }
            ;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 390, column 3 - line 390, column 27): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  };
  var elemIndex = function(dictEq) {
    var eq211 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq211(v)(x);
      });
    };
  };
  var drop = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v < 1) {
          $tco_done = true;
          return v1;
        }
        ;
        if (v1 instanceof Nil) {
          $tco_done = true;
          return Nil.value;
        }
        ;
        if (v1 instanceof Cons) {
          $tco_var_v = v - 1 | 0;
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 536, column 1 - line 536, column 42): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var concatMap = /* @__PURE__ */ flip(bind2);
  var concat = function(v) {
    return bind2(v)(identity6);
  };

  // output/Data.Map.Internal/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  }();
  var IterLeaf = /* @__PURE__ */ function() {
    function IterLeaf2() {
    }
    ;
    IterLeaf2.value = new IterLeaf2();
    return IterLeaf2;
  }();
  var IterEmit = /* @__PURE__ */ function() {
    function IterEmit2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    IterEmit2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new IterEmit2(value0, value1, value22);
        };
      };
    };
    return IterEmit2;
  }();
  var IterNode = /* @__PURE__ */ function() {
    function IterNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    IterNode2.create = function(value0) {
      return function(value1) {
        return new IterNode2(value0, value1);
      };
    };
    return IterNode2;
  }();
  var IterDone = /* @__PURE__ */ function() {
    function IterDone2() {
    }
    ;
    IterDone2.value = new IterDone2();
    return IterDone2;
  }();
  var IterNext = /* @__PURE__ */ function() {
    function IterNext2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    IterNext2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new IterNext2(value0, value1, value22);
        };
      };
    };
    return IterNext2;
  }();
  var Split = /* @__PURE__ */ function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  }();
  var SplitLast = /* @__PURE__ */ function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  }();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + function() {
          var $280 = l.value0 > r.value0;
          if ($280) {
            return l.value0;
          }
          ;
          return r.value0;
        }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
  };
  var toMapIter = /* @__PURE__ */ function() {
    return flip(IterNode.create)(IterLeaf.value);
  }();
  var stepWith = function(f) {
    return function(next) {
      return function(done) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v instanceof IterLeaf) {
              $tco_done = true;
              return done(unit);
            }
            ;
            if (v instanceof IterEmit) {
              $tco_done = true;
              return next(v.value0, v.value1, v.value2);
            }
            ;
            if (v instanceof IterNode) {
              $copy_v = f(v.value1)(v.value0);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 940, column 8 - line 946, column 20): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2;
      };
    };
  };
  var singleton4 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height8 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height8(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height8(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton4(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
    };
  }();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy2("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(793)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(796)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 791, column 5 - line 799, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 787, column 34 - line 799, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(786);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy2("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(779)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 776, column 37 - line 780, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(775);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 764, column 25 - line 768, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy2("unsafeUnionWith", "Data.Map.Internal", function() {
    return function(comp, app, l, r) {
      if (l instanceof Leaf) {
        return r;
      }
      ;
      if (r instanceof Leaf) {
        return l;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeUnionWith(809)(comp, app, v.value1, r.value4);
        var r$prime = $lazy_unsafeUnionWith(810)(comp, app, v.value2, r.value5);
        if (v.value0 instanceof Just) {
          return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
        }
        ;
        if (v.value0 instanceof Nothing) {
          return unsafeBalancedNode(r.value2, r.value3, l$prime, r$prime);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 811, column 5 - line 815, column 46): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 804, column 42 - line 815, column 46): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeUnionWith = /* @__PURE__ */ $lazy_unsafeUnionWith(803);
  var unionWith = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeUnionWith(compare5, app, m1, m2);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var member = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return false;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare5(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return true;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 7 - line 462, column 19): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 456, column 8 - line 462, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var lookup = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare5(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var iterMapL = /* @__PURE__ */ function() {
    var go2 = function($copy_iter) {
      return function($copy_v) {
        var $tco_var_iter = $copy_iter;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(iter, v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return iter;
          }
          ;
          if (v instanceof Node) {
            if (v.value5 instanceof Leaf) {
              $tco_var_iter = new IterEmit(v.value2, v.value3, iter);
              $copy_v = v.value4;
              return;
            }
            ;
            $tco_var_iter = new IterEmit(v.value2, v.value3, new IterNode(v.value5, iter));
            $copy_v = v.value4;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 951, column 13 - line 958, column 48): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_iter, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2;
  }();
  var stepAscCps = /* @__PURE__ */ stepWith(iterMapL);
  var stepAsc = /* @__PURE__ */ function() {
    return stepAscCps(function(k, v, next) {
      return new IterNext(k, v, next);
    })($$const(IterDone.value));
  }();
  var eqMapIter = function(dictEq) {
    var eq112 = eq(dictEq);
    return function(dictEq1) {
      var eq211 = eq(dictEq1);
      return {
        eq: /* @__PURE__ */ function() {
          var go2 = function($copy_a) {
            return function($copy_b) {
              var $tco_var_a = $copy_a;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(a2, b2) {
                var v = stepAsc(a2);
                if (v instanceof IterNext) {
                  var v2 = stepAsc(b2);
                  if (v2 instanceof IterNext && (eq112(v.value0)(v2.value0) && eq211(v.value1)(v2.value1))) {
                    $tco_var_a = v.value2;
                    $copy_b = v2.value2;
                    return;
                  }
                  ;
                  $tco_done = true;
                  return false;
                }
                ;
                if (v instanceof IterDone) {
                  $tco_done = true;
                  return true;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 859, column 14 - line 868, column 13): " + [v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_a, $copy_b);
              }
              ;
              return $tco_result;
            };
          };
          return go2;
        }()
      };
    };
  };
  var stepUnfoldr = /* @__PURE__ */ function() {
    var step4 = function(k, v, next) {
      return new Just(new Tuple(new Tuple(k, v), next));
    };
    return stepAscCps(step4)(function(v) {
      return Nothing.value;
    });
  }();
  var toUnfoldable2 = function(dictUnfoldable) {
    var $784 = unfoldr(dictUnfoldable)(stepUnfoldr);
    return function($785) {
      return $784(toMapIter($785));
    };
  };
  var isEmpty = function(v) {
    if (v instanceof Leaf) {
      return true;
    }
    ;
    return false;
  };
  var insert = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton4(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare5(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var functorMap = {
    map: function(f) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          return new Node(v.value0, v.value1, v.value2, f(v.value3), go2(v.value4), go2(v.value5));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 10 - line 150, column 39): " + [v.constructor.name]);
      };
      return go2;
    }
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy2("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(172)(m$prime.value4, f(m$prime.value3)($lazy_go(172)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 169, column 26 - line 172, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(169);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy2("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(178)(f($lazy_go(178)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 175, column 26 - line 178, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(175);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append13 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append13(go2(v.value4))(append13(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy2("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(192)(m$prime.value4, f(m$prime.value2)(m$prime.value3)($lazy_go(192)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 189, column 26 - line 192, column 45): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(189);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy2("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(198)(f(m$prime.value2)($lazy_go(198)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 195, column 26 - line 198, column 45): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(195);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append13 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append13(go2(v.value4))(append13(f(v.value2)(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 201, column 10 - line 204, column 30): " + [v.constructor.name]);
        };
        return go2;
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var keys = /* @__PURE__ */ function() {
    return foldrWithIndex(foldableWithIndexMap)(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  }();
  var traversableMap = {
    traverse: function(dictApplicative) {
      var pure39 = pure(dictApplicative);
      var Apply0 = dictApplicative.Apply0();
      var apply3 = apply(Apply0);
      var map119 = map(Apply0.Functor0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return pure39(Leaf.value);
          }
          ;
          if (v instanceof Node) {
            return apply3(apply3(map119(function(l$prime) {
              return function(v$prime) {
                return function(r$prime) {
                  return new Node(v.value0, v.value1, v.value2, v$prime, l$prime, r$prime);
                };
              };
            })(go2(v.value4)))(f(v.value3)))(go2(v.value5));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 209, column 10 - line 215, column 19): " + [v.constructor.name]);
        };
        return go2;
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableMap)(dictApplicative)(identity7);
    },
    Functor0: function() {
      return functorMap;
    },
    Foldable1: function() {
      return foldableMap;
    }
  };
  var eqMap = function(dictEq) {
    var eqMapIter1 = eqMapIter(dictEq);
    return function(dictEq1) {
      var eq112 = eq(eqMapIter1(dictEq1));
      return {
        eq: function(xs) {
          return function(ys) {
            if (xs instanceof Leaf) {
              if (ys instanceof Leaf) {
                return true;
              }
              ;
              return false;
            }
            ;
            if (xs instanceof Node) {
              if (ys instanceof Node && xs.value1 === ys.value1) {
                return eq112(toMapIter(xs))(toMapIter(ys));
              }
              ;
              return false;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 94, column 14 - line 105, column 16): " + [xs.constructor.name]);
          };
        }
      };
    };
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable = function(dictOrd) {
    var insert15 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert15(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var unions = function(dictOrd) {
    var union1 = union(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(union1)(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare5(k)(v.value2);
          if (v1 instanceof LT) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
          }
          ;
          if (v1 instanceof GT) {
            return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
          }
          ;
          if (v1 instanceof EQ) {
            return unsafeJoinNodes(v.value4, v.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 7 - line 501, column 43): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 8 - line 501, column 43): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var alter = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare5, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 514, column 3 - line 518, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Data.Set/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var foldr4 = /* @__PURE__ */ foldr(foldableList);
  var union2 = function(dictOrd) {
    return coerce3(union(dictOrd));
  };
  var toList = function(v) {
    return keys(v);
  };
  var toUnfoldable3 = function(dictUnfoldable) {
    var $96 = toUnfoldable(dictUnfoldable);
    return function($97) {
      return $96(toList($97));
    };
  };
  var singleton5 = function(a2) {
    return singleton4(a2)(unit);
  };
  var member2 = function(dictOrd) {
    return coerce3(member(dictOrd));
  };
  var insert2 = function(dictOrd) {
    var insert15 = insert(dictOrd);
    return function(a2) {
      return function(v) {
        return insert15(a2)(unit)(v);
      };
    };
  };
  var foldableSet = {
    foldMap: function(dictMonoid) {
      var foldMap12 = foldMap2(dictMonoid);
      return function(f) {
        var $98 = foldMap12(f);
        return function($99) {
          return $98(toList($99));
        };
      };
    },
    foldl: function(f) {
      return function(x) {
        var $100 = foldl3(f)(x);
        return function($101) {
          return $100(toList($101));
        };
      };
    },
    foldr: function(f) {
      return function(x) {
        var $102 = foldr4(f)(x);
        return function($103) {
          return $102(toList($103));
        };
      };
    }
  };
  var eqSet = function(dictEq) {
    var eq31 = eq(eqMap(dictEq)(eqUnit));
    return {
      eq: function(v) {
        return function(v1) {
          return eq31(v)(v1);
        };
      }
    };
  };
  var empty3 = empty2;
  var fromFoldable2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert15 = insert2(dictOrd);
      return foldl22(function(m) {
        return function(a2) {
          return insert15(a2)(m);
        };
      })(empty3);
    };
  };
  var unions2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      return foldl22(union2(dictOrd))(empty3);
    };
  };
  var $$delete2 = function(dictOrd) {
    return coerce3($$delete(dictOrd));
  };

  // output/Common/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var show2 = /* @__PURE__ */ show(showInt);
  var compare2 = /* @__PURE__ */ compare(ordString);
  var Xtorname = /* @__PURE__ */ function() {
    function Xtorname2(value0) {
      this.value0 = value0;
    }
    ;
    Xtorname2.create = function(value0) {
      return new Xtorname2(value0);
    };
    return Xtorname2;
  }();
  var Covariant = /* @__PURE__ */ function() {
    function Covariant2() {
    }
    ;
    Covariant2.value = new Covariant2();
    return Covariant2;
  }();
  var Contravariant = /* @__PURE__ */ function() {
    function Contravariant2() {
    }
    ;
    Contravariant2.value = new Contravariant2();
    return Contravariant2;
  }();
  var Variable = /* @__PURE__ */ function() {
    function Variable2(value0) {
      this.value0 = value0;
    }
    ;
    Variable2.create = function(value0) {
      return new Variable2(value0);
    };
    return Variable2;
  }();
  var Typevar = /* @__PURE__ */ function() {
    function Typevar2(value0) {
      this.value0 = value0;
    }
    ;
    Typevar2.create = function(value0) {
      return new Typevar2(value0);
    };
    return Typevar2;
  }();
  var VariantVar = /* @__PURE__ */ function() {
    function VariantVar2(value0) {
      this.value0 = value0;
    }
    ;
    VariantVar2.create = function(value0) {
      return new VariantVar2(value0);
    };
    return VariantVar2;
  }();
  var Typename = /* @__PURE__ */ function() {
    function Typename2(value0) {
      this.value0 = value0;
    }
    ;
    Typename2.create = function(value0) {
      return new Typename2(value0);
    };
    return Typename2;
  }();
  var Modulename = /* @__PURE__ */ function() {
    function Modulename2(value0) {
      this.value0 = value0;
    }
    ;
    Modulename2.create = function(value0) {
      return new Modulename2(value0);
    };
    return Modulename2;
  }();
  var CBV = /* @__PURE__ */ function() {
    function CBV2() {
    }
    ;
    CBV2.value = new CBV2();
    return CBV2;
  }();
  var CBN = /* @__PURE__ */ function() {
    function CBN2() {
    }
    ;
    CBN2.value = new CBN2();
    return CBN2;
  }();
  var MkKind = /* @__PURE__ */ function() {
    function MkKind2(value0) {
      this.value0 = value0;
    }
    ;
    MkKind2.create = function(value0) {
      return new MkKind2(value0);
    };
    return MkKind2;
  }();
  var MkKindVar = /* @__PURE__ */ function() {
    function MkKindVar2(value0) {
      this.value0 = value0;
    }
    ;
    MkKindVar2.create = function(value0) {
      return new MkKindVar2(value0);
    };
    return MkKindVar2;
  }();
  var Data = /* @__PURE__ */ function() {
    function Data2() {
    }
    ;
    Data2.value = new Data2();
    return Data2;
  }();
  var Codata = /* @__PURE__ */ function() {
    function Codata2() {
    }
    ;
    Codata2.value = new Codata2();
    return Codata2;
  }();
  var showXtorname = {
    show: function(v) {
      return v.value0;
    }
  };
  var showVariance = {
    show: function(v) {
      if (v instanceof Covariant) {
        return "+";
      }
      ;
      if (v instanceof Contravariant) {
        return "-";
      }
      ;
      throw new Error("Failed pattern match at Common (line 98, column 1 - line 100, column 27): " + [v.constructor.name]);
    }
  };
  var show1 = /* @__PURE__ */ show(showVariance);
  var showVariable = {
    show: function(v) {
      return v.value0;
    }
  };
  var showTypevar = {
    show: function(v) {
      return v.value0;
    }
  };
  var show22 = /* @__PURE__ */ show(showTypevar);
  var showVariantVar = {
    show: function(v) {
      return show1(v.value0.variantVariance) + (":" + show22(v.value0.variantVar));
    }
  };
  var showTypename = {
    show: function(v) {
      return v.value0;
    }
  };
  var showModulename = {
    show: function(v) {
      return v.value0;
    }
  };
  var showKindvar = {
    show: function(v) {
      return v.value0;
    }
  };
  var show3 = /* @__PURE__ */ show(showKindvar);
  var showEvaluationOrder = {
    show: function(v) {
      if (v instanceof CBV) {
        return "CBV";
      }
      ;
      if (v instanceof CBN) {
        return "CBN";
      }
      ;
      throw new Error("Failed pattern match at Common (line 114, column 1 - line 116, column 19): " + [v.constructor.name]);
    }
  };
  var show4 = /* @__PURE__ */ show(showEvaluationOrder);
  var showKind = {
    show: function(v) {
      if (v instanceof MkKind) {
        return show4(v.value0);
      }
      ;
      if (v instanceof MkKindVar) {
        return show3(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Common (line 121, column 1 - line 123, column 32): " + [v.constructor.name]);
    }
  };
  var showDeclTy = {
    show: function(v) {
      if (v instanceof Data) {
        return "data";
      }
      ;
      if (v instanceof Codata) {
        return "codata";
      }
      ;
      throw new Error("Failed pattern match at Common (line 156, column 1 - line 158, column 25): " + [v.constructor.name]);
    }
  };
  var shiftEvalOrderEvaluationO = {
    shiftEvalOrder: function(v) {
      if (v instanceof CBV) {
        return CBN.value;
      }
      ;
      if (v instanceof CBN) {
        return CBV.value;
      }
      ;
      throw new Error("Failed pattern match at Common (line 127, column 1 - line 129, column 27): " + [v.constructor.name]);
    }
  };
  var shiftEvalOrder = function(dict) {
    return dict.shiftEvalOrder;
  };
  var shiftEvalOrder1 = /* @__PURE__ */ shiftEvalOrder(shiftEvalOrderEvaluationO);
  var varianceEvalOrder = function(v) {
    if (v instanceof Covariant) {
      return identity8;
    }
    ;
    if (v instanceof Contravariant) {
      return shiftEvalOrder1;
    }
    ;
    throw new Error("Failed pattern match at Common (line 146, column 1 - line 146, column 68): " + [v.constructor.name]);
  };
  var shiftEvalOrderKind = {
    shiftEvalOrder: function(v) {
      if (v instanceof MkKind) {
        return new MkKind(shiftEvalOrder1(v.value0));
      }
      ;
      if (v instanceof MkKindVar) {
        return v;
      }
      ;
      throw new Error("Failed pattern match at Common (line 131, column 1 - line 133, column 38): " + [v.constructor.name]);
    }
  };
  var freshVarN = function(dictEq) {
    return function(dictOrd) {
      var member3 = member2(dictOrd);
      return function(n) {
        return function(prefix) {
          return function(ctor) {
            return function(vars) {
              var newV = ctor(prefix + show2(n));
              var $169 = member3(newV)(vars);
              if ($169) {
                return freshVarN(dictEq)(dictOrd)(n + 1 | 0)(prefix)(ctor)(vars);
              }
              ;
              return newV;
            };
          };
        };
      };
    };
  };
  var eqXtorname = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var ordXtorname = {
    compare: function(x) {
      return function(y) {
        return compare2(x.value0)(y.value0);
      };
    },
    Eq0: function() {
      return eqXtorname;
    }
  };
  var eqVariable = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var ordVariable = {
    compare: function(x) {
      return function(y) {
        return compare2(x.value0)(y.value0);
      };
    },
    Eq0: function() {
      return eqVariable;
    }
  };
  var eqTypevar = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var ordTypevar = {
    compare: function(x) {
      return function(y) {
        return compare2(x.value0)(y.value0);
      };
    },
    Eq0: function() {
      return eqTypevar;
    }
  };
  var eqTypename = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var ordTypename = {
    compare: function(x) {
      return function(y) {
        return compare2(x.value0)(y.value0);
      };
    },
    Eq0: function() {
      return eqTypename;
    }
  };
  var eqModulename = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var ordModulename = {
    compare: function(x) {
      return function(y) {
        return compare2(x.value0)(y.value0);
      };
    },
    Eq0: function() {
      return eqModulename;
    }
  };
  var eqKindvar = {
    eq: function(x) {
      return function(y) {
        return x.value0 === y.value0;
      };
    }
  };
  var eq32 = /* @__PURE__ */ eq(eqKindvar);
  var eqEvaluationOrder = {
    eq: function(x) {
      return function(y) {
        if (x instanceof CBV && y instanceof CBV) {
          return true;
        }
        ;
        if (x instanceof CBN && y instanceof CBN) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq4 = /* @__PURE__ */ eq(eqEvaluationOrder);
  var eqKind = {
    eq: function(x) {
      return function(y) {
        if (x instanceof MkKind && y instanceof MkKind) {
          return eq4(x.value0)(y.value0);
        }
        ;
        if (x instanceof MkKindVar && y instanceof MkKindVar) {
          return eq32(x.value0)(y.value0);
        }
        ;
        return false;
      };
    }
  };
  var defaultEo = function(v) {
    if (v instanceof Data) {
      return CBV.value;
    }
    ;
    if (v instanceof Codata) {
      return CBN.value;
    }
    ;
    throw new Error("Failed pattern match at Common (line 160, column 1 - line 160, column 39): " + [v.constructor.name]);
  };

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map60 = map(Monad0.Bind1().Apply0().Functor0());
    var pure39 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map60(Right.create)(a2))(function($52) {
        return pure39(Left.create($52));
      });
    };
  };

  // output/Control.Monad.Reader.Class/index.js
  var ask = function(dict) {
    return dict.ask;
  };
  var asks = function(dictMonadAsk) {
    var map60 = map(dictMonadAsk.Monad0().Bind1().Apply0().Functor0());
    var ask1 = ask(dictMonadAsk);
    return function(f) {
      return map60(f)(ask1);
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var modify2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        var s$prime = f(s);
        return new Tuple(s$prime, s$prime);
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map6 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var monadTransExceptT = {
    lift: function(dictMonad) {
      var bind36 = bind(dictMonad.Bind1());
      var pure39 = pure(dictMonad.Applicative0());
      return function(m) {
        return bind36(m)(function(a2) {
          return pure39(new Right(a2));
        });
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransExceptT);
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map119 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map119(map6(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind36 = bind(dictMonad.Bind1());
    var pure39 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind36(v)(either(function($187) {
            return pure39(Left.create($187));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadStateExceptT = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var lift1 = lift3(Monad0);
    var state3 = state(dictMonadState);
    var monadExceptT1 = monadExceptT(Monad0);
    return {
      state: function(f) {
        return lift1(state3(f));
      },
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var monadErrorExceptT = function(dictMonad) {
    var bind36 = bind(dictMonad.Bind1());
    var pure39 = pure(dictMonad.Applicative0());
    var monadThrowExceptT1 = monadThrowExceptT(dictMonad);
    return {
      catchError: function(v) {
        return function(k) {
          return bind36(v)(either(function(a2) {
            var v1 = k(a2);
            return v1;
          })(function($200) {
            return pure39(Right.create($200));
          }));
        };
      },
      MonadThrow0: function() {
        return monadThrowExceptT1;
      }
    };
  };

  // output/Control.Monad.State.Trans/index.js
  var runStateT = function(v) {
    return v;
  };
  var monadTransStateT = {
    lift: function(dictMonad) {
      var bind36 = bind(dictMonad.Bind1());
      var pure39 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind36(m)(function(x) {
            return pure39(new Tuple(x, s));
          });
        };
      };
    }
  };
  var lift4 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map60 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map60(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind36 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind36(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure39 = pure(dictMonad.Applicative0());
    return {
      pure: function(a2) {
        return function(s) {
          return pure39(new Tuple(a2, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure39 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure39(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };
  var monadThrowStateT = function(dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var lift1 = lift4(Monad0);
    var throwError22 = throwError(dictMonadThrow);
    var monadStateT1 = monadStateT(Monad0);
    return {
      throwError: function(e) {
        return lift1(throwError22(e));
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };
  var monadErrorStateT = function(dictMonadError) {
    var catchError2 = catchError(dictMonadError);
    var monadThrowStateT1 = monadThrowStateT(dictMonadError.MonadThrow0());
    return {
      catchError: function(v) {
        return function(h) {
          return function(s) {
            return catchError2(v(s))(function(e) {
              var v1 = h(e);
              return v1(s);
            });
          };
        };
      },
      MonadThrow0: function() {
        return monadThrowStateT1;
      }
    };
  };

  // output/Control.Monad.State/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var runState = function(v) {
    return function($18) {
      return unwrap2(v($18));
    };
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value14) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value14);
  };
  var replicatePolyfill = function(count, value14) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value14;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var length = function(xs) {
    return xs.length;
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i2 = 0, l = xs.length; i2 < l; i2++) {
      if (f(xs[i2]))
        return just(i2);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length)
      return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var unsafeIndexImpl = function(xs, n) {
    return xs[n];
  };

  // output/Data.Function.Uncurried/foreign.js
  var mkFn5 = function(fn) {
    return function(a2, b2, c, d, e) {
      return fn(a2)(b2)(c)(d)(e);
    };
  };
  var runFn2 = function(fn) {
    return function(a2) {
      return function(b2) {
        return fn(a2, b2);
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Data.Array/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var unsafeIndex = function() {
    return runFn2(unsafeIndexImpl);
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var toUnfoldable4 = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(xs) {
      var len = length(xs);
      var f = function(i2) {
        if (i2 < len) {
          return new Just(new Tuple(unsafeIndex1(xs)(i2), i2 + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 163, column 3 - line 165, column 26): " + [i2.constructor.name]);
      };
      return unfoldr3(f)(0);
    };
  };
  var findIndex2 = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var elemIndex2 = function(dictEq) {
    var eq211 = eq(dictEq);
    return function(x) {
      return findIndex2(function(v) {
        return eq211(v)(x);
      });
    };
  };
  var notElem2 = function(dictEq) {
    var elemIndex12 = elemIndex2(dictEq);
    return function(a2) {
      return function(arr) {
        return isNothing(elemIndex12(a2)(arr));
      };
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex2(v(v1))(v2));
      };
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index4) {
            return function(str) {
              var length9 = str.length;
              if (index4 < 0 || index4 >= length9)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i2 = index4; ; --i2) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i2 === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index4)(str);
            };
          };
        };
      };
    };
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom22);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Data.String.CodeUnits/foreign.js
  var singleton6 = function(c) {
    return c;
  };
  var length2 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x) {
        return function(s) {
          var i2 = s.indexOf(x);
          return i2 === -1 ? nothing : just(i2);
        };
      };
    };
  };
  var take2 = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length)
        return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length2(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };
  var indexOf = /* @__PURE__ */ function() {
    return _indexOf(Just.create)(Nothing.value);
  }();

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toUpper = function(s) {
    return s.toUpperCase();
  };
  var trim = function(s) {
    return s.trim();
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map7 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare3 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons2 = function(s) {
    var v = length2(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map7(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons2(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length2(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length3 = function($74) {
    return length(toCodePointArray($74));
  };
  var indexOf2 = function(p2) {
    return function(s) {
      return map7(function(i2) {
        return length3(take2(i2)(s));
      })(indexOf(p2)(s));
    };
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons2(v1);
      if (v2 instanceof Just) {
        return singleton7(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take3 = /* @__PURE__ */ _take(takeFallback);
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare3(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var drop3 = function(n) {
    return function(s) {
      return drop2(length2(take3(n)(s)))(s);
    };
  };
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum2($77));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons2(s);
        if (v instanceof Just) {
          var $66 = n === 0;
          if ($66) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy3("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Loc/index.js
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable4(unfoldableList);
  var show5 = /* @__PURE__ */ show(showInt);
  var showLocInSource = function(src9) {
    return function(loc) {
      var betweenString = function(start2) {
        return function(end) {
          return function(str) {
            return take3(end)(drop3(start2)(str));
          };
        };
      };
      var betweenList = function(start2) {
        return function(end) {
          return function(ls) {
            return take(end)(drop(start2)(ls));
          };
        };
      };
      var srcLines = toUnfoldable5(split("\n")(src9));
      var posLines = betweenList(loc.locStart.srcLine - 1 | 0)(loc.locEnd.srcCol)(srcLines);
      if (posLines instanceof Nil) {
        return "";
      }
      ;
      if (posLines instanceof Cons) {
        var msg = betweenString(loc.locStart.srcCol - 2 | 0)(loc.locEnd.srcCol + 2 | 0)(posLines.value0);
        var startMsg = "line " + (show5(loc.locStart.srcLine) + (", " + show5(loc.locStart.srcCol)));
        var endMsg = "line " + (show5(loc.locEnd.srcLine) + (", " + show5(loc.locEnd.srcCol)));
        return msg + ("\n" + (startMsg + ("-" + endMsg)));
      }
      ;
      throw new Error("Failed pattern match at Loc (line 31, column 3 - line 38, column 47): " + [posLines.constructor.name]);
    };
  };
  var setLoc = function(dict) {
    return dict.setLoc;
  };
  var getLoc = function(dict) {
    return dict.getLoc;
  };
  var defaultPos = {
    srcLine: 0,
    srcCol: 0
  };
  var defaultLoc = {
    locStart: defaultPos,
    locEnd: defaultPos
  };

  // output/Errors/index.js
  var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
  var eqRec2 = /* @__PURE__ */ eqRec();
  var eqRowCons2 = /* @__PURE__ */ eqRowCons(eqRowNil)();
  var locStartIsSymbol = {
    reflectSymbol: function() {
      return "locStart";
    }
  };
  var srcLineIsSymbol = {
    reflectSymbol: function() {
      return "srcLine";
    }
  };
  var srcColIsSymbol = {
    reflectSymbol: function() {
      return "srcCol";
    }
  };
  var eqRec1 = /* @__PURE__ */ eqRec2(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons2(srcLineIsSymbol)(eqInt))()(srcColIsSymbol)(eqInt));
  var locEndIsSymbol = {
    reflectSymbol: function() {
      return "locEnd";
    }
  };
  var eq5 = /* @__PURE__ */ eq(/* @__PURE__ */ eqRec2(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons2(locStartIsSymbol)(eqRec1))()(locEndIsSymbol)(eqRec1)));
  var zipWithError = function(dictError) {
    return function(v) {
      return function(v1) {
        return function(v2) {
          if (v instanceof Nil && v1 instanceof Nil) {
            return new Left(Nil.value);
          }
          ;
          if (v instanceof Nil && v1 instanceof Cons) {
            return new Right(v2);
          }
          ;
          if (v instanceof Cons && v1 instanceof Nil) {
            return new Right(v2);
          }
          ;
          if (v instanceof Cons && v1 instanceof Cons) {
            var rsZipped = zipWithError(dictError)(v.value1)(v1.value1)(v2);
            var leftFun = function(ls) {
              return new Cons(new Tuple(v.value0, v1.value0), ls);
            };
            return lmap2(leftFun)(rsZipped);
          }
          ;
          throw new Error("Failed pattern match at Errors (line 28, column 1 - line 28, column 93): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    };
  };
  var zipWithErrorM = function(dictError) {
    var zipWithError1 = zipWithError(dictError);
    return function(dictMonadError) {
      var MonadThrow0 = dictMonadError.MonadThrow0();
      var pure39 = pure(MonadThrow0.Monad0().Applicative0());
      var throwError22 = throwError(MonadThrow0);
      return function(as) {
        return function(bs) {
          return function(err) {
            var v = zipWithError1(as)(bs)(err);
            if (v instanceof Left) {
              return pure39(v.value0);
            }
            ;
            if (v instanceof Right) {
              return throwError22(err);
            }
            ;
            throw new Error("Failed pattern match at Errors (line 38, column 27 - line 40, column 28): " + [v.constructor.name]);
          };
        };
      };
    };
  };
  var toError = function(dict) {
    return dict.toError;
  };
  var getMessage = function(dict) {
    return dict.getMessage;
  };
  var getLocation = function(dict) {
    return dict.getLocation;
  };
  var showInSrc = function(dictError) {
    var getMessage1 = getMessage(dictError);
    var getLocation1 = getLocation(dictError);
    return function(e) {
      return function(src9) {
        var msg = getMessage1(e);
        var $80 = eq5(getLocation1(e))(defaultLoc);
        if ($80) {
          return msg;
        }
        ;
        return msg + ('\nAt: "' + (showLocInSource(src9)(getLocation1(e)) + '"'));
      };
    };
  };
  var convertError = function(dictError) {
    var getLocation1 = getLocation(dictError);
    var getMessage1 = getMessage(dictError);
    return function(dictError1) {
      var toError1 = toError(dictError1);
      return function(e) {
        return toError1(getLocation1(e))(getMessage1(e));
      };
    };
  };

  // output/Syntax.Desugared.Types/index.js
  var show6 = /* @__PURE__ */ show(showTypevar);
  var show12 = /* @__PURE__ */ show(showTypename);
  var intercalate3 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map8 = /* @__PURE__ */ map(functorList);
  var eq6 = /* @__PURE__ */ eq(eqTypevar);
  var eq13 = /* @__PURE__ */ eq(eqTypename);
  var eq22 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqTypevar));
  var TyVar = /* @__PURE__ */ function() {
    function TyVar5(value0) {
      this.value0 = value0;
    }
    ;
    TyVar5.create = function(value0) {
      return new TyVar5(value0);
    };
    return TyVar5;
  }();
  var TyDecl = /* @__PURE__ */ function() {
    function TyDecl5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyDecl5.create = function(value0) {
      return function(value1) {
        return new TyDecl5(value0, value1);
      };
    };
    return TyDecl5;
  }();
  var TyCo = /* @__PURE__ */ function() {
    function TyCo5(value0) {
      this.value0 = value0;
    }
    ;
    TyCo5.create = function(value0) {
      return new TyCo5(value0);
    };
    return TyCo5;
  }();
  var TyShift = /* @__PURE__ */ function() {
    function TyShift5(value0) {
      this.value0 = value0;
    }
    ;
    TyShift5.create = function(value0) {
      return new TyShift5(value0);
    };
    return TyShift5;
  }();
  var TyForall = /* @__PURE__ */ function() {
    function TyForall5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyForall5.create = function(value0) {
      return function(value1) {
        return new TyForall5(value0, value1);
      };
    };
    return TyForall5;
  }();
  var showTy = {
    show: function(v) {
      if (v instanceof TyVar) {
        return show6(v.value0);
      }
      ;
      if (v instanceof TyDecl && $$null(v.value1)) {
        return show12(v.value0);
      }
      ;
      if (v instanceof TyDecl) {
        return show12(v.value0) + ("(" + (intercalate3(", ")(map8(show(showTy))(v.value1)) + ")"));
      }
      ;
      if (v instanceof TyCo) {
        return "co " + show(showTy)(v.value0);
      }
      ;
      if (v instanceof TyShift) {
        return "{ " + (show(showTy)(v.value0) + " }");
      }
      ;
      if (v instanceof TyForall) {
        return "forall " + (intercalate3(" ")(map8(show6)(v.value0)) + (". " + show(showTy)(v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Desugared.Types (line 19, column 1 - line 25, column 92): " + [v.constructor.name]);
    }
  };
  var eqTy = {
    eq: function(x) {
      return function(y) {
        if (x instanceof TyVar && y instanceof TyVar) {
          return eq6(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyDecl && y instanceof TyDecl) {
          return eq13(x.value0)(y.value0) && eq(eqList(eqTy))(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyCo && y instanceof TyCo) {
          return eq(eqTy)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyShift && y instanceof TyShift) {
          return eq(eqTy)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyForall && y instanceof TyForall) {
          return eq22(x.value0)(y.value0) && eq(eqTy)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };

  // output/Syntax.Typed.Types/index.js
  var show7 = /* @__PURE__ */ show(showTypevar);
  var show13 = /* @__PURE__ */ show(showTypename);
  var intercalate4 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map9 = /* @__PURE__ */ map(functorList);
  var eq7 = /* @__PURE__ */ eq(eqTypevar);
  var eq14 = /* @__PURE__ */ eq(eqTypename);
  var eq23 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqTypevar));
  var foldr5 = /* @__PURE__ */ foldr(foldableList);
  var TyVar2 = /* @__PURE__ */ function() {
    function TyVar5(value0) {
      this.value0 = value0;
    }
    ;
    TyVar5.create = function(value0) {
      return new TyVar5(value0);
    };
    return TyVar5;
  }();
  var TyDecl2 = /* @__PURE__ */ function() {
    function TyDecl5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyDecl5.create = function(value0) {
      return function(value1) {
        return new TyDecl5(value0, value1);
      };
    };
    return TyDecl5;
  }();
  var TyShift2 = /* @__PURE__ */ function() {
    function TyShift5(value0) {
      this.value0 = value0;
    }
    ;
    TyShift5.create = function(value0) {
      return new TyShift5(value0);
    };
    return TyShift5;
  }();
  var TyCo2 = /* @__PURE__ */ function() {
    function TyCo5(value0) {
      this.value0 = value0;
    }
    ;
    TyCo5.create = function(value0) {
      return new TyCo5(value0);
    };
    return TyCo5;
  }();
  var TyForall2 = /* @__PURE__ */ function() {
    function TyForall5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyForall5.create = function(value0) {
      return function(value1) {
        return new TyForall5(value0, value1);
      };
    };
    return TyForall5;
  }();
  var showTy2 = {
    show: function(v) {
      if (v instanceof TyVar2) {
        return show7(v.value0);
      }
      ;
      if (v instanceof TyDecl2 && $$null(v.value1)) {
        return show13(v.value0);
      }
      ;
      if (v instanceof TyDecl2) {
        return show13(v.value0) + (" (" + (intercalate4(", ")(map9(show(showTy2))(v.value1)) + ")"));
      }
      ;
      if (v instanceof TyShift2) {
        return "{" + (show(showTy2)(v.value0) + "}");
      }
      ;
      if (v instanceof TyCo2) {
        return "co " + show(showTy2)(v.value0);
      }
      ;
      if (v instanceof TyForall2 && $$null(v.value0)) {
        return show(showTy2)(v.value1);
      }
      ;
      if (v instanceof TyForall2) {
        return "forall " + (intercalate4(" ")(map9(show7)(v.value0)) + (". " + show(showTy2)(v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Typed.Types (line 22, column 1 - line 29, column 92): " + [v.constructor.name]);
    }
  };
  var eqTy2 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof TyVar2 && y instanceof TyVar2) {
          return eq7(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyDecl2 && y instanceof TyDecl2) {
          return eq14(x.value0)(y.value0) && eq(eqList(eqTy2))(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyShift2 && y instanceof TyShift2) {
          return eq(eqTy2)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyCo2 && y instanceof TyCo2) {
          return eq(eqTy2)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyForall2 && y instanceof TyForall2) {
          return eq23(x.value0)(y.value0) && eq(eqTy2)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var eq42 = /* @__PURE__ */ eq(eqTy2);
  var isSubsumed = function(v) {
    return function(v1) {
      if (eq42(v)(v1)) {
        return true;
      }
      ;
      if (v1 instanceof TyForall2) {
        return isSubsumed(v)(v1.value1);
      }
      ;
      if (v instanceof TyForall2) {
        return isSubsumed(v.value1)(v1);
      }
      ;
      if (v1 instanceof TyVar2) {
        return true;
      }
      ;
      if (v instanceof TyDecl2 && v1 instanceof TyDecl2) {
        return eq14(v.value0)(v1.value0) && foldr5(function(v2) {
          return function(b2) {
            return b2 && isSubsumed(v2.value0)(v2.value1);
          };
        })(true)(zip(v.value1)(v1.value1));
      }
      ;
      if (v instanceof TyShift2) {
        return isSubsumed(v.value0)(v1);
      }
      ;
      if (v1 instanceof TyShift2) {
        return isSubsumed(v)(v1.value0);
      }
      ;
      if (v instanceof TyCo2 && v1 instanceof TyCo2) {
        return isSubsumed(v.value0)(v1.value0);
      }
      ;
      return false;
    };
  };
  var embedType = function(v) {
    if (v instanceof TyVar2) {
      return new TyVar(v.value0);
    }
    ;
    if (v instanceof TyDecl2) {
      return new TyDecl(v.value0, map9(embedType)(v.value1));
    }
    ;
    if (v instanceof TyShift2) {
      return new TyShift(embedType(v.value0));
    }
    ;
    if (v instanceof TyCo2) {
      return new TyCo(embedType(v.value0));
    }
    ;
    if (v instanceof TyForall2) {
      return new TyForall(v.value0, embedType(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Syntax.Typed.Types (line 48, column 1 - line 48, column 24): " + [v.constructor.name]);
  };

  // output/Driver.Errors/index.js
  var show8 = /* @__PURE__ */ show(showModulename);
  var intercalate5 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map10 = /* @__PURE__ */ map(functorList);
  var show14 = /* @__PURE__ */ show(showTy2);
  var show23 = /* @__PURE__ */ show(showVariable);
  var ErrTypeInference = /* @__PURE__ */ function() {
    function ErrTypeInference2(value0) {
      this.value0 = value0;
    }
    ;
    ErrTypeInference2.create = function(value0) {
      return new ErrTypeInference2(value0);
    };
    return ErrTypeInference2;
  }();
  var ErrWithWhere = /* @__PURE__ */ function() {
    function ErrWithWhere2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrWithWhere2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrWithWhere2(value0, value1, value22);
        };
      };
    };
    return ErrWithWhere2;
  }();
  var ErrNotFound = /* @__PURE__ */ function() {
    function ErrNotFound2(value0) {
      this.value0 = value0;
    }
    ;
    ErrNotFound2.create = function(value0) {
      return new ErrNotFound2(value0);
    };
    return ErrNotFound2;
  }();
  var ErrNotStdLib = /* @__PURE__ */ function() {
    function ErrNotStdLib2(value0) {
      this.value0 = value0;
    }
    ;
    ErrNotStdLib2.create = function(value0) {
      return new ErrNotStdLib2(value0);
    };
    return ErrNotStdLib2;
  }();
  var ErrAnnotMismatch = /* @__PURE__ */ function() {
    function ErrAnnotMismatch2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrAnnotMismatch2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrAnnotMismatch2(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrAnnotMismatch2;
  }();
  var ErrOther = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorDriverError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrTypeInference) {
          return "Type Inference is not implemented yet";
        }
        ;
        if (v instanceof ErrWithWhere) {
          return getMessage(errorDriverError)(v.value0) + (" in module " + (show8(v.value1) + (" during " + v.value2)));
        }
        ;
        if (v instanceof ErrNotFound) {
          return "Could not find " + (show8(v.value0) + " in Environment");
        }
        ;
        if (v instanceof ErrNotStdLib) {
          return "Modules " + (intercalate5(", ")(map10(show8)(v.value0)) + " are not in standard library");
        }
        ;
        if (v instanceof ErrAnnotMismatch) {
          return "Type annotation " + (show14(v.value2) + (" for variable " + (show23(v.value1) + (" does not match inferred type " + show14(v.value3)))));
        }
        ;
        if (v instanceof ErrOther) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at Driver.Errors (line 19, column 1 - line 34, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrTypeInference) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther) {
          return v.value0;
        }
        ;
        if (v instanceof ErrNotFound) {
          return defaultLoc;
        }
        ;
        if (v instanceof ErrNotStdLib) {
          return defaultLoc;
        }
        ;
        if (v instanceof ErrWithWhere) {
          return getLocation(errorDriverError)(v.value0);
        }
        ;
        if (v instanceof ErrAnnotMismatch) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Driver.Errors (line 19, column 1 - line 34, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther.create
    };
  }();

  // output/Syntax.Kinded.Types/index.js
  var show9 = /* @__PURE__ */ show(showTypevar);
  var show15 = /* @__PURE__ */ show(showKind);
  var show24 = /* @__PURE__ */ show(showTypename);
  var intercalate6 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map11 = /* @__PURE__ */ map(functorList);
  var shiftEvalOrder2 = /* @__PURE__ */ shiftEvalOrder(shiftEvalOrderKind);
  var eq8 = /* @__PURE__ */ eq(eqTypevar);
  var eq15 = /* @__PURE__ */ eq(eqKind);
  var eq24 = /* @__PURE__ */ eq(eqTypename);
  var eq33 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqTypevar));
  var TyVar3 = /* @__PURE__ */ function() {
    function TyVar5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyVar5.create = function(value0) {
      return function(value1) {
        return new TyVar5(value0, value1);
      };
    };
    return TyVar5;
  }();
  var TyDecl3 = /* @__PURE__ */ function() {
    function TyDecl5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TyDecl5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TyDecl5(value0, value1, value22);
        };
      };
    };
    return TyDecl5;
  }();
  var TyShift3 = /* @__PURE__ */ function() {
    function TyShift5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyShift5.create = function(value0) {
      return function(value1) {
        return new TyShift5(value0, value1);
      };
    };
    return TyShift5;
  }();
  var TyCo3 = /* @__PURE__ */ function() {
    function TyCo5(value0) {
      this.value0 = value0;
    }
    ;
    TyCo5.create = function(value0) {
      return new TyCo5(value0);
    };
    return TyCo5;
  }();
  var TyForall3 = /* @__PURE__ */ function() {
    function TyForall5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyForall5.create = function(value0) {
      return function(value1) {
        return new TyForall5(value0, value1);
      };
    };
    return TyForall5;
  }();
  var showTy3 = {
    show: function(v) {
      if (v instanceof TyVar3) {
        return show9(v.value0) + (" : " + show15(v.value1));
      }
      ;
      if (v instanceof TyDecl3 && $$null(v.value1)) {
        return show24(v.value0) + (": " + show15(v.value2));
      }
      ;
      if (v instanceof TyDecl3) {
        return show24(v.value0) + ("(" + (intercalate6(", ")(map11(show(showTy3))(v.value1)) + (") :" + show15(v.value2))));
      }
      ;
      if (v instanceof TyShift3) {
        return "{" + (show(showTy3)(v.value0) + ("}" + (":" + show15(v.value1))));
      }
      ;
      if (v instanceof TyCo3) {
        return "co " + show(showTy3)(v.value0);
      }
      ;
      if (v instanceof TyForall3 && $$null(v.value0)) {
        return show(showTy3)(v.value1);
      }
      ;
      if (v instanceof TyForall3) {
        return "forall " + (intercalate6(", ")(map11(show9)(v.value0)) + (", " + show(showTy3)(v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Types (line 19, column 1 - line 26, column 93): " + [v.constructor.name]);
    }
  };
  var shiftEvalOrderTy = {
    shiftEvalOrder: function(v) {
      if (v instanceof TyVar3) {
        return new TyVar3(v.value0, shiftEvalOrder2(v.value1));
      }
      ;
      if (v instanceof TyDecl3) {
        return new TyDecl3(v.value0, map11(shiftEvalOrder(shiftEvalOrderTy))(v.value1), shiftEvalOrder2(v.value2));
      }
      ;
      if (v instanceof TyShift3) {
        return new TyShift3(shiftEvalOrder(shiftEvalOrderTy)(v.value0), shiftEvalOrder2(v.value1));
      }
      ;
      if (v instanceof TyCo3) {
        return new TyCo3(shiftEvalOrder(shiftEvalOrderTy)(v.value0));
      }
      ;
      if (v instanceof TyForall3) {
        return new TyForall3(v.value0, shiftEvalOrder(shiftEvalOrderTy)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Types (line 39, column 1 - line 44, column 72): " + [v.constructor.name]);
    }
  };
  var eqTy3 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof TyVar3 && y instanceof TyVar3) {
          return eq8(x.value0)(y.value0) && eq15(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyDecl3 && y instanceof TyDecl3) {
          return eq24(x.value0)(y.value0) && eq(eqList(eqTy3))(x.value1)(y.value1) && eq15(x.value2)(y.value2);
        }
        ;
        if (x instanceof TyShift3 && y instanceof TyShift3) {
          return eq(eqTy3)(x.value0)(y.value0) && eq15(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyCo3 && y instanceof TyCo3) {
          return eq(eqTy3)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyForall3 && y instanceof TyForall3) {
          return eq33(x.value0)(y.value0) && eq(eqTy3)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var embedType2 = function(v) {
    if (v instanceof TyVar3) {
      return new TyVar2(v.value0);
    }
    ;
    if (v instanceof TyDecl3) {
      return new TyDecl2(v.value0, map11(embedType2)(v.value1));
    }
    ;
    if (v instanceof TyShift3) {
      return new TyShift2(embedType2(v.value0));
    }
    ;
    if (v instanceof TyCo3) {
      return new TyCo2(embedType2(v.value0));
    }
    ;
    if (v instanceof TyForall3) {
      return new TyForall2(v.value0, embedType2(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Syntax.Kinded.Types (line 46, column 1 - line 46, column 24): " + [v.constructor.name]);
  };

  // output/Syntax.Kinded.Terms/index.js
  var show10 = /* @__PURE__ */ show(showVariable);
  var show16 = /* @__PURE__ */ show(showXtorname);
  var intercalate7 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map12 = /* @__PURE__ */ map(functorList);
  var show25 = /* @__PURE__ */ show(showEvaluationOrder);
  var show32 = /* @__PURE__ */ show(showString);
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var eq16 = /* @__PURE__ */ eq(eqVariable);
  var eq25 = /* @__PURE__ */ eq(eqTy3);
  var eq34 = /* @__PURE__ */ eq(eqXtorname);
  var eq43 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqVariable));
  var eq52 = /* @__PURE__ */ eq(eqEvaluationOrder);
  var Cut = /* @__PURE__ */ function() {
    function Cut5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Cut5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Cut5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Cut5;
  }();
  var Done2 = /* @__PURE__ */ function() {
    function Done6(value0) {
      this.value0 = value0;
    }
    ;
    Done6.create = function(value0) {
      return new Done6(value0);
    };
    return Done6;
  }();
  var Err = /* @__PURE__ */ function() {
    function Err5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Err5.create = function(value0) {
      return function(value1) {
        return new Err5(value0, value1);
      };
    };
    return Err5;
  }();
  var Print = /* @__PURE__ */ function() {
    function Print5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Print5.create = function(value0) {
      return function(value1) {
        return new Print5(value0, value1);
      };
    };
    return Print5;
  }();
  var Var = /* @__PURE__ */ function() {
    function Var5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Var5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Var5(value0, value1, value22);
        };
      };
    };
    return Var5;
  }();
  var Mu = /* @__PURE__ */ function() {
    function Mu5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Mu5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Mu5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Mu5;
  }();
  var Xtor = /* @__PURE__ */ function() {
    function Xtor5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Xtor5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Xtor5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Xtor5;
  }();
  var XCase = /* @__PURE__ */ function() {
    function XCase5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    XCase5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new XCase5(value0, value1, value22);
        };
      };
    };
    return XCase5;
  }();
  var ShiftCBV = /* @__PURE__ */ function() {
    function ShiftCBV5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ShiftCBV5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ShiftCBV5(value0, value1, value22);
        };
      };
    };
    return ShiftCBV5;
  }();
  var ShiftCBN = /* @__PURE__ */ function() {
    function ShiftCBN5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ShiftCBN5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ShiftCBN5(value0, value1, value22);
        };
      };
    };
    return ShiftCBN5;
  }();
  var Pattern = /* @__PURE__ */ function() {
    function Pattern5(value0) {
      this.value0 = value0;
    }
    ;
    Pattern5.create = function(value0) {
      return new Pattern5(value0);
    };
    return Pattern5;
  }();
  var showTerm = {
    show: function(v) {
      if (v instanceof Var) {
        return show10(v.value1);
      }
      ;
      if (v instanceof Mu) {
        return "mu " + (show10(v.value1) + (", " + show(showCommand)(v.value2)));
      }
      ;
      if (v instanceof Xtor && $$null(v.value2)) {
        return show16(v.value1);
      }
      ;
      if (v instanceof Xtor) {
        return show16(v.value1) + ("(" + (intercalate7(", ")(map12(show(showTerm))(v.value2)) + ")"));
      }
      ;
      if (v instanceof XCase) {
        return "case { " + (intercalate7(", ")(map12(show(showPattern))(v.value1)) + " }");
      }
      ;
      if (v instanceof ShiftCBV) {
        return "{" + (show(showTerm)(v.value1) + ":CBV}");
      }
      ;
      if (v instanceof ShiftCBN) {
        return "{" + (show(showTerm)(v.value1) + ":CBV}");
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 55, column 1 - line 62, column 53): " + [v.constructor.name]);
    }
  };
  var showPattern = {
    show: function(v) {
      if ($$null(v.value0.ptv)) {
        return show16(v.value0.ptxt) + (" => " + show(showCommand)(v.value0.ptcmd));
      }
      ;
      return show16(v.value0.ptxt) + ("(" + (intercalate7(", ")(map12(show10)(v.value0.ptv)) + (") =>" + show(showCommand)(v.value0.ptcmd))));
    }
  };
  var showCommand = {
    show: function(v) {
      if (v instanceof Cut) {
        return "<" + (show(showTerm)(v.value1) + (" | " + (show25(v.value2) + (" | " + (show(showTerm)(v.value3) + ">")))));
      }
      ;
      if (v instanceof Done2) {
        return "Done";
      }
      ;
      if (v instanceof Err) {
        return "error " + show32(v.value1);
      }
      ;
      if (v instanceof Print) {
        return "Print " + show(showTerm)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 24, column 1 - line 28, column 40): " + [v.constructor.name]);
    }
  };
  var hasLocTerm = {
    getLoc: function(v) {
      if (v instanceof Var) {
        return v.value0;
      }
      ;
      if (v instanceof Mu) {
        return v.value0;
      }
      ;
      if (v instanceof Xtor) {
        return v.value0;
      }
      ;
      if (v instanceof XCase) {
        return v.value0;
      }
      ;
      if (v instanceof ShiftCBV) {
        return v.value0;
      }
      ;
      if (v instanceof ShiftCBN) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 63, column 1 - line 76, column 51): " + [v.constructor.name]);
    },
    setLoc: function(v) {
      return function(v1) {
        if (v1 instanceof Var) {
          return new Var(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof Mu) {
          return new Mu(v, v1.value1, v1.value2, v1.value3);
        }
        ;
        if (v1 instanceof Xtor) {
          return new Xtor(v, v1.value1, v1.value2, v1.value3);
        }
        ;
        if (v1 instanceof XCase) {
          return new XCase(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof ShiftCBV) {
          return new ShiftCBV(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof ShiftCBN) {
          return new ShiftCBN(v, v1.value1, v1.value2);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 63, column 1 - line 76, column 51): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var hasLocCommand = {
    getLoc: function(v) {
      if (v instanceof Cut) {
        return v.value0;
      }
      ;
      if (v instanceof Done2) {
        return v.value0;
      }
      ;
      if (v instanceof Err) {
        return v.value0;
      }
      ;
      if (v instanceof Print) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 30, column 1 - line 39, column 39): " + [v.constructor.name]);
    },
    setLoc: function(v) {
      return function(v1) {
        if (v1 instanceof Cut) {
          return new Cut(v, v1.value1, v1.value2, v1.value3);
        }
        ;
        if (v1 instanceof Done2) {
          return new Done2(v);
        }
        ;
        if (v1 instanceof Err) {
          return new Err(v, v1.value1);
        }
        ;
        if (v1 instanceof Print) {
          return new Print(v, v1.value1);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 30, column 1 - line 39, column 39): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var isValue = function(v) {
    return function(v1) {
      if (v instanceof CBV && v1 instanceof Var) {
        return true;
      }
      ;
      if (v instanceof CBV && v1 instanceof Xtor) {
        return $$null(filter(identity9)(map12(isValue(CBV.value))(v1.value2)));
      }
      ;
      if (v instanceof CBV && v1 instanceof XCase) {
        return true;
      }
      ;
      if (v instanceof CBV && v1 instanceof ShiftCBV) {
        return true;
      }
      ;
      if (v instanceof CBV) {
        return false;
      }
      ;
      if (v instanceof CBN) {
        return true;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 97, column 1 - line 97, column 46): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getType = function(v) {
    if (v instanceof Var) {
      return v.value2;
    }
    ;
    if (v instanceof Mu) {
      return v.value3;
    }
    ;
    if (v instanceof Xtor) {
      return v.value3;
    }
    ;
    if (v instanceof XCase) {
      return v.value2;
    }
    ;
    if (v instanceof ShiftCBV) {
      return v.value2;
    }
    ;
    if (v instanceof ShiftCBN) {
      return v.value2;
    }
    ;
    throw new Error("Failed pattern match at Syntax.Kinded.Terms (line 78, column 1 - line 78, column 22): " + [v.constructor.name]);
  };
  var eqTerm = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Var && y instanceof Var) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq16(x.value1)(y.value1) && eq25(x.value2)(y.value2);
        }
        ;
        if (x instanceof Mu && y instanceof Mu) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq16(x.value1)(y.value1) && eq(eqCommand)(x.value2)(y.value2) && eq25(x.value3)(y.value3);
        }
        ;
        if (x instanceof Xtor && y instanceof Xtor) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq34(x.value1)(y.value1) && eq(eqList(eqTerm))(x.value2)(y.value2) && eq25(x.value3)(y.value3);
        }
        ;
        if (x instanceof XCase && y instanceof XCase) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqList(eqPattern))(x.value1)(y.value1) && eq25(x.value2)(y.value2);
        }
        ;
        if (x instanceof ShiftCBV && y instanceof ShiftCBV) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm)(x.value1)(y.value1) && eq25(x.value2)(y.value2);
        }
        ;
        if (x instanceof ShiftCBN && y instanceof ShiftCBN) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm)(x.value1)(y.value1) && eq25(x.value2)(y.value2);
        }
        ;
        return false;
      };
    }
  };
  var eqPattern = {
    eq: function(x) {
      return function(y) {
        return eq(eqCommand)(x.value0.ptcmd)(y.value0.ptcmd) && eq43(x.value0.ptv)(y.value0.ptv) && eq34(x.value0.ptxt)(y.value0.ptxt);
      };
    }
  };
  var eqCommand = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Cut && y instanceof Cut) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm)(x.value1)(y.value1) && eq52(x.value2)(y.value2) && eq(eqTerm)(x.value3)(y.value3);
        }
        ;
        if (x instanceof Done2 && y instanceof Done2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine);
        }
        ;
        if (x instanceof Err && y instanceof Err) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && x.value1 === y.value1;
        }
        ;
        if (x instanceof Print && y instanceof Print) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };

  // output/Syntax.Desugared.Terms/index.js
  var show11 = /* @__PURE__ */ show(showVariable);
  var show17 = /* @__PURE__ */ show(showXtorname);
  var intercalate8 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map13 = /* @__PURE__ */ map(functorList);
  var show26 = /* @__PURE__ */ show(showEvaluationOrder);
  var show33 = /* @__PURE__ */ show(showTy);
  var show42 = /* @__PURE__ */ show(showString);
  var eq17 = /* @__PURE__ */ eq(eqVariable);
  var eq26 = /* @__PURE__ */ eq(eqXtorname);
  var eq35 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqVariable));
  var eq44 = /* @__PURE__ */ eq(eqEvaluationOrder);
  var eq53 = /* @__PURE__ */ eq(eqTy);
  var Cut2 = /* @__PURE__ */ function() {
    function Cut5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Cut5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Cut5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Cut5;
  }();
  var CutAnnot = /* @__PURE__ */ function() {
    function CutAnnot3(value0, value1, value22, value32, value42) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
    }
    ;
    CutAnnot3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return new CutAnnot3(value0, value1, value22, value32, value42);
            };
          };
        };
      };
    };
    return CutAnnot3;
  }();
  var Done3 = /* @__PURE__ */ function() {
    function Done6(value0) {
      this.value0 = value0;
    }
    ;
    Done6.create = function(value0) {
      return new Done6(value0);
    };
    return Done6;
  }();
  var Err2 = /* @__PURE__ */ function() {
    function Err5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Err5.create = function(value0) {
      return function(value1) {
        return new Err5(value0, value1);
      };
    };
    return Err5;
  }();
  var Print2 = /* @__PURE__ */ function() {
    function Print5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Print5.create = function(value0) {
      return function(value1) {
        return new Print5(value0, value1);
      };
    };
    return Print5;
  }();
  var PrintAnnot = /* @__PURE__ */ function() {
    function PrintAnnot3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    PrintAnnot3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new PrintAnnot3(value0, value1, value22);
        };
      };
    };
    return PrintAnnot3;
  }();
  var Var2 = /* @__PURE__ */ function() {
    function Var5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Var5.create = function(value0) {
      return function(value1) {
        return new Var5(value0, value1);
      };
    };
    return Var5;
  }();
  var Mu2 = /* @__PURE__ */ function() {
    function Mu5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Mu5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Mu5(value0, value1, value22);
        };
      };
    };
    return Mu5;
  }();
  var Xtor2 = /* @__PURE__ */ function() {
    function Xtor5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Xtor5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Xtor5(value0, value1, value22);
        };
      };
    };
    return Xtor5;
  }();
  var XCase2 = /* @__PURE__ */ function() {
    function XCase5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    XCase5.create = function(value0) {
      return function(value1) {
        return new XCase5(value0, value1);
      };
    };
    return XCase5;
  }();
  var ShiftCBV2 = /* @__PURE__ */ function() {
    function ShiftCBV5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ShiftCBV5.create = function(value0) {
      return function(value1) {
        return new ShiftCBV5(value0, value1);
      };
    };
    return ShiftCBV5;
  }();
  var ShiftCBN2 = /* @__PURE__ */ function() {
    function ShiftCBN5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ShiftCBN5.create = function(value0) {
      return function(value1) {
        return new ShiftCBN5(value0, value1);
      };
    };
    return ShiftCBN5;
  }();
  var Pattern2 = /* @__PURE__ */ function() {
    function Pattern5(value0) {
      this.value0 = value0;
    }
    ;
    Pattern5.create = function(value0) {
      return new Pattern5(value0);
    };
    return Pattern5;
  }();
  var showTerm2 = {
    show: function(v) {
      if (v instanceof Var2) {
        return show11(v.value1);
      }
      ;
      if (v instanceof Mu2) {
        return "mu " + (show11(v.value1) + (". " + show(showCommand2)(v.value2)));
      }
      ;
      if (v instanceof Xtor2 && $$null(v.value2)) {
        return show17(v.value1);
      }
      ;
      if (v instanceof Xtor2) {
        return show17(v.value1) + ("(" + (intercalate8(", ")(map13(show(showTerm2))(v.value2)) + ")"));
      }
      ;
      if (v instanceof XCase2) {
        return "case " + intercalate8(", ")(map13(show(showPattern2))(v.value1));
      }
      ;
      if (v instanceof ShiftCBV2) {
        return "{" + (show(showTerm2)(v.value1) + ":CBV}");
      }
      ;
      if (v instanceof ShiftCBN2) {
        return "{" + (show(showTerm2)(v.value1) + ":CBN}");
      }
      ;
      throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 59, column 1 - line 66, column 49): " + [v.constructor.name]);
    }
  };
  var showPattern2 = {
    show: function(v) {
      if ($$null(v.value0.ptv)) {
        return show17(v.value0.ptxt) + ("=>" + show(showCommand2)(v.value0.ptcmd));
      }
      ;
      return show17(v.value0.ptxt) + ("(" + (intercalate8(", ")(map13(show11)(v.value0.ptv)) + (") => " + show(showCommand2)(v.value0.ptcmd))));
    }
  };
  var showCommand2 = {
    show: function(v) {
      if (v instanceof Cut2) {
        return "<" + (show(showTerm2)(v.value1) + (" | " + (show26(v.value2) + (" | " + (show(showTerm2)(v.value3) + " >")))));
      }
      ;
      if (v instanceof CutAnnot) {
        return "<" + (show(showTerm2)(v.value1) + (" | " + (show33(v.value2) + (" : " + (show26(v.value3) + (" | " + show(showTerm2)(v.value4)))))));
      }
      ;
      if (v instanceof Done3) {
        return "Done";
      }
      ;
      if (v instanceof Err2) {
        return "errror " + show42(v.value1);
      }
      ;
      if (v instanceof Print2) {
        return "Print " + show(showTerm2)(v.value1);
      }
      ;
      if (v instanceof PrintAnnot) {
        return "Print " + (show(showTerm2)(v.value1) + (" : " + show33(v.value2)));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 22, column 1 - line 28, column 68): " + [v.constructor.name]);
    }
  };
  var hasLocTerm2 = {
    getLoc: function(v) {
      if (v instanceof Var2) {
        return v.value0;
      }
      ;
      if (v instanceof Mu2) {
        return v.value0;
      }
      ;
      if (v instanceof Xtor2) {
        return v.value0;
      }
      ;
      if (v instanceof XCase2) {
        return v.value0;
      }
      ;
      if (v instanceof ShiftCBV2) {
        return v.value0;
      }
      ;
      if (v instanceof ShiftCBN2) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 69, column 1 - line 82, column 45): " + [v.constructor.name]);
    },
    setLoc: function(v) {
      return function(v1) {
        if (v1 instanceof Var2) {
          return new Var2(v, v1.value1);
        }
        ;
        if (v1 instanceof Mu2) {
          return new Mu2(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof Xtor2) {
          return new Xtor2(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof XCase2) {
          return new XCase2(v, v1.value1);
        }
        ;
        if (v1 instanceof ShiftCBV2) {
          return new ShiftCBV2(v, v1.value1);
        }
        ;
        if (v1 instanceof ShiftCBN2) {
          return new ShiftCBN2(v, v1.value1);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 69, column 1 - line 82, column 45): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var hasLocCommand2 = {
    getLoc: function(v) {
      if (v instanceof Cut2) {
        return v.value0;
      }
      ;
      if (v instanceof CutAnnot) {
        return v.value0;
      }
      ;
      if (v instanceof Done3) {
        return v.value0;
      }
      ;
      if (v instanceof Err2) {
        return v.value0;
      }
      ;
      if (v instanceof Print2) {
        return v.value0;
      }
      ;
      if (v instanceof PrintAnnot) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 30, column 1 - line 43, column 55): " + [v.constructor.name]);
    },
    setLoc: function(v) {
      return function(v1) {
        if (v1 instanceof Cut2) {
          return new Cut2(v, v1.value1, v1.value2, v1.value3);
        }
        ;
        if (v1 instanceof CutAnnot) {
          return new CutAnnot(v, v1.value1, v1.value2, v1.value3, v1.value4);
        }
        ;
        if (v1 instanceof Done3) {
          return new Done3(v);
        }
        ;
        if (v1 instanceof Err2) {
          return new Err2(v, v1.value1);
        }
        ;
        if (v1 instanceof Print2) {
          return new Print2(v, v1.value1);
        }
        ;
        if (v1 instanceof PrintAnnot) {
          return new PrintAnnot(v, v1.value1, v1.value2);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Desugared.Terms (line 30, column 1 - line 43, column 55): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var eqTerm2 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Var2 && y instanceof Var2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq17(x.value1)(y.value1);
        }
        ;
        if (x instanceof Mu2 && y instanceof Mu2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq17(x.value1)(y.value1) && eq(eqCommand2)(x.value2)(y.value2);
        }
        ;
        if (x instanceof Xtor2 && y instanceof Xtor2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq26(x.value1)(y.value1) && eq(eqList(eqTerm2))(x.value2)(y.value2);
        }
        ;
        if (x instanceof XCase2 && y instanceof XCase2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqList(eqPattern2))(x.value1)(y.value1);
        }
        ;
        if (x instanceof ShiftCBV2 && y instanceof ShiftCBV2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1);
        }
        ;
        if (x instanceof ShiftCBN2 && y instanceof ShiftCBN2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var eqPattern2 = {
    eq: function(x) {
      return function(y) {
        return eq(eqCommand2)(x.value0.ptcmd)(y.value0.ptcmd) && eq35(x.value0.ptv)(y.value0.ptv) && eq26(x.value0.ptxt)(y.value0.ptxt);
      };
    }
  };
  var eqCommand2 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Cut2 && y instanceof Cut2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1) && eq44(x.value2)(y.value2) && eq(eqTerm2)(x.value3)(y.value3);
        }
        ;
        if (x instanceof CutAnnot && y instanceof CutAnnot) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1) && eq53(x.value2)(y.value2) && eq44(x.value3)(y.value3) && eq(eqTerm2)(x.value4)(y.value4);
        }
        ;
        if (x instanceof Done3 && y instanceof Done3) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine);
        }
        ;
        if (x instanceof Err2 && y instanceof Err2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && x.value1 === y.value1;
        }
        ;
        if (x instanceof Print2 && y instanceof Print2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1);
        }
        ;
        if (x instanceof PrintAnnot && y instanceof PrintAnnot) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm2)(x.value1)(y.value1) && eq53(x.value2)(y.value2);
        }
        ;
        return false;
      };
    }
  };

  // output/Syntax.Desugared.Program/index.js
  var show18 = /* @__PURE__ */ show(showXtorname);
  var intercalate9 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map14 = /* @__PURE__ */ map(functorList);
  var show19 = /* @__PURE__ */ show(showTy);
  var map1 = /* @__PURE__ */ map(functorMaybe);
  var show27 = /* @__PURE__ */ show(showVariable);
  var show34 = /* @__PURE__ */ show(showTerm2);
  var show43 = /* @__PURE__ */ show(showDeclTy);
  var show52 = /* @__PURE__ */ show(showTypename);
  var show62 = /* @__PURE__ */ show(showVariantVar);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var eq9 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqCommand2));
  var show72 = /* @__PURE__ */ show(showModulename);
  var show82 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showCommand2));
  var insert3 = /* @__PURE__ */ insert(ordVariable);
  var insert1 = /* @__PURE__ */ insert(ordTypename);
  var XtorSig = /* @__PURE__ */ function() {
    function XtorSig5(value0) {
      this.value0 = value0;
    }
    ;
    XtorSig5.create = function(value0) {
      return new XtorSig5(value0);
    };
    return XtorSig5;
  }();
  var VarDecl = /* @__PURE__ */ function() {
    function VarDecl5(value0) {
      this.value0 = value0;
    }
    ;
    VarDecl5.create = function(value0) {
      return new VarDecl5(value0);
    };
    return VarDecl5;
  }();
  var DataDecl = /* @__PURE__ */ function() {
    function DataDecl4(value0) {
      this.value0 = value0;
    }
    ;
    DataDecl4.create = function(value0) {
      return new DataDecl4(value0);
    };
    return DataDecl4;
  }();
  var Program = /* @__PURE__ */ function() {
    function Program4(value0) {
      this.value0 = value0;
    }
    ;
    Program4.create = function(value0) {
      return new Program4(value0);
    };
    return Program4;
  }();
  var showXtorSig = {
    show: function(v) {
      if ($$null(v.value0.sigArgs)) {
        return show18(v.value0.sigName);
      }
      ;
      return show18(v.value0.sigName) + ("(" + intercalate9(",")(map14(show19)(v.value0.sigArgs)));
    }
  };
  var show92 = /* @__PURE__ */ show(showXtorSig);
  var showVarDecl = {
    show: function(v) {
      var recPref = function() {
        if (v.value0.varIsRec) {
          return "rec ";
        }
        ;
        return "";
      }();
      var tyStr = fromMaybe("")(map1(show19)(v.value0.varTy));
      return recPref + (show27(v.value0.varName) + (tyStr + (" := " + show34(v.value0.varBody))));
    }
  };
  var showDataDecl = {
    show: function(v) {
      if ($$null(v.value0.declArgs)) {
        return show43(v.value0.declType) + (" " + (show52(v.value0.declName) + ("{" + (intercalate9(", ")(map14(show92)(v.value0.declXtors)) + "}"))));
      }
      ;
      return show43(v.value0.declType) + (" " + (show52(v.value0.declName) + ("(" + (intercalate9(", ")(map14(show62)(v.value0.declArgs)) + (") { " + (intercalate9(", ")(map14(show92)(v.value0.declXtors)) + "}"))))));
    }
  };
  var showMap = function(dictShow) {
    var show102 = show(dictShow);
    return function(declMap) {
      var decls = map14(snd)(toUnfoldable6(declMap));
      return intercalate9("\n		")(map14(show102)(decls));
    };
  };
  var showMap1 = /* @__PURE__ */ showMap(showDataDecl);
  var showMap2 = /* @__PURE__ */ showMap(showVarDecl);
  var showProgram = {
    show: function(v) {
      if (eq9(Nothing.value)(v.value0.progMain)) {
        return "	Module " + (show72(v.value0.progName) + ("\n" + ("	Decalations:\n		" + (showMap1(v.value0.progDecls) + ("\n" + ("	Variables:\n		" + (showMap2(v.value0.progVars) + "\n")))))));
      }
      ;
      return "	Module " + (show72(v.value0.progName) + ("\n" + ("	Decalations:\n		" + (showMap1(v.value0.progDecls) + ("\n" + ("	Variables:\n		" + (showMap2(v.value0.progVars) + ("\n" + ("	Main : " + show82(v.value0.progMain))))))))));
    }
  };
  var setMainProgram = function(c) {
    return function(v) {
      return new Program({
        progName: v.value0.progName,
        progDecls: v.value0.progDecls,
        progVars: v.value0.progVars,
        progSrc: v.value0.progSrc,
        progMain: new Just(c)
      });
    };
  };
  var emptyProg = function(nm) {
    return function(src9) {
      return new Program({
        progName: nm,
        progDecls: empty2,
        progVars: empty2,
        progMain: Nothing.value,
        progSrc: src9
      });
    };
  };
  var addVarProgram = function(v) {
    return function(v1) {
      return new Program({
        progName: v1.value0.progName,
        progDecls: v1.value0.progDecls,
        progMain: v1.value0.progMain,
        progSrc: v1.value0.progSrc,
        progVars: insert3(v.value0.varName)(new VarDecl(v.value0))(v1.value0.progVars)
      });
    };
  };
  var addDeclProgram = function(v) {
    return function(v1) {
      return new Program({
        progName: v1.value0.progName,
        progVars: v1.value0.progVars,
        progMain: v1.value0.progMain,
        progSrc: v1.value0.progSrc,
        progDecls: insert1(v.value0.declName)(new DataDecl(v.value0))(v1.value0.progDecls)
      });
    };
  };

  // output/Syntax.Typed.Terms/index.js
  var show20 = /* @__PURE__ */ show(showVariable);
  var show110 = /* @__PURE__ */ show(showTy2);
  var show28 = /* @__PURE__ */ show(showXtorname);
  var intercalate10 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map15 = /* @__PURE__ */ map(functorList);
  var show35 = /* @__PURE__ */ show(showEvaluationOrder);
  var show44 = /* @__PURE__ */ show(showString);
  var Cut3 = /* @__PURE__ */ function() {
    function Cut5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Cut5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Cut5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Cut5;
  }();
  var Done4 = /* @__PURE__ */ function() {
    function Done6(value0) {
      this.value0 = value0;
    }
    ;
    Done6.create = function(value0) {
      return new Done6(value0);
    };
    return Done6;
  }();
  var Err3 = /* @__PURE__ */ function() {
    function Err5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Err5.create = function(value0) {
      return function(value1) {
        return new Err5(value0, value1);
      };
    };
    return Err5;
  }();
  var Print3 = /* @__PURE__ */ function() {
    function Print5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Print5.create = function(value0) {
      return function(value1) {
        return new Print5(value0, value1);
      };
    };
    return Print5;
  }();
  var Var3 = /* @__PURE__ */ function() {
    function Var5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Var5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Var5(value0, value1, value22);
        };
      };
    };
    return Var5;
  }();
  var Mu3 = /* @__PURE__ */ function() {
    function Mu5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Mu5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Mu5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Mu5;
  }();
  var Xtor3 = /* @__PURE__ */ function() {
    function Xtor5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Xtor5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Xtor5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Xtor5;
  }();
  var XCase3 = /* @__PURE__ */ function() {
    function XCase5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    XCase5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new XCase5(value0, value1, value22);
        };
      };
    };
    return XCase5;
  }();
  var ShiftCBV3 = /* @__PURE__ */ function() {
    function ShiftCBV5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ShiftCBV5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ShiftCBV5(value0, value1, value22);
        };
      };
    };
    return ShiftCBV5;
  }();
  var ShiftCBN3 = /* @__PURE__ */ function() {
    function ShiftCBN5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ShiftCBN5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ShiftCBN5(value0, value1, value22);
        };
      };
    };
    return ShiftCBN5;
  }();
  var Pattern3 = /* @__PURE__ */ function() {
    function Pattern5(value0) {
      this.value0 = value0;
    }
    ;
    Pattern5.create = function(value0) {
      return new Pattern5(value0);
    };
    return Pattern5;
  }();
  var showTerm3 = {
    show: function(v) {
      if (v instanceof Var3) {
        return show20(v.value1) + (": " + show110(v.value2));
      }
      ;
      if (v instanceof Mu3) {
        return show20(v.value1) + (", " + (show(showCommand3)(v.value2) + (": " + show110(v.value3))));
      }
      ;
      if (v instanceof Xtor3) {
        return show28(v.value1) + (intercalate10(", ")(map15(show(showTerm3))(v.value2)) + (": " + show110(v.value3)));
      }
      ;
      if (v instanceof XCase3) {
        return " case { " + (intercalate10(", ")(map15(show(showPattern3))(v.value1)) + (": " + show110(v.value2)));
      }
      ;
      if (v instanceof ShiftCBV3) {
        return "{" + (show(showTerm3)(v.value1) + (":CBV} : " + show110(v.value2)));
      }
      ;
      if (v instanceof ShiftCBN3) {
        return "{" + (show(showTerm3)(v.value1) + (":CBM} : " + show110(v.value2)));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Typed.Terms (line 57, column 1 - line 63, column 66): " + [v.constructor.name]);
    }
  };
  var showPattern3 = {
    show: function(v) {
      if ($$null(v.value0.ptv)) {
        return show28(v.value0.ptxt) + (" => " + show(showCommand3)(v.value0.ptcmd));
      }
      ;
      return show28(v.value0.ptxt) + ("(" + (intercalate10(", ")(map15(show20)(v.value0.ptv)) + (" => " + show(showCommand3)(v.value0.ptcmd))));
    }
  };
  var showCommand3 = {
    show: function(v) {
      if (v instanceof Cut3) {
        return "<" + (show(showTerm3)(v.value1) + (" | " + (show35(v.value2) + (" | " + show(showTerm3)(v.value3)))));
      }
      ;
      if (v instanceof Done4) {
        return "Done";
      }
      ;
      if (v instanceof Err3) {
        return "error " + show44(v.value1);
      }
      ;
      if (v instanceof Print3) {
        return "Print " + show(showTerm3)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at Syntax.Typed.Terms (line 27, column 1 - line 31, column 39): " + [v.constructor.name]);
    }
  };
  var setType = function(v) {
    return function(v1) {
      if (v instanceof Var3) {
        return new Var3(v.value0, v.value1, v1);
      }
      ;
      if (v instanceof Mu3) {
        return new Mu3(v.value0, v.value1, v.value2, v1);
      }
      ;
      if (v instanceof Xtor3) {
        return new Xtor3(v.value0, v.value1, v.value2, v1);
      }
      ;
      if (v instanceof XCase3) {
        return new XCase3(v.value0, v.value1, v1);
      }
      ;
      if (v instanceof ShiftCBV3) {
        return new ShiftCBV3(v.value0, v.value1, v1);
      }
      ;
      if (v instanceof ShiftCBN3) {
        return new ShiftCBN3(v.value0, v.value1, v1);
      }
      ;
      throw new Error("Failed pattern match at Syntax.Typed.Terms (line 88, column 1 - line 88, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getType2 = function(v) {
    if (v instanceof Var3) {
      return v.value2;
    }
    ;
    if (v instanceof Mu3) {
      return v.value3;
    }
    ;
    if (v instanceof Xtor3) {
      return v.value3;
    }
    ;
    if (v instanceof XCase3) {
      return v.value2;
    }
    ;
    if (v instanceof ShiftCBV3) {
      return v.value2;
    }
    ;
    if (v instanceof ShiftCBN3) {
      return v.value2;
    }
    ;
    throw new Error("Failed pattern match at Syntax.Typed.Terms (line 80, column 1 - line 80, column 22): " + [v.constructor.name]);
  };

  // output/Syntax.Typed.Program/index.js
  var map16 = /* @__PURE__ */ map(functorList);
  var XtorSig2 = /* @__PURE__ */ function() {
    function XtorSig5(value0) {
      this.value0 = value0;
    }
    ;
    XtorSig5.create = function(value0) {
      return new XtorSig5(value0);
    };
    return XtorSig5;
  }();
  var VarDecl2 = /* @__PURE__ */ function() {
    function VarDecl5(value0) {
      this.value0 = value0;
    }
    ;
    VarDecl5.create = function(value0) {
      return new VarDecl5(value0);
    };
    return VarDecl5;
  }();
  var embedXtorSig = function(v) {
    return new XtorSig({
      sigPos: v.value0.sigPos,
      sigName: v.value0.sigName,
      sigArgs: map16(embedType)(v.value0.sigArgs)
    });
  };

  // output/Syntax.Kinded.Program/index.js
  var map17 = /* @__PURE__ */ map(functorList);
  var insert4 = /* @__PURE__ */ insert(ordVariable);
  var insert12 = /* @__PURE__ */ insert(ordTypename);
  var XtorSig3 = /* @__PURE__ */ function() {
    function XtorSig5(value0) {
      this.value0 = value0;
    }
    ;
    XtorSig5.create = function(value0) {
      return new XtorSig5(value0);
    };
    return XtorSig5;
  }();
  var VarDecl3 = /* @__PURE__ */ function() {
    function VarDecl5(value0) {
      this.value0 = value0;
    }
    ;
    VarDecl5.create = function(value0) {
      return new VarDecl5(value0);
    };
    return VarDecl5;
  }();
  var DataDecl2 = /* @__PURE__ */ function() {
    function DataDecl4(value0) {
      this.value0 = value0;
    }
    ;
    DataDecl4.create = function(value0) {
      return new DataDecl4(value0);
    };
    return DataDecl4;
  }();
  var Program2 = /* @__PURE__ */ function() {
    function Program4(value0) {
      this.value0 = value0;
    }
    ;
    Program4.create = function(value0) {
      return new Program4(value0);
    };
    return Program4;
  }();
  var emptyProg2 = function(nm) {
    return function(src9) {
      return new Program2({
        progName: nm,
        progDecls: empty2,
        progVars: empty2,
        progMain: Nothing.value,
        progSrc: src9
      });
    };
  };
  var embedXtorSig2 = function(v) {
    return new XtorSig2({
      sigPos: v.value0.sigPos,
      sigName: v.value0.sigName,
      sigArgs: map17(embedType2)(v.value0.sigArgs)
    });
  };
  var addVarProgram2 = function(v) {
    return function(v1) {
      return new Program2({
        progName: v1.value0.progName,
        progDecls: v1.value0.progDecls,
        progMain: v1.value0.progMain,
        progSrc: v1.value0.progSrc,
        progVars: insert4(v.value0.varName)(new VarDecl3(v.value0))(v1.value0.progVars)
      });
    };
  };
  var addDeclProgram2 = function(v) {
    return function(v1) {
      return new Program2({
        progName: v1.value0.progName,
        progVars: v1.value0.progVars,
        progMain: v1.value0.progMain,
        progSrc: v1.value0.progSrc,
        progDecls: insert12(v.value0.declName)(new DataDecl2(v.value0))(v1.value0.progDecls)
      });
    };
  };

  // output/Environment/index.js
  var map18 = /* @__PURE__ */ map(functorList);
  var toUnfoldable7 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var show111 = /* @__PURE__ */ show(showXtorname);
  var show29 = /* @__PURE__ */ show(showVariable);
  var unions3 = /* @__PURE__ */ unions(ordVariable)(foldableList);
  var lookup2 = /* @__PURE__ */ lookup(ordVariable);
  var lookup1 = /* @__PURE__ */ lookup(ordModulename);
  var unions1 = /* @__PURE__ */ unions(ordTypename)(foldableList);
  var find2 = /* @__PURE__ */ find(foldableList);
  var eq10 = /* @__PURE__ */ eq(eqXtorname);
  var lookup22 = /* @__PURE__ */ lookup(ordTypename);
  var elem2 = /* @__PURE__ */ elem(foldableList)(eqXtorname);
  var show36 = /* @__PURE__ */ show(showTypename);
  var insert5 = /* @__PURE__ */ insert(ordModulename);
  var xtorErr = function(loc) {
    return function(dictError) {
      var toError2 = toError(dictError);
      return function(xtn) {
        return toError2(loc)("Xtor " + (show111(xtn) + " not found in environment"));
      };
    };
  };
  var varErr = function(loc) {
    return function(dictError) {
      var toError2 = toError(dictError);
      return function($$var2) {
        return toError2(loc)("Variable " + (show29($$var2) + " not found in environment"));
      };
    };
  };
  var getVars = function(dictError) {
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        return bind36(asks(dictMonadReader.MonadAsk0())(function(v) {
          return v;
        }))(function(defs) {
          var progs = map18(snd)(toUnfoldable7(defs));
          var vars = unions3(map18(function(v) {
            return v.value0.progVars;
          })(progs));
          return pure39(vars);
        });
      };
    };
  };
  var lookupMVar = function(dictError) {
    var getVars1 = getVars(dictError);
    return function(dictMonadError) {
      var map119 = map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0());
      var getVars2 = getVars1(dictMonadError);
      return function(dictMonadReader) {
        var getVars3 = getVars2(dictMonadReader);
        return function(v) {
          return map119(lookup2(v))(getVars3);
        };
      };
    };
  };
  var lookupVar = function(dictError) {
    var lookupMVar1 = lookupMVar(dictError);
    return function(dictMonadError) {
      var MonadThrow0 = dictMonadError.MonadThrow0();
      var Monad0 = MonadThrow0.Monad0();
      var bind36 = bind(Monad0.Bind1());
      var lookupMVar22 = lookupMVar1(dictMonadError);
      var throwError22 = throwError(MonadThrow0);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var lookupMVar32 = lookupMVar22(dictMonadReader);
        return function(loc) {
          return function(v) {
            return bind36(lookupMVar32(v))(function(mvar) {
              if (mvar instanceof Nothing) {
                return throwError22(varErr(loc)(dictError)(v));
              }
              ;
              if (mvar instanceof Just) {
                return pure39(mvar.value0);
              }
              ;
              throw new Error("Failed pattern match at Environment (line 104, column 3 - line 106, column 25): " + [mvar.constructor.name]);
            });
          };
        };
      };
    };
  };
  var lookupBody = function(dictError) {
    var lookupVar1 = lookupVar(dictError);
    return function(dictMonadError) {
      var map119 = map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0());
      var lookupVar2 = lookupVar1(dictMonadError);
      return function(dictMonadReader) {
        var lookupVar3 = lookupVar2(dictMonadReader);
        return function(loc) {
          return function(v) {
            return map119(function(v1) {
              return v1.value0.varBody;
            })(lookupVar3(loc)(v));
          };
        };
      };
    };
  };
  var getDecls = function(dictError) {
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        return bind36(asks(dictMonadReader.MonadAsk0())(function(v) {
          return v;
        }))(function(defs) {
          var progs = map18(snd)(toUnfoldable7(defs));
          var decls = unions1(map18(function(v) {
            return v.value0.progDecls;
          })(progs));
          return pure39(decls);
        });
      };
    };
  };
  var getTypeNames = function(dictError) {
    var getDecls1 = getDecls(dictError);
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var getDecls2 = getDecls1(dictMonadError);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        return bind36(getDecls2(dictMonadReader))(function(decls) {
          return pure39(map18(fst)(toUnfoldable7(decls)));
        });
      };
    };
  };
  var getXtors = function(dictError) {
    var getDecls1 = getDecls(dictError);
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var getDecls2 = getDecls1(dictMonadError);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        return bind36(getDecls2(dictMonadReader))(function(decls) {
          var declList = map18(snd)(toUnfoldable7(decls));
          return pure39(concatMap(function(v) {
            return v.value0.declXtors;
          })(declList));
        });
      };
    };
  };
  var getXtorNames = function(dictError) {
    var getXtors1 = getXtors(dictError);
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var getXtors2 = getXtors1(dictMonadError);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        return bind36(getXtors2(dictMonadReader))(function(xtors) {
          return pure39(map18(function(v) {
            return v.value0.sigName;
          })(xtors));
        });
      };
    };
  };
  var lookupMXtor = function(dictError) {
    var getXtors1 = getXtors(dictError);
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var getXtors2 = getXtors1(dictMonadError);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var getXtors3 = getXtors2(dictMonadReader);
        return function(xtn) {
          return bind36(getXtors3)(function(xtors) {
            return pure39(find2(function(v) {
              return eq10(v.value0.sigName)(xtn);
            })(xtors));
          });
        };
      };
    };
  };
  var lookupXtor = function(dictError) {
    var lookupMXtor1 = lookupMXtor(dictError);
    return function(dictMonadError) {
      var MonadThrow0 = dictMonadError.MonadThrow0();
      var Monad0 = MonadThrow0.Monad0();
      var bind36 = bind(Monad0.Bind1());
      var lookupMXtor22 = lookupMXtor1(dictMonadError);
      var throwError22 = throwError(MonadThrow0);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var lookupMXtor32 = lookupMXtor22(dictMonadReader);
        return function(loc) {
          return function(xtn) {
            return bind36(lookupMXtor32(xtn))(function(mxt) {
              if (mxt instanceof Nothing) {
                return throwError22(xtorErr(loc)(dictError)(xtn));
              }
              ;
              if (mxt instanceof Just) {
                return pure39(mxt.value0);
              }
              ;
              throw new Error("Failed pattern match at Environment (line 126, column 3 - line 128, column 23): " + [mxt.constructor.name]);
            });
          };
        };
      };
    };
  };
  var lookupMDecl = function(dictError) {
    var getDecls1 = getDecls(dictError);
    return function(dictMonadError) {
      var map119 = map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0());
      var getDecls2 = getDecls1(dictMonadError);
      return function(dictMonadReader) {
        var getDecls3 = getDecls2(dictMonadReader);
        return function(tyn) {
          return map119(lookup22(tyn))(getDecls3);
        };
      };
    };
  };
  var lookupXtorMDecl = function(dictError) {
    var getDecls1 = getDecls(dictError);
    return function(dictMonadError) {
      var Monad0 = dictMonadError.MonadThrow0().Monad0();
      var bind36 = bind(Monad0.Bind1());
      var getDecls2 = getDecls1(dictMonadError);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var getDecls3 = getDecls2(dictMonadReader);
        return function(xtn) {
          return bind36(getDecls3)(function(decls) {
            var declLs = map18(snd)(toUnfoldable7(decls));
            var inDecl = function(v) {
              return elem2(xtn)(map18(function(v1) {
                return v1.value0.sigName;
              })(v.value0.declXtors));
            };
            return pure39(find2(inDecl)(declLs));
          });
        };
      };
    };
  };
  var lookupXtorDecl = function(dictError) {
    var lookupXtorMDecl1 = lookupXtorMDecl(dictError);
    return function(dictMonadError) {
      var MonadThrow0 = dictMonadError.MonadThrow0();
      var Monad0 = MonadThrow0.Monad0();
      var bind36 = bind(Monad0.Bind1());
      var lookupXtorMDecl2 = lookupXtorMDecl1(dictMonadError);
      var throwError22 = throwError(MonadThrow0);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var lookupXtorMDecl3 = lookupXtorMDecl2(dictMonadReader);
        return function(loc) {
          return function(xtn) {
            return bind36(lookupXtorMDecl3(xtn))(function(decl) {
              if (decl instanceof Nothing) {
                return throwError22(xtorErr(loc)(dictError)(xtn));
              }
              ;
              if (decl instanceof Just) {
                return pure39(decl.value0);
              }
              ;
              throw new Error("Failed pattern match at Environment (line 142, column 3 - line 144, column 29): " + [decl.constructor.name]);
            });
          };
        };
      };
    };
  };
  var emptyEnv = empty2;
  var declErr = function(loc) {
    return function(dictError) {
      var toError2 = toError(dictError);
      return function(tyn) {
        return toError2(loc)("Type " + (show36(tyn) + " not found in environment"));
      };
    };
  };
  var lookupDecl = function(dictError) {
    var lookupMDecl1 = lookupMDecl(dictError);
    return function(dictMonadError) {
      var MonadThrow0 = dictMonadError.MonadThrow0();
      var Monad0 = MonadThrow0.Monad0();
      var bind36 = bind(Monad0.Bind1());
      var lookupMDecl2 = lookupMDecl1(dictMonadError);
      var throwError22 = throwError(MonadThrow0);
      var pure39 = pure(Monad0.Applicative0());
      return function(dictMonadReader) {
        var lookupMDecl3 = lookupMDecl2(dictMonadReader);
        return function(loc) {
          return function(tyn) {
            return bind36(lookupMDecl3(tyn))(function(mdecl) {
              if (mdecl instanceof Nothing) {
                return throwError22(declErr(loc)(dictError)(tyn));
              }
              ;
              if (mdecl instanceof Just) {
                return pure39(mdecl.value0);
              }
              ;
              throw new Error("Failed pattern match at Environment (line 94, column 3 - line 96, column 27): " + [mdecl.constructor.name]);
            });
          };
        };
      };
    };
  };
  var addVarEnv = function(nm) {
    return function($$var2) {
      return function(v) {
        var v1 = lookup1(nm)(v);
        if (v1 instanceof Nothing) {
          var newProg = addVarProgram2($$var2)(emptyProg2(nm)(""));
          return insert5(nm)(newProg)(v);
        }
        ;
        if (v1 instanceof Just) {
          return insert5(nm)(addVarProgram2($$var2)(v1.value0))(v);
        }
        ;
        throw new Error("Failed pattern match at Environment (line 68, column 3 - line 70, column 70): " + [v1.constructor.name]);
      };
    };
  };
  var addDeclEnv = function(nm) {
    return function(decl) {
      return function(v) {
        var v1 = lookup1(nm)(v);
        if (v1 instanceof Nothing) {
          var newProg = addDeclProgram2(decl)(emptyProg2(nm)(""));
          return insert5(nm)(newProg)(v);
        }
        ;
        if (v1 instanceof Just) {
          return insert5(nm)(addDeclProgram2(decl)(v1.value0))(v);
        }
        ;
        throw new Error("Failed pattern match at Environment (line 62, column 3 - line 64, column 72): " + [v1.constructor.name]);
      };
    };
  };

  // output/Driver.Definition/index.js
  var monadStateT2 = /* @__PURE__ */ monadStateT(monadIdentity);
  var bind3 = /* @__PURE__ */ bind(/* @__PURE__ */ bindExceptT(monadStateT2));
  var monadStateExceptT2 = /* @__PURE__ */ monadStateExceptT(/* @__PURE__ */ monadStateStateT(monadIdentity));
  var gets2 = /* @__PURE__ */ gets(monadStateExceptT2);
  var modify5 = /* @__PURE__ */ modify2(monadStateExceptT2);
  var pure2 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadStateT2));
  var throwError2 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowExceptT(monadStateT2));
  var lookup3 = /* @__PURE__ */ lookup(ordModulename);
  var MkDriverState = /* @__PURE__ */ function() {
    function MkDriverState2(value0) {
      this.value0 = value0;
    }
    ;
    MkDriverState2.create = function(value0) {
      return new MkDriverState2(value0);
    };
    return MkDriverState2;
  }();
  var withDebug = function(newDb) {
    return function(f) {
      return function(a2) {
        return bind3(gets2(function(v) {
          return v.value0.drvDebugOn;
        }))(function(currDebug) {
          return bind3(modify5(function(v) {
            return new MkDriverState({
              drvEnv: v.value0.drvEnv,
              drvDebug: v.value0.drvDebug,
              drvDebugOn: newDb
            });
          }))(function() {
            return bind3(f(a2))(function(b2) {
              return bind3(modify5(function(v) {
                return new MkDriverState({
                  drvEnv: v.value0.drvEnv,
                  drvDebug: v.value0.drvDebug,
                  drvDebugOn: currDebug
                });
              }))(function() {
                return pure2(b2);
              });
            });
          });
        });
      };
    };
  };
  var runDriverM = function(drvst) {
    return function(m) {
      return runState(runExceptT(m))(drvst);
    };
  };
  var liftErr = function(dictError) {
    var convertError2 = convertError(dictError)(errorDriverError);
    return function(v) {
      return function(v1) {
        return function(v2) {
          if (v instanceof Left) {
            return throwError2(new ErrWithWhere(convertError2(v.value0), v1, v2));
          }
          ;
          if (v instanceof Right) {
            return pure2(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Driver.Definition (line 64, column 1 - line 64, column 82): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    };
  };
  var initialDriverState = /* @__PURE__ */ function() {
    return new MkDriverState({
      drvEnv: emptyEnv,
      drvDebug: Nil.value,
      drvDebugOn: true
    });
  }();
  var inEnv = function(mn) {
    return bind3(gets2(function(v) {
      return v.value0.drvEnv;
    }))(function(v) {
      return pure2(isJust(lookup3(mn)(v)));
    });
  };
  var getProg = function(mn) {
    return bind3(gets2(function(v) {
      return v.value0.drvEnv;
    }))(function(v) {
      var v1 = lookup3(mn)(v);
      if (v1 instanceof Nothing) {
        return throwError2(new ErrNotFound(mn));
      }
      ;
      if (v1 instanceof Just) {
        return pure2(v1.value0);
      }
      ;
      throw new Error("Failed pattern match at Driver.Definition (line 60, column 3 - line 62, column 21): " + [v1.constructor.name]);
    });
  };
  var debug = function(st) {
    return bind3(gets2(function(v) {
      return v.value0.drvDebugOn;
    }))(function(dbOn) {
      if (dbOn) {
        return bind3(modify5(function(v) {
          return new MkDriverState({
            drvEnv: v.value0.drvEnv,
            drvDebugOn: v.value0.drvDebugOn,
            drvDebug: snoc(v.value0.drvDebug)(st)
          });
        }))(function() {
          return pure2(unit);
        });
      }
      ;
      return pure2(unit);
    });
  };
  var addVarDecl = function(nm) {
    return function($$var2) {
      return bind3(modify5(function(v) {
        return new MkDriverState({
          drvDebug: v.value0.drvDebug,
          drvDebugOn: v.value0.drvDebugOn,
          drvEnv: addVarEnv(nm)($$var2)(v.value0.drvEnv)
        });
      }))(function() {
        return pure2(unit);
      });
    };
  };
  var addDecl = function(nm) {
    return function(decl) {
      return bind3(modify5(function(v) {
        return new MkDriverState({
          drvDebug: v.value0.drvDebug,
          drvDebugOn: v.value0.drvDebugOn,
          drvEnv: addDeclEnv(nm)(decl)(v.value0.drvEnv)
        });
      }))(function() {
        return pure2(unit);
      });
    };
  };

  // output/Constraints/index.js
  var show21 = /* @__PURE__ */ show(showTy2);
  var show112 = /* @__PURE__ */ show(showKind);
  var intercalate11 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map19 = /* @__PURE__ */ map(functorList);
  var MkTyEq = /* @__PURE__ */ function() {
    function MkTyEq2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MkTyEq2.create = function(value0) {
      return function(value1) {
        return new MkTyEq2(value0, value1);
      };
    };
    return MkTyEq2;
  }();
  var MkKindEq = /* @__PURE__ */ function() {
    function MkKindEq2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MkKindEq2.create = function(value0) {
      return function(value1) {
        return new MkKindEq2(value0, value1);
      };
    };
    return MkKindEq2;
  }();
  var MkKindNeq = /* @__PURE__ */ function() {
    function MkKindNeq2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MkKindNeq2.create = function(value0) {
      return function(value1) {
        return new MkKindNeq2(value0, value1);
      };
    };
    return MkKindNeq2;
  }();
  var showConstr = {
    show: function(v) {
      if (v instanceof MkTyEq) {
        return show21(v.value0) + (" = " + show21(v.value1));
      }
      ;
      if (v instanceof MkKindEq) {
        return show112(v.value0) + (" = " + show112(v.value1));
      }
      ;
      if (v instanceof MkKindNeq) {
        return show112(v.value0) + ("!=" + show112(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Constraints (line 17, column 1 - line 20, column 61): " + [v.constructor.name]);
    }
  };
  var show210 = /* @__PURE__ */ show(showConstr);
  var showConstrs = function(cs) {
    return intercalate11("\n	")(map19(show210)(cs));
  };

  // output/Control.Monad.Except/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap3(runExceptT($3));
  };

  // output/Control.Monad.Reader.Trans/index.js
  var ReaderT = function(x) {
    return x;
  };
  var withReaderT = function(f) {
    return function(v) {
      return function($146) {
        return v(f($146));
      };
    };
  };
  var runReaderT = function(v) {
    return v;
  };
  var monadTransReaderT = {
    lift: function(dictMonad) {
      return function($147) {
        return ReaderT($$const($147));
      };
    }
  };
  var lift5 = /* @__PURE__ */ lift(monadTransReaderT);
  var mapReaderT = function(f) {
    return function(v) {
      return function($148) {
        return f(v($148));
      };
    };
  };
  var functorReaderT = function(dictFunctor) {
    return {
      map: function() {
        var $149 = map(dictFunctor);
        return function($150) {
          return mapReaderT($149($150));
        };
      }()
    };
  };
  var applyReaderT = function(dictApply) {
    var apply3 = apply(dictApply);
    var functorReaderT1 = functorReaderT(dictApply.Functor0());
    return {
      apply: function(v) {
        return function(v1) {
          return function(r) {
            return apply3(v(r))(v1(r));
          };
        };
      },
      Functor0: function() {
        return functorReaderT1;
      }
    };
  };
  var bindReaderT = function(dictBind) {
    var bind36 = bind(dictBind);
    var applyReaderT1 = applyReaderT(dictBind.Apply0());
    return {
      bind: function(v) {
        return function(k) {
          return function(r) {
            return bind36(v(r))(function(a2) {
              var v1 = k(a2);
              return v1(r);
            });
          };
        };
      },
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var applicativeReaderT = function(dictApplicative) {
    var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
    return {
      pure: function() {
        var $154 = pure(dictApplicative);
        return function($155) {
          return ReaderT($$const($154($155)));
        };
      }(),
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var monadReaderT = function(dictMonad) {
    var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
    var bindReaderT1 = bindReaderT(dictMonad.Bind1());
    return {
      Applicative0: function() {
        return applicativeReaderT1;
      },
      Bind1: function() {
        return bindReaderT1;
      }
    };
  };
  var monadAskReaderT = function(dictMonad) {
    var monadReaderT1 = monadReaderT(dictMonad);
    return {
      ask: pure(dictMonad.Applicative0()),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadReaderReaderT = function(dictMonad) {
    var monadAskReaderT1 = monadAskReaderT(dictMonad);
    return {
      local: withReaderT,
      MonadAsk0: function() {
        return monadAskReaderT1;
      }
    };
  };
  var monadStateReaderT = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
      state: function() {
        var $160 = lift5(Monad0);
        var $161 = state(dictMonadState);
        return function($162) {
          return $160($161($162));
        };
      }(),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadThrowReaderT = function(dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
      throwError: function() {
        var $166 = lift5(Monad0);
        var $167 = throwError(dictMonadThrow);
        return function($168) {
          return $166($167($168));
        };
      }(),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadErrorReaderT = function(dictMonadError) {
    var catchError2 = catchError(dictMonadError);
    var monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
    return {
      catchError: function(v) {
        return function(h) {
          return function(r) {
            return catchError2(v(r))(function(e) {
              var v1 = h(e);
              return v1(r);
            });
          };
        };
      },
      MonadThrow0: function() {
        return monadThrowReaderT1;
      }
    };
  };

  // output/Dependencies.Graph/index.js
  var find3 = /* @__PURE__ */ find(foldableSet);
  var elem3 = /* @__PURE__ */ elem(foldableList);
  var MkEdge = /* @__PURE__ */ function() {
    function MkEdge2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MkEdge2.create = function(value0) {
      return function(value1) {
        return new MkEdge2(value0, value1);
      };
    };
    return MkEdge2;
  }();
  var MkGraph = /* @__PURE__ */ function() {
    function MkGraph2(value0) {
      this.value0 = value0;
    }
    ;
    MkGraph2.create = function(value0) {
      return new MkGraph2(value0);
    };
    return MkGraph2;
  }();
  var getVertexLabel = function(v) {
    return v;
  };
  var getVertex = function(dictEq) {
    var eq31 = eq(dictEq);
    return function(lb) {
      return function(v) {
        return find3(function(v1) {
          return eq31(lb)(v1);
        })(v.value0.grVerts);
      };
    };
  };
  var getStartingVert = function(v) {
    return v.value0;
  };
  var getEndingVert = function(v) {
    return v.value1;
  };
  var eqVertex = function(dictEq) {
    var eq31 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          return eq31(x)(y);
        };
      }
    };
  };
  var getEdgesEndingAt = function(dictEq) {
    var eq31 = eq(eqVertex(dictEq));
    return function(vert) {
      return function(v) {
        return filter(function(e) {
          return eq31(getEndingVert(e))(vert);
        })(v.value0.grEdges);
      };
    };
  };
  var getEdgesStartingAt = function(dictEq) {
    var eq31 = eq(eqVertex(dictEq));
    return function(vert) {
      return function(v) {
        return filter(function(e) {
          return eq31(getStartingVert(e))(vert);
        })(v.value0.grEdges);
      };
    };
  };
  var ordVertex = function(dictOrd) {
    var compare5 = compare(dictOrd);
    var eqVertex1 = eqVertex(dictOrd.Eq0());
    return {
      compare: function(x) {
        return function(y) {
          return compare5(x)(y);
        };
      },
      Eq0: function() {
        return eqVertex1;
      }
    };
  };
  var eqEdge = function(dictEq) {
    var eq31 = eq(eqVertex(dictEq));
    return {
      eq: function(x) {
        return function(y) {
          return eq31(x.value0)(y.value0) && eq31(x.value1)(y.value1);
        };
      }
    };
  };
  var emptyGraph = /* @__PURE__ */ function() {
    return new MkGraph({
      grVerts: empty3,
      grEdges: Nil.value
    });
  }();
  var addVertex = function(dictEq) {
    var getVertex1 = getVertex(dictEq);
    return function(dictOrd) {
      var insert15 = insert2(ordVertex(dictOrd));
      return function(v) {
        return function(v1) {
          var v3 = getVertex1(v)(v1);
          if (v3 instanceof Just) {
            return new Tuple(v3.value0, v1);
          }
          ;
          if (v3 instanceof Nothing) {
            return new Tuple(v, new MkGraph({
              grVerts: insert15(v)(v1.value0.grVerts),
              grEdges: v1.value0.grEdges
            }));
          }
          ;
          throw new Error("Failed pattern match at Dependencies.Graph (line 43, column 3 - line 47, column 79): " + [v3.constructor.name]);
        };
      };
    };
  };
  var addEdge = function(dictEq) {
    var elem14 = elem3(eqEdge(dictEq));
    return function(v) {
      return function(v1) {
        var $140 = elem14(new MkEdge(v.value0, v.value1))(v1.value0.grEdges);
        if ($140) {
          return v1;
        }
        ;
        return new MkGraph({
          grVerts: v1.value0.grVerts,
          grEdges: new Cons(new MkEdge(v.value0, v.value1), v1.value0.grEdges)
        });
      };
    };
  };

  // output/Dependencies.Definition/index.js
  var monadExceptT2 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind4 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT2)));
  var monadStateReaderT2 = /* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT2));
  var gets3 = /* @__PURE__ */ gets(monadStateReaderT2);
  var modify6 = /* @__PURE__ */ modify2(monadStateReaderT2);
  var applicativeReaderT2 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT2));
  var pure3 = /* @__PURE__ */ pure(applicativeReaderT2);
  var throwError3 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var map20 = /* @__PURE__ */ map(functorList);
  var elem4 = /* @__PURE__ */ elem(foldableList);
  var unless2 = /* @__PURE__ */ unless(applicativeReaderT2);
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var $$for2 = /* @__PURE__ */ $$for(applicativeReaderT2)(traversableList);
  var toUnfoldable8 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var runDepM = function(env) {
    return function(m) {
      var v = runExcept(runStateT(runReaderT(m)(env))(emptyGraph));
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return new Right(v.value0.value0);
      }
      ;
      throw new Error("Failed pattern match at Dependencies.Definition (line 33, column 17 - line 35, column 31): " + [v.constructor.name]);
    };
  };
  var removeSelfLoops = function(dictShow) {
    return function(dictEq) {
      var notEq5 = notEq(eqVertex(dictEq));
      return bind4(gets3(function(v) {
        return v.value0;
      }))(function(gr) {
        var newEdgs = filter(function(v) {
          return notEq5(v.value0)(v.value1);
        })(gr.grEdges);
        var gr$prime = new MkGraph({
          grVerts: gr.grVerts,
          grEdges: newEdgs
        });
        return bind4(modify6($$const(gr$prime)))(function() {
          return pure3(unit);
        });
      });
    };
  };
  var getVertexError = function(dictEq) {
    var getVertex2 = getVertex(dictEq);
    return function(lb) {
      return function(err) {
        return bind4(gets3(function(v) {
          return v.value0;
        }))(function(gr) {
          var mvert = getVertex2(lb)(new MkGraph(gr));
          if (mvert instanceof Nothing) {
            return throwError3(err);
          }
          ;
          if (mvert instanceof Just) {
            return pure3(mvert.value0);
          }
          ;
          throw new Error("Failed pattern match at Dependencies.Definition (line 48, column 3 - line 50, column 27): " + [mvert.constructor.name]);
        });
      };
    };
  };
  var ensureAcyclic = function(dictShow) {
    return function(dictEq) {
      var getEdgesStartingAt3 = getEdgesStartingAt(dictEq);
      var elem14 = elem4(eqVertex(dictEq));
      return function(dictShow1) {
        return function(err) {
          var traverseGr = function(dictShow2) {
            return function(dictEq1) {
              return function(gr) {
                return function(startV) {
                  return function(seenV) {
                    var newSeenV = new Cons(startV, seenV);
                    var outg = map20(getEndingVert)(getEdgesStartingAt3(startV)(gr));
                    var elems = map20(function(x) {
                      return elem14(x)(outg);
                    })(newSeenV);
                    return bind4(unless2($$null(filter(identity10)(elems)))(throwError3(err)))(function() {
                      return bind4($$for2(outg)(function(v) {
                        return traverseGr(dictShow)(dictEq)(gr)(v)(newSeenV);
                      }))(function() {
                        return pure3(Nil.value);
                      });
                    });
                  };
                };
              };
            };
          };
          var traverseGr1 = traverseGr(dictShow)(dictEq);
          return bind4(gets3(function(v) {
            return v.value0;
          }))(function(gr) {
            var verts = toUnfoldable8(gr.grVerts);
            return bind4($$for2(verts)(function(v) {
              return traverseGr1(new MkGraph(gr))(v)(Nil.value);
            }))(function() {
              return pure3(unit);
            });
          });
        };
      };
    };
  };
  var addVertexM = function(dictEq) {
    var addVertex2 = addVertex(dictEq);
    return function(dictOrd) {
      var addVertex1 = addVertex2(dictOrd);
      return function(a2) {
        return bind4(gets3(function(v) {
          return v.value0;
        }))(function(gr) {
          var v = addVertex1(a2)(new MkGraph(gr));
          return bind4(modify6($$const(v.value1)))(function() {
            return pure3(v.value0);
          });
        });
      };
    };
  };
  var addEdgeM = function(dictEq) {
    var addEdge2 = addEdge(dictEq);
    return function(e) {
      return bind4(modify6(addEdge2(e)))(function() {
        return pure3(unit);
      });
    };
  };

  // output/Dependencies.Errors/index.js
  var show30 = /* @__PURE__ */ show(showModulename);
  var show113 = /* @__PURE__ */ show(showVariable);
  var ErrDuplModule = /* @__PURE__ */ function() {
    function ErrDuplModule2(value0) {
      this.value0 = value0;
    }
    ;
    ErrDuplModule2.create = function(value0) {
      return new ErrDuplModule2(value0);
    };
    return ErrDuplModule2;
  }();
  var ErrUndefinedModule = /* @__PURE__ */ function() {
    function ErrUndefinedModule2(value0) {
      this.value0 = value0;
    }
    ;
    ErrUndefinedModule2.create = function(value0) {
      return new ErrUndefinedModule2(value0);
    };
    return ErrUndefinedModule2;
  }();
  var ErrMutualRec = /* @__PURE__ */ function() {
    function ErrMutualRec2(value0) {
      this.value0 = value0;
    }
    ;
    ErrMutualRec2.create = function(value0) {
      return new ErrMutualRec2(value0);
    };
    return ErrMutualRec2;
  }();
  var ErrUndefinedVar = /* @__PURE__ */ function() {
    function ErrUndefinedVar3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrUndefinedVar3.create = function(value0) {
      return function(value1) {
        return new ErrUndefinedVar3(value0, value1);
      };
    };
    return ErrUndefinedVar3;
  }();
  var ErrOther2 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorDepError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrDuplModule) {
          return "Module " + (show30(v.value0) + " was defined multiple times");
        }
        ;
        if (v instanceof ErrUndefinedModule) {
          return "Module " + (show30(v.value0) + " was not defined");
        }
        ;
        if (v instanceof ErrMutualRec) {
          return "Mutual Recusrion in module " + show30(v.value0);
        }
        ;
        if (v instanceof ErrUndefinedVar) {
          return "Variable " + (show113(v.value1) + " was not defined");
        }
        ;
        if (v instanceof ErrOther2) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at Dependencies.Errors (line 16, column 1 - line 29, column 23): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrDuplModule) {
          return defaultLoc;
        }
        ;
        if (v instanceof ErrUndefinedModule) {
          return defaultLoc;
        }
        ;
        if (v instanceof ErrMutualRec) {
          return defaultLoc;
        }
        ;
        if (v instanceof ErrUndefinedVar) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther2) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Dependencies.Errors (line 16, column 1 - line 29, column 23): " + [v.constructor.name]);
      },
      toError: ErrOther2.create
    };
  }();

  // output/Dependencies.ImportsGraph/index.js
  var monadExceptT3 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bindReaderT2 = /* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT3));
  var bind5 = /* @__PURE__ */ bind(bindReaderT2);
  var get2 = /* @__PURE__ */ get(/* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT3)));
  var getVertexError2 = /* @__PURE__ */ getVertexError(eqModulename);
  var map21 = /* @__PURE__ */ map(functorList);
  var getEdgesStartingAt2 = /* @__PURE__ */ getEdgesStartingAt(eqModulename);
  var applicativeReaderT3 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT3));
  var pure4 = /* @__PURE__ */ pure(applicativeReaderT3);
  var map110 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var $$for3 = /* @__PURE__ */ $$for(applicativeReaderT3)(traversableList);
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindReaderT2);
  var when2 = /* @__PURE__ */ when(applicativeReaderT3);
  var elem5 = /* @__PURE__ */ elem(foldableList)(eqModulename);
  var throwError4 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var append2 = /* @__PURE__ */ append(semigroupList);
  var addVertexM2 = /* @__PURE__ */ addVertexM(eqModulename)(ordModulename);
  var addEdgeM2 = /* @__PURE__ */ addEdgeM(eqModulename);
  var ensureAcyclic2 = /* @__PURE__ */ ensureAcyclic(showModulename)(eqModulename)(showModulename);
  var getImportOrder = function(mn) {
    return bind5(get2)(function(gr) {
      return bind5(getVertexError2(mn)(new ErrUndefinedModule(mn)))(function(vert) {
        var deps = map21(function(x) {
          return getVertexLabel(getEndingVert(x));
        })(getEdgesStartingAt2(vert)(gr));
        if (deps instanceof Nil) {
          return pure4(new Cons(mn, Nil.value));
        }
        ;
        return bind5(map110(concat)($$for3(deps)(getImportOrder)))(function(orders) {
          return discard2(when2(elem5(mn)(orders))(throwError4(new ErrDuplModule(mn))))(function() {
            return pure4(append2(orders)(new Cons(mn, Nil.value)));
          });
        });
      });
    });
  };
  var addDependencies = function(v) {
    return bind5(addVertexM2(v.value0.progName))(function(v1) {
      return bind5($$for3(v.value0.progImports)(function(v2) {
        return bind5(addVertexM2(v2.value0.importName))(function(v21) {
          return discard2(addEdgeM2(new Tuple(v1, v21)))(function() {
            return pure4(unit);
          });
        });
      }))(function() {
        return pure4(unit);
      });
    });
  };
  var depOrderModule = function(v) {
    return function(imports) {
      return discard2(addDependencies(v))(function() {
        return bind5($$for3(imports)(addDependencies))(function() {
          return discard2(ensureAcyclic2(new ErrDuplModule(v.value0.progName)))(function() {
            return getImportOrder(v.value0.progName);
          });
        });
      });
    };
  };

  // output/Dependencies.VariablesGraph/index.js
  var monadExceptT4 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind6 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT4)));
  var get3 = /* @__PURE__ */ get(/* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT4)));
  var map22 = /* @__PURE__ */ map(functorList);
  var getEdgesEndingAt2 = /* @__PURE__ */ getEdgesEndingAt(eqVariable);
  var applicativeReaderT4 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT4));
  var pure5 = /* @__PURE__ */ pure(applicativeReaderT4);
  var $$for4 = /* @__PURE__ */ $$for(applicativeReaderT4);
  var for1 = /* @__PURE__ */ $$for4(traversableList);
  var monadStateT3 = /* @__PURE__ */ monadStateT(monadExceptT4);
  var foldM2 = /* @__PURE__ */ foldM(foldableSet)(/* @__PURE__ */ monadReaderT(monadStateT3));
  var elem6 = /* @__PURE__ */ elem(foldableList);
  var elem1 = /* @__PURE__ */ elem6(/* @__PURE__ */ eqVertex(eqVariable));
  var map111 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var append3 = /* @__PURE__ */ append(semigroupList);
  var addVertexM3 = /* @__PURE__ */ addVertexM(eqVariable)(ordVariable);
  var monadErrorReaderT2 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity)));
  var monadReaderReaderT2 = /* @__PURE__ */ monadReaderReaderT(monadStateT3);
  var lookupMVar2 = /* @__PURE__ */ lookupMVar(errorDepError)(monadErrorReaderT2)(monadReaderReaderT2);
  var lookupMXtor2 = /* @__PURE__ */ lookupMXtor(errorDepError)(monadErrorReaderT2)(monadReaderReaderT2);
  var elem22 = /* @__PURE__ */ elem6(eqVariable);
  var getVertexError3 = /* @__PURE__ */ getVertexError(eqVariable);
  var addEdgeM3 = /* @__PURE__ */ addEdgeM(eqVariable);
  var for2 = /* @__PURE__ */ $$for4(traversableMap);
  var toUnfoldable9 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var removeSelfLoops2 = /* @__PURE__ */ removeSelfLoops(showVariable)(eqVariable);
  var ensureAcyclic3 = /* @__PURE__ */ ensureAcyclic(showVariable)(eqVariable)(showVariable);
  var getVarOrderFrom = function(vert) {
    return bind6(get3)(function(gr) {
      var preds = map22(getStartingVert)(getEdgesEndingAt2(vert)(gr));
      if (preds instanceof Nil) {
        return pure5(new Cons(vert, Nil.value));
      }
      ;
      return bind6(for1(preds)(getVarOrderFrom))(function(predOrders) {
        return pure5(new Cons(vert, concat(predOrders)));
      });
    });
  };
  var getVarOrder = /* @__PURE__ */ bind6(get3)(function(v) {
    return foldM2(function(doneOrder) {
      return function(v1) {
        var $66 = elem1(v1)(doneOrder);
        if ($66) {
          return pure5(doneOrder);
        }
        ;
        return map111(function(x) {
          return append3(doneOrder)(x);
        })(getVarOrderFrom(v1));
      };
    })(Nil.value)(v.value0.grVerts);
  });
  var addVariable = function(v) {
    return bind6(addVertexM3(v.value0.varName))(function(vert) {
      return pure5(new Tuple(vert, v.value0.varBody));
    });
  };
  var addEdgesVariableT = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Var2) {
          return bind6(lookupMVar2(v2.value1))(function(mdef) {
            return bind6(lookupMXtor2(function(v3) {
              return new Xtorname(v3.value0);
            }(v2.value1)))(function(mxtor) {
              var $75 = isJust(mdef) || (isJust(mxtor) || elem22(v2.value1)(v1));
              if ($75) {
                return pure5(unit);
              }
              ;
              return bind6(getVertexError3(v2.value1)(new ErrUndefinedVar(v2.value0, v2.value1)))(function(vert$prime) {
                return addEdgeM3(new Tuple(v, vert$prime));
              });
            });
          });
        }
        ;
        if (v2 instanceof Mu2) {
          return addEdgesVariableC(v)(new Cons(v2.value1, v1))(v2.value2);
        }
        ;
        if (v2 instanceof Xtor2) {
          return bind6(for1(v2.value2)(addEdgesVariableT(v)(new Cons(new Variable(v2.value1.value0), v1))))(function() {
            return pure5(unit);
          });
        }
        ;
        if (v2 instanceof XCase2) {
          return bind6(for1(v2.value1)(addEdgesVariablePt(v)(v1)))(function() {
            return pure5(unit);
          });
        }
        ;
        if (v2 instanceof ShiftCBV2) {
          return addEdgesVariableT(v)(v1)(v2.value1);
        }
        ;
        if (v2 instanceof ShiftCBN2) {
          return addEdgesVariableT(v)(v1)(v2.value1);
        }
        ;
        throw new Error("Failed pattern match at Dependencies.VariablesGraph (line 42, column 1 - line 42, column 77): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var addEdgesVariablePt = function(vert) {
    return function(ignore) {
      return function(v) {
        return addEdgesVariableC(vert)(append3(v.value0.ptv)(new Cons(new Variable(v.value0.ptxt.value0), ignore)))(v.value0.ptcmd);
      };
    };
  };
  var addEdgesVariableC = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Cut2) {
          return bind6(addEdgesVariableT(v)(v1)(v2.value1))(function() {
            return bind6(addEdgesVariableT(v)(v1)(v2.value3))(function() {
              return pure5(unit);
            });
          });
        }
        ;
        if (v2 instanceof CutAnnot) {
          return bind6(addEdgesVariableT(v)(v1)(v2.value1))(function() {
            return bind6(addEdgesVariableT(v)(v1)(v2.value4))(function() {
              return pure5(unit);
            });
          });
        }
        ;
        if (v2 instanceof Done3) {
          return pure5(unit);
        }
        ;
        if (v2 instanceof Err2) {
          return pure5(unit);
        }
        ;
        if (v2 instanceof Print2) {
          return addEdgesVariableT(v)(v1)(v2.value1);
        }
        ;
        if (v2 instanceof PrintAnnot) {
          return addEdgesVariableT(v)(v1)(v2.value1);
        }
        ;
        throw new Error("Failed pattern match at Dependencies.VariablesGraph (line 65, column 1 - line 65, column 80): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var depOrderProgram = function(v) {
    return bind6(for2(v.value0.progVars)(addVariable))(function(vertsTerms) {
      var xtToVar = function(v1) {
        return new Variable(v1.value0);
      };
      var ignore = map22(function(v1) {
        return xtToVar(v1.value0.sigName);
      })(concatMap(function(v1) {
        return v1.value0.declXtors;
      })(map22(snd)(toUnfoldable9(v.value0.progDecls))));
      return bind6(for2(vertsTerms)(function(v1) {
        return addEdgesVariableT(v1.value0)(ignore)(v1.value1);
      }))(function() {
        return bind6(removeSelfLoops2)(function() {
          return bind6(ensureAcyclic3(new ErrMutualRec(v.value0.progName)))(function() {
            return bind6(getVarOrder)(function(order) {
              return pure5(map22(getVertexLabel)(order));
            });
          });
        });
      });
    });
  };

  // output/Desugar.Errors/index.js
  var show31 = /* @__PURE__ */ show(showVariable);
  var show114 = /* @__PURE__ */ show(showTypename);
  var show211 = /* @__PURE__ */ show(showTy);
  var show37 = /* @__PURE__ */ show(showXtorname);
  var ErrVariable = /* @__PURE__ */ function() {
    function ErrVariable2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrVariable2.create = function(value0) {
      return function(value1) {
        return new ErrVariable2(value0, value1);
      };
    };
    return ErrVariable2;
  }();
  var ErrMultipleNames = /* @__PURE__ */ function() {
    function ErrMultipleNames2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrMultipleNames2.create = function(value0) {
      return function(value1) {
        return new ErrMultipleNames2(value0, value1);
      };
    };
    return ErrMultipleNames2;
  }();
  var ErrMultipleXtor = /* @__PURE__ */ function() {
    function ErrMultipleXtor2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrMultipleXtor2.create = function(value0) {
      return function(value1) {
        return new ErrMultipleXtor2(value0, value1);
      };
    };
    return ErrMultipleXtor2;
  }();
  var ErrMultipleAnnot = /* @__PURE__ */ function() {
    function ErrMultipleAnnot2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrMultipleAnnot2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrMultipleAnnot2(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrMultipleAnnot2;
  }();
  var ErrMultipleMain = /* @__PURE__ */ function() {
    function ErrMultipleMain2(value0) {
      this.value0 = value0;
    }
    ;
    ErrMultipleMain2.create = function(value0) {
      return new ErrMultipleMain2(value0);
    };
    return ErrMultipleMain2;
  }();
  var ErrEmptyPair = /* @__PURE__ */ function() {
    function ErrEmptyPair2(value0) {
      this.value0 = value0;
    }
    ;
    ErrEmptyPair2.create = function(value0) {
      return new ErrEmptyPair2(value0);
    };
    return ErrEmptyPair2;
  }();
  var ErrOther3 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorDesugarError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrVariable) {
          return "Definition for variable " + (show31(v.value1) + "could not be found");
        }
        ;
        if (v instanceof ErrMultipleNames) {
          return show114(v.value1) + " was defined multiple times";
        }
        ;
        if (v instanceof ErrMultipleAnnot) {
          return "Multiple incompatible annotations for variable" + (show31(v.value1) + (": " + (show211(v.value2) + (" and " + show211(v.value3)))));
        }
        ;
        if (v instanceof ErrMultipleXtor) {
          return show37(v.value1) + " was defined multiple times ";
        }
        ;
        if (v instanceof ErrMultipleMain) {
          return "Multiple definitions of main";
        }
        ;
        if (v instanceof ErrEmptyPair) {
          return "Pairs require at least two elements";
        }
        ;
        if (v instanceof ErrOther3) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at Desugar.Errors (line 19, column 1 - line 36, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrVariable) {
          return v.value0;
        }
        ;
        if (v instanceof ErrMultipleNames) {
          return v.value0;
        }
        ;
        if (v instanceof ErrMultipleXtor) {
          return v.value0;
        }
        ;
        if (v instanceof ErrMultipleAnnot) {
          return v.value0;
        }
        ;
        if (v instanceof ErrMultipleMain) {
          return v.value0;
        }
        ;
        if (v instanceof ErrEmptyPair) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther3) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Desugar.Errors (line 19, column 1 - line 36, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther3.create
    };
  }();

  // output/Desugar.Definition/index.js
  var monadExceptT5 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind7 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT5)));
  var monadStateReaderT3 = /* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT5));
  var modify7 = /* @__PURE__ */ modify2(monadStateReaderT3);
  var pure6 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT5)));
  var lookupMXtor3 = /* @__PURE__ */ lookupMXtor(errorDesugarError)(/* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity))))(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT5)));
  var gets4 = /* @__PURE__ */ gets(monadStateReaderT3);
  var map23 = /* @__PURE__ */ map(functorList);
  var toUnfoldable10 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var find4 = /* @__PURE__ */ find(foldableList);
  var eq11 = /* @__PURE__ */ eq(eqXtorname);
  var lookup4 = /* @__PURE__ */ lookup(ordVariable);
  var throwError5 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var MkDesugarState = /* @__PURE__ */ function() {
    function MkDesugarState2(value0) {
      this.value0 = value0;
    }
    ;
    MkDesugarState2.create = function(value0) {
      return new MkDesugarState2(value0);
    };
    return MkDesugarState2;
  }();
  var varToXtor = function(v) {
    return new Xtorname(v.value0);
  };
  var tyvarToTyName = function(v) {
    return new Typename(v.value0);
  };
  var setDesMain = function(m) {
    return bind7(modify7(function(v) {
      return new MkDesugarState({
        desCurrDecl: v.value0.desCurrDecl,
        desDone: setMainProgram(m)(v.value0.desDone)
      });
    }))(function() {
      return pure6(unit);
    });
  };
  var setDesCurrDecl = function(decl) {
    return bind7(modify7(function(v) {
      return new MkDesugarState({
        desDone: v.value0.desDone,
        desCurrDecl: new Just(decl)
      });
    }))(function() {
      return pure6(unit);
    });
  };
  var initialDesugarState = function(nm) {
    return new MkDesugarState({
      desCurrDecl: Nothing.value,
      desDone: emptyProg(nm)("")
    });
  };
  var runDesugarM = function(env) {
    return function(nm) {
      return function(m) {
        var v = runExcept(runStateT(runReaderT(m)(env))(initialDesugarState(nm)));
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return new Right(v.value0.value0);
        }
        ;
        throw new Error("Failed pattern match at Desugar.Definition (line 47, column 24 - line 49, column 32): " + [v.constructor.name]);
      };
    };
  };
  var getDesMXtor = function(xtn) {
    return bind7(lookupMXtor3(xtn))(function(msig) {
      return bind7(gets4(function(v) {
        return v.value0.desDone;
      }))(function(doneProg) {
        var doneDecls = function(v2) {
          return map23(snd)(toUnfoldable10(v2.value0.progDecls));
        }(doneProg);
        var xtors = concatMap(function(v2) {
          return v2.value0.declXtors;
        })(doneDecls);
        var msig$prime = find4(function(v2) {
          return eq11(v2.value0.sigName)(xtn);
        })(xtors);
        var v = new Tuple(msig, msig$prime);
        if (v.value0 instanceof Nothing && v.value1 instanceof Nothing) {
          return pure6(Nothing.value);
        }
        ;
        if (v.value0 instanceof Just) {
          return pure6(new Just(embedXtorSig(embedXtorSig2(v.value0.value0))));
        }
        ;
        if (v.value1 instanceof Just) {
          return pure6(new Just(v.value1.value0));
        }
        ;
        throw new Error("Failed pattern match at Desugar.Definition (line 81, column 3 - line 84, column 44): " + [v.constructor.name]);
      });
    });
  };
  var getDesDoneVar = function(loc) {
    return function(v) {
      return bind7(gets4(function(v1) {
        return v1.value0.desDone;
      }))(function(doneProg) {
        var doneVars = function(v12) {
          return v12.value0.progVars;
        }(doneProg);
        var v1 = lookup4(v)(doneVars);
        if (v1 instanceof Nothing) {
          return throwError5(new ErrVariable(loc, v));
        }
        ;
        if (v1 instanceof Just) {
          return pure6(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Desugar.Definition (line 90, column 3 - line 92, column 31): " + [v1.constructor.name]);
      });
    };
  };
  var getDesDoneProg = /* @__PURE__ */ gets4(function(v) {
    return v.value0.desDone;
  });
  var getDesDefNames = /* @__PURE__ */ bind7(getDesDoneProg)(function(doneProg) {
    var doneNames = function(v) {
      return map23(fst)(toUnfoldable10(v.value0.progDecls));
    }(doneProg);
    return bind7(gets4(function(v) {
      return v.value0.desCurrDecl;
    }))(function(curr) {
      if (curr instanceof Nothing) {
        return pure6(doneNames);
      }
      ;
      if (curr instanceof Just) {
        return pure6(new Cons(curr.value0.value0.declName, doneNames));
      }
      ;
      throw new Error("Failed pattern match at Desugar.Definition (line 65, column 3 - line 67, column 66): " + [curr.constructor.name]);
    });
  });
  var addDesVar = function($$var2) {
    return bind7(modify7(function(v) {
      return new MkDesugarState({
        desCurrDecl: v.value0.desCurrDecl,
        desDone: addVarProgram($$var2)(v.value0.desDone)
      });
    }))(function() {
      return pure6(unit);
    });
  };
  var addDesDecl = function(decl) {
    return bind7(modify7(function(v) {
      return new MkDesugarState({
        desCurrDecl: v.value0.desCurrDecl,
        desDone: addDeclProgram(decl)(v.value0.desDone)
      });
    }))(function() {
      return pure6(unit);
    });
  };

  // output/Syntax.Parsed.Types/index.js
  var show38 = /* @__PURE__ */ show(showTypevar);
  var show115 = /* @__PURE__ */ show(showTypename);
  var intercalate12 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map24 = /* @__PURE__ */ map(functorList);
  var eq18 = /* @__PURE__ */ eq(eqTypevar);
  var eq19 = /* @__PURE__ */ eq(eqTypename);
  var eq27 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqTypevar));
  var TyVar4 = /* @__PURE__ */ function() {
    function TyVar5(value0) {
      this.value0 = value0;
    }
    ;
    TyVar5.create = function(value0) {
      return new TyVar5(value0);
    };
    return TyVar5;
  }();
  var TyDecl4 = /* @__PURE__ */ function() {
    function TyDecl5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyDecl5.create = function(value0) {
      return function(value1) {
        return new TyDecl5(value0, value1);
      };
    };
    return TyDecl5;
  }();
  var TyCo4 = /* @__PURE__ */ function() {
    function TyCo5(value0) {
      this.value0 = value0;
    }
    ;
    TyCo5.create = function(value0) {
      return new TyCo5(value0);
    };
    return TyCo5;
  }();
  var TyShift4 = /* @__PURE__ */ function() {
    function TyShift5(value0) {
      this.value0 = value0;
    }
    ;
    TyShift5.create = function(value0) {
      return new TyShift5(value0);
    };
    return TyShift5;
  }();
  var TyForall4 = /* @__PURE__ */ function() {
    function TyForall5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyForall5.create = function(value0) {
      return function(value1) {
        return new TyForall5(value0, value1);
      };
    };
    return TyForall5;
  }();
  var TyFun = /* @__PURE__ */ function() {
    function TyFun2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TyFun2.create = function(value0) {
      return function(value1) {
        return new TyFun2(value0, value1);
      };
    };
    return TyFun2;
  }();
  var showTy4 = {
    show: function(v) {
      if (v instanceof TyVar4) {
        return show38(v.value0);
      }
      ;
      if (v instanceof TyDecl4 && $$null(v.value1)) {
        return show115(v.value0);
      }
      ;
      if (v instanceof TyDecl4) {
        return show115(v.value0) + ("(" + (intercalate12(", ")(map24(show(showTy4))(v.value1)) + ")"));
      }
      ;
      if (v instanceof TyCo4) {
        return "co " + show(showTy4)(v.value0);
      }
      ;
      if (v instanceof TyShift4) {
        return "{" + (show(showTy4)(v.value0) + "}");
      }
      ;
      if (v instanceof TyForall4) {
        return "forall " + (intercalate12(", ")(map24(show38)(v.value0)) + (". " + show(showTy4)(v.value1)));
      }
      ;
      if (v instanceof TyFun) {
        return show(showTy4)(v.value0) + (" -> " + show(showTy4)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Parsed.Types (line 20, column 1 - line 27, column 52): " + [v.constructor.name]);
    }
  };
  var eqTy4 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof TyVar4 && y instanceof TyVar4) {
          return eq18(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyDecl4 && y instanceof TyDecl4) {
          return eq19(x.value0)(y.value0) && eq(eqList(eqTy4))(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyCo4 && y instanceof TyCo4) {
          return eq(eqTy4)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyShift4 && y instanceof TyShift4) {
          return eq(eqTy4)(x.value0)(y.value0);
        }
        ;
        if (x instanceof TyForall4 && y instanceof TyForall4) {
          return eq27(x.value0)(y.value0) && eq(eqTy4)(x.value1)(y.value1);
        }
        ;
        if (x instanceof TyFun && y instanceof TyFun) {
          return eq(eqTy4)(x.value0)(y.value0) && eq(eqTy4)(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };

  // output/Desugar.Types/index.js
  var monadExceptT6 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind8 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT6)));
  var getTypeNames2 = /* @__PURE__ */ getTypeNames(errorDesugarError)(/* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity))))(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT6)));
  var elem7 = /* @__PURE__ */ elem(foldableList)(eqTypename);
  var append4 = /* @__PURE__ */ append(semigroupList);
  var applicativeReaderT5 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT6));
  var pure7 = /* @__PURE__ */ pure(applicativeReaderT5);
  var $$for5 = /* @__PURE__ */ $$for(applicativeReaderT5)(traversableList);
  var map25 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var desugarTy = function(v) {
    if (v instanceof TyVar4) {
      var vty = tyvarToTyName(v.value0);
      return bind8(getTypeNames2)(function(declTys) {
        return bind8(getDesDefNames)(function(currNames) {
          var $27 = elem7(vty)(append4(declTys)(currNames));
          if ($27) {
            return pure7(new TyDecl(vty, Nil.value));
          }
          ;
          return pure7(new TyVar(v.value0));
        });
      });
    }
    ;
    if (v instanceof TyDecl4) {
      return bind8($$for5(v.value1)(desugarTy))(function(args$prime) {
        return pure7(new TyDecl(v.value0, args$prime));
      });
    }
    ;
    if (v instanceof TyCo4) {
      return map25(TyCo.create)(desugarTy(v.value0));
    }
    ;
    if (v instanceof TyShift4) {
      return map25(TyShift.create)(desugarTy(v.value0));
    }
    ;
    if (v instanceof TyForall4) {
      return map25(TyForall.create(v.value0))(desugarTy(v.value1));
    }
    ;
    if (v instanceof TyFun) {
      return bind8(desugarTy(v.value0))(function(ty1$prime) {
        return bind8(desugarTy(v.value1))(function(ty2$prime) {
          return pure7(new TyDecl(new Typename("Fun"), new Cons(ty1$prime, new Cons(ty2$prime, Nil.value))));
        });
      });
    }
    ;
    throw new Error("Failed pattern match at Desugar.Types (line 17, column 1 - line 17, column 35): " + [v.constructor.name]);
  };

  // output/Syntax.Parsed.Terms/index.js
  var show39 = /* @__PURE__ */ show(showVariable);
  var show116 = /* @__PURE__ */ show(showXtorname);
  var intercalate13 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map26 = /* @__PURE__ */ map(functorList);
  var show212 = /* @__PURE__ */ show(showEvaluationOrder);
  var show310 = /* @__PURE__ */ show(showTy4);
  var eq110 = /* @__PURE__ */ eq(eqVariable);
  var eq28 = /* @__PURE__ */ eq(eqXtorname);
  var eq36 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqVariable));
  var eq45 = /* @__PURE__ */ eq(eqEvaluationOrder);
  var eq54 = /* @__PURE__ */ eq(eqTy4);
  var Cut4 = /* @__PURE__ */ function() {
    function Cut5(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Cut5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Cut5(value0, value1, value22, value32);
          };
        };
      };
    };
    return Cut5;
  }();
  var CutAnnot2 = /* @__PURE__ */ function() {
    function CutAnnot3(value0, value1, value22, value32, value42) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
    }
    ;
    CutAnnot3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return new CutAnnot3(value0, value1, value22, value32, value42);
            };
          };
        };
      };
    };
    return CutAnnot3;
  }();
  var Err4 = /* @__PURE__ */ function() {
    function Err5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Err5.create = function(value0) {
      return function(value1) {
        return new Err5(value0, value1);
      };
    };
    return Err5;
  }();
  var Print4 = /* @__PURE__ */ function() {
    function Print5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Print5.create = function(value0) {
      return function(value1) {
        return new Print5(value0, value1);
      };
    };
    return Print5;
  }();
  var PrintAnnot2 = /* @__PURE__ */ function() {
    function PrintAnnot3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    PrintAnnot3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new PrintAnnot3(value0, value1, value22);
        };
      };
    };
    return PrintAnnot3;
  }();
  var Done5 = /* @__PURE__ */ function() {
    function Done6(value0) {
      this.value0 = value0;
    }
    ;
    Done6.create = function(value0) {
      return new Done6(value0);
    };
    return Done6;
  }();
  var CaseOf = /* @__PURE__ */ function() {
    function CaseOf2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    CaseOf2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new CaseOf2(value0, value1, value22);
        };
      };
    };
    return CaseOf2;
  }();
  var Var4 = /* @__PURE__ */ function() {
    function Var5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Var5.create = function(value0) {
      return function(value1) {
        return new Var5(value0, value1);
      };
    };
    return Var5;
  }();
  var Mu4 = /* @__PURE__ */ function() {
    function Mu5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Mu5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Mu5(value0, value1, value22);
        };
      };
    };
    return Mu5;
  }();
  var Xtor4 = /* @__PURE__ */ function() {
    function Xtor5(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Xtor5.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Xtor5(value0, value1, value22);
        };
      };
    };
    return Xtor5;
  }();
  var XCase4 = /* @__PURE__ */ function() {
    function XCase5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    XCase5.create = function(value0) {
      return function(value1) {
        return new XCase5(value0, value1);
      };
    };
    return XCase5;
  }();
  var ShiftCBV4 = /* @__PURE__ */ function() {
    function ShiftCBV5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ShiftCBV5.create = function(value0) {
      return function(value1) {
        return new ShiftCBV5(value0, value1);
      };
    };
    return ShiftCBV5;
  }();
  var ShiftCBN4 = /* @__PURE__ */ function() {
    function ShiftCBN5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ShiftCBN5.create = function(value0) {
      return function(value1) {
        return new ShiftCBN5(value0, value1);
      };
    };
    return ShiftCBN5;
  }();
  var App2 = /* @__PURE__ */ function() {
    function App3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    App3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new App3(value0, value1, value22);
        };
      };
    };
    return App3;
  }();
  var Lam = /* @__PURE__ */ function() {
    function Lam2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Lam2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Lam2(value0, value1, value22);
        };
      };
    };
    return Lam2;
  }();
  var Seq = /* @__PURE__ */ function() {
    function Seq2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Seq2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Seq2(value0, value1, value22);
        };
      };
    };
    return Seq2;
  }();
  var Tup = /* @__PURE__ */ function() {
    function Tup2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tup2.create = function(value0) {
      return function(value1) {
        return new Tup2(value0, value1);
      };
    };
    return Tup2;
  }();
  var Lst = /* @__PURE__ */ function() {
    function Lst2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Lst2.create = function(value0) {
      return function(value1) {
        return new Lst2(value0, value1);
      };
    };
    return Lst2;
  }();
  var NotBool = /* @__PURE__ */ function() {
    function NotBool2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NotBool2.create = function(value0) {
      return function(value1) {
        return new NotBool2(value0, value1);
      };
    };
    return NotBool2;
  }();
  var AndBool = /* @__PURE__ */ function() {
    function AndBool2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    AndBool2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new AndBool2(value0, value1, value22);
        };
      };
    };
    return AndBool2;
  }();
  var OrBool = /* @__PURE__ */ function() {
    function OrBool2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrBool2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrBool2(value0, value1, value22);
        };
      };
    };
    return OrBool2;
  }();
  var IfThenElse = /* @__PURE__ */ function() {
    function IfThenElse2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    IfThenElse2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new IfThenElse2(value0, value1, value22, value32);
          };
        };
      };
    };
    return IfThenElse2;
  }();
  var Pattern4 = /* @__PURE__ */ function() {
    function Pattern5(value0) {
      this.value0 = value0;
    }
    ;
    Pattern5.create = function(value0) {
      return new Pattern5(value0);
    };
    return Pattern5;
  }();
  var showTerm4 = {
    show: function(v) {
      if (v instanceof Var4) {
        return show39(v.value1);
      }
      ;
      if (v instanceof Mu4) {
        return "mu " + (show39(v.value1) + (". " + show(showCommand4)(v.value2)));
      }
      ;
      if (v instanceof Xtor4 && $$null(v.value2)) {
        return show116(v.value1);
      }
      ;
      if (v instanceof Xtor4) {
        return show116(v.value1) + ("(" + (intercalate13(", ")(map26(show(showTerm4))(v.value2)) + ")"));
      }
      ;
      if (v instanceof XCase4) {
        return "case {" + (intercalate13(", ")(map26(show(showPattern4))(v.value1)) + "}");
      }
      ;
      if (v instanceof ShiftCBV4) {
        return "{" + (show(showTerm4)(v.value1) + ":CBV}");
      }
      ;
      if (v instanceof ShiftCBN4) {
        return "{" + (show(showTerm4)(v.value1) + ":CBN}");
      }
      ;
      if (v instanceof App2) {
        return show(showTerm4)(v.value1) + (" [" + (show(showTerm4)(v.value2) + "]"));
      }
      ;
      if (v instanceof Lam) {
        return "\\" + (show39(v.value1) + (". " + show(showTerm4)(v.value2)));
      }
      ;
      if (v instanceof Seq) {
        return show(showTerm4)(v.value1) + ("; " + show(showTerm4)(v.value2));
      }
      ;
      if (v instanceof Tup) {
        return "(" + (intercalate13(", ")(map26(show(showTerm4))(v.value1)) + ")");
      }
      ;
      if (v instanceof Lst) {
        return "[" + (intercalate13(", ")(map26(show(showTerm4))(v.value1)) + "]");
      }
      ;
      if (v instanceof NotBool) {
        return "!" + show(showTerm4)(v.value1);
      }
      ;
      if (v instanceof AndBool) {
        return show(showTerm4)(v.value1) + ("&&" + show(showTerm4)(v.value2));
      }
      ;
      if (v instanceof OrBool) {
        return show(showTerm4)(v.value1) + ("||" + show(showTerm4)(v.value2));
      }
      ;
      if (v instanceof IfThenElse) {
        return "if " + (show(showTerm4)(v.value1) + (" then " + (show(showTerm4)(v.value2) + (" else " + show(showTerm4)(v.value3)))));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Parsed.Terms (line 79, column 1 - line 95, column 94): " + [v.constructor.name]);
    }
  };
  var showPattern4 = {
    show: function(v) {
      if ($$null(v.value0.ptv)) {
        return show116(v.value0.ptxt) + (" => " + show(showCommand4)(v.value0.ptcmd));
      }
      ;
      return show116(v.value0.ptxt) + ("(" + (intercalate13(", ")(map26(show39)(v.value0.ptv)) + (") => " + show(showCommand4)(v.value0.ptcmd))));
    }
  };
  var showCommand4 = {
    show: function(v) {
      if (v instanceof Cut4) {
        return "\u3008" + (show(showTerm4)(v.value1) + (" | " + (show212(v.value2) + (" | " + (show(showTerm4)(v.value3) + "\u3009")))));
      }
      ;
      if (v instanceof CutAnnot2) {
        return "\u3008" + (show(showTerm4)(v.value1) + (" | " + (show310(v.value2) + (" | " + (show212(v.value3) + (" | " + (show(showTerm4)(v.value4) + "\u3009")))))));
      }
      ;
      if (v instanceof Done5) {
        return "Done";
      }
      ;
      if (v instanceof Err4) {
        return "error " + v.value1;
      }
      ;
      if (v instanceof Print4) {
        return "Print " + show(showTerm4)(v.value1);
      }
      ;
      if (v instanceof PrintAnnot2) {
        return " Print " + (show(showTerm4)(v.value1) + (" :: " + show310(v.value2)));
      }
      ;
      if (v instanceof CaseOf) {
        return "case " + (show(showTerm4)(v.value1) + (" of { " + (intercalate13(", ")(map26(show(showPattern4))(v.value2)) + "}")));
      }
      ;
      throw new Error("Failed pattern match at Syntax.Parsed.Terms (line 26, column 1 - line 33, column 98): " + [v.constructor.name]);
    }
  };
  var hasLocCommand3 = {
    getLoc: function(v) {
      if (v instanceof Cut4) {
        return v.value0;
      }
      ;
      if (v instanceof CutAnnot2) {
        return v.value0;
      }
      ;
      if (v instanceof Err4) {
        return v.value0;
      }
      ;
      if (v instanceof Done5) {
        return v.value0;
      }
      ;
      if (v instanceof Print4) {
        return v.value0;
      }
      ;
      if (v instanceof PrintAnnot2) {
        return v.value0;
      }
      ;
      if (v instanceof CaseOf) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Syntax.Parsed.Terms (line 35, column 1 - line 50, column 49): " + [v.constructor.name]);
    },
    setLoc: function(v) {
      return function(v1) {
        if (v1 instanceof Cut4) {
          return new Cut4(v, v1.value1, v1.value2, v1.value3);
        }
        ;
        if (v1 instanceof CutAnnot2) {
          return new CutAnnot2(v, v1.value1, v1.value2, v1.value3, v1.value4);
        }
        ;
        if (v1 instanceof Err4) {
          return new Err4(v, v1.value1);
        }
        ;
        if (v1 instanceof Done5) {
          return new Done5(v);
        }
        ;
        if (v1 instanceof Print4) {
          return new Print4(v, v1.value1);
        }
        ;
        if (v1 instanceof PrintAnnot2) {
          return new PrintAnnot2(v, v1.value1, v1.value2);
        }
        ;
        if (v1 instanceof CaseOf) {
          return new CaseOf(v, v1.value1, v1.value2);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Parsed.Terms (line 35, column 1 - line 50, column 49): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var eqTerm3 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Var4 && y instanceof Var4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq110(x.value1)(y.value1);
        }
        ;
        if (x instanceof Mu4 && y instanceof Mu4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq110(x.value1)(y.value1) && eq(eqCommand3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof Xtor4 && y instanceof Xtor4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq28(x.value1)(y.value1) && eq(eqList(eqTerm3))(x.value2)(y.value2);
        }
        ;
        if (x instanceof XCase4 && y instanceof XCase4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqList(eqPattern3))(x.value1)(y.value1);
        }
        ;
        if (x instanceof ShiftCBV4 && y instanceof ShiftCBV4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1);
        }
        ;
        if (x instanceof ShiftCBN4 && y instanceof ShiftCBN4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1);
        }
        ;
        if (x instanceof App2 && y instanceof App2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof Lam && y instanceof Lam) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq110(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof Seq && y instanceof Seq) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof Tup && y instanceof Tup) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqList(eqTerm3))(x.value1)(y.value1);
        }
        ;
        if (x instanceof Lst && y instanceof Lst) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqList(eqTerm3))(x.value1)(y.value1);
        }
        ;
        if (x instanceof NotBool && y instanceof NotBool) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1);
        }
        ;
        if (x instanceof AndBool && y instanceof AndBool) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof OrBool && y instanceof OrBool) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2);
        }
        ;
        if (x instanceof IfThenElse && y instanceof IfThenElse) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqTerm3)(x.value2)(y.value2) && eq(eqTerm3)(x.value3)(y.value3);
        }
        ;
        return false;
      };
    }
  };
  var eqPattern3 = {
    eq: function(x) {
      return function(y) {
        return eq(eqCommand3)(x.value0.ptcmd)(y.value0.ptcmd) && eq36(x.value0.ptv)(y.value0.ptv) && eq28(x.value0.ptxt)(y.value0.ptxt);
      };
    }
  };
  var eqCommand3 = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Cut4 && y instanceof Cut4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq45(x.value2)(y.value2) && eq(eqTerm3)(x.value3)(y.value3);
        }
        ;
        if (x instanceof CutAnnot2 && y instanceof CutAnnot2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq54(x.value2)(y.value2) && eq45(x.value3)(y.value3) && eq(eqTerm3)(x.value4)(y.value4);
        }
        ;
        if (x instanceof Err4 && y instanceof Err4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && x.value1 === y.value1;
        }
        ;
        if (x instanceof Print4 && y instanceof Print4) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1);
        }
        ;
        if (x instanceof PrintAnnot2 && y instanceof PrintAnnot2) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq54(x.value2)(y.value2);
        }
        ;
        if (x instanceof Done5 && y instanceof Done5) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine);
        }
        ;
        if (x instanceof CaseOf && y instanceof CaseOf) {
          return x.value0.locEnd.srcCol === y.value0.locEnd.srcCol && x.value0.locEnd.srcLine === y.value0.locEnd.srcLine && (x.value0.locStart.srcCol === y.value0.locStart.srcCol && x.value0.locStart.srcLine === y.value0.locStart.srcLine) && eq(eqTerm3)(x.value1)(y.value1) && eq(eqList(eqPattern3))(x.value2)(y.value2);
        }
        ;
        return false;
      };
    }
  };

  // output/FreeVars.FreeVariables/index.js
  var freshVarN2 = /* @__PURE__ */ freshVarN(eqVariable)(ordVariable);
  var unions4 = /* @__PURE__ */ unions2(foldableList)(ordVariable);
  var map27 = /* @__PURE__ */ map(functorList);
  var $$delete3 = /* @__PURE__ */ $$delete2(ordVariable);
  var foldr6 = /* @__PURE__ */ foldr(foldableList);
  var union3 = /* @__PURE__ */ union2(ordVariable);
  var freeVars = function(dict) {
    return dict.freeVars;
  };
  var freshVar = function(dictFreeVariables) {
    var freeVars1 = freeVars(dictFreeVariables);
    return function(a2) {
      var frV = freeVars1(a2);
      return freshVarN2(0)("x")(Variable.create)(frV);
    };
  };
  var freeVariablesList = function(dictFreeVariables) {
    var freeVars1 = freeVars(dictFreeVariables);
    return {
      freeVars: function(ls) {
        return unions4(map27(freeVars1)(ls));
      }
    };
  };
  var freeVariablesTerm = {
    freeVars: function(v) {
      if (v instanceof Var) {
        return singleton5(v.value1);
      }
      ;
      if (v instanceof Mu) {
        return $$delete3(v.value1)(freeVars(freeVariablesCommand)(v.value2));
      }
      ;
      if (v instanceof Xtor) {
        return unions4(map27(freeVars(freeVariablesTerm))(v.value2));
      }
      ;
      if (v instanceof XCase) {
        return unions4(map27(freeVars(freeVariablesPattern))(v.value1));
      }
      ;
      if (v instanceof ShiftCBV) {
        return freeVars(freeVariablesTerm)(v.value1);
      }
      ;
      if (v instanceof ShiftCBN) {
        return freeVars(freeVariablesTerm)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at FreeVars.FreeVariables (line 24, column 1 - line 30, column 47): " + [v.constructor.name]);
    }
  };
  var freeVariablesPattern = {
    freeVars: function(v) {
      return foldr6($$delete3)(freeVars(freeVariablesCommand)(v.value0.ptcmd))(v.value0.ptv);
    }
  };
  var freeVariablesCommand = {
    freeVars: function(v) {
      if (v instanceof Cut) {
        return union3(freeVars(freeVariablesTerm)(v.value1))(freeVars(freeVariablesTerm)(v.value3));
      }
      ;
      if (v instanceof Done2) {
        return empty3;
      }
      ;
      if (v instanceof Err) {
        return empty3;
      }
      ;
      if (v instanceof Print) {
        return freeVars(freeVariablesTerm)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at FreeVars.FreeVariables (line 35, column 1 - line 39, column 38): " + [v.constructor.name]);
    }
  };
  var freeVariablesTerm1 = {
    freeVars: function(v) {
      if (v instanceof Var4) {
        return singleton5(v.value1);
      }
      ;
      if (v instanceof Mu4) {
        return $$delete3(v.value1)(freeVars(freeVariablesCommand1)(v.value2));
      }
      ;
      if (v instanceof Xtor4) {
        return unions4(map27(freeVars(freeVariablesTerm1))(v.value2));
      }
      ;
      if (v instanceof XCase4) {
        return unions4(map27(freeVars(freeVariablesPattern1))(v.value1));
      }
      ;
      if (v instanceof ShiftCBV4) {
        return freeVars(freeVariablesTerm1)(v.value1);
      }
      ;
      if (v instanceof ShiftCBN4) {
        return freeVars(freeVariablesTerm1)(v.value1);
      }
      ;
      if (v instanceof Lam) {
        return $$delete3(v.value1)(freeVars(freeVariablesTerm1)(v.value2));
      }
      ;
      if (v instanceof App2) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value2));
      }
      ;
      if (v instanceof Seq) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value2));
      }
      ;
      if (v instanceof Tup) {
        return unions4(map27(freeVars(freeVariablesTerm1))(v.value1));
      }
      ;
      if (v instanceof Lst) {
        return unions4(map27(freeVars(freeVariablesTerm1))(v.value1));
      }
      ;
      if (v instanceof NotBool) {
        return freeVars(freeVariablesTerm1)(v.value1);
      }
      ;
      if (v instanceof AndBool) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value2));
      }
      ;
      if (v instanceof OrBool) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value2));
      }
      ;
      if (v instanceof IfThenElse) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(union3(freeVars(freeVariablesTerm1)(v.value2))(freeVars(freeVariablesTerm1)(v.value3)));
      }
      ;
      throw new Error("Failed pattern match at FreeVars.FreeVariables (line 41, column 1 - line 56, column 93): " + [v.constructor.name]);
    }
  };
  var freeVariablesPattern1 = {
    freeVars: function(v) {
      return foldr6($$delete3)(freeVars(freeVariablesCommand1)(v.value0.ptcmd))(v.value0.ptv);
    }
  };
  var freeVariablesCommand1 = {
    freeVars: function(v) {
      if (v instanceof Cut4) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value3));
      }
      ;
      if (v instanceof CutAnnot2) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(freeVars(freeVariablesTerm1)(v.value4));
      }
      ;
      if (v instanceof Done5) {
        return empty3;
      }
      ;
      if (v instanceof Err4) {
        return empty3;
      }
      ;
      if (v instanceof Print4) {
        return freeVars(freeVariablesTerm1)(v.value1);
      }
      ;
      if (v instanceof PrintAnnot2) {
        return freeVars(freeVariablesTerm1)(v.value1);
      }
      ;
      if (v instanceof CaseOf) {
        return union3(freeVars(freeVariablesTerm1)(v.value1))(unions4(map27(freeVars(freeVariablesPattern1))(v.value2)));
      }
      ;
      throw new Error("Failed pattern match at FreeVars.FreeVariables (line 58, column 1 - line 65, column 79): " + [v.constructor.name]);
    }
  };

  // output/Desugar.Terms/index.js
  var monadExceptT7 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind9 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT7)));
  var applicativeReaderT6 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT7));
  var pure8 = /* @__PURE__ */ pure(applicativeReaderT6);
  var $$for6 = /* @__PURE__ */ $$for(applicativeReaderT6)(traversableList);
  var freshVar2 = /* @__PURE__ */ freshVar(freeVariablesTerm1);
  var throwError6 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var map28 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var desugarTerm = function(v) {
    if (v instanceof Var4) {
      var vxt = varToXtor(v.value1);
      return bind9(getDesMXtor(vxt))(function(mxt) {
        if (mxt instanceof Nothing) {
          return pure8(new Var2(v.value0, v.value1));
        }
        ;
        if (mxt instanceof Just) {
          return pure8(new Xtor2(v.value0, vxt, Nil.value));
        }
        ;
        throw new Error("Failed pattern match at Desugar.Terms (line 24, column 3 - line 26, column 40): " + [mxt.constructor.name]);
      });
    }
    ;
    if (v instanceof Mu4) {
      return bind9(desugarCommand(v.value2))(function(c$prime) {
        return pure8(new Mu2(v.value0, v.value1, c$prime));
      });
    }
    ;
    if (v instanceof Xtor4) {
      return bind9($$for6(v.value2)(desugarTerm))(function(args$prime) {
        return pure8(new Xtor2(v.value0, v.value1, args$prime));
      });
    }
    ;
    if (v instanceof XCase4) {
      return bind9($$for6(v.value1)(desugarPattern))(function(pts$prime) {
        return pure8(new XCase2(v.value0, pts$prime));
      });
    }
    ;
    if (v instanceof ShiftCBV4) {
      return bind9(desugarTerm(v.value1))(function(t$prime) {
        return pure8(new ShiftCBV2(v.value0, t$prime));
      });
    }
    ;
    if (v instanceof ShiftCBN4) {
      return bind9(desugarTerm(v.value1))(function(t$prime) {
        return pure8(new ShiftCBN2(v.value0, t$prime));
      });
    }
    ;
    if (v instanceof App2) {
      return bind9(desugarTerm(v.value1))(function(t1$prime) {
        return bind9(desugarTerm(v.value2))(function(t2$prime) {
          var v12 = freshVar2(v);
          var args = new Cons(t2$prime, new Cons(new Var2(v.value0, v12), Nil.value));
          var cut2 = new Cut2(v.value0, t1$prime, CBV.value, new Xtor2(v.value0, new Xtorname("Ap"), args));
          return pure8(new Mu2(v.value0, v12, cut2));
        });
      });
    }
    ;
    if (v instanceof Lam) {
      var v$prime = freshVar2(v);
      var ptVars = new Cons(v.value1, new Cons(v$prime, Nil.value));
      return bind9(desugarTerm(v.value2))(function(t$prime$prime) {
        var cut2 = new Cut2(v.value0, t$prime$prime, CBV.value, new Var2(v.value0, v$prime));
        var pt = new Pattern2({
          ptxt: new Xtorname("Ap"),
          ptv: ptVars,
          ptcmd: cut2
        });
        return pure8(new XCase2(v.value0, new Cons(pt, Nil.value)));
      });
    }
    ;
    if (v instanceof Seq) {
      var v1 = freshVar2(v);
      return desugarTerm(new App2(v.value0, new Lam(v.value0, v1, v.value2), v.value1));
    }
    ;
    if (v instanceof Tup) {
      var v1 = uncons(v.value1);
      if (v1 instanceof Nothing) {
        return throwError6(new ErrEmptyPair(v.value0));
      }
      ;
      if (v1 instanceof Just && v1.value0.tail instanceof Nil) {
        return throwError6(new ErrEmptyPair(v.value0));
      }
      ;
      if (v1 instanceof Just && (v1.value0.tail instanceof Cons && v1.value0.tail.value1 instanceof Nil)) {
        return bind9(desugarTerm(v1.value0.head))(function(t1$prime) {
          return bind9(desugarTerm(v1.value0.tail.value0))(function(t2$prime) {
            return pure8(new Xtor2(v.value0, new Xtorname("Tup"), new Cons(t1$prime, new Cons(t2$prime, Nil.value))));
          });
        });
      }
      ;
      if (v1 instanceof Just) {
        return bind9(desugarTerm(v1.value0.head))(function(t1$prime) {
          return bind9(desugarTerm(new Tup(v.value0, v1.value0.tail)))(function(pairRest) {
            return pure8(new Xtor2(v.value0, new Xtorname("Tup"), new Cons(t1$prime, new Cons(pairRest, Nil.value))));
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Desugar.Terms (line 59, column 30 - line 69, column 70): " + [v1.constructor.name]);
    }
    ;
    if (v instanceof Lst) {
      if (v.value1 instanceof Nil) {
        return pure8(new Xtor2(v.value0, new Xtorname("Nil"), Nil.value));
      }
      ;
      if (v.value1 instanceof Cons) {
        return bind9(desugarTerm(v.value1.value0))(function(t1$prime) {
          return bind9(desugarTerm(new Lst(v.value0, v.value1.value1)))(function(listRest) {
            return pure8(new Xtor2(v.value0, new Xtorname("Cons"), new Cons(t1$prime, new Cons(listRest, Nil.value))));
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Desugar.Terms (line 70, column 30 - line 75, column 72): " + [v.value1.constructor.name]);
    }
    ;
    if (v instanceof NotBool) {
      var notFun = new Var4(v.value0, new Variable("not"));
      return desugarTerm(new App2(v.value0, notFun, v.value1));
    }
    ;
    if (v instanceof AndBool) {
      var andFun = new Var4(v.value0, new Variable("and"));
      return desugarTerm(new App2(v.value0, new App2(v.value0, andFun, v.value1), v.value2));
    }
    ;
    if (v instanceof OrBool) {
      var orFun = new Var4(v.value0, new Variable("or"));
      return desugarTerm(new App2(v.value0, new App2(v.value0, orFun, v.value1), v.value2));
    }
    ;
    if (v instanceof IfThenElse) {
      var iteFun = new Var4(v.value0, new Variable("ifthenelse"));
      return desugarTerm(new App2(v.value0, new App2(v.value0, new App2(v.value0, iteFun, v.value1), v.value2), v.value3));
    }
    ;
    throw new Error("Failed pattern match at Desugar.Terms (line 20, column 1 - line 20, column 41): " + [v.constructor.name]);
  };
  var desugarPattern = function(v) {
    return bind9(desugarCommand(v.value0.ptcmd))(function(c$prime) {
      return pure8(new Pattern2({
        ptxt: v.value0.ptxt,
        ptv: v.value0.ptv,
        ptcmd: c$prime
      }));
    });
  };
  var desugarCommand = function(v) {
    if (v instanceof Cut4) {
      return bind9(desugarTerm(v.value1))(function(t$prime) {
        return bind9(desugarTerm(v.value3))(function(u$prime) {
          return pure8(new Cut2(v.value0, t$prime, v.value2, u$prime));
        });
      });
    }
    ;
    if (v instanceof CutAnnot2) {
      return bind9(desugarTerm(v.value1))(function(t$prime) {
        return bind9(desugarTerm(v.value4))(function(u$prime) {
          return bind9(desugarTy(v.value2))(function(ty$prime) {
            return pure8(new CutAnnot(v.value0, t$prime, ty$prime, v.value3, u$prime));
          });
        });
      });
    }
    ;
    if (v instanceof Done5) {
      return pure8(new Done3(v.value0));
    }
    ;
    if (v instanceof Err4) {
      return pure8(new Err2(v.value0, v.value1));
    }
    ;
    if (v instanceof Print4) {
      return map28(Print2.create(v.value0))(desugarTerm(v.value1));
    }
    ;
    if (v instanceof PrintAnnot2) {
      return bind9(desugarTerm(v.value1))(function(t$prime) {
        return bind9(desugarTy(v.value2))(function(ty$prime) {
          return pure8(new PrintAnnot(v.value0, t$prime, ty$prime));
        });
      });
    }
    ;
    if (v instanceof CaseOf) {
      return bind9($$for6(v.value2)(desugarPattern))(function(pts$prime) {
        var xcase = new XCase2(v.value0, pts$prime);
        return bind9(desugarTerm(v.value1))(function(t$prime) {
          return pure8(new Cut2(v.value0, t$prime, CBV.value, xcase));
        });
      });
    }
    ;
    throw new Error("Failed pattern match at Desugar.Terms (line 94, column 1 - line 94, column 50): " + [v.constructor.name]);
  };

  // output/Desugar.Program/index.js
  var monadExceptT8 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind10 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT8)));
  var applicativeReaderT7 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT8));
  var $$for7 = /* @__PURE__ */ $$for(applicativeReaderT7)(traversableList);
  var pure9 = /* @__PURE__ */ pure(applicativeReaderT7);
  var throwError7 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var getLoc2 = /* @__PURE__ */ getLoc(hasLocCommand3);
  var eq20 = /* @__PURE__ */ eq(eqTy);
  var elem8 = /* @__PURE__ */ elem(foldableList);
  var elem12 = /* @__PURE__ */ elem8(eqXtorname);
  var map29 = /* @__PURE__ */ map(functorList);
  var elem23 = /* @__PURE__ */ elem8(eqTypename);
  var monadErrorReaderT3 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity)));
  var monadReaderReaderT3 = /* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT8));
  var getTypeNames3 = /* @__PURE__ */ getTypeNames(errorDesugarError)(monadErrorReaderT3)(monadReaderReaderT3);
  var getXtorNames2 = /* @__PURE__ */ getXtorNames(errorDesugarError)(monadErrorReaderT3)(monadReaderReaderT3);
  var desugarXtorSig = function(v) {
    return bind10($$for7(v.value0.sigArgs)(desugarTy))(function(args$prime) {
      return pure9(new XtorSig({
        sigPos: v.value0.sigPos,
        sigName: v.value0.sigName,
        sigArgs: args$prime
      }));
    });
  };
  var desugarVar = function(v) {
    return bind10(desugarTerm(v.value0.varBody))(function(t$prime) {
      var newVar = {
        varPos: v.value0.varPos,
        varName: v.value0.varName,
        varIsRec: v.value0.varIsRec,
        varTy: Nothing.value,
        varBody: t$prime
      };
      return addDesVar(new VarDecl(newVar));
    });
  };
  var desugarMain = function(v) {
    if (v instanceof Nil) {
      return pure9(unit);
    }
    ;
    if (v instanceof Cons && v.value1 instanceof Nil) {
      return bind10(desugarCommand(v.value0))(function(c$prime) {
        return setDesMain(c$prime);
      });
    }
    ;
    if (v instanceof Cons) {
      return throwError7(new ErrMultipleMain(getLoc2(v.value0)));
    }
    ;
    throw new Error("Failed pattern match at Desugar.Program (line 74, column 1 - line 74, column 48): " + [v.constructor.name]);
  };
  var desugarDecl = function(v) {
    return bind10(setDesCurrDecl(v))(function() {
      return bind10($$for7(v.value0.declXtors)(desugarXtorSig))(function(sigs$prime) {
        var newD = new DataDecl({
          declPos: v.value0.declPos,
          declName: v.value0.declName,
          declArgs: v.value0.declArgs,
          declType: v.value0.declType,
          declXtors: sigs$prime
        });
        return addDesDecl(newD);
      });
    });
  };
  var desugarAnnot = function(v) {
    return bind10(getDesDoneVar(v.value0.annotPos)(v.value0.annotName))(function(v1) {
      return bind10(desugarTy(v.value0.annotType))(function(ty$prime) {
        if (v1.value0.varTy instanceof Nothing) {
          return addDesVar(new VarDecl({
            varPos: v1.value0.varPos,
            varName: v1.value0.varName,
            varIsRec: v1.value0.varIsRec,
            varBody: v1.value0.varBody,
            varTy: new Just(ty$prime)
          }));
        }
        ;
        if (v1.value0.varTy instanceof Just) {
          var $60 = eq20(ty$prime)(v1.value0.varTy.value0);
          if ($60) {
            return pure9(unit);
          }
          ;
          return throwError7(new ErrMultipleAnnot(v.value0.annotPos, v1.value0.varName, ty$prime, v1.value0.varTy.value0));
        }
        ;
        throw new Error("Failed pattern match at Desugar.Program (line 65, column 3 - line 67, column 119): " + [v1.value0.varTy.constructor.name]);
      });
    });
  };
  var checkXtorNames = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Nil) {
          $tco_done = true;
          return pure9(unit);
        }
        ;
        if (v1 instanceof Cons) {
          var $68 = elem12(v1.value0.value0.sigName)(map29(function(v2) {
            return v2.value0.sigName;
          })(v1.value1)) || elem12(v1.value0.value0.sigName)(v);
          if ($68) {
            $tco_done = true;
            return throwError7(new ErrMultipleXtor(v1.value0.value0.sigPos, v1.value0.value0.sigName));
          }
          ;
          $tco_var_v = v;
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Desugar.Program (line 29, column 1 - line 29, column 67): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var checkTypeNames = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Nil) {
          $tco_done = true;
          return pure9(unit);
        }
        ;
        if (v1 instanceof Cons) {
          var $76 = elem23(v1.value0.value0.declName)(map29(function(v2) {
            return v2.value0.declName;
          })(v1.value1)) || elem23(v1.value0.value0.declName)(v);
          if ($76) {
            $tco_done = true;
            return throwError7(new ErrMultipleNames(v1.value0.value0.declPos, v1.value0.value0.declName));
          }
          ;
          $tco_var_v = v;
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Desugar.Program (line 23, column 1 - line 23, column 68): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var desugarProgram = function(v) {
    return bind10(getTypeNames3)(function(envTyNames) {
      return bind10(checkTypeNames(envTyNames)(v.value0.progDecls))(function() {
        return bind10(getXtorNames2)(function(envXtns) {
          return bind10(checkXtorNames(envXtns)(concatMap(function(v1) {
            return v1.value0.declXtors;
          })(v.value0.progDecls)))(function() {
            return bind10($$for7(v.value0.progDecls)(desugarDecl))(function() {
              return bind10($$for7(v.value0.progVars)(desugarVar))(function() {
                return bind10($$for7(v.value0.progAnnots)(desugarAnnot))(function() {
                  return bind10(desugarMain(v.value0.progMain))(function() {
                    return getDesDoneProg;
                  });
                });
              });
            });
          });
        });
      });
    });
  };

  // output/Eval.Definition/index.js
  var show40 = /* @__PURE__ */ show(showCommand);
  var show117 = /* @__PURE__ */ show(showXtorname);
  var append12 = /* @__PURE__ */ append(semigroupList);
  var MkTrace = /* @__PURE__ */ function() {
    function MkTrace2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MkTrace2.create = function(value0) {
      return function(value1) {
        return new MkTrace2(value0, value1);
      };
    };
    return MkTrace2;
  }();
  var ErrXtorArity = /* @__PURE__ */ function() {
    function ErrXtorArity4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrXtorArity4.create = function(value0) {
      return function(value1) {
        return new ErrXtorArity4(value0, value1);
      };
    };
    return ErrXtorArity4;
  }();
  var ErrMissingPt = /* @__PURE__ */ function() {
    function ErrMissingPt2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrMissingPt2.create = function(value0) {
      return function(value1) {
        return new ErrMissingPt2(value0, value1);
      };
    };
    return ErrMissingPt2;
  }();
  var ErrLoop = /* @__PURE__ */ function() {
    function ErrLoop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrLoop2.create = function(value0) {
      return function(value1) {
        return new ErrLoop2(value0, value1);
      };
    };
    return ErrLoop2;
  }();
  var ErrTwoCase = /* @__PURE__ */ function() {
    function ErrTwoCase2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTwoCase2.create = function(value0) {
      return function(value1) {
        return new ErrTwoCase2(value0, value1);
      };
    };
    return ErrTwoCase2;
  }();
  var ErrTwoXtor = /* @__PURE__ */ function() {
    function ErrTwoXtor2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTwoXtor2.create = function(value0) {
      return function(value1) {
        return new ErrTwoXtor2(value0, value1);
      };
    };
    return ErrTwoXtor2;
  }();
  var ErrOther4 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorEvalError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrXtorArity) {
          return "Wrong number of arguments for xtor " + show117(v.value1);
        }
        ;
        if (v instanceof ErrMissingPt) {
          return "No pattern for xtor " + show117(v.value1);
        }
        ;
        if (v instanceof ErrLoop) {
          return "Cannot evaluate " + (show40(v.value1) + ", evaluation results in loop");
        }
        ;
        if (v instanceof ErrTwoCase) {
          return "Cannot evaluate " + (show40(v.value1) + ", cut between cases");
        }
        ;
        if (v instanceof ErrTwoXtor) {
          return "Cannot evaluate " + (show40(v.value1) + ", cut between xtors");
        }
        ;
        if (v instanceof ErrOther4) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at Eval.Definition (line 32, column 1 - line 47, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrXtorArity) {
          return v.value0;
        }
        ;
        if (v instanceof ErrMissingPt) {
          return v.value0;
        }
        ;
        if (v instanceof ErrLoop) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTwoCase) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTwoXtor) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther4) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Eval.Definition (line 32, column 1 - line 47, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther4.create
    };
  }();
  var runEvalM = function(env) {
    return function(m) {
      return runExcept(runReaderT(m)(env));
    };
  };
  var emptyTrace = function(c) {
    return new MkTrace(c, Nil.value);
  };
  var appendTrace = function(v) {
    return function(c$prime) {
      return new MkTrace(c$prime, append12(v.value1)(new Cons(v.value0, Nil.value)));
    };
  };

  // output/Syntax.Kinded.Substitution/index.js
  var fromFoldable3 = /* @__PURE__ */ fromFoldable(ordVariable)(foldableArray);
  var lookup12 = /* @__PURE__ */ lookup(ordVariable);
  var $$delete4 = /* @__PURE__ */ $$delete(ordVariable);
  var map30 = /* @__PURE__ */ map(functorList);
  var foldr7 = /* @__PURE__ */ foldr(foldableList);
  var substVars = function(dict) {
    return dict.substVars;
  };
  var substituteVariable = function(dictSubstituteVariables) {
    var substVars1 = substVars(dictSubstituteVariables);
    return function(substVar) {
      return function(substT) {
        return function(a2) {
          var varmap = fromFoldable3([new Tuple(substVar, substT)]);
          return substVars1(varmap)(a2);
        };
      };
    };
  };
  var substituteVariablesTerm = {
    substVars: function(v) {
      return function(v1) {
        if (v1 instanceof Var) {
          return fromMaybe(v1)(lookup12(v1.value1)(v));
        }
        ;
        if (v1 instanceof Mu) {
          return new Mu(v1.value0, v1.value1, substVars(substituteVariablesComman)($$delete4(v1.value1)(v))(v1.value2), v1.value3);
        }
        ;
        if (v1 instanceof Xtor) {
          return new Xtor(v1.value0, v1.value1, map30(substVars(substituteVariablesTerm)(v))(v1.value2), v1.value3);
        }
        ;
        if (v1 instanceof XCase) {
          return new XCase(v1.value0, map30(substVars(substituteVariablesPatter)(v))(v1.value1), v1.value2);
        }
        ;
        if (v1 instanceof ShiftCBV) {
          return new ShiftCBV(v1.value0, substVars(substituteVariablesTerm)(v)(v1.value1), v1.value2);
        }
        ;
        if (v1 instanceof ShiftCBN) {
          return new ShiftCBN(v1.value0, substVars(substituteVariablesTerm)(v)(v1.value1), v1.value2);
        }
        ;
        throw new Error("Failed pattern match at Syntax.Kinded.Substitution (line 60, column 1 - line 66, column 78): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var substituteVariablesPatter = {
    substVars: function(varmap) {
      return function(v) {
        var newMap = foldr7($$delete4)(varmap)(v.value0.ptv);
        return new Pattern({
          ptxt: v.value0.ptxt,
          ptv: v.value0.ptv,
          ptcmd: substVars(substituteVariablesComman)(newMap)(v.value0.ptcmd)
        });
      };
    }
  };
  var substituteVariablesComman = {
    substVars: function(v) {
      return function(v1) {
        if (v1 instanceof Cut) {
          return new Cut(v1.value0, substVars(substituteVariablesTerm)(v)(v1.value1), v1.value2, substVars(substituteVariablesTerm)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Done2) {
          return v1;
        }
        ;
        if (v1 instanceof Err) {
          return v1;
        }
        ;
        if (v1 instanceof Print) {
          return new Print(v1.value0, substVars(substituteVariablesTerm)(v)(v1.value1));
        }
        ;
        throw new Error("Failed pattern match at Syntax.Kinded.Substitution (line 71, column 1 - line 75, column 66): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };

  // output/Eval.Eval/index.js
  var applicativeReaderT8 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var pure10 = /* @__PURE__ */ pure(applicativeReaderT8);
  var bind11 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindExceptT(monadIdentity)));
  var monadErrorReaderT4 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorExceptT(monadIdentity));
  var lookupBody2 = /* @__PURE__ */ lookupBody(errorEvalError)(monadErrorReaderT4)(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadExceptT(monadIdentity)));
  var setLoc2 = /* @__PURE__ */ setLoc(hasLocTerm);
  var substituteVariable2 = /* @__PURE__ */ substituteVariable(substituteVariablesComman);
  var throwError8 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowExceptT(monadIdentity)));
  var notEq3 = /* @__PURE__ */ notEq(eqXtorname);
  var zipWithErrorM2 = /* @__PURE__ */ zipWithErrorM(errorEvalError)(monadErrorReaderT4);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable(ordVariable)(foldableList);
  var substVars2 = /* @__PURE__ */ substVars(substituteVariablesComman);
  var freshVar3 = /* @__PURE__ */ freshVar(/* @__PURE__ */ freeVariablesList(freeVariablesTerm));
  var getLoc3 = /* @__PURE__ */ getLoc(hasLocTerm);
  var shiftEvalOrder3 = /* @__PURE__ */ shiftEvalOrder(shiftEvalOrderEvaluationO);
  var when3 = /* @__PURE__ */ when(applicativeReaderT8);
  var eq21 = /* @__PURE__ */ eq(eqCommand);
  var getLoc1 = /* @__PURE__ */ getLoc(hasLocCommand);
  var evalOnce = function(v) {
    if (v instanceof Err) {
      return pure10(new Err(v.value0, v.value1));
    }
    ;
    if (v instanceof Done2) {
      return pure10(new Done2(v.value0));
    }
    ;
    if (v instanceof Print) {
      return pure10(new Print(v.value0, v.value1));
    }
    ;
    if (v instanceof Cut && v.value1 instanceof ShiftCBV) {
      return pure10(new Cut(v.value0, v.value1.value1, v.value2, v.value3));
    }
    ;
    if (v instanceof Cut && v.value1 instanceof ShiftCBN) {
      return pure10(new Cut(v.value0, v.value1.value1, v.value2, v.value3));
    }
    ;
    if (v instanceof Cut && v.value3 instanceof ShiftCBV) {
      return pure10(new Cut(v.value0, v.value1, v.value2, v.value3.value1));
    }
    ;
    if (v instanceof Cut && v.value3 instanceof ShiftCBN) {
      return pure10(new Cut(v.value0, v.value1, v.value2, v.value3.value1));
    }
    ;
    if (v instanceof Cut && v.value1 instanceof Var) {
      return bind11(lookupBody2(v.value0)(v.value1.value1))(function(t) {
        return pure10(new Cut(v.value0, setLoc2(v.value1.value0)(t), v.value2, v.value3));
      });
    }
    ;
    if (v instanceof Cut && v.value3 instanceof Var) {
      return bind11(lookupBody2(v.value0)(v.value3.value1))(function(u2) {
        return pure10(new Cut(v.value0, v.value1, v.value2, setLoc2(v.value3.value0)(u2)));
      });
    }
    ;
    if (v instanceof Cut && v.value3 instanceof Mu) {
      return pure10(substituteVariable2(v.value3.value1)(v.value1)(v.value3.value2));
    }
    ;
    if (v instanceof Cut && v.value1 instanceof Mu) {
      return pure10(substituteVariable2(v.value1.value1)(v.value3)(v.value1.value2));
    }
    ;
    if (v instanceof Cut && (v.value1 instanceof Xtor && v.value3 instanceof XCase)) {
      var substCase = function($copy_v1) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v12) {
          if (v12 instanceof Nil) {
            $tco_done = true;
            return throwError8(new ErrMissingPt(v.value0, v.value1.value1));
          }
          ;
          if (v12 instanceof Cons && notEq3(v12.value0.value0.ptxt)(v.value1.value1)) {
            $copy_v1 = v12.value1;
            return;
          }
          ;
          if (v12 instanceof Cons) {
            $tco_done = true;
            return bind11(zipWithErrorM2(v12.value0.value0.ptv)(v.value1.value2)(new ErrMissingPt(v.value0, v.value1.value1)))(function(argsZipped) {
              var varmap = fromFoldable4(argsZipped);
              return pure10(substVars2(varmap)(v12.value0.value0.ptcmd));
            });
          }
          ;
          throw new Error("Failed pattern match at Eval.Eval (line 76, column 5 - line 76, column 47): " + [v12.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v1);
        }
        ;
        return $tco_result;
      };
      var evalArgs = function(v12) {
        if (v12 instanceof Nil) {
          return new Tuple(Nil.value, Nothing.value);
        }
        ;
        if (v12 instanceof Cons && isValue(v.value2)(v12.value0)) {
          var v2 = evalArgs(v12.value1);
          return new Tuple(new Cons(v12.value0, v2.value0), v2.value1);
        }
        ;
        if (v12 instanceof Cons) {
          var frv = freshVar3(v12.value1);
          var newArgs = new Cons(new Var(getLoc3(v12.value0), frv, getType(v12.value0)), v12.value1);
          return new Tuple(newArgs, new Just(new Tuple(frv, v12.value0)));
        }
        ;
        throw new Error("Failed pattern match at Eval.Eval (line 68, column 5 - line 68, column 77): " + [v12.constructor.name]);
      };
      var v1 = evalArgs(v.value1.value2);
      if (v1.value1 instanceof Nothing) {
        return substCase(v.value3.value1);
      }
      ;
      if (v1.value1 instanceof Just) {
        var xtt = new Xtor(v.value1.value0, v.value1.value1, v1.value0, v.value1.value3);
        return pure10(new Cut(v.value0, new Mu(v.value1.value0, v1.value1.value0.value0, new Cut(v.value1.value0, xtt, v.value2, v1.value1.value0.value1), getType(v1.value1.value0.value1)), v.value2, v.value3));
      }
      ;
      throw new Error("Failed pattern match at Eval.Eval (line 60, column 3 - line 66, column 77): " + [v1.value1.constructor.name]);
    }
    ;
    if (v instanceof Cut && (v.value1 instanceof XCase && v.value3 instanceof Xtor)) {
      return pure10(new Cut(v.value0, v.value3, shiftEvalOrder3(v.value2), v.value1));
    }
    ;
    if (v instanceof Cut && (v.value1 instanceof XCase && v.value3 instanceof XCase)) {
      return throwError8(new ErrTwoCase(v.value0, v));
    }
    ;
    if (v instanceof Cut && (v.value1 instanceof Xtor && v.value3 instanceof Xtor)) {
      return throwError8(new ErrTwoXtor(v.value0, v));
    }
    ;
    throw new Error("Failed pattern match at Eval.Eval (line 42, column 1 - line 42, column 37): " + [v.constructor.name]);
  };
  var evalWithTrace = function(c) {
    return function(tr2) {
      return bind11(evalOnce(c))(function(c$prime) {
        var newTr = appendTrace(tr2)(c$prime);
        if (c$prime instanceof Done2) {
          return pure10(newTr);
        }
        ;
        if (c$prime instanceof Err) {
          return pure10(newTr);
        }
        ;
        if (c$prime instanceof Print) {
          return pure10(newTr);
        }
        ;
        if (c$prime instanceof Cut) {
          return bind11(when3(eq21(c)(c$prime))(throwError8(new ErrLoop(getLoc1(c), c))))(function() {
            return evalWithTrace(c$prime)(newTr);
          });
        }
        ;
        throw new Error("Failed pattern match at Eval.Eval (line 28, column 3 - line 34, column 30): " + [c$prime.constructor.name]);
      });
    };
  };
  var $$eval = function(c) {
    return bind11(evalWithTrace(c)(emptyTrace(c)))(function(v) {
      return pure10(v.value0);
    });
  };

  // output/GenerateConstraints.Errors/index.js
  var show41 = /* @__PURE__ */ show(showXtorname);
  var show118 = /* @__PURE__ */ show(showTy);
  var intercalate14 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map31 = /* @__PURE__ */ map(functorList);
  var show213 = /* @__PURE__ */ show(showTypename);
  var ErrXtorArity2 = /* @__PURE__ */ function() {
    function ErrXtorArity4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrXtorArity4.create = function(value0) {
      return function(value1) {
        return new ErrXtorArity4(value0, value1);
      };
    };
    return ErrXtorArity4;
  }();
  var ErrTyArity = /* @__PURE__ */ function() {
    function ErrTyArity3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTyArity3.create = function(value0) {
      return function(value1) {
        return new ErrTyArity3(value0, value1);
      };
    };
    return ErrTyArity3;
  }();
  var ErrKindNeq = /* @__PURE__ */ function() {
    function ErrKindNeq4(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrKindNeq4.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrKindNeq4(value0, value1, value22);
        };
      };
    };
    return ErrKindNeq4;
  }();
  var ErrBadPattern = /* @__PURE__ */ function() {
    function ErrBadPattern3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrBadPattern3.create = function(value0) {
      return function(value1) {
        return new ErrBadPattern3(value0, value1);
      };
    };
    return ErrBadPattern3;
  }();
  var ErrOther5 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorGenerateError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrXtorArity2) {
          return "Wrong number of arguments for xtor " + show41(v.value1);
        }
        ;
        if (v instanceof ErrKindNeq) {
          return "Kinds of types " + (show118(v.value1) + (" and " + (show118(v.value2) + " are not equal")));
        }
        ;
        if (v instanceof ErrBadPattern) {
          return "Malformed pattern: " + intercalate14(", ")(map31(show41)(v.value1));
        }
        ;
        if (v instanceof ErrTyArity) {
          return "Wrong number of arguments for type " + show213(v.value1);
        }
        ;
        if (v instanceof ErrOther5) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at GenerateConstraints.Errors (line 20, column 1 - line 33, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrXtorArity2) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTyArity) {
          return v.value0;
        }
        ;
        if (v instanceof ErrKindNeq) {
          return v.value0;
        }
        ;
        if (v instanceof ErrBadPattern) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther5) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at GenerateConstraints.Errors (line 20, column 1 - line 33, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther5.create
    };
  }();

  // output/GenerateConstraints.Definition/index.js
  var monadExceptT9 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var monadStateReaderT4 = /* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT9));
  var gets5 = /* @__PURE__ */ gets(monadStateReaderT4);
  var bind12 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT9)));
  var show45 = /* @__PURE__ */ show(showInt);
  var modify8 = /* @__PURE__ */ modify2(monadStateReaderT4);
  var applicativeReaderT9 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT9));
  var pure11 = /* @__PURE__ */ pure(applicativeReaderT9);
  var $$for8 = /* @__PURE__ */ $$for(applicativeReaderT9)(traversableList);
  var map32 = /* @__PURE__ */ map(functorList);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable(ordTypevar)(foldableList);
  var insert6 = /* @__PURE__ */ insert(ordVariable);
  var throwError9 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var MkGenState = /* @__PURE__ */ function() {
    function MkGenState2(value0) {
      this.value0 = value0;
    }
    ;
    MkGenState2.create = function(value0) {
      return new MkGenState2(value0);
    };
    return MkGenState2;
  }();
  var initialGenState = /* @__PURE__ */ function() {
    return new MkGenState({
      varEnv: empty2,
      tyVarCnt: 0,
      kVarCnt: 0,
      constrSet: Nil.value,
      genTyvars: Nil.value
    });
  }();
  var runGenM = function(env) {
    return function(m) {
      var v = runExcept(runStateT(runReaderT(m)(env))(initialGenState));
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return new Right(new Tuple(v.value0.value0, new Tuple(v.value0.value1.value0.genTyvars, v.value0.value1.value0.constrSet)));
      }
      ;
      throw new Error("Failed pattern match at GenerateConstraints.Definition (line 51, column 17 - line 53, column 88): " + [v.constructor.name]);
    };
  };
  var getGenVars = /* @__PURE__ */ gets5(function(v) {
    return v.value0.varEnv;
  });
  var freshTyVar = /* @__PURE__ */ bind12(/* @__PURE__ */ gets5(function(v) {
    return v.value0.tyVarCnt;
  }))(function(cnt) {
    var newVar = new Typevar("X" + show45(cnt));
    return bind12(modify8(function(v) {
      return new MkGenState({
        varEnv: v.value0.varEnv,
        kVarCnt: v.value0.kVarCnt,
        constrSet: v.value0.constrSet,
        tyVarCnt: cnt + 1 | 0,
        genTyvars: new Cons(newVar, v.value0.genTyvars)
      });
    }))(function() {
      return pure11(new TyVar2(newVar));
    });
  });
  var freshTyVarsDecl = function(vars) {
    return bind12($$for8(vars)(function(v) {
      return bind12(freshTyVar)(function(v$prime) {
        var varpair = new Tuple(v.value0.variantVar, v$prime);
        return pure11(new Tuple(v$prime, varpair));
      });
    }))(function(varL) {
      var newVars = map32(fst)(varL);
      var newMap = fromFoldable5(map32(snd)(varL));
      return pure11(new Tuple(newVars, newMap));
    });
  };
  var addGenVar = function(v) {
    return function(ty) {
      return bind12(gets5(function(v1) {
        return v1.value0.varEnv;
      }))(function(vars) {
        return bind12(modify8(function(v1) {
          return new MkGenState({
            tyVarCnt: v1.value0.tyVarCnt,
            kVarCnt: v1.value0.kVarCnt,
            constrSet: v1.value0.constrSet,
            genTyvars: v1.value0.genTyvars,
            varEnv: insert6(v)(ty)(vars)
          });
        }))(function() {
          return pure11(unit);
        });
      });
    };
  };
  var addConstraint = function(ctr) {
    return bind12(modify8(function(v) {
      return new MkGenState({
        varEnv: v.value0.varEnv,
        tyVarCnt: v.value0.tyVarCnt,
        kVarCnt: v.value0.kVarCnt,
        genTyvars: v.value0.genTyvars,
        constrSet: new Cons(ctr, v.value0.constrSet)
      });
    }))(function() {
      return pure11(unit);
    });
  };
  var addConstraintsXtor = function(v) {
    return function(v1) {
      return function(v2) {
        return function(v3) {
          if (v2 instanceof Nil && v3 instanceof Nil) {
            return pure11(unit);
          }
          ;
          if (v3 instanceof Nil) {
            return throwError9(new ErrXtorArity2(v, v1));
          }
          ;
          if (v2 instanceof Nil) {
            return throwError9(new ErrXtorArity2(v, v1));
          }
          ;
          if (v2 instanceof Cons && v3 instanceof Cons) {
            return bind12(addConstraint(new MkTyEq(v2.value0, v3.value0)))(function() {
              return bind12(addConstraintsXtor(v)(v1)(v2.value1)(v3.value1))(function() {
                return pure11(unit);
              });
            });
          }
          ;
          throw new Error("Failed pattern match at GenerateConstraints.Definition (line 96, column 1 - line 96, column 73): " + [v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name]);
        };
      };
    };
  };

  // output/GenerateConstraints.Types/index.js
  var monadExceptT10 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var applicativeReaderT10 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT10));
  var pure12 = /* @__PURE__ */ pure(applicativeReaderT10);
  var bind13 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT10)));
  var lookupDecl2 = /* @__PURE__ */ lookupDecl(errorGenerateError)(/* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity))))(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT10)));
  var map33 = /* @__PURE__ */ map(functorList);
  var zipWithError2 = /* @__PURE__ */ zipWithError(errorGenerateError);
  var throwError10 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var $$for9 = /* @__PURE__ */ $$for(applicativeReaderT10)(traversableList);
  var map112 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var genConstraintsTy = function(v) {
    return function(v1) {
      if (v1 instanceof TyVar) {
        return pure12(new TyVar2(v1.value0));
      }
      ;
      if (v1 instanceof TyDecl) {
        return bind13(lookupDecl2(v)(v1.value0))(function(v2) {
          var argPols = map33(function(v3) {
            return new MkKind(varianceEvalOrder(v3.value0.variantVariance)(defaultEo(v2.value0.declType)));
          })(v2.value0.declArgs);
          var argsZipped = zipWithError2(v1.value1)(argPols)(new ErrTyArity(v, v1.value0));
          if (argsZipped instanceof Right) {
            return throwError10(argsZipped.value0);
          }
          ;
          if (argsZipped instanceof Left) {
            return bind13($$for9(v1.value1)(genConstraintsTy(v)))(function(args$prime) {
              return pure12(new TyDecl2(v1.value0, args$prime));
            });
          }
          ;
          throw new Error("Failed pattern match at GenerateConstraints.Types (line 27, column 3 - line 31, column 32): " + [argsZipped.constructor.name]);
        });
      }
      ;
      if (v1 instanceof TyCo) {
        return bind13(genConstraintsTy(v)(v1.value0))(function(ty$prime) {
          return pure12(new TyCo2(ty$prime));
        });
      }
      ;
      if (v1 instanceof TyShift) {
        return bind13(genConstraintsTy(v)(v1.value0))(function(ty$prime) {
          return pure12(new TyShift2(ty$prime));
        });
      }
      ;
      if (v1 instanceof TyForall) {
        return map112(TyForall2.create(v1.value0))(genConstraintsTy(v)(v1.value1));
      }
      ;
      throw new Error("Failed pattern match at GenerateConstraints.Types (line 21, column 1 - line 21, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };

  // output/Syntax.Typed.Substitution/index.js
  var lookup5 = /* @__PURE__ */ lookup(ordTypevar);
  var map34 = /* @__PURE__ */ map(functorList);
  var foldr8 = /* @__PURE__ */ foldr(foldableList);
  var $$delete5 = /* @__PURE__ */ $$delete(ordTypevar);
  var substTyvars = function(dict) {
    return dict.substTyvars;
  };
  var substituteTypevarsTy = {
    substTyvars: function(v) {
      return function(v1) {
        if (v1 instanceof TyVar2) {
          return fromMaybe(v1)(lookup5(v1.value0)(v));
        }
        ;
        if (v1 instanceof TyDecl2) {
          return new TyDecl2(v1.value0, map34(substTyvars(substituteTypevarsTy)(v))(v1.value1));
        }
        ;
        if (v1 instanceof TyShift2) {
          return new TyShift2(substTyvars(substituteTypevarsTy)(v)(v1.value0));
        }
        ;
        if (v1 instanceof TyCo2) {
          return new TyCo2(substTyvars(substituteTypevarsTy)(v)(v1.value0));
        }
        ;
        if (v1 instanceof TyForall2) {
          var newmap = foldr8($$delete5)(v)(v1.value0);
          return new TyForall2(v1.value0, substTyvars(substituteTypevarsTy)(newmap)(v1.value1));
        }
        ;
        throw new Error("Failed pattern match at Syntax.Typed.Substitution (line 28, column 1 - line 33, column 121): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var substTyvars1 = /* @__PURE__ */ substTyvars(substituteTypevarsTy);
  var substituteTypevarsTerm = {
    substTyvars: function(v) {
      return function(v1) {
        if (v1 instanceof Var3) {
          return new Var3(v1.value0, v1.value1, substTyvars1(v)(v1.value2));
        }
        ;
        if (v1 instanceof Mu3) {
          return new Mu3(v1.value0, v1.value1, substTyvars(substituteTypevarsCommand)(v)(v1.value2), substTyvars1(v)(v1.value3));
        }
        ;
        if (v1 instanceof Xtor3) {
          return new Xtor3(v1.value0, v1.value1, map34(substTyvars(substituteTypevarsTerm)(v))(v1.value2), substTyvars1(v)(v1.value3));
        }
        ;
        if (v1 instanceof XCase3) {
          return new XCase3(v1.value0, map34(substTyvars(substituteTypevarsPattern)(v))(v1.value1), substTyvars1(v)(v1.value2));
        }
        ;
        if (v1 instanceof ShiftCBV3) {
          return new ShiftCBV3(v1.value0, substTyvars(substituteTypevarsTerm)(v)(v1.value1), substTyvars1(v)(v1.value2));
        }
        ;
        if (v1 instanceof ShiftCBN3) {
          return new ShiftCBN3(v1.value0, substTyvars(substituteTypevarsTerm)(v)(v1.value1), substTyvars1(v)(v1.value2));
        }
        ;
        throw new Error("Failed pattern match at Syntax.Typed.Substitution (line 35, column 1 - line 41, column 103): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var substituteTypevarsPattern = {
    substTyvars: function(varmap) {
      return function(v) {
        return new Pattern3({
          ptxt: v.value0.ptxt,
          ptv: v.value0.ptv,
          ptcmd: substTyvars(substituteTypevarsCommand)(varmap)(v.value0.ptcmd)
        });
      };
    }
  };
  var substituteTypevarsCommand = {
    substTyvars: function(v) {
      return function(v1) {
        if (v1 instanceof Cut3) {
          return new Cut3(v1.value0, substTyvars(substituteTypevarsTerm)(v)(v1.value1), v1.value2, substTyvars(substituteTypevarsTerm)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Done4) {
          return new Done4(v1.value0);
        }
        ;
        if (v1 instanceof Err3) {
          return new Err3(v1.value0, v1.value1);
        }
        ;
        if (v1 instanceof Print3) {
          return new Print3(v1.value0, substTyvars(substituteTypevarsTerm)(v)(v1.value1));
        }
        ;
        throw new Error("Failed pattern match at Syntax.Typed.Substitution (line 46, column 1 - line 50, column 70): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var substTyvars2 = /* @__PURE__ */ substTyvars(substituteTypevarsTerm);
  var substituteTypevarsVarDecl = {
    substTyvars: function(varmap) {
      return function(v) {
        return new VarDecl2({
          varPos: v.value0.varPos,
          varName: v.value0.varName,
          varIsRec: v.value0.varIsRec,
          varTy: v.value0.varTy,
          varBody: substTyvars2(varmap)(v.value0.varBody)
        });
      };
    }
  };

  // output/GenerateConstraints.Terms/index.js
  var monadExceptT11 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var applicativeReaderT11 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT11));
  var pure13 = /* @__PURE__ */ pure(applicativeReaderT11);
  var bind14 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT11)));
  var monadErrorReaderT5 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity)));
  var monadReaderReaderT4 = /* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT11));
  var lookupXtorDecl2 = /* @__PURE__ */ lookupXtorDecl(errorGenerateError)(monadErrorReaderT5)(monadReaderReaderT4);
  var map35 = /* @__PURE__ */ map(functorList);
  var elem9 = /* @__PURE__ */ elem(foldableList)(eqXtorname);
  var lookup6 = /* @__PURE__ */ lookup(ordVariable);
  var lookupXtor2 = /* @__PURE__ */ lookupXtor(errorGenerateError)(monadErrorReaderT5)(monadReaderReaderT4);
  var $$for10 = /* @__PURE__ */ $$for(applicativeReaderT11)(traversableList);
  var substTyvars3 = /* @__PURE__ */ substTyvars(substituteTypevarsTy);
  var throwError11 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var substTyvars12 = /* @__PURE__ */ substTyvars(substituteTypevarsPattern);
  var checkPts = function(v) {
    return function(v1) {
      if (v1 instanceof Nil) {
        return pure13(Nothing.value);
      }
      ;
      if (v1 instanceof Cons) {
        return bind14(lookupXtorDecl2(v)(v1.value0.value0.ptxt))(function(v2) {
          var sigNames = map35(function(v3) {
            return v3.value0.sigName;
          })(v2.value0.declXtors);
          var ptNames = map35(function(v3) {
            return v3.value0.ptxt;
          })(v1.value1);
          var notElems = filter(function(xtn) {
            return !elem9(xtn)(sigNames);
          })(ptNames);
          var notElems2 = filter(function(xtn) {
            return !elem9(xtn)(new Cons(v1.value0.value0.ptxt, ptNames));
          })(sigNames);
          var $54 = $$null(notElems) && $$null(notElems2);
          if ($54) {
            return pure13(new Just(new DataDecl2(v2.value0)));
          }
          ;
          return pure13(Nothing.value);
        });
      }
      ;
      throw new Error("Failed pattern match at GenerateConstraints.Terms (line 28, column 1 - line 28, column 61): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var genConstraintsTerm = function(v) {
    if (v instanceof Var2) {
      return bind14(getGenVars)(function(vars) {
        var v2 = lookup6(v.value1)(vars);
        if (v2 instanceof Nothing) {
          return bind14(freshTyVar)(function(tyV) {
            return bind14(addGenVar(v.value1)(tyV))(function() {
              return pure13(new Var3(v.value0, v.value1, tyV));
            });
          });
        }
        ;
        if (v2 instanceof Just) {
          return pure13(new Var3(v.value0, v.value1, v2.value0));
        }
        ;
        throw new Error("Failed pattern match at GenerateConstraints.Terms (line 65, column 4 - line 70, column 38): " + [v2.constructor.name]);
      });
    }
    ;
    if (v instanceof Mu2) {
      return bind14(freshTyVar)(function(tyV) {
        return bind14(addGenVar(v.value1)(tyV))(function() {
          return bind14(genConstraintsCmd(v.value2))(function(c$prime) {
            return pure13(new Mu3(v.value0, v.value1, c$prime, tyV));
          });
        });
      });
    }
    ;
    if (v instanceof Xtor2) {
      return bind14(lookupXtorDecl2(v.value0)(v.value1))(function(v1) {
        return bind14(lookupXtor2(v.value0)(v.value1))(function(v2) {
          return bind14(freshTyVarsDecl(v1.value0.declArgs))(function(v3) {
            return bind14($$for10(v.value2)(genConstraintsTerm))(function(args$prime) {
              var argTys = map35(getType2)(args$prime);
              var varsSubst = map35(function(ty) {
                return substTyvars3(v3.value1)(embedType2(ty));
              })(v2.value0.sigArgs);
              return bind14(addConstraintsXtor(v.value0)(v.value1)(argTys)(varsSubst))(function() {
                var newT = new TyDecl2(v1.value0.declName, v3.value0);
                return pure13(new Xtor3(v.value0, v.value1, args$prime, newT));
              });
            });
          });
        });
      });
    }
    ;
    if (v instanceof XCase2) {
      return bind14(checkPts(v.value0)(v.value1))(function(mdecl) {
        if (mdecl instanceof Nothing) {
          return throwError11(new ErrBadPattern(v.value0, map35(function(v1) {
            return v1.value0.ptxt;
          })(v.value1)));
        }
        ;
        if (mdecl instanceof Just) {
          return bind14(freshTyVarsDecl(mdecl.value0.value0.declArgs))(function(v1) {
            return bind14($$for10(v.value1)(function(v2) {
              return bind14($$for10(zip(v2.value0.ptv)(v1.value0))(function(v3) {
                return addGenVar(v3.value0)(v3.value1);
              }))(function() {
                return bind14(genConstraintsCmd(v2.value0.ptcmd))(function(c$prime) {
                  return pure13(new Pattern3({
                    ptxt: v2.value0.ptxt,
                    ptv: v2.value0.ptv,
                    ptcmd: c$prime
                  }));
                });
              });
            }))(function(pts$prime) {
              var pts$prime$prime = map35(substTyvars12(v1.value1))(pts$prime);
              var newT = new TyDecl2(mdecl.value0.value0.declName, v1.value0);
              return pure13(new XCase3(v.value0, pts$prime$prime, newT));
            });
          });
        }
        ;
        throw new Error("Failed pattern match at GenerateConstraints.Terms (line 90, column 3 - line 100, column 36): " + [mdecl.constructor.name]);
      });
    }
    ;
    if (v instanceof ShiftCBV2) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        var newT = new TyShift2(getType2(t$prime));
        return pure13(new ShiftCBV3(v.value0, t$prime, newT));
      });
    }
    ;
    if (v instanceof ShiftCBN2) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        var newT = new TyShift2(getType2(t$prime));
        return pure13(new ShiftCBN3(v.value0, t$prime, newT));
      });
    }
    ;
    throw new Error("Failed pattern match at GenerateConstraints.Terms (line 62, column 1 - line 62, column 44): " + [v.constructor.name]);
  };
  var genConstraintsCmd = function(v) {
    if (v instanceof Cut2) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        return bind14(genConstraintsTerm(v.value3))(function(u$prime) {
          return bind14(addConstraint(new MkTyEq(getType2(t$prime), getType2(u$prime))))(function() {
            return pure13(new Cut3(v.value0, t$prime, v.value2, u$prime));
          });
        });
      });
    }
    ;
    if (v instanceof CutAnnot) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        var ty1$prime = getType2(t$prime);
        return bind14(genConstraintsTerm(v.value4))(function(u$prime) {
          return bind14(genConstraintsTy(v.value0)(v.value2))(function(ty$prime) {
            return bind14(addConstraint(new MkTyEq(ty1$prime, getType2(u$prime))))(function() {
              return bind14(addConstraint(new MkTyEq(ty1$prime, ty$prime)))(function() {
                return pure13(new Cut3(v.value0, t$prime, v.value3, u$prime));
              });
            });
          });
        });
      });
    }
    ;
    if (v instanceof Done3) {
      return pure13(new Done4(v.value0));
    }
    ;
    if (v instanceof Err2) {
      return pure13(new Err3(v.value0, v.value1));
    }
    ;
    if (v instanceof Print2) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        return pure13(new Print3(v.value0, t$prime));
      });
    }
    ;
    if (v instanceof PrintAnnot) {
      return bind14(genConstraintsTerm(v.value1))(function(t$prime) {
        return bind14(genConstraintsTy(v.value0)(v.value2))(function(ty$prime) {
          return bind14(addConstraint(new MkTyEq(getType2(t$prime), ty$prime)))(function() {
            return pure13(new Print3(v.value0, t$prime));
          });
        });
      });
    }
    ;
    throw new Error("Failed pattern match at GenerateConstraints.Terms (line 38, column 1 - line 38, column 49): " + [v.constructor.name]);
  };

  // output/GenerateConstraints.Program/index.js
  var monadExceptT12 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind15 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT12)));
  var pure14 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT12)));
  var genConstraintsVarDecl = function(v) {
    return bind15(genConstraintsTerm(v.value0.varBody))(function(bd$prime) {
      return pure14(new VarDecl2({
        varPos: v.value0.varPos,
        varIsRec: v.value0.varIsRec,
        varName: v.value0.varName,
        varTy: getType2(bd$prime),
        varBody: bd$prime
      }));
    });
  };

  // output/InferDecl/index.js
  var show46 = /* @__PURE__ */ show(showTypevar);
  var show119 = /* @__PURE__ */ show(showTypename);
  var show214 = /* @__PURE__ */ show(showTy);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable(ordTypevar)(foldableList);
  var map36 = /* @__PURE__ */ map(functorList);
  var monadExceptT13 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind16 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadExceptT13));
  var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadExceptT13);
  var modify9 = /* @__PURE__ */ modify2(monadStateStateT2);
  var applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadExceptT13);
  var pure15 = /* @__PURE__ */ pure(applicativeStateT2);
  var gets6 = /* @__PURE__ */ gets(monadStateStateT2);
  var lookup7 = /* @__PURE__ */ lookup(ordTypevar);
  var throwError12 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity)));
  var $$for11 = /* @__PURE__ */ $$for(applicativeStateT2)(traversableList);
  var lookup13 = /* @__PURE__ */ lookup(ordTypename);
  var map113 = /* @__PURE__ */ map(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity)));
  var shiftEvalOrder4 = /* @__PURE__ */ shiftEvalOrder(shiftEvalOrderTy);
  var ErrUndefinedTyVar = /* @__PURE__ */ function() {
    function ErrUndefinedTyVar3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrUndefinedTyVar3.create = function(value0) {
      return function(value1) {
        return new ErrUndefinedTyVar3(value0, value1);
      };
    };
    return ErrUndefinedTyVar3;
  }();
  var ErrUndefinedType = /* @__PURE__ */ function() {
    function ErrUndefinedType2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrUndefinedType2.create = function(value0) {
      return function(value1) {
        return new ErrUndefinedType2(value0, value1);
      };
    };
    return ErrUndefinedType2;
  }();
  var ErrIllegalType = /* @__PURE__ */ function() {
    function ErrIllegalType2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrIllegalType2.create = function(value0) {
      return function(value1) {
        return new ErrIllegalType2(value0, value1);
      };
    };
    return ErrIllegalType2;
  }();
  var ErrOther6 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var MkDeclState = /* @__PURE__ */ function() {
    function MkDeclState2(value0) {
      this.value0 = value0;
    }
    ;
    MkDeclState2.create = function(value0) {
      return new MkDeclState2(value0);
    };
    return MkDeclState2;
  }();
  var errorInferDeclError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrUndefinedTyVar) {
          return "Type Variable " + (show46(v.value1) + " was not defined");
        }
        ;
        if (v instanceof ErrUndefinedType) {
          return "Type " + (show119(v.value1) + " was not defined");
        }
        ;
        if (v instanceof ErrIllegalType) {
          return "Type " + (show214(v.value1) + " is not allowed in data declaration");
        }
        ;
        if (v instanceof ErrOther6) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at InferDecl (line 42, column 1 - line 53, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrUndefinedTyVar) {
          return v.value0;
        }
        ;
        if (v instanceof ErrUndefinedType) {
          return v.value0;
        }
        ;
        if (v instanceof ErrIllegalType) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther6) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at InferDecl (line 42, column 1 - line 53, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther6.create
    };
  }();
  var setCurrVars = function(vars) {
    return function(isco) {
      var eo = defaultEo(isco);
      var newM = fromFoldable6(map36(function(v) {
        return new Tuple(v.value0.variantVar, varianceEvalOrder(v.value0.variantVariance)(eo));
      })(vars));
      return bind16(modify9(function(v) {
        return new MkDeclState({
          declsDone: v.value0.declsDone,
          currEo: v.value0.currEo,
          currVars: newM
        });
      }))(function() {
        return pure15(unit);
      });
    };
  };
  var setCurrPol = function(eo) {
    return bind16(modify9(function(v) {
      return new MkDeclState({
        declsDone: v.value0.declsDone,
        currVars: v.value0.currVars,
        currEo: new Just(eo)
      });
    }))(function() {
      return pure15(unit);
    });
  };
  var initialDeclState = /* @__PURE__ */ function() {
    return new MkDeclState({
      declsDone: empty2,
      currVars: empty2,
      currEo: Nothing.value
    });
  }();
  var runDeclM = function(m) {
    var v = runExcept(runStateT(m)(initialDeclState));
    if (v instanceof Left) {
      return new Left(v.value0);
    }
    ;
    if (v instanceof Right) {
      return new Right(v.value0.value0);
    }
    ;
    throw new Error("Failed pattern match at InferDecl (line 58, column 14 - line 60, column 32): " + [v.constructor.name]);
  };
  var inferType = function(v) {
    return function(v1) {
      if (v1 instanceof TyVar) {
        return bind16(gets6(function(v3) {
          return v3.value0.currVars;
        }))(function(vars) {
          var v3 = lookup7(v1.value0)(vars);
          if (v3 instanceof Nothing) {
            return throwError12(new ErrUndefinedTyVar(v, v1.value0));
          }
          ;
          if (v3 instanceof Just) {
            return pure15(new TyVar3(v1.value0, new MkKind(v3.value0)));
          }
          ;
          throw new Error("Failed pattern match at InferDecl (line 89, column 3 - line 91, column 44): " + [v3.constructor.name]);
        });
      }
      ;
      if (v1 instanceof TyDecl) {
        return bind16($$for11(v1.value1)(inferType(v)))(function(args$prime) {
          return bind16(gets6(function(v2) {
            return v2.value0.declsDone;
          }))(function(decls) {
            var v2 = lookup13(v1.value0)(decls);
            if (v2 instanceof Nothing) {
              return bind16(gets6(function(v3) {
                return v3.value0.currEo;
              }))(function(eo) {
                if (eo instanceof Nothing) {
                  return throwError12(new ErrUndefinedType(v, v1.value0));
                }
                ;
                if (eo instanceof Just) {
                  return pure15(new TyDecl3(v1.value0, args$prime, new MkKind(eo.value0)));
                }
                ;
                throw new Error("Failed pattern match at InferDecl (line 99, column 7 - line 101, column 59): " + [eo.constructor.name]);
              });
            }
            ;
            if (v2 instanceof Just) {
              return pure15(new TyDecl3(v1.value0, args$prime, new MkKind(defaultEo(v2.value0.value0.declType))));
            }
            ;
            throw new Error("Failed pattern match at InferDecl (line 96, column 3 - line 102, column 91): " + [v2.constructor.name]);
          });
        });
      }
      ;
      if (v1 instanceof TyCo) {
        return map113(TyCo3.create)(inferType(v)(v1.value0));
      }
      ;
      if (v1 instanceof TyShift) {
        return map113(shiftEvalOrder4)(inferType(v)(v1.value0));
      }
      ;
      if (v1 instanceof TyForall) {
        return throwError12(new ErrIllegalType(v, v1));
      }
      ;
      throw new Error("Failed pattern match at InferDecl (line 86, column 1 - line 86, column 39): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var inferXtorSig = function(v) {
    return bind16($$for11(v.value0.sigArgs)(inferType(v.value0.sigPos)))(function(args$prime) {
      return pure15(new XtorSig3({
        sigPos: v.value0.sigPos,
        sigName: v.value0.sigName,
        sigArgs: args$prime
      }));
    });
  };
  var inferDecl = function(v) {
    return bind16(setCurrVars(v.value0.declArgs)(v.value0.declType))(function() {
      return bind16(setCurrPol(defaultEo(v.value0.declType)))(function() {
        return bind16($$for11(v.value0.declXtors)(inferXtorSig))(function(xtors$prime) {
          return pure15(new DataDecl2({
            declPos: v.value0.declPos,
            declName: v.value0.declName,
            declArgs: v.value0.declArgs,
            declType: v.value0.declType,
            declXtors: xtors$prime
          }));
        });
      });
    });
  };

  // output/Kinding.Definition/index.js
  var runKindM = function(env) {
    return function(m) {
      return runExcept(runReaderT(m)(env));
    };
  };

  // output/Kinding.Errors/index.js
  var ErrNotImplemented = /* @__PURE__ */ function() {
    function ErrNotImplemented2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrNotImplemented2.create = function(value0) {
      return function(value1) {
        return new ErrNotImplemented2(value0, value1);
      };
    };
    return ErrNotImplemented2;
  }();
  var ErrOther7 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorKindError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrOther7) {
          return v.value1;
        }
        ;
        if (v instanceof ErrNotImplemented) {
          return "Function " + (v.value1 + " is not yet implemented");
        }
        ;
        throw new Error("Failed pattern match at Kinding.Errors (line 14, column 1 - line 21, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrOther7) {
          return v.value0;
        }
        ;
        if (v instanceof ErrNotImplemented) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Kinding.Errors (line 14, column 1 - line 21, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther7.create
    };
  }();

  // output/Kinding.Types/index.js
  var applicativeReaderT12 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var pure16 = /* @__PURE__ */ pure(applicativeReaderT12);
  var bind17 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindExceptT(monadIdentity)));
  var $$for12 = /* @__PURE__ */ $$for(applicativeReaderT12)(traversableList);
  var map37 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorExceptT(functorIdentity)));
  var defaultKind = /* @__PURE__ */ function() {
    return new MkKind(CBV.value);
  }();
  var kindType = function(v) {
    if (v instanceof TyVar2) {
      return pure16(new TyVar3(v.value0, defaultKind));
    }
    ;
    if (v instanceof TyDecl2) {
      return bind17($$for12(v.value1)(kindType))(function(tyArgs$prime) {
        return pure16(new TyDecl3(v.value0, tyArgs$prime, defaultKind));
      });
    }
    ;
    if (v instanceof TyShift2) {
      return map37(function(x) {
        return new TyShift3(x, defaultKind);
      })(kindType(v.value0));
    }
    ;
    if (v instanceof TyCo2) {
      return map37(TyCo3.create)(kindType(v.value0));
    }
    ;
    if (v instanceof TyForall2) {
      return map37(TyForall3.create(v.value0))(kindType(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Kinding.Types (line 16, column 1 - line 16, column 31): " + [v.constructor.name]);
  };

  // output/Kinding.Terms/index.js
  var bind18 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindExceptT(monadIdentity)));
  var applicativeReaderT13 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var pure17 = /* @__PURE__ */ pure(applicativeReaderT13);
  var $$for13 = /* @__PURE__ */ $$for(applicativeReaderT13)(traversableList);
  var map38 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorExceptT(functorIdentity)));
  var kindTerm = function(v) {
    if (v instanceof Var3) {
      return bind18(kindType(v.value2))(function(ty$prime) {
        return pure17(new Var(v.value0, v.value1, ty$prime));
      });
    }
    ;
    if (v instanceof Mu3) {
      return bind18(kindCommand(v.value2))(function(c$prime) {
        return bind18(kindType(v.value3))(function(ty$prime) {
          return pure17(new Mu(v.value0, v.value1, c$prime, ty$prime));
        });
      });
    }
    ;
    if (v instanceof Xtor3) {
      return bind18($$for13(v.value2)(kindTerm))(function(args$prime) {
        return bind18(kindType(v.value3))(function(ty$prime) {
          return pure17(new Xtor(v.value0, v.value1, args$prime, ty$prime));
        });
      });
    }
    ;
    if (v instanceof XCase3) {
      return bind18($$for13(v.value1)(kindPattern))(function(pts$prime) {
        return bind18(kindType(v.value2))(function(ty$prime) {
          return pure17(new XCase(v.value0, pts$prime, ty$prime));
        });
      });
    }
    ;
    if (v instanceof ShiftCBV3) {
      return bind18(kindTerm(v.value1))(function(t$prime) {
        return bind18(kindType(v.value2))(function(ty$prime) {
          return pure17(new ShiftCBV(v.value0, t$prime, ty$prime));
        });
      });
    }
    ;
    if (v instanceof ShiftCBN3) {
      return bind18(kindTerm(v.value1))(function(t$prime) {
        return bind18(kindType(v.value2))(function(ty$prime) {
          return pure17(new ShiftCBN(v.value0, t$prime, ty$prime));
        });
      });
    }
    ;
    throw new Error("Failed pattern match at Kinding.Terms (line 14, column 1 - line 14, column 35): " + [v.constructor.name]);
  };
  var kindPattern = function(v) {
    return bind18(kindCommand(v.value0.ptcmd))(function(c$prime) {
      return pure17(new Pattern({
        ptxt: v.value0.ptxt,
        ptv: v.value0.ptv,
        ptcmd: c$prime
      }));
    });
  };
  var kindCommand = function(v) {
    if (v instanceof Cut3) {
      return bind18(kindTerm(v.value1))(function(t$prime) {
        return bind18(kindTerm(v.value3))(function(u$prime) {
          return pure17(new Cut(v.value0, t$prime, v.value2, u$prime));
        });
      });
    }
    ;
    if (v instanceof Done4) {
      return pure17(new Done2(v.value0));
    }
    ;
    if (v instanceof Err3) {
      return pure17(new Err(v.value0, v.value1));
    }
    ;
    if (v instanceof Print3) {
      return map38(Print.create(v.value0))(kindTerm(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Kinding.Terms (line 45, column 1 - line 45, column 44): " + [v.constructor.name]);
  };

  // output/Kinding.Program/index.js
  var bind19 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindExceptT(monadIdentity)));
  var applicativeReaderT14 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var pure18 = /* @__PURE__ */ pure(applicativeReaderT14);
  var kindVariable = function(v) {
    return bind19(kindType(v.value0.varTy))(function(ty$prime) {
      return bind19(kindTerm(v.value0.varBody))(function(bd$prime) {
        return pure18(new VarDecl3({
          varPos: v.value0.varPos,
          varName: v.value0.varName,
          varIsRec: v.value0.varIsRec,
          varBody: bd$prime,
          varTy: ty$prime
        }));
      });
    });
  };

  // output/Parser.Errors/index.js
  var ErrParser = /* @__PURE__ */ function() {
    function ErrParser2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrParser2.create = function(value0) {
      return function(value1) {
        return new ErrParser2(value0, value1);
      };
    };
    return ErrParser2;
  }();
  var errorParserErr = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        return v.value1;
      },
      getLocation: function(v) {
        return v.value0;
      },
      toError: ErrParser.create
    };
  }();
  var parseErrorToParserErr = function(v) {
    var pos = {
      srcCol: v.value1.column,
      srcLine: v.value1.line
    };
    var loc = {
      locStart: pos,
      locEnd: pos
    };
    return new ErrParser(loc, v.value0);
  };

  // output/Data.Lazy/foreign.js
  var defer2 = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l) {
    return l();
  };

  // output/Parsing/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unwrap4 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ParseState2(value0, value1, value22);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var lazyParserT = {
    defer: function(f) {
      var m = defer2(f);
      return function(state1, more, lift1, $$throw2, done) {
        var v = force(m);
        return v(state1, more, lift1, $$throw2, done);
      };
    }
  };
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                return done(state22, f(a2));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state22, f) {
              return more(function(v3) {
                return v1(state22, more, lift1, $$throw2, function(state3, a2) {
                  return more(function(v4) {
                    return done(state3, f(a2));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                var v3 = next(a2);
                return v3(state22, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a2) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a2);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadRecParserT = {
    tailRecM: function(next) {
      return function(initArg) {
        return function(state1, more, lift1, $$throw2, done) {
          var $lazy_loop = $runtime_lazy4("loop", "Parsing", function() {
            return function(state22, arg, gas) {
              var v = next(arg);
              return v(state22, more, lift1, $$throw2, function(state3, step4) {
                if (step4 instanceof Loop) {
                  var $206 = gas === 0;
                  if ($206) {
                    return more(function(v1) {
                      return $lazy_loop(277)(state3, step4.value0, 30);
                    });
                  }
                  ;
                  return $lazy_loop(279)(state3, step4.value0, gas - 1 | 0);
                }
                ;
                if (step4 instanceof Done) {
                  return done(state3, step4.value0);
                }
                ;
                throw new Error("Failed pattern match at Parsing (line 273, column 39 - line 281, column 43): " + [step4.constructor.name]);
              });
            };
          });
          var loop2 = $lazy_loop(270);
          return loop2(state1, initArg, 30);
        };
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError13 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k) {
    return function(state1, v, v1, v2, done) {
      var v3 = k(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map60 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go2 = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step4) {
            var v1 = step4(unit);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift) {
              $tco_done = true;
              return map60(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure111(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM5(go2)(function(v1) {
          return v(state1, More.create, Lift.create, function(state22, err) {
            return new Stop(state22, new Left(err));
          }, function(state22, res) {
            return new Stop(state22, new Right(res));
          });
        });
      };
    };
  };
  var position = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map60 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p2) {
        var initialState2 = new ParseState(s, initialPos, false);
        return map60(fst)(runParserT$prime1(initialState2)(p2));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $281 = runParserT1(s);
    return function($282) {
      return unwrap4($281($282));
    };
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError13(new ParseError(message2, pos));
    };
  };
  var fail = function(message2) {
    return bindFlipped3(failWithPosition(message2))(position);
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };

  // output/Parser.Definition/index.js
  var MkD = /* @__PURE__ */ function() {
    function MkD2(value0) {
      this.value0 = value0;
    }
    ;
    MkD2.create = function(value0) {
      return new MkD2(value0);
    };
    return MkD2;
  }();
  var MkV = /* @__PURE__ */ function() {
    function MkV2(value0) {
      this.value0 = value0;
    }
    ;
    MkV2.create = function(value0) {
      return new MkV2(value0);
    };
    return MkV2;
  }();
  var MkA = /* @__PURE__ */ function() {
    function MkA2(value0) {
      this.value0 = value0;
    }
    ;
    MkA2.create = function(value0) {
      return new MkA2(value0);
    };
    return MkA2;
  }();
  var MkI = /* @__PURE__ */ function() {
    function MkI2(value0) {
      this.value0 = value0;
    }
    ;
    MkI2.create = function(value0) {
      return new MkI2(value0);
    };
    return MkI2;
  }();
  var MkM = /* @__PURE__ */ function() {
    function MkM2(value0) {
      this.value0 = value0;
    }
    ;
    MkM2.create = function(value0) {
      return new MkM2(value0);
    };
    return MkM2;
  }();
  var runSourceParser = function(src9) {
    return function(p2) {
      var v = runParser(src9)(p2);
      if (v instanceof Left) {
        return new Left(parseErrorToParserErr(v.value0));
      }
      ;
      if (v instanceof Right) {
        return new Right(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser.Definition (line 20, column 25 - line 22, column 21): " + [v.constructor.name]);
    };
  };

  // output/Parser.Keywords/index.js
  var KwModule = /* @__PURE__ */ function() {
    function KwModule2() {
    }
    ;
    KwModule2.value = new KwModule2();
    return KwModule2;
  }();
  var KwData = /* @__PURE__ */ function() {
    function KwData2() {
    }
    ;
    KwData2.value = new KwData2();
    return KwData2;
  }();
  var KwCodata = /* @__PURE__ */ function() {
    function KwCodata2() {
    }
    ;
    KwCodata2.value = new KwCodata2();
    return KwCodata2;
  }();
  var KwCBV = /* @__PURE__ */ function() {
    function KwCBV2() {
    }
    ;
    KwCBV2.value = new KwCBV2();
    return KwCBV2;
  }();
  var KwCBN = /* @__PURE__ */ function() {
    function KwCBN2() {
    }
    ;
    KwCBN2.value = new KwCBN2();
    return KwCBN2;
  }();
  var Kwmu = /* @__PURE__ */ function() {
    function Kwmu2() {
    }
    ;
    Kwmu2.value = new Kwmu2();
    return Kwmu2;
  }();
  var KwMu = /* @__PURE__ */ function() {
    function KwMu2() {
    }
    ;
    KwMu2.value = new KwMu2();
    return KwMu2;
  }();
  var KwCase = /* @__PURE__ */ function() {
    function KwCase2() {
    }
    ;
    KwCase2.value = new KwCase2();
    return KwCase2;
  }();
  var Kwcase = /* @__PURE__ */ function() {
    function Kwcase2() {
    }
    ;
    Kwcase2.value = new Kwcase2();
    return Kwcase2;
  }();
  var KwDone = /* @__PURE__ */ function() {
    function KwDone2() {
    }
    ;
    KwDone2.value = new KwDone2();
    return KwDone2;
  }();
  var KwForall = /* @__PURE__ */ function() {
    function KwForall2() {
    }
    ;
    KwForall2.value = new KwForall2();
    return KwForall2;
  }();
  var Kwforall = /* @__PURE__ */ function() {
    function Kwforall2() {
    }
    ;
    Kwforall2.value = new Kwforall2();
    return Kwforall2;
  }();
  var KwCo = /* @__PURE__ */ function() {
    function KwCo2() {
    }
    ;
    KwCo2.value = new KwCo2();
    return KwCo2;
  }();
  var Kwco = /* @__PURE__ */ function() {
    function Kwco2() {
    }
    ;
    Kwco2.value = new Kwco2();
    return Kwco2;
  }();
  var KwImport = /* @__PURE__ */ function() {
    function KwImport2() {
    }
    ;
    KwImport2.value = new KwImport2();
    return KwImport2;
  }();
  var KwMain = /* @__PURE__ */ function() {
    function KwMain2() {
    }
    ;
    KwMain2.value = new KwMain2();
    return KwMain2;
  }();
  var Kwmain = /* @__PURE__ */ function() {
    function Kwmain2() {
    }
    ;
    Kwmain2.value = new Kwmain2();
    return Kwmain2;
  }();
  var KwError = /* @__PURE__ */ function() {
    function KwError2() {
    }
    ;
    KwError2.value = new KwError2();
    return KwError2;
  }();
  var KwRec = /* @__PURE__ */ function() {
    function KwRec2() {
    }
    ;
    KwRec2.value = new KwRec2();
    return KwRec2;
  }();
  var KwPrint = /* @__PURE__ */ function() {
    function KwPrint2() {
    }
    ;
    KwPrint2.value = new KwPrint2();
    return KwPrint2;
  }();
  var Kwprint = /* @__PURE__ */ function() {
    function Kwprint2() {
    }
    ;
    Kwprint2.value = new Kwprint2();
    return Kwprint2;
  }();
  var KwIf = /* @__PURE__ */ function() {
    function KwIf2() {
    }
    ;
    KwIf2.value = new KwIf2();
    return KwIf2;
  }();
  var Kwif = /* @__PURE__ */ function() {
    function Kwif2() {
    }
    ;
    Kwif2.value = new Kwif2();
    return Kwif2;
  }();
  var KwThen = /* @__PURE__ */ function() {
    function KwThen2() {
    }
    ;
    KwThen2.value = new KwThen2();
    return KwThen2;
  }();
  var Kwthen = /* @__PURE__ */ function() {
    function Kwthen2() {
    }
    ;
    Kwthen2.value = new Kwthen2();
    return Kwthen2;
  }();
  var KwElse = /* @__PURE__ */ function() {
    function KwElse2() {
    }
    ;
    KwElse2.value = new KwElse2();
    return KwElse2;
  }();
  var Kwelse = /* @__PURE__ */ function() {
    function Kwelse2() {
    }
    ;
    Kwelse2.value = new Kwelse2();
    return Kwelse2;
  }();
  var KwOf = /* @__PURE__ */ function() {
    function KwOf2() {
    }
    ;
    KwOf2.value = new KwOf2();
    return KwOf2;
  }();
  var Kwof = /* @__PURE__ */ function() {
    function Kwof2() {
    }
    ;
    Kwof2.value = new Kwof2();
    return Kwof2;
  }();
  var showKeyword = {
    show: function(v) {
      if (v instanceof KwModule) {
        return "module";
      }
      ;
      if (v instanceof KwData) {
        return "data";
      }
      ;
      if (v instanceof KwCodata) {
        return "codata";
      }
      ;
      if (v instanceof KwCBV) {
        return "CBV";
      }
      ;
      if (v instanceof KwCBN) {
        return "CBN";
      }
      ;
      if (v instanceof KwMu) {
        return "Mu";
      }
      ;
      if (v instanceof Kwmu) {
        return "mu";
      }
      ;
      if (v instanceof KwCase) {
        return "Case";
      }
      ;
      if (v instanceof Kwcase) {
        return "case";
      }
      ;
      if (v instanceof KwDone) {
        return "Done";
      }
      ;
      if (v instanceof KwForall) {
        return "Forall";
      }
      ;
      if (v instanceof Kwforall) {
        return "forall";
      }
      ;
      if (v instanceof KwCo) {
        return "Co";
      }
      ;
      if (v instanceof Kwco) {
        return "co";
      }
      ;
      if (v instanceof KwImport) {
        return "import";
      }
      ;
      if (v instanceof KwMain) {
        return "Main";
      }
      ;
      if (v instanceof Kwmain) {
        return "main";
      }
      ;
      if (v instanceof KwError) {
        return "error";
      }
      ;
      if (v instanceof KwRec) {
        return "rec";
      }
      ;
      if (v instanceof KwPrint) {
        return "Print";
      }
      ;
      if (v instanceof Kwprint) {
        return "print";
      }
      ;
      if (v instanceof KwIf) {
        return "If";
      }
      ;
      if (v instanceof Kwif) {
        return "if";
      }
      ;
      if (v instanceof KwThen) {
        return "Then";
      }
      ;
      if (v instanceof Kwthen) {
        return "then";
      }
      ;
      if (v instanceof KwElse) {
        return "Else";
      }
      ;
      if (v instanceof Kwelse) {
        return "else";
      }
      ;
      if (v instanceof KwOf) {
        return "Of";
      }
      ;
      if (v instanceof Kwof) {
        return "of";
      }
      ;
      throw new Error("Failed pattern match at Parser.Keywords (line 40, column 1 - line 69, column 23): " + [v.constructor.name]);
    }
  };
  var allKws = /* @__PURE__ */ function() {
    return new Cons(KwModule.value, new Cons(KwData.value, new Cons(Kwmu.value, new Cons(KwMu.value, new Cons(KwCase.value, new Cons(Kwcase.value, new Cons(KwDone.value, new Cons(KwForall.value, new Cons(Kwforall.value, new Cons(KwCo.value, new Cons(Kwco.value, new Cons(KwImport.value, new Cons(KwMain.value, new Cons(Kwmain.value, new Cons(KwError.value, new Cons(KwRec.value, new Cons(KwCBV.value, new Cons(KwCBN.value, new Cons(KwCodata.value, new Cons(KwIf.value, new Cons(Kwif.value, new Cons(KwThen.value, new Cons(Kwthen.value, new Cons(KwElse.value, new Cons(Kwelse.value, new Cons(KwOf.value, new Cons(Kwof.value, Nil.value)))))))))))))))))))))))))));
  }();

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.List.NonEmpty/index.js
  var toList2 = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var singleton8 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons$prime = function(x) {
    return function(xs) {
      return new NonEmpty(x, xs);
    };
  };
  var cons = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Parser.Symbols/index.js
  var SymParensO = /* @__PURE__ */ function() {
    function SymParensO2() {
    }
    ;
    SymParensO2.value = new SymParensO2();
    return SymParensO2;
  }();
  var SymParensC = /* @__PURE__ */ function() {
    function SymParensC2() {
    }
    ;
    SymParensC2.value = new SymParensC2();
    return SymParensC2;
  }();
  var SymComma = /* @__PURE__ */ function() {
    function SymComma2() {
    }
    ;
    SymComma2.value = new SymComma2();
    return SymComma2;
  }();
  var SymPlus = /* @__PURE__ */ function() {
    function SymPlus2() {
    }
    ;
    SymPlus2.value = new SymPlus2();
    return SymPlus2;
  }();
  var SymMinus = /* @__PURE__ */ function() {
    function SymMinus2() {
    }
    ;
    SymMinus2.value = new SymMinus2();
    return SymMinus2;
  }();
  var SymColon = /* @__PURE__ */ function() {
    function SymColon2() {
    }
    ;
    SymColon2.value = new SymColon2();
    return SymColon2;
  }();
  var SymBrackO = /* @__PURE__ */ function() {
    function SymBrackO2() {
    }
    ;
    SymBrackO2.value = new SymBrackO2();
    return SymBrackO2;
  }();
  var SymBrackC = /* @__PURE__ */ function() {
    function SymBrackC2() {
    }
    ;
    SymBrackC2.value = new SymBrackC2();
    return SymBrackC2;
  }();
  var SymLambda = /* @__PURE__ */ function() {
    function SymLambda2() {
    }
    ;
    SymLambda2.value = new SymLambda2();
    return SymLambda2;
  }();
  var SymMu = /* @__PURE__ */ function() {
    function SymMu2() {
    }
    ;
    SymMu2.value = new SymMu2();
    return SymMu2;
  }();
  var SymSqBrackO = /* @__PURE__ */ function() {
    function SymSqBrackO2() {
    }
    ;
    SymSqBrackO2.value = new SymSqBrackO2();
    return SymSqBrackO2;
  }();
  var SymSqBrackC = /* @__PURE__ */ function() {
    function SymSqBrackC2() {
    }
    ;
    SymSqBrackC2.value = new SymSqBrackC2();
    return SymSqBrackC2;
  }();
  var SymEq = /* @__PURE__ */ function() {
    function SymEq2() {
    }
    ;
    SymEq2.value = new SymEq2();
    return SymEq2;
  }();
  var SymSemi = /* @__PURE__ */ function() {
    function SymSemi2() {
    }
    ;
    SymSemi2.value = new SymSemi2();
    return SymSemi2;
  }();
  var SymAngO = /* @__PURE__ */ function() {
    function SymAngO2() {
    }
    ;
    SymAngO2.value = new SymAngO2();
    return SymAngO2;
  }();
  var SymAngOUnicode = /* @__PURE__ */ function() {
    function SymAngOUnicode2() {
    }
    ;
    SymAngOUnicode2.value = new SymAngOUnicode2();
    return SymAngOUnicode2;
  }();
  var SymAngC = /* @__PURE__ */ function() {
    function SymAngC2() {
    }
    ;
    SymAngC2.value = new SymAngC2();
    return SymAngC2;
  }();
  var SymAngCUnicode = /* @__PURE__ */ function() {
    function SymAngCUnicode2() {
    }
    ;
    SymAngCUnicode2.value = new SymAngCUnicode2();
    return SymAngCUnicode2;
  }();
  var SymBar = /* @__PURE__ */ function() {
    function SymBar2() {
    }
    ;
    SymBar2.value = new SymBar2();
    return SymBar2;
  }();
  var SymDot = /* @__PURE__ */ function() {
    function SymDot2() {
    }
    ;
    SymDot2.value = new SymDot2();
    return SymDot2;
  }();
  var SymQuot = /* @__PURE__ */ function() {
    function SymQuot2() {
    }
    ;
    SymQuot2.value = new SymQuot2();
    return SymQuot2;
  }();
  var SymBackSl = /* @__PURE__ */ function() {
    function SymBackSl2() {
    }
    ;
    SymBackSl2.value = new SymBackSl2();
    return SymBackSl2;
  }();
  var SymAmper = /* @__PURE__ */ function() {
    function SymAmper2() {
    }
    ;
    SymAmper2.value = new SymAmper2();
    return SymAmper2;
  }();
  var SymExcl = /* @__PURE__ */ function() {
    function SymExcl2() {
    }
    ;
    SymExcl2.value = new SymExcl2();
    return SymExcl2;
  }();
  var showSym = {
    show: function(v) {
      if (v instanceof SymParensO) {
        return "(";
      }
      ;
      if (v instanceof SymParensC) {
        return ")";
      }
      ;
      if (v instanceof SymComma) {
        return ",";
      }
      ;
      if (v instanceof SymPlus) {
        return "+";
      }
      ;
      if (v instanceof SymMinus) {
        return "-";
      }
      ;
      if (v instanceof SymColon) {
        return ":";
      }
      ;
      if (v instanceof SymBrackO) {
        return "{";
      }
      ;
      if (v instanceof SymBrackC) {
        return "}";
      }
      ;
      if (v instanceof SymSqBrackO) {
        return "[";
      }
      ;
      if (v instanceof SymSqBrackC) {
        return "]";
      }
      ;
      if (v instanceof SymLambda) {
        return "\u03BB";
      }
      ;
      if (v instanceof SymMu) {
        return "\u03BC";
      }
      ;
      if (v instanceof SymEq) {
        return "=";
      }
      ;
      if (v instanceof SymSemi) {
        return ";";
      }
      ;
      if (v instanceof SymAngO) {
        return "<";
      }
      ;
      if (v instanceof SymAngOUnicode) {
        return "\u3008";
      }
      ;
      if (v instanceof SymAngCUnicode) {
        return "\u3009";
      }
      ;
      if (v instanceof SymAngC) {
        return ">";
      }
      ;
      if (v instanceof SymBar) {
        return "|";
      }
      ;
      if (v instanceof SymDot) {
        return ".";
      }
      ;
      if (v instanceof SymQuot) {
        return '"';
      }
      ;
      if (v instanceof SymBackSl) {
        return "\\";
      }
      ;
      if (v instanceof SymAmper) {
        return "&";
      }
      ;
      if (v instanceof SymExcl) {
        return "!";
      }
      ;
      throw new Error("Failed pattern match at Parser.Symbols (line 31, column 1 - line 55, column 24): " + [v.constructor.name]);
    }
  };

  // output/Parsing.Combinators/index.js
  var alt2 = /* @__PURE__ */ alt(altParserT);
  var defer3 = /* @__PURE__ */ defer(lazyParserT);
  var pure19 = /* @__PURE__ */ pure(applicativeParserT);
  var applySecond2 = /* @__PURE__ */ applySecond(applyParserT);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecParserT);
  var bind20 = /* @__PURE__ */ bind(bindParserT);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorParserT);
  var map39 = /* @__PURE__ */ map(functorParserT);
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var apply2 = /* @__PURE__ */ apply(applyParserT);
  var withLazyErrorMessage = function(p2) {
    return function(msg) {
      return alt2(p2)(defer3(function(v) {
        return fail("Expected " + msg(unit));
      }));
    };
  };
  var withErrorMessage = function(p2) {
    return function(msg) {
      return alt2(p2)(fail("Expected " + msg));
    };
  };
  var $$try2 = function(v) {
    return function(v1, more, lift6, $$throw2, done) {
      return v(v1, more, lift6, function(v2, err) {
        return $$throw2(new ParseState(v2.value0, v2.value1, v1.value2), err);
      }, done);
    };
  };
  var sepBy1 = function(p2) {
    return function(sep) {
      return bind20(p2)(function(a2) {
        return bind20(manyRec2(applySecond2(sep)(p2)))(function(as) {
          return pure19(cons$prime(a2)(as));
        });
      });
    };
  };
  var sepBy = function(p2) {
    return function(sep) {
      return alt2(map39(toList2)(sepBy1(p2)(sep)))(pure19(Nil.value));
    };
  };
  var option = function(a2) {
    return function(p2) {
      return alt2(p2)(pure19(a2));
    };
  };
  var optionMaybe = function(p2) {
    return option(Nothing.value)(map39(Just.create)(p2));
  };
  var manyTill = function(p2) {
    return function(end) {
      var go2 = function(acc) {
        return alt2(mapFlipped2(end)(function(v) {
          return new Done(reverse(acc));
        }))(mapFlipped2(p2)(function(x) {
          return new Loop(new Cons(x, acc));
        }));
      };
      return tailRecM3(go2)(Nil.value);
    };
  };
  var many1 = function(p2) {
    return apply2(map39(cons$prime)(p2))(manyRec2(p2));
  };
  var many = manyRec2;

  // output/Parsing.String/index.js
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show120 = /* @__PURE__ */ show(showString);
  var show215 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum3(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum3(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var updatePosString = function($copy_pos) {
    return function($copy_before) {
      return function($copy_after) {
        var $tco_var_pos = $copy_pos;
        var $tco_var_before = $copy_before;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(pos, before, after) {
          var v = uncons2(before);
          if (v instanceof Nothing) {
            $tco_done = true;
            return pos;
          }
          ;
          if (v instanceof Just) {
            var newPos = function() {
              if ($$null2(v.value0.tail)) {
                return updatePosSingle(pos)(v.value0.head)(after);
              }
              ;
              if (otherwise) {
                return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): ");
            }();
            $tco_var_pos = newPos;
            $tco_var_before = v.value0.tail;
            $copy_after = after;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
        }
        ;
        return $tco_result;
      };
    };
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons2(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum3(v3.value0.head);
                var $85 = cp < 0 || cp > 65535;
                if ($85) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust5(toEnum2(cp));
                var $86 = f(ch);
                if ($86) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var eof = /* @__PURE__ */ mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw2) {
          return function(done) {
            var $133 = $$null2(v.value0);
            if ($133) {
              return done(new ParseState(v.value0, v.value1, true), unit);
            }
            ;
            return $$throw2(v, new ParseError("Expected EOF", v.value1));
          };
        };
      };
    };
  });
  var consumeWith = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = f(v.value0);
              if (v3 instanceof Left) {
                return $$throw2(v, new ParseError(v3.value0, v.value1));
              }
              ;
              if (v3 instanceof Right) {
                return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var string = function(str) {
    return consumeWith(function(input3) {
      var v = stripPrefix(str)(input3);
      if (v instanceof Just) {
        return new Right({
          value: str,
          consumed: str,
          remainder: v.value0
        });
      }
      ;
      return new Left("Expected " + show120(str));
    });
  };
  var $$char = function(c) {
    return withErrorMessage(satisfy(function(v) {
      return v === c;
    }))(show215(c));
  };
  var anyChar = /* @__PURE__ */ satisfy(/* @__PURE__ */ $$const(true));

  // output/Data.CodePoint.Unicode.Internal/index.js
  var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
  var elemIndex3 = /* @__PURE__ */ elemIndex2(eqInt);
  var NUMCAT_LU = /* @__PURE__ */ function() {
    function NUMCAT_LU2() {
    }
    ;
    NUMCAT_LU2.value = new NUMCAT_LU2();
    return NUMCAT_LU2;
  }();
  var NUMCAT_LL = /* @__PURE__ */ function() {
    function NUMCAT_LL2() {
    }
    ;
    NUMCAT_LL2.value = new NUMCAT_LL2();
    return NUMCAT_LL2;
  }();
  var NUMCAT_LT = /* @__PURE__ */ function() {
    function NUMCAT_LT2() {
    }
    ;
    NUMCAT_LT2.value = new NUMCAT_LT2();
    return NUMCAT_LT2;
  }();
  var NUMCAT_LM = /* @__PURE__ */ function() {
    function NUMCAT_LM2() {
    }
    ;
    NUMCAT_LM2.value = new NUMCAT_LM2();
    return NUMCAT_LM2;
  }();
  var NUMCAT_LO = /* @__PURE__ */ function() {
    function NUMCAT_LO2() {
    }
    ;
    NUMCAT_LO2.value = new NUMCAT_LO2();
    return NUMCAT_LO2;
  }();
  var NUMCAT_MN = /* @__PURE__ */ function() {
    function NUMCAT_MN2() {
    }
    ;
    NUMCAT_MN2.value = new NUMCAT_MN2();
    return NUMCAT_MN2;
  }();
  var NUMCAT_MC = /* @__PURE__ */ function() {
    function NUMCAT_MC2() {
    }
    ;
    NUMCAT_MC2.value = new NUMCAT_MC2();
    return NUMCAT_MC2;
  }();
  var NUMCAT_ME = /* @__PURE__ */ function() {
    function NUMCAT_ME2() {
    }
    ;
    NUMCAT_ME2.value = new NUMCAT_ME2();
    return NUMCAT_ME2;
  }();
  var NUMCAT_ND = /* @__PURE__ */ function() {
    function NUMCAT_ND2() {
    }
    ;
    NUMCAT_ND2.value = new NUMCAT_ND2();
    return NUMCAT_ND2;
  }();
  var NUMCAT_NL = /* @__PURE__ */ function() {
    function NUMCAT_NL2() {
    }
    ;
    NUMCAT_NL2.value = new NUMCAT_NL2();
    return NUMCAT_NL2;
  }();
  var NUMCAT_NO = /* @__PURE__ */ function() {
    function NUMCAT_NO2() {
    }
    ;
    NUMCAT_NO2.value = new NUMCAT_NO2();
    return NUMCAT_NO2;
  }();
  var NUMCAT_PC = /* @__PURE__ */ function() {
    function NUMCAT_PC2() {
    }
    ;
    NUMCAT_PC2.value = new NUMCAT_PC2();
    return NUMCAT_PC2;
  }();
  var NUMCAT_PD = /* @__PURE__ */ function() {
    function NUMCAT_PD2() {
    }
    ;
    NUMCAT_PD2.value = new NUMCAT_PD2();
    return NUMCAT_PD2;
  }();
  var NUMCAT_PS = /* @__PURE__ */ function() {
    function NUMCAT_PS2() {
    }
    ;
    NUMCAT_PS2.value = new NUMCAT_PS2();
    return NUMCAT_PS2;
  }();
  var NUMCAT_PE = /* @__PURE__ */ function() {
    function NUMCAT_PE2() {
    }
    ;
    NUMCAT_PE2.value = new NUMCAT_PE2();
    return NUMCAT_PE2;
  }();
  var NUMCAT_PI = /* @__PURE__ */ function() {
    function NUMCAT_PI2() {
    }
    ;
    NUMCAT_PI2.value = new NUMCAT_PI2();
    return NUMCAT_PI2;
  }();
  var NUMCAT_PF = /* @__PURE__ */ function() {
    function NUMCAT_PF2() {
    }
    ;
    NUMCAT_PF2.value = new NUMCAT_PF2();
    return NUMCAT_PF2;
  }();
  var NUMCAT_PO = /* @__PURE__ */ function() {
    function NUMCAT_PO2() {
    }
    ;
    NUMCAT_PO2.value = new NUMCAT_PO2();
    return NUMCAT_PO2;
  }();
  var NUMCAT_SM = /* @__PURE__ */ function() {
    function NUMCAT_SM2() {
    }
    ;
    NUMCAT_SM2.value = new NUMCAT_SM2();
    return NUMCAT_SM2;
  }();
  var NUMCAT_SC = /* @__PURE__ */ function() {
    function NUMCAT_SC2() {
    }
    ;
    NUMCAT_SC2.value = new NUMCAT_SC2();
    return NUMCAT_SC2;
  }();
  var NUMCAT_SK = /* @__PURE__ */ function() {
    function NUMCAT_SK2() {
    }
    ;
    NUMCAT_SK2.value = new NUMCAT_SK2();
    return NUMCAT_SK2;
  }();
  var NUMCAT_SO = /* @__PURE__ */ function() {
    function NUMCAT_SO2() {
    }
    ;
    NUMCAT_SO2.value = new NUMCAT_SO2();
    return NUMCAT_SO2;
  }();
  var NUMCAT_ZS = /* @__PURE__ */ function() {
    function NUMCAT_ZS2() {
    }
    ;
    NUMCAT_ZS2.value = new NUMCAT_ZS2();
    return NUMCAT_ZS2;
  }();
  var NUMCAT_ZL = /* @__PURE__ */ function() {
    function NUMCAT_ZL2() {
    }
    ;
    NUMCAT_ZL2.value = new NUMCAT_ZL2();
    return NUMCAT_ZL2;
  }();
  var NUMCAT_ZP = /* @__PURE__ */ function() {
    function NUMCAT_ZP2() {
    }
    ;
    NUMCAT_ZP2.value = new NUMCAT_ZP2();
    return NUMCAT_ZP2;
  }();
  var NUMCAT_CC = /* @__PURE__ */ function() {
    function NUMCAT_CC2() {
    }
    ;
    NUMCAT_CC2.value = new NUMCAT_CC2();
    return NUMCAT_CC2;
  }();
  var NUMCAT_CF = /* @__PURE__ */ function() {
    function NUMCAT_CF2() {
    }
    ;
    NUMCAT_CF2.value = new NUMCAT_CF2();
    return NUMCAT_CF2;
  }();
  var NUMCAT_CS = /* @__PURE__ */ function() {
    function NUMCAT_CS2() {
    }
    ;
    NUMCAT_CS2.value = new NUMCAT_CS2();
    return NUMCAT_CS2;
  }();
  var NUMCAT_CO = /* @__PURE__ */ function() {
    function NUMCAT_CO2() {
    }
    ;
    NUMCAT_CO2.value = new NUMCAT_CO2();
    return NUMCAT_CO2;
  }();
  var NUMCAT_CN = /* @__PURE__ */ function() {
    function NUMCAT_CN2() {
    }
    ;
    NUMCAT_CN2.value = new NUMCAT_CN2();
    return NUMCAT_CN2;
  }();
  var numSpaceBlocks = 7;
  var numLat1Blocks = 63;
  var numBlocks = 3396;
  var gencatZS = 2;
  var rule1 = /* @__PURE__ */ function() {
    return {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var spacechars = [{
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }];
  var gencatZP = 67108864;
  var rule162 = /* @__PURE__ */ function() {
    return {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZL = 33554432;
  var rule161 = /* @__PURE__ */ function() {
    return {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSO = 8192;
  var rule13 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule170 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
    };
  }();
  var rule171 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
    };
  }();
  var gencatSM = 64;
  var rule6 = /* @__PURE__ */ function() {
    return {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSK = 1024;
  var rule10 = /* @__PURE__ */ function() {
    return {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSC = 8;
  var rule3 = /* @__PURE__ */ function() {
    return {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPS = 16;
  var rule4 = /* @__PURE__ */ function() {
    return {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPO = 4;
  var rule2 = /* @__PURE__ */ function() {
    return {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPI = 32768;
  var rule15 = /* @__PURE__ */ function() {
    return {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPF = 262144;
  var rule19 = /* @__PURE__ */ function() {
    return {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPE = 32;
  var rule5 = /* @__PURE__ */ function() {
    return {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPD = 128;
  var rule7 = /* @__PURE__ */ function() {
    return {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPC = 2048;
  var rule11 = /* @__PURE__ */ function() {
    return {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNO = 131072;
  var rule17 = /* @__PURE__ */ function() {
    return {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNL = 16777216;
  var rule128 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule168 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
    };
  }();
  var rule169 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
    };
  }();
  var gencatND = 256;
  var rule8 = /* @__PURE__ */ function() {
    return {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMN = 2097152;
  var rule92 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule93 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
    };
  }();
  var gencatME = 4194304;
  var rule119 = /* @__PURE__ */ function() {
    return {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMC = 8388608;
  var rule124 = /* @__PURE__ */ function() {
    return {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLU = 512;
  var nullrule = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule104 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
    };
  }();
  var rule107 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule115 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
    };
  }();
  var rule117 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
    };
  }();
  var rule118 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
    };
  }();
  var rule120 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
    };
  }();
  var rule122 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
    };
  }();
  var rule125 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
    };
  }();
  var rule127 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38864,
      titledist: 0
    };
  }();
  var rule137 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3008 | 0,
      titledist: 0
    };
  }();
  var rule142 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
    };
  }();
  var rule144 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule153 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
    };
  }();
  var rule156 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
    };
  }();
  var rule157 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
    };
  }();
  var rule158 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
    };
  }();
  var rule159 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
    };
  }();
  var rule160 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
    };
  }();
  var rule163 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
    };
  }();
  var rule164 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
    };
  }();
  var rule165 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
    };
  }();
  var rule166 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
    };
  }();
  var rule172 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
    };
  }();
  var rule173 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
    };
  }();
  var rule174 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
    };
  }();
  var rule177 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
    };
  }();
  var rule178 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
    };
  }();
  var rule179 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
    };
  }();
  var rule180 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
    };
  }();
  var rule181 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
    };
  }();
  var rule183 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
    };
  }();
  var rule184 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
    };
  }();
  var rule186 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42308 | 0,
      titledist: 0
    };
  }();
  var rule187 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42319 | 0,
      titledist: 0
    };
  }();
  var rule188 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42315 | 0,
      titledist: 0
    };
  }();
  var rule189 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42305 | 0,
      titledist: 0
    };
  }();
  var rule190 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42258 | 0,
      titledist: 0
    };
  }();
  var rule191 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42282 | 0,
      titledist: 0
    };
  }();
  var rule192 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42261 | 0,
      titledist: 0
    };
  }();
  var rule193 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 928,
      titledist: 0
    };
  }();
  var rule194 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -48 | 0,
      titledist: 0
    };
  }();
  var rule195 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42307 | 0,
      titledist: 0
    };
  }();
  var rule196 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35384 | 0,
      titledist: 0
    };
  }();
  var rule201 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
    };
  }();
  var rule203 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 34,
      titledist: 0
    };
  }();
  var rule22 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var rule24 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
    };
  }();
  var rule26 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
    };
  }();
  var rule29 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
    };
  }();
  var rule30 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
    };
  }();
  var rule31 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
    };
  }();
  var rule32 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
    };
  }();
  var rule33 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
    };
  }();
  var rule34 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
    };
  }();
  var rule35 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
    };
  }();
  var rule37 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
    };
  }();
  var rule38 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
    };
  }();
  var rule40 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
    };
  }();
  var rule42 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
    };
  }();
  var rule43 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
    };
  }();
  var rule44 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
    };
  }();
  var rule45 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
    };
  }();
  var rule47 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
    };
  }();
  var rule51 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
    };
  }();
  var rule52 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
    };
  }();
  var rule53 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
    };
  }();
  var rule54 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
    };
  }();
  var rule55 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
    };
  }();
  var rule56 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
    };
  }();
  var rule58 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
    };
  }();
  var rule59 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
    };
  }();
  var rule60 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
    };
  }();
  var rule9 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
    };
  }();
  var rule94 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 116,
      titledist: 0
    };
  }();
  var rule95 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
    };
  }();
  var rule96 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
    };
  }();
  var rule97 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
    };
  }();
  var rule98 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
    };
  }();
  var gencatLT = 524288;
  var rule151 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule154 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
    };
  }();
  var rule48 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var gencatLO = 16384;
  var rule14 = /* @__PURE__ */ function() {
    return {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLM = 1048576;
  var rule91 = /* @__PURE__ */ function() {
    return {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLL = 4096;
  var rule100 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
    };
  }();
  var rule101 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
    };
  }();
  var rule102 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
    };
  }();
  var rule103 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
    };
  }();
  var rule105 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
    };
  }();
  var rule106 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
    };
  }();
  var rule108 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
    };
  }();
  var rule109 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
    };
  }();
  var rule110 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
    };
  }();
  var rule111 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
    };
  }();
  var rule112 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
    };
  }();
  var rule113 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
    };
  }();
  var rule114 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -116 | 0,
      lowdist: 0,
      titledist: -116 | 0
    };
  }();
  var rule116 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
    };
  }();
  var rule12 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
    };
  }();
  var rule121 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
    };
  }();
  var rule123 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
    };
  }();
  var rule126 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3008,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule129 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6254 | 0,
      lowdist: 0,
      titledist: -6254 | 0
    };
  }();
  var rule130 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6253 | 0,
      lowdist: 0,
      titledist: -6253 | 0
    };
  }();
  var rule131 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6244 | 0,
      lowdist: 0,
      titledist: -6244 | 0
    };
  }();
  var rule132 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6242 | 0,
      lowdist: 0,
      titledist: -6242 | 0
    };
  }();
  var rule133 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6243 | 0,
      lowdist: 0,
      titledist: -6243 | 0
    };
  }();
  var rule134 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6236 | 0,
      lowdist: 0,
      titledist: -6236 | 0
    };
  }();
  var rule135 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6181 | 0,
      lowdist: 0,
      titledist: -6181 | 0
    };
  }();
  var rule136 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35266,
      lowdist: 0,
      titledist: 35266
    };
  }();
  var rule138 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
    };
  }();
  var rule139 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
    };
  }();
  var rule140 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35384,
      lowdist: 0,
      titledist: 35384
    };
  }();
  var rule141 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
    };
  }();
  var rule143 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
    };
  }();
  var rule145 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
    };
  }();
  var rule146 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
    };
  }();
  var rule147 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
    };
  }();
  var rule148 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
    };
  }();
  var rule149 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
    };
  }();
  var rule150 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
    };
  }();
  var rule152 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
    };
  }();
  var rule155 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
    };
  }();
  var rule167 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
    };
  }();
  var rule175 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
    };
  }();
  var rule176 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
    };
  }();
  var rule18 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
    };
  }();
  var rule182 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
    };
  }();
  var rule185 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 48,
      lowdist: 0,
      titledist: 48
    };
  }();
  var rule197 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -928 | 0,
      lowdist: 0,
      titledist: -928 | 0
    };
  }();
  var rule198 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38864 | 0,
      lowdist: 0,
      titledist: -38864 | 0
    };
  }();
  var rule20 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule202 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
    };
  }();
  var rule204 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -34 | 0,
      lowdist: 0,
      titledist: -34 | 0
    };
  }();
  var rule21 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
    };
  }();
  var rule23 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule25 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
    };
  }();
  var rule27 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
    };
  }();
  var rule28 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
    };
  }();
  var rule36 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
    };
  }();
  var rule39 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
    };
  }();
  var rule41 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
    };
  }();
  var rule46 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
    };
  }();
  var rule49 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule50 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
    };
  }();
  var rule57 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
    };
  }();
  var rule61 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
    };
  }();
  var rule62 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
    };
  }();
  var rule63 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
    };
  }();
  var rule64 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
    };
  }();
  var rule65 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
    };
  }();
  var rule66 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
    };
  }();
  var rule67 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
    };
  }();
  var rule68 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
    };
  }();
  var rule69 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42319,
      lowdist: 0,
      titledist: 42319
    };
  }();
  var rule70 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42315,
      lowdist: 0,
      titledist: 42315
    };
  }();
  var rule71 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
    };
  }();
  var rule72 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
    };
  }();
  var rule73 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42308,
      lowdist: 0,
      titledist: 42308
    };
  }();
  var rule74 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
    };
  }();
  var rule75 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
    };
  }();
  var rule76 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
    };
  }();
  var rule77 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42305,
      lowdist: 0,
      titledist: 42305
    };
  }();
  var rule78 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
    };
  }();
  var rule79 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
    };
  }();
  var rule80 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
    };
  }();
  var rule81 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
    };
  }();
  var rule82 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
    };
  }();
  var rule83 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42307,
      lowdist: 0,
      titledist: 42307
    };
  }();
  var rule84 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42282,
      lowdist: 0,
      titledist: 42282
    };
  }();
  var rule85 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
    };
  }();
  var rule86 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
    };
  }();
  var rule87 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
    };
  }();
  var rule88 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
    };
  }();
  var rule89 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42261,
      lowdist: 0,
      titledist: 42261
    };
  }();
  var rule90 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42258,
      lowdist: 0,
      titledist: 42258
    };
  }();
  var rule99 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
    };
  }();
  var gencatCS = 134217728;
  var rule199 = /* @__PURE__ */ function() {
    return {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCO = 268435456;
  var rule200 = /* @__PURE__ */ function() {
    return {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCF = 65536;
  var rule16 = /* @__PURE__ */ function() {
    return {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCC = 1;
  var rule0 = /* @__PURE__ */ function() {
    return {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var bsearch = function(a2) {
    return function(array) {
      return function(size5) {
        return function(compare5) {
          var go2 = function($copy_i) {
            return function($copy_k) {
              var $tco_var_i = $copy_i;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(i2, k) {
                if (i2 > k || i2 >= length(array)) {
                  $tco_done = true;
                  return Nothing.value;
                }
                ;
                if (otherwise) {
                  var j = floor2(toNumber(i2 + k | 0) / 2);
                  var b2 = unsafeIndex2(array)(j);
                  var v = compare5(a2)(b2);
                  if (v instanceof EQ) {
                    $tco_done = true;
                    return new Just(b2);
                  }
                  ;
                  if (v instanceof GT) {
                    $tco_var_i = j + 1 | 0;
                    $copy_k = k;
                    return;
                  }
                  ;
                  $tco_var_i = i2;
                  $copy_k = j - 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i2.constructor.name, k.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_k);
              }
              ;
              return $tco_result;
            };
          };
          return go2(0)(size5);
        };
      };
    };
  };
  var blkCmp = function(v) {
    return function(v1) {
      if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
        return EQ.value;
      }
      ;
      if (v.start > v1.start) {
        return GT.value;
      }
      ;
      if (otherwise) {
        return LT.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getRule = function(blocks) {
    return function(unichar) {
      return function(size5) {
        var key = {
          start: unichar,
          length: 1,
          convRule: nullrule
        };
        var maybeCharBlock = bsearch(key)(blocks)(size5)(blkCmp);
        if (maybeCharBlock instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (maybeCharBlock instanceof Just) {
          return new Just(maybeCharBlock.value0.convRule);
        }
        ;
        throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
      };
    };
  };
  var checkAttrS = function(categories) {
    return function($$char2) {
      var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex3(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
  var allchars = [{
    start: 0,
    length: 32,
    convRule: rule0
  }, {
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 33,
    length: 3,
    convRule: rule2
  }, {
    start: 36,
    length: 1,
    convRule: rule3
  }, {
    start: 37,
    length: 3,
    convRule: rule2
  }, {
    start: 40,
    length: 1,
    convRule: rule4
  }, {
    start: 41,
    length: 1,
    convRule: rule5
  }, {
    start: 42,
    length: 1,
    convRule: rule2
  }, {
    start: 43,
    length: 1,
    convRule: rule6
  }, {
    start: 44,
    length: 1,
    convRule: rule2
  }, {
    start: 45,
    length: 1,
    convRule: rule7
  }, {
    start: 46,
    length: 2,
    convRule: rule2
  }, {
    start: 48,
    length: 10,
    convRule: rule8
  }, {
    start: 58,
    length: 2,
    convRule: rule2
  }, {
    start: 60,
    length: 3,
    convRule: rule6
  }, {
    start: 63,
    length: 2,
    convRule: rule2
  }, {
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 91,
    length: 1,
    convRule: rule4
  }, {
    start: 92,
    length: 1,
    convRule: rule2
  }, {
    start: 93,
    length: 1,
    convRule: rule5
  }, {
    start: 94,
    length: 1,
    convRule: rule10
  }, {
    start: 95,
    length: 1,
    convRule: rule11
  }, {
    start: 96,
    length: 1,
    convRule: rule10
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 123,
    length: 1,
    convRule: rule4
  }, {
    start: 124,
    length: 1,
    convRule: rule6
  }, {
    start: 125,
    length: 1,
    convRule: rule5
  }, {
    start: 126,
    length: 1,
    convRule: rule6
  }, {
    start: 127,
    length: 33,
    convRule: rule0
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 161,
    length: 1,
    convRule: rule2
  }, {
    start: 162,
    length: 4,
    convRule: rule3
  }, {
    start: 166,
    length: 1,
    convRule: rule13
  }, {
    start: 167,
    length: 1,
    convRule: rule2
  }, {
    start: 168,
    length: 1,
    convRule: rule10
  }, {
    start: 169,
    length: 1,
    convRule: rule13
  }, {
    start: 170,
    length: 1,
    convRule: rule14
  }, {
    start: 171,
    length: 1,
    convRule: rule15
  }, {
    start: 172,
    length: 1,
    convRule: rule6
  }, {
    start: 173,
    length: 1,
    convRule: rule16
  }, {
    start: 174,
    length: 1,
    convRule: rule13
  }, {
    start: 175,
    length: 1,
    convRule: rule10
  }, {
    start: 176,
    length: 1,
    convRule: rule13
  }, {
    start: 177,
    length: 1,
    convRule: rule6
  }, {
    start: 178,
    length: 2,
    convRule: rule17
  }, {
    start: 180,
    length: 1,
    convRule: rule10
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 182,
    length: 2,
    convRule: rule2
  }, {
    start: 184,
    length: 1,
    convRule: rule10
  }, {
    start: 185,
    length: 1,
    convRule: rule17
  }, {
    start: 186,
    length: 1,
    convRule: rule14
  }, {
    start: 187,
    length: 1,
    convRule: rule19
  }, {
    start: 188,
    length: 3,
    convRule: rule17
  }, {
    start: 191,
    length: 1,
    convRule: rule2
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 215,
    length: 1,
    convRule: rule6
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 223,
    length: 1,
    convRule: rule20
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 247,
    length: 1,
    convRule: rule6
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 312,
    length: 1,
    convRule: rule20
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 329,
    length: 1,
    convRule: rule20
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 397,
    length: 1,
    convRule: rule20
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 411,
    length: 1,
    convRule: rule20
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 426,
    length: 2,
    convRule: rule20
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 442,
    length: 1,
    convRule: rule20
  }, {
    start: 443,
    length: 1,
    convRule: rule14
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 446,
    length: 1,
    convRule: rule20
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 448,
    length: 4,
    convRule: rule14
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 496,
    length: 1,
    convRule: rule20
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 545,
    length: 1,
    convRule: rule20
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 564,
    length: 6,
    convRule: rule20
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 597,
    length: 1,
    convRule: rule20
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 600,
    length: 1,
    convRule: rule20
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 602,
    length: 1,
    convRule: rule20
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 605,
    length: 3,
    convRule: rule20
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 610,
    length: 1,
    convRule: rule20
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 612,
    length: 1,
    convRule: rule20
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 615,
    length: 1,
    convRule: rule20
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 621,
    length: 2,
    convRule: rule20
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 624,
    length: 1,
    convRule: rule20
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 627,
    length: 2,
    convRule: rule20
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 630,
    length: 7,
    convRule: rule20
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 638,
    length: 2,
    convRule: rule20
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 641,
    length: 1,
    convRule: rule20
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 644,
    length: 3,
    convRule: rule20
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 653,
    length: 5,
    convRule: rule20
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 659,
    length: 1,
    convRule: rule20
  }, {
    start: 660,
    length: 1,
    convRule: rule14
  }, {
    start: 661,
    length: 8,
    convRule: rule20
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 671,
    length: 17,
    convRule: rule20
  }, {
    start: 688,
    length: 18,
    convRule: rule91
  }, {
    start: 706,
    length: 4,
    convRule: rule10
  }, {
    start: 710,
    length: 12,
    convRule: rule91
  }, {
    start: 722,
    length: 14,
    convRule: rule10
  }, {
    start: 736,
    length: 5,
    convRule: rule91
  }, {
    start: 741,
    length: 7,
    convRule: rule10
  }, {
    start: 748,
    length: 1,
    convRule: rule91
  }, {
    start: 749,
    length: 1,
    convRule: rule10
  }, {
    start: 750,
    length: 1,
    convRule: rule91
  }, {
    start: 751,
    length: 17,
    convRule: rule10
  }, {
    start: 768,
    length: 69,
    convRule: rule92
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 838,
    length: 42,
    convRule: rule92
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 884,
    length: 1,
    convRule: rule91
  }, {
    start: 885,
    length: 1,
    convRule: rule10
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 890,
    length: 1,
    convRule: rule91
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 894,
    length: 1,
    convRule: rule2
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 900,
    length: 2,
    convRule: rule10
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 903,
    length: 1,
    convRule: rule2
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 912,
    length: 1,
    convRule: rule20
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 944,
    length: 1,
    convRule: rule20
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 978,
    length: 3,
    convRule: rule107
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1014,
    length: 1,
    convRule: rule6
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1020,
    length: 1,
    convRule: rule20
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1154,
    length: 1,
    convRule: rule13
  }, {
    start: 1155,
    length: 5,
    convRule: rule92
  }, {
    start: 1160,
    length: 2,
    convRule: rule119
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1369,
    length: 1,
    convRule: rule91
  }, {
    start: 1370,
    length: 6,
    convRule: rule2
  }, {
    start: 1376,
    length: 1,
    convRule: rule20
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 1415,
    length: 2,
    convRule: rule20
  }, {
    start: 1417,
    length: 1,
    convRule: rule2
  }, {
    start: 1418,
    length: 1,
    convRule: rule7
  }, {
    start: 1421,
    length: 2,
    convRule: rule13
  }, {
    start: 1423,
    length: 1,
    convRule: rule3
  }, {
    start: 1425,
    length: 45,
    convRule: rule92
  }, {
    start: 1470,
    length: 1,
    convRule: rule7
  }, {
    start: 1471,
    length: 1,
    convRule: rule92
  }, {
    start: 1472,
    length: 1,
    convRule: rule2
  }, {
    start: 1473,
    length: 2,
    convRule: rule92
  }, {
    start: 1475,
    length: 1,
    convRule: rule2
  }, {
    start: 1476,
    length: 2,
    convRule: rule92
  }, {
    start: 1478,
    length: 1,
    convRule: rule2
  }, {
    start: 1479,
    length: 1,
    convRule: rule92
  }, {
    start: 1488,
    length: 27,
    convRule: rule14
  }, {
    start: 1519,
    length: 4,
    convRule: rule14
  }, {
    start: 1523,
    length: 2,
    convRule: rule2
  }, {
    start: 1536,
    length: 6,
    convRule: rule16
  }, {
    start: 1542,
    length: 3,
    convRule: rule6
  }, {
    start: 1545,
    length: 2,
    convRule: rule2
  }, {
    start: 1547,
    length: 1,
    convRule: rule3
  }, {
    start: 1548,
    length: 2,
    convRule: rule2
  }, {
    start: 1550,
    length: 2,
    convRule: rule13
  }, {
    start: 1552,
    length: 11,
    convRule: rule92
  }, {
    start: 1563,
    length: 1,
    convRule: rule2
  }, {
    start: 1564,
    length: 1,
    convRule: rule16
  }, {
    start: 1566,
    length: 2,
    convRule: rule2
  }, {
    start: 1568,
    length: 32,
    convRule: rule14
  }, {
    start: 1600,
    length: 1,
    convRule: rule91
  }, {
    start: 1601,
    length: 10,
    convRule: rule14
  }, {
    start: 1611,
    length: 21,
    convRule: rule92
  }, {
    start: 1632,
    length: 10,
    convRule: rule8
  }, {
    start: 1642,
    length: 4,
    convRule: rule2
  }, {
    start: 1646,
    length: 2,
    convRule: rule14
  }, {
    start: 1648,
    length: 1,
    convRule: rule92
  }, {
    start: 1649,
    length: 99,
    convRule: rule14
  }, {
    start: 1748,
    length: 1,
    convRule: rule2
  }, {
    start: 1749,
    length: 1,
    convRule: rule14
  }, {
    start: 1750,
    length: 7,
    convRule: rule92
  }, {
    start: 1757,
    length: 1,
    convRule: rule16
  }, {
    start: 1758,
    length: 1,
    convRule: rule13
  }, {
    start: 1759,
    length: 6,
    convRule: rule92
  }, {
    start: 1765,
    length: 2,
    convRule: rule91
  }, {
    start: 1767,
    length: 2,
    convRule: rule92
  }, {
    start: 1769,
    length: 1,
    convRule: rule13
  }, {
    start: 1770,
    length: 4,
    convRule: rule92
  }, {
    start: 1774,
    length: 2,
    convRule: rule14
  }, {
    start: 1776,
    length: 10,
    convRule: rule8
  }, {
    start: 1786,
    length: 3,
    convRule: rule14
  }, {
    start: 1789,
    length: 2,
    convRule: rule13
  }, {
    start: 1791,
    length: 1,
    convRule: rule14
  }, {
    start: 1792,
    length: 14,
    convRule: rule2
  }, {
    start: 1807,
    length: 1,
    convRule: rule16
  }, {
    start: 1808,
    length: 1,
    convRule: rule14
  }, {
    start: 1809,
    length: 1,
    convRule: rule92
  }, {
    start: 1810,
    length: 30,
    convRule: rule14
  }, {
    start: 1840,
    length: 27,
    convRule: rule92
  }, {
    start: 1869,
    length: 89,
    convRule: rule14
  }, {
    start: 1958,
    length: 11,
    convRule: rule92
  }, {
    start: 1969,
    length: 1,
    convRule: rule14
  }, {
    start: 1984,
    length: 10,
    convRule: rule8
  }, {
    start: 1994,
    length: 33,
    convRule: rule14
  }, {
    start: 2027,
    length: 9,
    convRule: rule92
  }, {
    start: 2036,
    length: 2,
    convRule: rule91
  }, {
    start: 2038,
    length: 1,
    convRule: rule13
  }, {
    start: 2039,
    length: 3,
    convRule: rule2
  }, {
    start: 2042,
    length: 1,
    convRule: rule91
  }, {
    start: 2045,
    length: 1,
    convRule: rule92
  }, {
    start: 2046,
    length: 2,
    convRule: rule3
  }, {
    start: 2048,
    length: 22,
    convRule: rule14
  }, {
    start: 2070,
    length: 4,
    convRule: rule92
  }, {
    start: 2074,
    length: 1,
    convRule: rule91
  }, {
    start: 2075,
    length: 9,
    convRule: rule92
  }, {
    start: 2084,
    length: 1,
    convRule: rule91
  }, {
    start: 2085,
    length: 3,
    convRule: rule92
  }, {
    start: 2088,
    length: 1,
    convRule: rule91
  }, {
    start: 2089,
    length: 5,
    convRule: rule92
  }, {
    start: 2096,
    length: 15,
    convRule: rule2
  }, {
    start: 2112,
    length: 25,
    convRule: rule14
  }, {
    start: 2137,
    length: 3,
    convRule: rule92
  }, {
    start: 2142,
    length: 1,
    convRule: rule2
  }, {
    start: 2144,
    length: 11,
    convRule: rule14
  }, {
    start: 2208,
    length: 21,
    convRule: rule14
  }, {
    start: 2230,
    length: 18,
    convRule: rule14
  }, {
    start: 2259,
    length: 15,
    convRule: rule92
  }, {
    start: 2274,
    length: 1,
    convRule: rule16
  }, {
    start: 2275,
    length: 32,
    convRule: rule92
  }, {
    start: 2307,
    length: 1,
    convRule: rule124
  }, {
    start: 2308,
    length: 54,
    convRule: rule14
  }, {
    start: 2362,
    length: 1,
    convRule: rule92
  }, {
    start: 2363,
    length: 1,
    convRule: rule124
  }, {
    start: 2364,
    length: 1,
    convRule: rule92
  }, {
    start: 2365,
    length: 1,
    convRule: rule14
  }, {
    start: 2366,
    length: 3,
    convRule: rule124
  }, {
    start: 2369,
    length: 8,
    convRule: rule92
  }, {
    start: 2377,
    length: 4,
    convRule: rule124
  }, {
    start: 2381,
    length: 1,
    convRule: rule92
  }, {
    start: 2382,
    length: 2,
    convRule: rule124
  }, {
    start: 2384,
    length: 1,
    convRule: rule14
  }, {
    start: 2385,
    length: 7,
    convRule: rule92
  }, {
    start: 2392,
    length: 10,
    convRule: rule14
  }, {
    start: 2402,
    length: 2,
    convRule: rule92
  }, {
    start: 2404,
    length: 2,
    convRule: rule2
  }, {
    start: 2406,
    length: 10,
    convRule: rule8
  }, {
    start: 2416,
    length: 1,
    convRule: rule2
  }, {
    start: 2417,
    length: 1,
    convRule: rule91
  }, {
    start: 2418,
    length: 15,
    convRule: rule14
  }, {
    start: 2433,
    length: 1,
    convRule: rule92
  }, {
    start: 2434,
    length: 2,
    convRule: rule124
  }, {
    start: 2437,
    length: 8,
    convRule: rule14
  }, {
    start: 2447,
    length: 2,
    convRule: rule14
  }, {
    start: 2451,
    length: 22,
    convRule: rule14
  }, {
    start: 2474,
    length: 7,
    convRule: rule14
  }, {
    start: 2482,
    length: 1,
    convRule: rule14
  }, {
    start: 2486,
    length: 4,
    convRule: rule14
  }, {
    start: 2492,
    length: 1,
    convRule: rule92
  }, {
    start: 2493,
    length: 1,
    convRule: rule14
  }, {
    start: 2494,
    length: 3,
    convRule: rule124
  }, {
    start: 2497,
    length: 4,
    convRule: rule92
  }, {
    start: 2503,
    length: 2,
    convRule: rule124
  }, {
    start: 2507,
    length: 2,
    convRule: rule124
  }, {
    start: 2509,
    length: 1,
    convRule: rule92
  }, {
    start: 2510,
    length: 1,
    convRule: rule14
  }, {
    start: 2519,
    length: 1,
    convRule: rule124
  }, {
    start: 2524,
    length: 2,
    convRule: rule14
  }, {
    start: 2527,
    length: 3,
    convRule: rule14
  }, {
    start: 2530,
    length: 2,
    convRule: rule92
  }, {
    start: 2534,
    length: 10,
    convRule: rule8
  }, {
    start: 2544,
    length: 2,
    convRule: rule14
  }, {
    start: 2546,
    length: 2,
    convRule: rule3
  }, {
    start: 2548,
    length: 6,
    convRule: rule17
  }, {
    start: 2554,
    length: 1,
    convRule: rule13
  }, {
    start: 2555,
    length: 1,
    convRule: rule3
  }, {
    start: 2556,
    length: 1,
    convRule: rule14
  }, {
    start: 2557,
    length: 1,
    convRule: rule2
  }, {
    start: 2558,
    length: 1,
    convRule: rule92
  }, {
    start: 2561,
    length: 2,
    convRule: rule92
  }, {
    start: 2563,
    length: 1,
    convRule: rule124
  }, {
    start: 2565,
    length: 6,
    convRule: rule14
  }, {
    start: 2575,
    length: 2,
    convRule: rule14
  }, {
    start: 2579,
    length: 22,
    convRule: rule14
  }, {
    start: 2602,
    length: 7,
    convRule: rule14
  }, {
    start: 2610,
    length: 2,
    convRule: rule14
  }, {
    start: 2613,
    length: 2,
    convRule: rule14
  }, {
    start: 2616,
    length: 2,
    convRule: rule14
  }, {
    start: 2620,
    length: 1,
    convRule: rule92
  }, {
    start: 2622,
    length: 3,
    convRule: rule124
  }, {
    start: 2625,
    length: 2,
    convRule: rule92
  }, {
    start: 2631,
    length: 2,
    convRule: rule92
  }, {
    start: 2635,
    length: 3,
    convRule: rule92
  }, {
    start: 2641,
    length: 1,
    convRule: rule92
  }, {
    start: 2649,
    length: 4,
    convRule: rule14
  }, {
    start: 2654,
    length: 1,
    convRule: rule14
  }, {
    start: 2662,
    length: 10,
    convRule: rule8
  }, {
    start: 2672,
    length: 2,
    convRule: rule92
  }, {
    start: 2674,
    length: 3,
    convRule: rule14
  }, {
    start: 2677,
    length: 1,
    convRule: rule92
  }, {
    start: 2678,
    length: 1,
    convRule: rule2
  }, {
    start: 2689,
    length: 2,
    convRule: rule92
  }, {
    start: 2691,
    length: 1,
    convRule: rule124
  }, {
    start: 2693,
    length: 9,
    convRule: rule14
  }, {
    start: 2703,
    length: 3,
    convRule: rule14
  }, {
    start: 2707,
    length: 22,
    convRule: rule14
  }, {
    start: 2730,
    length: 7,
    convRule: rule14
  }, {
    start: 2738,
    length: 2,
    convRule: rule14
  }, {
    start: 2741,
    length: 5,
    convRule: rule14
  }, {
    start: 2748,
    length: 1,
    convRule: rule92
  }, {
    start: 2749,
    length: 1,
    convRule: rule14
  }, {
    start: 2750,
    length: 3,
    convRule: rule124
  }, {
    start: 2753,
    length: 5,
    convRule: rule92
  }, {
    start: 2759,
    length: 2,
    convRule: rule92
  }, {
    start: 2761,
    length: 1,
    convRule: rule124
  }, {
    start: 2763,
    length: 2,
    convRule: rule124
  }, {
    start: 2765,
    length: 1,
    convRule: rule92
  }, {
    start: 2768,
    length: 1,
    convRule: rule14
  }, {
    start: 2784,
    length: 2,
    convRule: rule14
  }, {
    start: 2786,
    length: 2,
    convRule: rule92
  }, {
    start: 2790,
    length: 10,
    convRule: rule8
  }, {
    start: 2800,
    length: 1,
    convRule: rule2
  }, {
    start: 2801,
    length: 1,
    convRule: rule3
  }, {
    start: 2809,
    length: 1,
    convRule: rule14
  }, {
    start: 2810,
    length: 6,
    convRule: rule92
  }, {
    start: 2817,
    length: 1,
    convRule: rule92
  }, {
    start: 2818,
    length: 2,
    convRule: rule124
  }, {
    start: 2821,
    length: 8,
    convRule: rule14
  }, {
    start: 2831,
    length: 2,
    convRule: rule14
  }, {
    start: 2835,
    length: 22,
    convRule: rule14
  }, {
    start: 2858,
    length: 7,
    convRule: rule14
  }, {
    start: 2866,
    length: 2,
    convRule: rule14
  }, {
    start: 2869,
    length: 5,
    convRule: rule14
  }, {
    start: 2876,
    length: 1,
    convRule: rule92
  }, {
    start: 2877,
    length: 1,
    convRule: rule14
  }, {
    start: 2878,
    length: 1,
    convRule: rule124
  }, {
    start: 2879,
    length: 1,
    convRule: rule92
  }, {
    start: 2880,
    length: 1,
    convRule: rule124
  }, {
    start: 2881,
    length: 4,
    convRule: rule92
  }, {
    start: 2887,
    length: 2,
    convRule: rule124
  }, {
    start: 2891,
    length: 2,
    convRule: rule124
  }, {
    start: 2893,
    length: 1,
    convRule: rule92
  }, {
    start: 2901,
    length: 2,
    convRule: rule92
  }, {
    start: 2903,
    length: 1,
    convRule: rule124
  }, {
    start: 2908,
    length: 2,
    convRule: rule14
  }, {
    start: 2911,
    length: 3,
    convRule: rule14
  }, {
    start: 2914,
    length: 2,
    convRule: rule92
  }, {
    start: 2918,
    length: 10,
    convRule: rule8
  }, {
    start: 2928,
    length: 1,
    convRule: rule13
  }, {
    start: 2929,
    length: 1,
    convRule: rule14
  }, {
    start: 2930,
    length: 6,
    convRule: rule17
  }, {
    start: 2946,
    length: 1,
    convRule: rule92
  }, {
    start: 2947,
    length: 1,
    convRule: rule14
  }, {
    start: 2949,
    length: 6,
    convRule: rule14
  }, {
    start: 2958,
    length: 3,
    convRule: rule14
  }, {
    start: 2962,
    length: 4,
    convRule: rule14
  }, {
    start: 2969,
    length: 2,
    convRule: rule14
  }, {
    start: 2972,
    length: 1,
    convRule: rule14
  }, {
    start: 2974,
    length: 2,
    convRule: rule14
  }, {
    start: 2979,
    length: 2,
    convRule: rule14
  }, {
    start: 2984,
    length: 3,
    convRule: rule14
  }, {
    start: 2990,
    length: 12,
    convRule: rule14
  }, {
    start: 3006,
    length: 2,
    convRule: rule124
  }, {
    start: 3008,
    length: 1,
    convRule: rule92
  }, {
    start: 3009,
    length: 2,
    convRule: rule124
  }, {
    start: 3014,
    length: 3,
    convRule: rule124
  }, {
    start: 3018,
    length: 3,
    convRule: rule124
  }, {
    start: 3021,
    length: 1,
    convRule: rule92
  }, {
    start: 3024,
    length: 1,
    convRule: rule14
  }, {
    start: 3031,
    length: 1,
    convRule: rule124
  }, {
    start: 3046,
    length: 10,
    convRule: rule8
  }, {
    start: 3056,
    length: 3,
    convRule: rule17
  }, {
    start: 3059,
    length: 6,
    convRule: rule13
  }, {
    start: 3065,
    length: 1,
    convRule: rule3
  }, {
    start: 3066,
    length: 1,
    convRule: rule13
  }, {
    start: 3072,
    length: 1,
    convRule: rule92
  }, {
    start: 3073,
    length: 3,
    convRule: rule124
  }, {
    start: 3076,
    length: 1,
    convRule: rule92
  }, {
    start: 3077,
    length: 8,
    convRule: rule14
  }, {
    start: 3086,
    length: 3,
    convRule: rule14
  }, {
    start: 3090,
    length: 23,
    convRule: rule14
  }, {
    start: 3114,
    length: 16,
    convRule: rule14
  }, {
    start: 3133,
    length: 1,
    convRule: rule14
  }, {
    start: 3134,
    length: 3,
    convRule: rule92
  }, {
    start: 3137,
    length: 4,
    convRule: rule124
  }, {
    start: 3142,
    length: 3,
    convRule: rule92
  }, {
    start: 3146,
    length: 4,
    convRule: rule92
  }, {
    start: 3157,
    length: 2,
    convRule: rule92
  }, {
    start: 3160,
    length: 3,
    convRule: rule14
  }, {
    start: 3168,
    length: 2,
    convRule: rule14
  }, {
    start: 3170,
    length: 2,
    convRule: rule92
  }, {
    start: 3174,
    length: 10,
    convRule: rule8
  }, {
    start: 3191,
    length: 1,
    convRule: rule2
  }, {
    start: 3192,
    length: 7,
    convRule: rule17
  }, {
    start: 3199,
    length: 1,
    convRule: rule13
  }, {
    start: 3200,
    length: 1,
    convRule: rule14
  }, {
    start: 3201,
    length: 1,
    convRule: rule92
  }, {
    start: 3202,
    length: 2,
    convRule: rule124
  }, {
    start: 3204,
    length: 1,
    convRule: rule2
  }, {
    start: 3205,
    length: 8,
    convRule: rule14
  }, {
    start: 3214,
    length: 3,
    convRule: rule14
  }, {
    start: 3218,
    length: 23,
    convRule: rule14
  }, {
    start: 3242,
    length: 10,
    convRule: rule14
  }, {
    start: 3253,
    length: 5,
    convRule: rule14
  }, {
    start: 3260,
    length: 1,
    convRule: rule92
  }, {
    start: 3261,
    length: 1,
    convRule: rule14
  }, {
    start: 3262,
    length: 1,
    convRule: rule124
  }, {
    start: 3263,
    length: 1,
    convRule: rule92
  }, {
    start: 3264,
    length: 5,
    convRule: rule124
  }, {
    start: 3270,
    length: 1,
    convRule: rule92
  }, {
    start: 3271,
    length: 2,
    convRule: rule124
  }, {
    start: 3274,
    length: 2,
    convRule: rule124
  }, {
    start: 3276,
    length: 2,
    convRule: rule92
  }, {
    start: 3285,
    length: 2,
    convRule: rule124
  }, {
    start: 3294,
    length: 1,
    convRule: rule14
  }, {
    start: 3296,
    length: 2,
    convRule: rule14
  }, {
    start: 3298,
    length: 2,
    convRule: rule92
  }, {
    start: 3302,
    length: 10,
    convRule: rule8
  }, {
    start: 3313,
    length: 2,
    convRule: rule14
  }, {
    start: 3328,
    length: 2,
    convRule: rule92
  }, {
    start: 3330,
    length: 2,
    convRule: rule124
  }, {
    start: 3332,
    length: 9,
    convRule: rule14
  }, {
    start: 3342,
    length: 3,
    convRule: rule14
  }, {
    start: 3346,
    length: 41,
    convRule: rule14
  }, {
    start: 3387,
    length: 2,
    convRule: rule92
  }, {
    start: 3389,
    length: 1,
    convRule: rule14
  }, {
    start: 3390,
    length: 3,
    convRule: rule124
  }, {
    start: 3393,
    length: 4,
    convRule: rule92
  }, {
    start: 3398,
    length: 3,
    convRule: rule124
  }, {
    start: 3402,
    length: 3,
    convRule: rule124
  }, {
    start: 3405,
    length: 1,
    convRule: rule92
  }, {
    start: 3406,
    length: 1,
    convRule: rule14
  }, {
    start: 3407,
    length: 1,
    convRule: rule13
  }, {
    start: 3412,
    length: 3,
    convRule: rule14
  }, {
    start: 3415,
    length: 1,
    convRule: rule124
  }, {
    start: 3416,
    length: 7,
    convRule: rule17
  }, {
    start: 3423,
    length: 3,
    convRule: rule14
  }, {
    start: 3426,
    length: 2,
    convRule: rule92
  }, {
    start: 3430,
    length: 10,
    convRule: rule8
  }, {
    start: 3440,
    length: 9,
    convRule: rule17
  }, {
    start: 3449,
    length: 1,
    convRule: rule13
  }, {
    start: 3450,
    length: 6,
    convRule: rule14
  }, {
    start: 3457,
    length: 1,
    convRule: rule92
  }, {
    start: 3458,
    length: 2,
    convRule: rule124
  }, {
    start: 3461,
    length: 18,
    convRule: rule14
  }, {
    start: 3482,
    length: 24,
    convRule: rule14
  }, {
    start: 3507,
    length: 9,
    convRule: rule14
  }, {
    start: 3517,
    length: 1,
    convRule: rule14
  }, {
    start: 3520,
    length: 7,
    convRule: rule14
  }, {
    start: 3530,
    length: 1,
    convRule: rule92
  }, {
    start: 3535,
    length: 3,
    convRule: rule124
  }, {
    start: 3538,
    length: 3,
    convRule: rule92
  }, {
    start: 3542,
    length: 1,
    convRule: rule92
  }, {
    start: 3544,
    length: 8,
    convRule: rule124
  }, {
    start: 3558,
    length: 10,
    convRule: rule8
  }, {
    start: 3570,
    length: 2,
    convRule: rule124
  }, {
    start: 3572,
    length: 1,
    convRule: rule2
  }, {
    start: 3585,
    length: 48,
    convRule: rule14
  }, {
    start: 3633,
    length: 1,
    convRule: rule92
  }, {
    start: 3634,
    length: 2,
    convRule: rule14
  }, {
    start: 3636,
    length: 7,
    convRule: rule92
  }, {
    start: 3647,
    length: 1,
    convRule: rule3
  }, {
    start: 3648,
    length: 6,
    convRule: rule14
  }, {
    start: 3654,
    length: 1,
    convRule: rule91
  }, {
    start: 3655,
    length: 8,
    convRule: rule92
  }, {
    start: 3663,
    length: 1,
    convRule: rule2
  }, {
    start: 3664,
    length: 10,
    convRule: rule8
  }, {
    start: 3674,
    length: 2,
    convRule: rule2
  }, {
    start: 3713,
    length: 2,
    convRule: rule14
  }, {
    start: 3716,
    length: 1,
    convRule: rule14
  }, {
    start: 3718,
    length: 5,
    convRule: rule14
  }, {
    start: 3724,
    length: 24,
    convRule: rule14
  }, {
    start: 3749,
    length: 1,
    convRule: rule14
  }, {
    start: 3751,
    length: 10,
    convRule: rule14
  }, {
    start: 3761,
    length: 1,
    convRule: rule92
  }, {
    start: 3762,
    length: 2,
    convRule: rule14
  }, {
    start: 3764,
    length: 9,
    convRule: rule92
  }, {
    start: 3773,
    length: 1,
    convRule: rule14
  }, {
    start: 3776,
    length: 5,
    convRule: rule14
  }, {
    start: 3782,
    length: 1,
    convRule: rule91
  }, {
    start: 3784,
    length: 6,
    convRule: rule92
  }, {
    start: 3792,
    length: 10,
    convRule: rule8
  }, {
    start: 3804,
    length: 4,
    convRule: rule14
  }, {
    start: 3840,
    length: 1,
    convRule: rule14
  }, {
    start: 3841,
    length: 3,
    convRule: rule13
  }, {
    start: 3844,
    length: 15,
    convRule: rule2
  }, {
    start: 3859,
    length: 1,
    convRule: rule13
  }, {
    start: 3860,
    length: 1,
    convRule: rule2
  }, {
    start: 3861,
    length: 3,
    convRule: rule13
  }, {
    start: 3864,
    length: 2,
    convRule: rule92
  }, {
    start: 3866,
    length: 6,
    convRule: rule13
  }, {
    start: 3872,
    length: 10,
    convRule: rule8
  }, {
    start: 3882,
    length: 10,
    convRule: rule17
  }, {
    start: 3892,
    length: 1,
    convRule: rule13
  }, {
    start: 3893,
    length: 1,
    convRule: rule92
  }, {
    start: 3894,
    length: 1,
    convRule: rule13
  }, {
    start: 3895,
    length: 1,
    convRule: rule92
  }, {
    start: 3896,
    length: 1,
    convRule: rule13
  }, {
    start: 3897,
    length: 1,
    convRule: rule92
  }, {
    start: 3898,
    length: 1,
    convRule: rule4
  }, {
    start: 3899,
    length: 1,
    convRule: rule5
  }, {
    start: 3900,
    length: 1,
    convRule: rule4
  }, {
    start: 3901,
    length: 1,
    convRule: rule5
  }, {
    start: 3902,
    length: 2,
    convRule: rule124
  }, {
    start: 3904,
    length: 8,
    convRule: rule14
  }, {
    start: 3913,
    length: 36,
    convRule: rule14
  }, {
    start: 3953,
    length: 14,
    convRule: rule92
  }, {
    start: 3967,
    length: 1,
    convRule: rule124
  }, {
    start: 3968,
    length: 5,
    convRule: rule92
  }, {
    start: 3973,
    length: 1,
    convRule: rule2
  }, {
    start: 3974,
    length: 2,
    convRule: rule92
  }, {
    start: 3976,
    length: 5,
    convRule: rule14
  }, {
    start: 3981,
    length: 11,
    convRule: rule92
  }, {
    start: 3993,
    length: 36,
    convRule: rule92
  }, {
    start: 4030,
    length: 8,
    convRule: rule13
  }, {
    start: 4038,
    length: 1,
    convRule: rule92
  }, {
    start: 4039,
    length: 6,
    convRule: rule13
  }, {
    start: 4046,
    length: 2,
    convRule: rule13
  }, {
    start: 4048,
    length: 5,
    convRule: rule2
  }, {
    start: 4053,
    length: 4,
    convRule: rule13
  }, {
    start: 4057,
    length: 2,
    convRule: rule2
  }, {
    start: 4096,
    length: 43,
    convRule: rule14
  }, {
    start: 4139,
    length: 2,
    convRule: rule124
  }, {
    start: 4141,
    length: 4,
    convRule: rule92
  }, {
    start: 4145,
    length: 1,
    convRule: rule124
  }, {
    start: 4146,
    length: 6,
    convRule: rule92
  }, {
    start: 4152,
    length: 1,
    convRule: rule124
  }, {
    start: 4153,
    length: 2,
    convRule: rule92
  }, {
    start: 4155,
    length: 2,
    convRule: rule124
  }, {
    start: 4157,
    length: 2,
    convRule: rule92
  }, {
    start: 4159,
    length: 1,
    convRule: rule14
  }, {
    start: 4160,
    length: 10,
    convRule: rule8
  }, {
    start: 4170,
    length: 6,
    convRule: rule2
  }, {
    start: 4176,
    length: 6,
    convRule: rule14
  }, {
    start: 4182,
    length: 2,
    convRule: rule124
  }, {
    start: 4184,
    length: 2,
    convRule: rule92
  }, {
    start: 4186,
    length: 4,
    convRule: rule14
  }, {
    start: 4190,
    length: 3,
    convRule: rule92
  }, {
    start: 4193,
    length: 1,
    convRule: rule14
  }, {
    start: 4194,
    length: 3,
    convRule: rule124
  }, {
    start: 4197,
    length: 2,
    convRule: rule14
  }, {
    start: 4199,
    length: 7,
    convRule: rule124
  }, {
    start: 4206,
    length: 3,
    convRule: rule14
  }, {
    start: 4209,
    length: 4,
    convRule: rule92
  }, {
    start: 4213,
    length: 13,
    convRule: rule14
  }, {
    start: 4226,
    length: 1,
    convRule: rule92
  }, {
    start: 4227,
    length: 2,
    convRule: rule124
  }, {
    start: 4229,
    length: 2,
    convRule: rule92
  }, {
    start: 4231,
    length: 6,
    convRule: rule124
  }, {
    start: 4237,
    length: 1,
    convRule: rule92
  }, {
    start: 4238,
    length: 1,
    convRule: rule14
  }, {
    start: 4239,
    length: 1,
    convRule: rule124
  }, {
    start: 4240,
    length: 10,
    convRule: rule8
  }, {
    start: 4250,
    length: 3,
    convRule: rule124
  }, {
    start: 4253,
    length: 1,
    convRule: rule92
  }, {
    start: 4254,
    length: 2,
    convRule: rule13
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4347,
    length: 1,
    convRule: rule2
  }, {
    start: 4348,
    length: 1,
    convRule: rule91
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 4352,
    length: 329,
    convRule: rule14
  }, {
    start: 4682,
    length: 4,
    convRule: rule14
  }, {
    start: 4688,
    length: 7,
    convRule: rule14
  }, {
    start: 4696,
    length: 1,
    convRule: rule14
  }, {
    start: 4698,
    length: 4,
    convRule: rule14
  }, {
    start: 4704,
    length: 41,
    convRule: rule14
  }, {
    start: 4746,
    length: 4,
    convRule: rule14
  }, {
    start: 4752,
    length: 33,
    convRule: rule14
  }, {
    start: 4786,
    length: 4,
    convRule: rule14
  }, {
    start: 4792,
    length: 7,
    convRule: rule14
  }, {
    start: 4800,
    length: 1,
    convRule: rule14
  }, {
    start: 4802,
    length: 4,
    convRule: rule14
  }, {
    start: 4808,
    length: 15,
    convRule: rule14
  }, {
    start: 4824,
    length: 57,
    convRule: rule14
  }, {
    start: 4882,
    length: 4,
    convRule: rule14
  }, {
    start: 4888,
    length: 67,
    convRule: rule14
  }, {
    start: 4957,
    length: 3,
    convRule: rule92
  }, {
    start: 4960,
    length: 9,
    convRule: rule2
  }, {
    start: 4969,
    length: 20,
    convRule: rule17
  }, {
    start: 4992,
    length: 16,
    convRule: rule14
  }, {
    start: 5008,
    length: 10,
    convRule: rule13
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 5120,
    length: 1,
    convRule: rule7
  }, {
    start: 5121,
    length: 620,
    convRule: rule14
  }, {
    start: 5741,
    length: 1,
    convRule: rule13
  }, {
    start: 5742,
    length: 1,
    convRule: rule2
  }, {
    start: 5743,
    length: 17,
    convRule: rule14
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 5761,
    length: 26,
    convRule: rule14
  }, {
    start: 5787,
    length: 1,
    convRule: rule4
  }, {
    start: 5788,
    length: 1,
    convRule: rule5
  }, {
    start: 5792,
    length: 75,
    convRule: rule14
  }, {
    start: 5867,
    length: 3,
    convRule: rule2
  }, {
    start: 5870,
    length: 3,
    convRule: rule128
  }, {
    start: 5873,
    length: 8,
    convRule: rule14
  }, {
    start: 5888,
    length: 13,
    convRule: rule14
  }, {
    start: 5902,
    length: 4,
    convRule: rule14
  }, {
    start: 5906,
    length: 3,
    convRule: rule92
  }, {
    start: 5920,
    length: 18,
    convRule: rule14
  }, {
    start: 5938,
    length: 3,
    convRule: rule92
  }, {
    start: 5941,
    length: 2,
    convRule: rule2
  }, {
    start: 5952,
    length: 18,
    convRule: rule14
  }, {
    start: 5970,
    length: 2,
    convRule: rule92
  }, {
    start: 5984,
    length: 13,
    convRule: rule14
  }, {
    start: 5998,
    length: 3,
    convRule: rule14
  }, {
    start: 6002,
    length: 2,
    convRule: rule92
  }, {
    start: 6016,
    length: 52,
    convRule: rule14
  }, {
    start: 6068,
    length: 2,
    convRule: rule92
  }, {
    start: 6070,
    length: 1,
    convRule: rule124
  }, {
    start: 6071,
    length: 7,
    convRule: rule92
  }, {
    start: 6078,
    length: 8,
    convRule: rule124
  }, {
    start: 6086,
    length: 1,
    convRule: rule92
  }, {
    start: 6087,
    length: 2,
    convRule: rule124
  }, {
    start: 6089,
    length: 11,
    convRule: rule92
  }, {
    start: 6100,
    length: 3,
    convRule: rule2
  }, {
    start: 6103,
    length: 1,
    convRule: rule91
  }, {
    start: 6104,
    length: 3,
    convRule: rule2
  }, {
    start: 6107,
    length: 1,
    convRule: rule3
  }, {
    start: 6108,
    length: 1,
    convRule: rule14
  }, {
    start: 6109,
    length: 1,
    convRule: rule92
  }, {
    start: 6112,
    length: 10,
    convRule: rule8
  }, {
    start: 6128,
    length: 10,
    convRule: rule17
  }, {
    start: 6144,
    length: 6,
    convRule: rule2
  }, {
    start: 6150,
    length: 1,
    convRule: rule7
  }, {
    start: 6151,
    length: 4,
    convRule: rule2
  }, {
    start: 6155,
    length: 3,
    convRule: rule92
  }, {
    start: 6158,
    length: 1,
    convRule: rule16
  }, {
    start: 6160,
    length: 10,
    convRule: rule8
  }, {
    start: 6176,
    length: 35,
    convRule: rule14
  }, {
    start: 6211,
    length: 1,
    convRule: rule91
  }, {
    start: 6212,
    length: 53,
    convRule: rule14
  }, {
    start: 6272,
    length: 5,
    convRule: rule14
  }, {
    start: 6277,
    length: 2,
    convRule: rule92
  }, {
    start: 6279,
    length: 34,
    convRule: rule14
  }, {
    start: 6313,
    length: 1,
    convRule: rule92
  }, {
    start: 6314,
    length: 1,
    convRule: rule14
  }, {
    start: 6320,
    length: 70,
    convRule: rule14
  }, {
    start: 6400,
    length: 31,
    convRule: rule14
  }, {
    start: 6432,
    length: 3,
    convRule: rule92
  }, {
    start: 6435,
    length: 4,
    convRule: rule124
  }, {
    start: 6439,
    length: 2,
    convRule: rule92
  }, {
    start: 6441,
    length: 3,
    convRule: rule124
  }, {
    start: 6448,
    length: 2,
    convRule: rule124
  }, {
    start: 6450,
    length: 1,
    convRule: rule92
  }, {
    start: 6451,
    length: 6,
    convRule: rule124
  }, {
    start: 6457,
    length: 3,
    convRule: rule92
  }, {
    start: 6464,
    length: 1,
    convRule: rule13
  }, {
    start: 6468,
    length: 2,
    convRule: rule2
  }, {
    start: 6470,
    length: 10,
    convRule: rule8
  }, {
    start: 6480,
    length: 30,
    convRule: rule14
  }, {
    start: 6512,
    length: 5,
    convRule: rule14
  }, {
    start: 6528,
    length: 44,
    convRule: rule14
  }, {
    start: 6576,
    length: 26,
    convRule: rule14
  }, {
    start: 6608,
    length: 10,
    convRule: rule8
  }, {
    start: 6618,
    length: 1,
    convRule: rule17
  }, {
    start: 6622,
    length: 34,
    convRule: rule13
  }, {
    start: 6656,
    length: 23,
    convRule: rule14
  }, {
    start: 6679,
    length: 2,
    convRule: rule92
  }, {
    start: 6681,
    length: 2,
    convRule: rule124
  }, {
    start: 6683,
    length: 1,
    convRule: rule92
  }, {
    start: 6686,
    length: 2,
    convRule: rule2
  }, {
    start: 6688,
    length: 53,
    convRule: rule14
  }, {
    start: 6741,
    length: 1,
    convRule: rule124
  }, {
    start: 6742,
    length: 1,
    convRule: rule92
  }, {
    start: 6743,
    length: 1,
    convRule: rule124
  }, {
    start: 6744,
    length: 7,
    convRule: rule92
  }, {
    start: 6752,
    length: 1,
    convRule: rule92
  }, {
    start: 6753,
    length: 1,
    convRule: rule124
  }, {
    start: 6754,
    length: 1,
    convRule: rule92
  }, {
    start: 6755,
    length: 2,
    convRule: rule124
  }, {
    start: 6757,
    length: 8,
    convRule: rule92
  }, {
    start: 6765,
    length: 6,
    convRule: rule124
  }, {
    start: 6771,
    length: 10,
    convRule: rule92
  }, {
    start: 6783,
    length: 1,
    convRule: rule92
  }, {
    start: 6784,
    length: 10,
    convRule: rule8
  }, {
    start: 6800,
    length: 10,
    convRule: rule8
  }, {
    start: 6816,
    length: 7,
    convRule: rule2
  }, {
    start: 6823,
    length: 1,
    convRule: rule91
  }, {
    start: 6824,
    length: 6,
    convRule: rule2
  }, {
    start: 6832,
    length: 14,
    convRule: rule92
  }, {
    start: 6846,
    length: 1,
    convRule: rule119
  }, {
    start: 6847,
    length: 2,
    convRule: rule92
  }, {
    start: 6912,
    length: 4,
    convRule: rule92
  }, {
    start: 6916,
    length: 1,
    convRule: rule124
  }, {
    start: 6917,
    length: 47,
    convRule: rule14
  }, {
    start: 6964,
    length: 1,
    convRule: rule92
  }, {
    start: 6965,
    length: 1,
    convRule: rule124
  }, {
    start: 6966,
    length: 5,
    convRule: rule92
  }, {
    start: 6971,
    length: 1,
    convRule: rule124
  }, {
    start: 6972,
    length: 1,
    convRule: rule92
  }, {
    start: 6973,
    length: 5,
    convRule: rule124
  }, {
    start: 6978,
    length: 1,
    convRule: rule92
  }, {
    start: 6979,
    length: 2,
    convRule: rule124
  }, {
    start: 6981,
    length: 7,
    convRule: rule14
  }, {
    start: 6992,
    length: 10,
    convRule: rule8
  }, {
    start: 7002,
    length: 7,
    convRule: rule2
  }, {
    start: 7009,
    length: 10,
    convRule: rule13
  }, {
    start: 7019,
    length: 9,
    convRule: rule92
  }, {
    start: 7028,
    length: 9,
    convRule: rule13
  }, {
    start: 7040,
    length: 2,
    convRule: rule92
  }, {
    start: 7042,
    length: 1,
    convRule: rule124
  }, {
    start: 7043,
    length: 30,
    convRule: rule14
  }, {
    start: 7073,
    length: 1,
    convRule: rule124
  }, {
    start: 7074,
    length: 4,
    convRule: rule92
  }, {
    start: 7078,
    length: 2,
    convRule: rule124
  }, {
    start: 7080,
    length: 2,
    convRule: rule92
  }, {
    start: 7082,
    length: 1,
    convRule: rule124
  }, {
    start: 7083,
    length: 3,
    convRule: rule92
  }, {
    start: 7086,
    length: 2,
    convRule: rule14
  }, {
    start: 7088,
    length: 10,
    convRule: rule8
  }, {
    start: 7098,
    length: 44,
    convRule: rule14
  }, {
    start: 7142,
    length: 1,
    convRule: rule92
  }, {
    start: 7143,
    length: 1,
    convRule: rule124
  }, {
    start: 7144,
    length: 2,
    convRule: rule92
  }, {
    start: 7146,
    length: 3,
    convRule: rule124
  }, {
    start: 7149,
    length: 1,
    convRule: rule92
  }, {
    start: 7150,
    length: 1,
    convRule: rule124
  }, {
    start: 7151,
    length: 3,
    convRule: rule92
  }, {
    start: 7154,
    length: 2,
    convRule: rule124
  }, {
    start: 7164,
    length: 4,
    convRule: rule2
  }, {
    start: 7168,
    length: 36,
    convRule: rule14
  }, {
    start: 7204,
    length: 8,
    convRule: rule124
  }, {
    start: 7212,
    length: 8,
    convRule: rule92
  }, {
    start: 7220,
    length: 2,
    convRule: rule124
  }, {
    start: 7222,
    length: 2,
    convRule: rule92
  }, {
    start: 7227,
    length: 5,
    convRule: rule2
  }, {
    start: 7232,
    length: 10,
    convRule: rule8
  }, {
    start: 7245,
    length: 3,
    convRule: rule14
  }, {
    start: 7248,
    length: 10,
    convRule: rule8
  }, {
    start: 7258,
    length: 30,
    convRule: rule14
  }, {
    start: 7288,
    length: 6,
    convRule: rule91
  }, {
    start: 7294,
    length: 2,
    convRule: rule2
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7360,
    length: 8,
    convRule: rule2
  }, {
    start: 7376,
    length: 3,
    convRule: rule92
  }, {
    start: 7379,
    length: 1,
    convRule: rule2
  }, {
    start: 7380,
    length: 13,
    convRule: rule92
  }, {
    start: 7393,
    length: 1,
    convRule: rule124
  }, {
    start: 7394,
    length: 7,
    convRule: rule92
  }, {
    start: 7401,
    length: 4,
    convRule: rule14
  }, {
    start: 7405,
    length: 1,
    convRule: rule92
  }, {
    start: 7406,
    length: 6,
    convRule: rule14
  }, {
    start: 7412,
    length: 1,
    convRule: rule92
  }, {
    start: 7413,
    length: 2,
    convRule: rule14
  }, {
    start: 7415,
    length: 1,
    convRule: rule124
  }, {
    start: 7416,
    length: 2,
    convRule: rule92
  }, {
    start: 7418,
    length: 1,
    convRule: rule14
  }, {
    start: 7424,
    length: 44,
    convRule: rule20
  }, {
    start: 7468,
    length: 63,
    convRule: rule91
  }, {
    start: 7531,
    length: 13,
    convRule: rule20
  }, {
    start: 7544,
    length: 1,
    convRule: rule91
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7546,
    length: 3,
    convRule: rule20
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7550,
    length: 16,
    convRule: rule20
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7567,
    length: 12,
    convRule: rule20
  }, {
    start: 7579,
    length: 37,
    convRule: rule91
  }, {
    start: 7616,
    length: 58,
    convRule: rule92
  }, {
    start: 7675,
    length: 5,
    convRule: rule92
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7830,
    length: 5,
    convRule: rule20
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7836,
    length: 2,
    convRule: rule20
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7839,
    length: 1,
    convRule: rule20
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8016,
    length: 1,
    convRule: rule20
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8018,
    length: 1,
    convRule: rule20
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8020,
    length: 1,
    convRule: rule20
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8022,
    length: 1,
    convRule: rule20
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8114,
    length: 1,
    convRule: rule20
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8116,
    length: 1,
    convRule: rule20
  }, {
    start: 8118,
    length: 2,
    convRule: rule20
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8125,
    length: 1,
    convRule: rule10
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8127,
    length: 3,
    convRule: rule10
  }, {
    start: 8130,
    length: 1,
    convRule: rule20
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8132,
    length: 1,
    convRule: rule20
  }, {
    start: 8134,
    length: 2,
    convRule: rule20
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8141,
    length: 3,
    convRule: rule10
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8146,
    length: 2,
    convRule: rule20
  }, {
    start: 8150,
    length: 2,
    convRule: rule20
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8157,
    length: 3,
    convRule: rule10
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8162,
    length: 3,
    convRule: rule20
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8166,
    length: 2,
    convRule: rule20
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8173,
    length: 3,
    convRule: rule10
  }, {
    start: 8178,
    length: 1,
    convRule: rule20
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8180,
    length: 1,
    convRule: rule20
  }, {
    start: 8182,
    length: 2,
    convRule: rule20
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8189,
    length: 2,
    convRule: rule10
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8203,
    length: 5,
    convRule: rule16
  }, {
    start: 8208,
    length: 6,
    convRule: rule7
  }, {
    start: 8214,
    length: 2,
    convRule: rule2
  }, {
    start: 8216,
    length: 1,
    convRule: rule15
  }, {
    start: 8217,
    length: 1,
    convRule: rule19
  }, {
    start: 8218,
    length: 1,
    convRule: rule4
  }, {
    start: 8219,
    length: 2,
    convRule: rule15
  }, {
    start: 8221,
    length: 1,
    convRule: rule19
  }, {
    start: 8222,
    length: 1,
    convRule: rule4
  }, {
    start: 8223,
    length: 1,
    convRule: rule15
  }, {
    start: 8224,
    length: 8,
    convRule: rule2
  }, {
    start: 8232,
    length: 1,
    convRule: rule161
  }, {
    start: 8233,
    length: 1,
    convRule: rule162
  }, {
    start: 8234,
    length: 5,
    convRule: rule16
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8240,
    length: 9,
    convRule: rule2
  }, {
    start: 8249,
    length: 1,
    convRule: rule15
  }, {
    start: 8250,
    length: 1,
    convRule: rule19
  }, {
    start: 8251,
    length: 4,
    convRule: rule2
  }, {
    start: 8255,
    length: 2,
    convRule: rule11
  }, {
    start: 8257,
    length: 3,
    convRule: rule2
  }, {
    start: 8260,
    length: 1,
    convRule: rule6
  }, {
    start: 8261,
    length: 1,
    convRule: rule4
  }, {
    start: 8262,
    length: 1,
    convRule: rule5
  }, {
    start: 8263,
    length: 11,
    convRule: rule2
  }, {
    start: 8274,
    length: 1,
    convRule: rule6
  }, {
    start: 8275,
    length: 1,
    convRule: rule2
  }, {
    start: 8276,
    length: 1,
    convRule: rule11
  }, {
    start: 8277,
    length: 10,
    convRule: rule2
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 8288,
    length: 5,
    convRule: rule16
  }, {
    start: 8294,
    length: 10,
    convRule: rule16
  }, {
    start: 8304,
    length: 1,
    convRule: rule17
  }, {
    start: 8305,
    length: 1,
    convRule: rule91
  }, {
    start: 8308,
    length: 6,
    convRule: rule17
  }, {
    start: 8314,
    length: 3,
    convRule: rule6
  }, {
    start: 8317,
    length: 1,
    convRule: rule4
  }, {
    start: 8318,
    length: 1,
    convRule: rule5
  }, {
    start: 8319,
    length: 1,
    convRule: rule91
  }, {
    start: 8320,
    length: 10,
    convRule: rule17
  }, {
    start: 8330,
    length: 3,
    convRule: rule6
  }, {
    start: 8333,
    length: 1,
    convRule: rule4
  }, {
    start: 8334,
    length: 1,
    convRule: rule5
  }, {
    start: 8336,
    length: 13,
    convRule: rule91
  }, {
    start: 8352,
    length: 32,
    convRule: rule3
  }, {
    start: 8400,
    length: 13,
    convRule: rule92
  }, {
    start: 8413,
    length: 4,
    convRule: rule119
  }, {
    start: 8417,
    length: 1,
    convRule: rule92
  }, {
    start: 8418,
    length: 3,
    convRule: rule119
  }, {
    start: 8421,
    length: 12,
    convRule: rule92
  }, {
    start: 8448,
    length: 2,
    convRule: rule13
  }, {
    start: 8450,
    length: 1,
    convRule: rule107
  }, {
    start: 8451,
    length: 4,
    convRule: rule13
  }, {
    start: 8455,
    length: 1,
    convRule: rule107
  }, {
    start: 8456,
    length: 2,
    convRule: rule13
  }, {
    start: 8458,
    length: 1,
    convRule: rule20
  }, {
    start: 8459,
    length: 3,
    convRule: rule107
  }, {
    start: 8462,
    length: 2,
    convRule: rule20
  }, {
    start: 8464,
    length: 3,
    convRule: rule107
  }, {
    start: 8467,
    length: 1,
    convRule: rule20
  }, {
    start: 8468,
    length: 1,
    convRule: rule13
  }, {
    start: 8469,
    length: 1,
    convRule: rule107
  }, {
    start: 8470,
    length: 2,
    convRule: rule13
  }, {
    start: 8472,
    length: 1,
    convRule: rule6
  }, {
    start: 8473,
    length: 5,
    convRule: rule107
  }, {
    start: 8478,
    length: 6,
    convRule: rule13
  }, {
    start: 8484,
    length: 1,
    convRule: rule107
  }, {
    start: 8485,
    length: 1,
    convRule: rule13
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8487,
    length: 1,
    convRule: rule13
  }, {
    start: 8488,
    length: 1,
    convRule: rule107
  }, {
    start: 8489,
    length: 1,
    convRule: rule13
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8492,
    length: 2,
    convRule: rule107
  }, {
    start: 8494,
    length: 1,
    convRule: rule13
  }, {
    start: 8495,
    length: 1,
    convRule: rule20
  }, {
    start: 8496,
    length: 2,
    convRule: rule107
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8499,
    length: 1,
    convRule: rule107
  }, {
    start: 8500,
    length: 1,
    convRule: rule20
  }, {
    start: 8501,
    length: 4,
    convRule: rule14
  }, {
    start: 8505,
    length: 1,
    convRule: rule20
  }, {
    start: 8506,
    length: 2,
    convRule: rule13
  }, {
    start: 8508,
    length: 2,
    convRule: rule20
  }, {
    start: 8510,
    length: 2,
    convRule: rule107
  }, {
    start: 8512,
    length: 5,
    convRule: rule6
  }, {
    start: 8517,
    length: 1,
    convRule: rule107
  }, {
    start: 8518,
    length: 4,
    convRule: rule20
  }, {
    start: 8522,
    length: 1,
    convRule: rule13
  }, {
    start: 8523,
    length: 1,
    convRule: rule6
  }, {
    start: 8524,
    length: 2,
    convRule: rule13
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8527,
    length: 1,
    convRule: rule13
  }, {
    start: 8528,
    length: 16,
    convRule: rule17
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8576,
    length: 3,
    convRule: rule128
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 8581,
    length: 4,
    convRule: rule128
  }, {
    start: 8585,
    length: 1,
    convRule: rule17
  }, {
    start: 8586,
    length: 2,
    convRule: rule13
  }, {
    start: 8592,
    length: 5,
    convRule: rule6
  }, {
    start: 8597,
    length: 5,
    convRule: rule13
  }, {
    start: 8602,
    length: 2,
    convRule: rule6
  }, {
    start: 8604,
    length: 4,
    convRule: rule13
  }, {
    start: 8608,
    length: 1,
    convRule: rule6
  }, {
    start: 8609,
    length: 2,
    convRule: rule13
  }, {
    start: 8611,
    length: 1,
    convRule: rule6
  }, {
    start: 8612,
    length: 2,
    convRule: rule13
  }, {
    start: 8614,
    length: 1,
    convRule: rule6
  }, {
    start: 8615,
    length: 7,
    convRule: rule13
  }, {
    start: 8622,
    length: 1,
    convRule: rule6
  }, {
    start: 8623,
    length: 31,
    convRule: rule13
  }, {
    start: 8654,
    length: 2,
    convRule: rule6
  }, {
    start: 8656,
    length: 2,
    convRule: rule13
  }, {
    start: 8658,
    length: 1,
    convRule: rule6
  }, {
    start: 8659,
    length: 1,
    convRule: rule13
  }, {
    start: 8660,
    length: 1,
    convRule: rule6
  }, {
    start: 8661,
    length: 31,
    convRule: rule13
  }, {
    start: 8692,
    length: 268,
    convRule: rule6
  }, {
    start: 8960,
    length: 8,
    convRule: rule13
  }, {
    start: 8968,
    length: 1,
    convRule: rule4
  }, {
    start: 8969,
    length: 1,
    convRule: rule5
  }, {
    start: 8970,
    length: 1,
    convRule: rule4
  }, {
    start: 8971,
    length: 1,
    convRule: rule5
  }, {
    start: 8972,
    length: 20,
    convRule: rule13
  }, {
    start: 8992,
    length: 2,
    convRule: rule6
  }, {
    start: 8994,
    length: 7,
    convRule: rule13
  }, {
    start: 9001,
    length: 1,
    convRule: rule4
  }, {
    start: 9002,
    length: 1,
    convRule: rule5
  }, {
    start: 9003,
    length: 81,
    convRule: rule13
  }, {
    start: 9084,
    length: 1,
    convRule: rule6
  }, {
    start: 9085,
    length: 30,
    convRule: rule13
  }, {
    start: 9115,
    length: 25,
    convRule: rule6
  }, {
    start: 9140,
    length: 40,
    convRule: rule13
  }, {
    start: 9180,
    length: 6,
    convRule: rule6
  }, {
    start: 9186,
    length: 69,
    convRule: rule13
  }, {
    start: 9280,
    length: 11,
    convRule: rule13
  }, {
    start: 9312,
    length: 60,
    convRule: rule17
  }, {
    start: 9372,
    length: 26,
    convRule: rule13
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 9450,
    length: 22,
    convRule: rule17
  }, {
    start: 9472,
    length: 183,
    convRule: rule13
  }, {
    start: 9655,
    length: 1,
    convRule: rule6
  }, {
    start: 9656,
    length: 9,
    convRule: rule13
  }, {
    start: 9665,
    length: 1,
    convRule: rule6
  }, {
    start: 9666,
    length: 54,
    convRule: rule13
  }, {
    start: 9720,
    length: 8,
    convRule: rule6
  }, {
    start: 9728,
    length: 111,
    convRule: rule13
  }, {
    start: 9839,
    length: 1,
    convRule: rule6
  }, {
    start: 9840,
    length: 248,
    convRule: rule13
  }, {
    start: 10088,
    length: 1,
    convRule: rule4
  }, {
    start: 10089,
    length: 1,
    convRule: rule5
  }, {
    start: 10090,
    length: 1,
    convRule: rule4
  }, {
    start: 10091,
    length: 1,
    convRule: rule5
  }, {
    start: 10092,
    length: 1,
    convRule: rule4
  }, {
    start: 10093,
    length: 1,
    convRule: rule5
  }, {
    start: 10094,
    length: 1,
    convRule: rule4
  }, {
    start: 10095,
    length: 1,
    convRule: rule5
  }, {
    start: 10096,
    length: 1,
    convRule: rule4
  }, {
    start: 10097,
    length: 1,
    convRule: rule5
  }, {
    start: 10098,
    length: 1,
    convRule: rule4
  }, {
    start: 10099,
    length: 1,
    convRule: rule5
  }, {
    start: 10100,
    length: 1,
    convRule: rule4
  }, {
    start: 10101,
    length: 1,
    convRule: rule5
  }, {
    start: 10102,
    length: 30,
    convRule: rule17
  }, {
    start: 10132,
    length: 44,
    convRule: rule13
  }, {
    start: 10176,
    length: 5,
    convRule: rule6
  }, {
    start: 10181,
    length: 1,
    convRule: rule4
  }, {
    start: 10182,
    length: 1,
    convRule: rule5
  }, {
    start: 10183,
    length: 31,
    convRule: rule6
  }, {
    start: 10214,
    length: 1,
    convRule: rule4
  }, {
    start: 10215,
    length: 1,
    convRule: rule5
  }, {
    start: 10216,
    length: 1,
    convRule: rule4
  }, {
    start: 10217,
    length: 1,
    convRule: rule5
  }, {
    start: 10218,
    length: 1,
    convRule: rule4
  }, {
    start: 10219,
    length: 1,
    convRule: rule5
  }, {
    start: 10220,
    length: 1,
    convRule: rule4
  }, {
    start: 10221,
    length: 1,
    convRule: rule5
  }, {
    start: 10222,
    length: 1,
    convRule: rule4
  }, {
    start: 10223,
    length: 1,
    convRule: rule5
  }, {
    start: 10224,
    length: 16,
    convRule: rule6
  }, {
    start: 10240,
    length: 256,
    convRule: rule13
  }, {
    start: 10496,
    length: 131,
    convRule: rule6
  }, {
    start: 10627,
    length: 1,
    convRule: rule4
  }, {
    start: 10628,
    length: 1,
    convRule: rule5
  }, {
    start: 10629,
    length: 1,
    convRule: rule4
  }, {
    start: 10630,
    length: 1,
    convRule: rule5
  }, {
    start: 10631,
    length: 1,
    convRule: rule4
  }, {
    start: 10632,
    length: 1,
    convRule: rule5
  }, {
    start: 10633,
    length: 1,
    convRule: rule4
  }, {
    start: 10634,
    length: 1,
    convRule: rule5
  }, {
    start: 10635,
    length: 1,
    convRule: rule4
  }, {
    start: 10636,
    length: 1,
    convRule: rule5
  }, {
    start: 10637,
    length: 1,
    convRule: rule4
  }, {
    start: 10638,
    length: 1,
    convRule: rule5
  }, {
    start: 10639,
    length: 1,
    convRule: rule4
  }, {
    start: 10640,
    length: 1,
    convRule: rule5
  }, {
    start: 10641,
    length: 1,
    convRule: rule4
  }, {
    start: 10642,
    length: 1,
    convRule: rule5
  }, {
    start: 10643,
    length: 1,
    convRule: rule4
  }, {
    start: 10644,
    length: 1,
    convRule: rule5
  }, {
    start: 10645,
    length: 1,
    convRule: rule4
  }, {
    start: 10646,
    length: 1,
    convRule: rule5
  }, {
    start: 10647,
    length: 1,
    convRule: rule4
  }, {
    start: 10648,
    length: 1,
    convRule: rule5
  }, {
    start: 10649,
    length: 63,
    convRule: rule6
  }, {
    start: 10712,
    length: 1,
    convRule: rule4
  }, {
    start: 10713,
    length: 1,
    convRule: rule5
  }, {
    start: 10714,
    length: 1,
    convRule: rule4
  }, {
    start: 10715,
    length: 1,
    convRule: rule5
  }, {
    start: 10716,
    length: 32,
    convRule: rule6
  }, {
    start: 10748,
    length: 1,
    convRule: rule4
  }, {
    start: 10749,
    length: 1,
    convRule: rule5
  }, {
    start: 10750,
    length: 258,
    convRule: rule6
  }, {
    start: 11008,
    length: 48,
    convRule: rule13
  }, {
    start: 11056,
    length: 21,
    convRule: rule6
  }, {
    start: 11077,
    length: 2,
    convRule: rule13
  }, {
    start: 11079,
    length: 6,
    convRule: rule6
  }, {
    start: 11085,
    length: 39,
    convRule: rule13
  }, {
    start: 11126,
    length: 32,
    convRule: rule13
  }, {
    start: 11159,
    length: 105,
    convRule: rule13
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11377,
    length: 1,
    convRule: rule20
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11380,
    length: 1,
    convRule: rule20
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11383,
    length: 5,
    convRule: rule20
  }, {
    start: 11388,
    length: 2,
    convRule: rule91
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11492,
    length: 1,
    convRule: rule20
  }, {
    start: 11493,
    length: 6,
    convRule: rule13
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11503,
    length: 3,
    convRule: rule92
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11513,
    length: 4,
    convRule: rule2
  }, {
    start: 11517,
    length: 1,
    convRule: rule17
  }, {
    start: 11518,
    length: 2,
    convRule: rule2
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 11568,
    length: 56,
    convRule: rule14
  }, {
    start: 11631,
    length: 1,
    convRule: rule91
  }, {
    start: 11632,
    length: 1,
    convRule: rule2
  }, {
    start: 11647,
    length: 1,
    convRule: rule92
  }, {
    start: 11648,
    length: 23,
    convRule: rule14
  }, {
    start: 11680,
    length: 7,
    convRule: rule14
  }, {
    start: 11688,
    length: 7,
    convRule: rule14
  }, {
    start: 11696,
    length: 7,
    convRule: rule14
  }, {
    start: 11704,
    length: 7,
    convRule: rule14
  }, {
    start: 11712,
    length: 7,
    convRule: rule14
  }, {
    start: 11720,
    length: 7,
    convRule: rule14
  }, {
    start: 11728,
    length: 7,
    convRule: rule14
  }, {
    start: 11736,
    length: 7,
    convRule: rule14
  }, {
    start: 11744,
    length: 32,
    convRule: rule92
  }, {
    start: 11776,
    length: 2,
    convRule: rule2
  }, {
    start: 11778,
    length: 1,
    convRule: rule15
  }, {
    start: 11779,
    length: 1,
    convRule: rule19
  }, {
    start: 11780,
    length: 1,
    convRule: rule15
  }, {
    start: 11781,
    length: 1,
    convRule: rule19
  }, {
    start: 11782,
    length: 3,
    convRule: rule2
  }, {
    start: 11785,
    length: 1,
    convRule: rule15
  }, {
    start: 11786,
    length: 1,
    convRule: rule19
  }, {
    start: 11787,
    length: 1,
    convRule: rule2
  }, {
    start: 11788,
    length: 1,
    convRule: rule15
  }, {
    start: 11789,
    length: 1,
    convRule: rule19
  }, {
    start: 11790,
    length: 9,
    convRule: rule2
  }, {
    start: 11799,
    length: 1,
    convRule: rule7
  }, {
    start: 11800,
    length: 2,
    convRule: rule2
  }, {
    start: 11802,
    length: 1,
    convRule: rule7
  }, {
    start: 11803,
    length: 1,
    convRule: rule2
  }, {
    start: 11804,
    length: 1,
    convRule: rule15
  }, {
    start: 11805,
    length: 1,
    convRule: rule19
  }, {
    start: 11806,
    length: 2,
    convRule: rule2
  }, {
    start: 11808,
    length: 1,
    convRule: rule15
  }, {
    start: 11809,
    length: 1,
    convRule: rule19
  }, {
    start: 11810,
    length: 1,
    convRule: rule4
  }, {
    start: 11811,
    length: 1,
    convRule: rule5
  }, {
    start: 11812,
    length: 1,
    convRule: rule4
  }, {
    start: 11813,
    length: 1,
    convRule: rule5
  }, {
    start: 11814,
    length: 1,
    convRule: rule4
  }, {
    start: 11815,
    length: 1,
    convRule: rule5
  }, {
    start: 11816,
    length: 1,
    convRule: rule4
  }, {
    start: 11817,
    length: 1,
    convRule: rule5
  }, {
    start: 11818,
    length: 5,
    convRule: rule2
  }, {
    start: 11823,
    length: 1,
    convRule: rule91
  }, {
    start: 11824,
    length: 10,
    convRule: rule2
  }, {
    start: 11834,
    length: 2,
    convRule: rule7
  }, {
    start: 11836,
    length: 4,
    convRule: rule2
  }, {
    start: 11840,
    length: 1,
    convRule: rule7
  }, {
    start: 11841,
    length: 1,
    convRule: rule2
  }, {
    start: 11842,
    length: 1,
    convRule: rule4
  }, {
    start: 11843,
    length: 13,
    convRule: rule2
  }, {
    start: 11856,
    length: 2,
    convRule: rule13
  }, {
    start: 11858,
    length: 1,
    convRule: rule2
  }, {
    start: 11904,
    length: 26,
    convRule: rule13
  }, {
    start: 11931,
    length: 89,
    convRule: rule13
  }, {
    start: 12032,
    length: 214,
    convRule: rule13
  }, {
    start: 12272,
    length: 12,
    convRule: rule13
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }, {
    start: 12289,
    length: 3,
    convRule: rule2
  }, {
    start: 12292,
    length: 1,
    convRule: rule13
  }, {
    start: 12293,
    length: 1,
    convRule: rule91
  }, {
    start: 12294,
    length: 1,
    convRule: rule14
  }, {
    start: 12295,
    length: 1,
    convRule: rule128
  }, {
    start: 12296,
    length: 1,
    convRule: rule4
  }, {
    start: 12297,
    length: 1,
    convRule: rule5
  }, {
    start: 12298,
    length: 1,
    convRule: rule4
  }, {
    start: 12299,
    length: 1,
    convRule: rule5
  }, {
    start: 12300,
    length: 1,
    convRule: rule4
  }, {
    start: 12301,
    length: 1,
    convRule: rule5
  }, {
    start: 12302,
    length: 1,
    convRule: rule4
  }, {
    start: 12303,
    length: 1,
    convRule: rule5
  }, {
    start: 12304,
    length: 1,
    convRule: rule4
  }, {
    start: 12305,
    length: 1,
    convRule: rule5
  }, {
    start: 12306,
    length: 2,
    convRule: rule13
  }, {
    start: 12308,
    length: 1,
    convRule: rule4
  }, {
    start: 12309,
    length: 1,
    convRule: rule5
  }, {
    start: 12310,
    length: 1,
    convRule: rule4
  }, {
    start: 12311,
    length: 1,
    convRule: rule5
  }, {
    start: 12312,
    length: 1,
    convRule: rule4
  }, {
    start: 12313,
    length: 1,
    convRule: rule5
  }, {
    start: 12314,
    length: 1,
    convRule: rule4
  }, {
    start: 12315,
    length: 1,
    convRule: rule5
  }, {
    start: 12316,
    length: 1,
    convRule: rule7
  }, {
    start: 12317,
    length: 1,
    convRule: rule4
  }, {
    start: 12318,
    length: 2,
    convRule: rule5
  }, {
    start: 12320,
    length: 1,
    convRule: rule13
  }, {
    start: 12321,
    length: 9,
    convRule: rule128
  }, {
    start: 12330,
    length: 4,
    convRule: rule92
  }, {
    start: 12334,
    length: 2,
    convRule: rule124
  }, {
    start: 12336,
    length: 1,
    convRule: rule7
  }, {
    start: 12337,
    length: 5,
    convRule: rule91
  }, {
    start: 12342,
    length: 2,
    convRule: rule13
  }, {
    start: 12344,
    length: 3,
    convRule: rule128
  }, {
    start: 12347,
    length: 1,
    convRule: rule91
  }, {
    start: 12348,
    length: 1,
    convRule: rule14
  }, {
    start: 12349,
    length: 1,
    convRule: rule2
  }, {
    start: 12350,
    length: 2,
    convRule: rule13
  }, {
    start: 12353,
    length: 86,
    convRule: rule14
  }, {
    start: 12441,
    length: 2,
    convRule: rule92
  }, {
    start: 12443,
    length: 2,
    convRule: rule10
  }, {
    start: 12445,
    length: 2,
    convRule: rule91
  }, {
    start: 12447,
    length: 1,
    convRule: rule14
  }, {
    start: 12448,
    length: 1,
    convRule: rule7
  }, {
    start: 12449,
    length: 90,
    convRule: rule14
  }, {
    start: 12539,
    length: 1,
    convRule: rule2
  }, {
    start: 12540,
    length: 3,
    convRule: rule91
  }, {
    start: 12543,
    length: 1,
    convRule: rule14
  }, {
    start: 12549,
    length: 43,
    convRule: rule14
  }, {
    start: 12593,
    length: 94,
    convRule: rule14
  }, {
    start: 12688,
    length: 2,
    convRule: rule13
  }, {
    start: 12690,
    length: 4,
    convRule: rule17
  }, {
    start: 12694,
    length: 10,
    convRule: rule13
  }, {
    start: 12704,
    length: 32,
    convRule: rule14
  }, {
    start: 12736,
    length: 36,
    convRule: rule13
  }, {
    start: 12784,
    length: 16,
    convRule: rule14
  }, {
    start: 12800,
    length: 31,
    convRule: rule13
  }, {
    start: 12832,
    length: 10,
    convRule: rule17
  }, {
    start: 12842,
    length: 30,
    convRule: rule13
  }, {
    start: 12872,
    length: 8,
    convRule: rule17
  }, {
    start: 12880,
    length: 1,
    convRule: rule13
  }, {
    start: 12881,
    length: 15,
    convRule: rule17
  }, {
    start: 12896,
    length: 32,
    convRule: rule13
  }, {
    start: 12928,
    length: 10,
    convRule: rule17
  }, {
    start: 12938,
    length: 39,
    convRule: rule13
  }, {
    start: 12977,
    length: 15,
    convRule: rule17
  }, {
    start: 12992,
    length: 320,
    convRule: rule13
  }, {
    start: 13312,
    length: 6592,
    convRule: rule14
  }, {
    start: 19904,
    length: 64,
    convRule: rule13
  }, {
    start: 19968,
    length: 20989,
    convRule: rule14
  }, {
    start: 40960,
    length: 21,
    convRule: rule14
  }, {
    start: 40981,
    length: 1,
    convRule: rule91
  }, {
    start: 40982,
    length: 1143,
    convRule: rule14
  }, {
    start: 42128,
    length: 55,
    convRule: rule13
  }, {
    start: 42192,
    length: 40,
    convRule: rule14
  }, {
    start: 42232,
    length: 6,
    convRule: rule91
  }, {
    start: 42238,
    length: 2,
    convRule: rule2
  }, {
    start: 42240,
    length: 268,
    convRule: rule14
  }, {
    start: 42508,
    length: 1,
    convRule: rule91
  }, {
    start: 42509,
    length: 3,
    convRule: rule2
  }, {
    start: 42512,
    length: 16,
    convRule: rule14
  }, {
    start: 42528,
    length: 10,
    convRule: rule8
  }, {
    start: 42538,
    length: 2,
    convRule: rule14
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42606,
    length: 1,
    convRule: rule14
  }, {
    start: 42607,
    length: 1,
    convRule: rule92
  }, {
    start: 42608,
    length: 3,
    convRule: rule119
  }, {
    start: 42611,
    length: 1,
    convRule: rule2
  }, {
    start: 42612,
    length: 10,
    convRule: rule92
  }, {
    start: 42622,
    length: 1,
    convRule: rule2
  }, {
    start: 42623,
    length: 1,
    convRule: rule91
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42652,
    length: 2,
    convRule: rule91
  }, {
    start: 42654,
    length: 2,
    convRule: rule92
  }, {
    start: 42656,
    length: 70,
    convRule: rule14
  }, {
    start: 42726,
    length: 10,
    convRule: rule128
  }, {
    start: 42736,
    length: 2,
    convRule: rule92
  }, {
    start: 42738,
    length: 6,
    convRule: rule2
  }, {
    start: 42752,
    length: 23,
    convRule: rule10
  }, {
    start: 42775,
    length: 9,
    convRule: rule91
  }, {
    start: 42784,
    length: 2,
    convRule: rule10
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42800,
    length: 2,
    convRule: rule20
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42864,
    length: 1,
    convRule: rule91
  }, {
    start: 42865,
    length: 8,
    convRule: rule20
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42888,
    length: 1,
    convRule: rule91
  }, {
    start: 42889,
    length: 2,
    convRule: rule10
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42894,
    length: 1,
    convRule: rule20
  }, {
    start: 42895,
    length: 1,
    convRule: rule14
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42901,
    length: 1,
    convRule: rule20
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42927,
    length: 1,
    convRule: rule20
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 42999,
    length: 1,
    convRule: rule14
  }, {
    start: 43e3,
    length: 2,
    convRule: rule91
  }, {
    start: 43002,
    length: 1,
    convRule: rule20
  }, {
    start: 43003,
    length: 7,
    convRule: rule14
  }, {
    start: 43010,
    length: 1,
    convRule: rule92
  }, {
    start: 43011,
    length: 3,
    convRule: rule14
  }, {
    start: 43014,
    length: 1,
    convRule: rule92
  }, {
    start: 43015,
    length: 4,
    convRule: rule14
  }, {
    start: 43019,
    length: 1,
    convRule: rule92
  }, {
    start: 43020,
    length: 23,
    convRule: rule14
  }, {
    start: 43043,
    length: 2,
    convRule: rule124
  }, {
    start: 43045,
    length: 2,
    convRule: rule92
  }, {
    start: 43047,
    length: 1,
    convRule: rule124
  }, {
    start: 43048,
    length: 4,
    convRule: rule13
  }, {
    start: 43052,
    length: 1,
    convRule: rule92
  }, {
    start: 43056,
    length: 6,
    convRule: rule17
  }, {
    start: 43062,
    length: 2,
    convRule: rule13
  }, {
    start: 43064,
    length: 1,
    convRule: rule3
  }, {
    start: 43065,
    length: 1,
    convRule: rule13
  }, {
    start: 43072,
    length: 52,
    convRule: rule14
  }, {
    start: 43124,
    length: 4,
    convRule: rule2
  }, {
    start: 43136,
    length: 2,
    convRule: rule124
  }, {
    start: 43138,
    length: 50,
    convRule: rule14
  }, {
    start: 43188,
    length: 16,
    convRule: rule124
  }, {
    start: 43204,
    length: 2,
    convRule: rule92
  }, {
    start: 43214,
    length: 2,
    convRule: rule2
  }, {
    start: 43216,
    length: 10,
    convRule: rule8
  }, {
    start: 43232,
    length: 18,
    convRule: rule92
  }, {
    start: 43250,
    length: 6,
    convRule: rule14
  }, {
    start: 43256,
    length: 3,
    convRule: rule2
  }, {
    start: 43259,
    length: 1,
    convRule: rule14
  }, {
    start: 43260,
    length: 1,
    convRule: rule2
  }, {
    start: 43261,
    length: 2,
    convRule: rule14
  }, {
    start: 43263,
    length: 1,
    convRule: rule92
  }, {
    start: 43264,
    length: 10,
    convRule: rule8
  }, {
    start: 43274,
    length: 28,
    convRule: rule14
  }, {
    start: 43302,
    length: 8,
    convRule: rule92
  }, {
    start: 43310,
    length: 2,
    convRule: rule2
  }, {
    start: 43312,
    length: 23,
    convRule: rule14
  }, {
    start: 43335,
    length: 11,
    convRule: rule92
  }, {
    start: 43346,
    length: 2,
    convRule: rule124
  }, {
    start: 43359,
    length: 1,
    convRule: rule2
  }, {
    start: 43360,
    length: 29,
    convRule: rule14
  }, {
    start: 43392,
    length: 3,
    convRule: rule92
  }, {
    start: 43395,
    length: 1,
    convRule: rule124
  }, {
    start: 43396,
    length: 47,
    convRule: rule14
  }, {
    start: 43443,
    length: 1,
    convRule: rule92
  }, {
    start: 43444,
    length: 2,
    convRule: rule124
  }, {
    start: 43446,
    length: 4,
    convRule: rule92
  }, {
    start: 43450,
    length: 2,
    convRule: rule124
  }, {
    start: 43452,
    length: 2,
    convRule: rule92
  }, {
    start: 43454,
    length: 3,
    convRule: rule124
  }, {
    start: 43457,
    length: 13,
    convRule: rule2
  }, {
    start: 43471,
    length: 1,
    convRule: rule91
  }, {
    start: 43472,
    length: 10,
    convRule: rule8
  }, {
    start: 43486,
    length: 2,
    convRule: rule2
  }, {
    start: 43488,
    length: 5,
    convRule: rule14
  }, {
    start: 43493,
    length: 1,
    convRule: rule92
  }, {
    start: 43494,
    length: 1,
    convRule: rule91
  }, {
    start: 43495,
    length: 9,
    convRule: rule14
  }, {
    start: 43504,
    length: 10,
    convRule: rule8
  }, {
    start: 43514,
    length: 5,
    convRule: rule14
  }, {
    start: 43520,
    length: 41,
    convRule: rule14
  }, {
    start: 43561,
    length: 6,
    convRule: rule92
  }, {
    start: 43567,
    length: 2,
    convRule: rule124
  }, {
    start: 43569,
    length: 2,
    convRule: rule92
  }, {
    start: 43571,
    length: 2,
    convRule: rule124
  }, {
    start: 43573,
    length: 2,
    convRule: rule92
  }, {
    start: 43584,
    length: 3,
    convRule: rule14
  }, {
    start: 43587,
    length: 1,
    convRule: rule92
  }, {
    start: 43588,
    length: 8,
    convRule: rule14
  }, {
    start: 43596,
    length: 1,
    convRule: rule92
  }, {
    start: 43597,
    length: 1,
    convRule: rule124
  }, {
    start: 43600,
    length: 10,
    convRule: rule8
  }, {
    start: 43612,
    length: 4,
    convRule: rule2
  }, {
    start: 43616,
    length: 16,
    convRule: rule14
  }, {
    start: 43632,
    length: 1,
    convRule: rule91
  }, {
    start: 43633,
    length: 6,
    convRule: rule14
  }, {
    start: 43639,
    length: 3,
    convRule: rule13
  }, {
    start: 43642,
    length: 1,
    convRule: rule14
  }, {
    start: 43643,
    length: 1,
    convRule: rule124
  }, {
    start: 43644,
    length: 1,
    convRule: rule92
  }, {
    start: 43645,
    length: 1,
    convRule: rule124
  }, {
    start: 43646,
    length: 50,
    convRule: rule14
  }, {
    start: 43696,
    length: 1,
    convRule: rule92
  }, {
    start: 43697,
    length: 1,
    convRule: rule14
  }, {
    start: 43698,
    length: 3,
    convRule: rule92
  }, {
    start: 43701,
    length: 2,
    convRule: rule14
  }, {
    start: 43703,
    length: 2,
    convRule: rule92
  }, {
    start: 43705,
    length: 5,
    convRule: rule14
  }, {
    start: 43710,
    length: 2,
    convRule: rule92
  }, {
    start: 43712,
    length: 1,
    convRule: rule14
  }, {
    start: 43713,
    length: 1,
    convRule: rule92
  }, {
    start: 43714,
    length: 1,
    convRule: rule14
  }, {
    start: 43739,
    length: 2,
    convRule: rule14
  }, {
    start: 43741,
    length: 1,
    convRule: rule91
  }, {
    start: 43742,
    length: 2,
    convRule: rule2
  }, {
    start: 43744,
    length: 11,
    convRule: rule14
  }, {
    start: 43755,
    length: 1,
    convRule: rule124
  }, {
    start: 43756,
    length: 2,
    convRule: rule92
  }, {
    start: 43758,
    length: 2,
    convRule: rule124
  }, {
    start: 43760,
    length: 2,
    convRule: rule2
  }, {
    start: 43762,
    length: 1,
    convRule: rule14
  }, {
    start: 43763,
    length: 2,
    convRule: rule91
  }, {
    start: 43765,
    length: 1,
    convRule: rule124
  }, {
    start: 43766,
    length: 1,
    convRule: rule92
  }, {
    start: 43777,
    length: 6,
    convRule: rule14
  }, {
    start: 43785,
    length: 6,
    convRule: rule14
  }, {
    start: 43793,
    length: 6,
    convRule: rule14
  }, {
    start: 43808,
    length: 7,
    convRule: rule14
  }, {
    start: 43816,
    length: 7,
    convRule: rule14
  }, {
    start: 43824,
    length: 35,
    convRule: rule20
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43860,
    length: 7,
    convRule: rule20
  }, {
    start: 43867,
    length: 1,
    convRule: rule10
  }, {
    start: 43868,
    length: 4,
    convRule: rule91
  }, {
    start: 43872,
    length: 9,
    convRule: rule20
  }, {
    start: 43881,
    length: 1,
    convRule: rule91
  }, {
    start: 43882,
    length: 2,
    convRule: rule10
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 43968,
    length: 35,
    convRule: rule14
  }, {
    start: 44003,
    length: 2,
    convRule: rule124
  }, {
    start: 44005,
    length: 1,
    convRule: rule92
  }, {
    start: 44006,
    length: 2,
    convRule: rule124
  }, {
    start: 44008,
    length: 1,
    convRule: rule92
  }, {
    start: 44009,
    length: 2,
    convRule: rule124
  }, {
    start: 44011,
    length: 1,
    convRule: rule2
  }, {
    start: 44012,
    length: 1,
    convRule: rule124
  }, {
    start: 44013,
    length: 1,
    convRule: rule92
  }, {
    start: 44016,
    length: 10,
    convRule: rule8
  }, {
    start: 44032,
    length: 11172,
    convRule: rule14
  }, {
    start: 55216,
    length: 23,
    convRule: rule14
  }, {
    start: 55243,
    length: 49,
    convRule: rule14
  }, {
    start: 55296,
    length: 896,
    convRule: rule199
  }, {
    start: 56192,
    length: 128,
    convRule: rule199
  }, {
    start: 56320,
    length: 1024,
    convRule: rule199
  }, {
    start: 57344,
    length: 6400,
    convRule: rule200
  }, {
    start: 63744,
    length: 366,
    convRule: rule14
  }, {
    start: 64112,
    length: 106,
    convRule: rule14
  }, {
    start: 64256,
    length: 7,
    convRule: rule20
  }, {
    start: 64275,
    length: 5,
    convRule: rule20
  }, {
    start: 64285,
    length: 1,
    convRule: rule14
  }, {
    start: 64286,
    length: 1,
    convRule: rule92
  }, {
    start: 64287,
    length: 10,
    convRule: rule14
  }, {
    start: 64297,
    length: 1,
    convRule: rule6
  }, {
    start: 64298,
    length: 13,
    convRule: rule14
  }, {
    start: 64312,
    length: 5,
    convRule: rule14
  }, {
    start: 64318,
    length: 1,
    convRule: rule14
  }, {
    start: 64320,
    length: 2,
    convRule: rule14
  }, {
    start: 64323,
    length: 2,
    convRule: rule14
  }, {
    start: 64326,
    length: 108,
    convRule: rule14
  }, {
    start: 64434,
    length: 16,
    convRule: rule10
  }, {
    start: 64467,
    length: 363,
    convRule: rule14
  }, {
    start: 64830,
    length: 1,
    convRule: rule5
  }, {
    start: 64831,
    length: 1,
    convRule: rule4
  }, {
    start: 64848,
    length: 64,
    convRule: rule14
  }, {
    start: 64914,
    length: 54,
    convRule: rule14
  }, {
    start: 65008,
    length: 12,
    convRule: rule14
  }, {
    start: 65020,
    length: 1,
    convRule: rule3
  }, {
    start: 65021,
    length: 1,
    convRule: rule13
  }, {
    start: 65024,
    length: 16,
    convRule: rule92
  }, {
    start: 65040,
    length: 7,
    convRule: rule2
  }, {
    start: 65047,
    length: 1,
    convRule: rule4
  }, {
    start: 65048,
    length: 1,
    convRule: rule5
  }, {
    start: 65049,
    length: 1,
    convRule: rule2
  }, {
    start: 65056,
    length: 16,
    convRule: rule92
  }, {
    start: 65072,
    length: 1,
    convRule: rule2
  }, {
    start: 65073,
    length: 2,
    convRule: rule7
  }, {
    start: 65075,
    length: 2,
    convRule: rule11
  }, {
    start: 65077,
    length: 1,
    convRule: rule4
  }, {
    start: 65078,
    length: 1,
    convRule: rule5
  }, {
    start: 65079,
    length: 1,
    convRule: rule4
  }, {
    start: 65080,
    length: 1,
    convRule: rule5
  }, {
    start: 65081,
    length: 1,
    convRule: rule4
  }, {
    start: 65082,
    length: 1,
    convRule: rule5
  }, {
    start: 65083,
    length: 1,
    convRule: rule4
  }, {
    start: 65084,
    length: 1,
    convRule: rule5
  }, {
    start: 65085,
    length: 1,
    convRule: rule4
  }, {
    start: 65086,
    length: 1,
    convRule: rule5
  }, {
    start: 65087,
    length: 1,
    convRule: rule4
  }, {
    start: 65088,
    length: 1,
    convRule: rule5
  }, {
    start: 65089,
    length: 1,
    convRule: rule4
  }, {
    start: 65090,
    length: 1,
    convRule: rule5
  }, {
    start: 65091,
    length: 1,
    convRule: rule4
  }, {
    start: 65092,
    length: 1,
    convRule: rule5
  }, {
    start: 65093,
    length: 2,
    convRule: rule2
  }, {
    start: 65095,
    length: 1,
    convRule: rule4
  }, {
    start: 65096,
    length: 1,
    convRule: rule5
  }, {
    start: 65097,
    length: 4,
    convRule: rule2
  }, {
    start: 65101,
    length: 3,
    convRule: rule11
  }, {
    start: 65104,
    length: 3,
    convRule: rule2
  }, {
    start: 65108,
    length: 4,
    convRule: rule2
  }, {
    start: 65112,
    length: 1,
    convRule: rule7
  }, {
    start: 65113,
    length: 1,
    convRule: rule4
  }, {
    start: 65114,
    length: 1,
    convRule: rule5
  }, {
    start: 65115,
    length: 1,
    convRule: rule4
  }, {
    start: 65116,
    length: 1,
    convRule: rule5
  }, {
    start: 65117,
    length: 1,
    convRule: rule4
  }, {
    start: 65118,
    length: 1,
    convRule: rule5
  }, {
    start: 65119,
    length: 3,
    convRule: rule2
  }, {
    start: 65122,
    length: 1,
    convRule: rule6
  }, {
    start: 65123,
    length: 1,
    convRule: rule7
  }, {
    start: 65124,
    length: 3,
    convRule: rule6
  }, {
    start: 65128,
    length: 1,
    convRule: rule2
  }, {
    start: 65129,
    length: 1,
    convRule: rule3
  }, {
    start: 65130,
    length: 2,
    convRule: rule2
  }, {
    start: 65136,
    length: 5,
    convRule: rule14
  }, {
    start: 65142,
    length: 135,
    convRule: rule14
  }, {
    start: 65279,
    length: 1,
    convRule: rule16
  }, {
    start: 65281,
    length: 3,
    convRule: rule2
  }, {
    start: 65284,
    length: 1,
    convRule: rule3
  }, {
    start: 65285,
    length: 3,
    convRule: rule2
  }, {
    start: 65288,
    length: 1,
    convRule: rule4
  }, {
    start: 65289,
    length: 1,
    convRule: rule5
  }, {
    start: 65290,
    length: 1,
    convRule: rule2
  }, {
    start: 65291,
    length: 1,
    convRule: rule6
  }, {
    start: 65292,
    length: 1,
    convRule: rule2
  }, {
    start: 65293,
    length: 1,
    convRule: rule7
  }, {
    start: 65294,
    length: 2,
    convRule: rule2
  }, {
    start: 65296,
    length: 10,
    convRule: rule8
  }, {
    start: 65306,
    length: 2,
    convRule: rule2
  }, {
    start: 65308,
    length: 3,
    convRule: rule6
  }, {
    start: 65311,
    length: 2,
    convRule: rule2
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65339,
    length: 1,
    convRule: rule4
  }, {
    start: 65340,
    length: 1,
    convRule: rule2
  }, {
    start: 65341,
    length: 1,
    convRule: rule5
  }, {
    start: 65342,
    length: 1,
    convRule: rule10
  }, {
    start: 65343,
    length: 1,
    convRule: rule11
  }, {
    start: 65344,
    length: 1,
    convRule: rule10
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 65371,
    length: 1,
    convRule: rule4
  }, {
    start: 65372,
    length: 1,
    convRule: rule6
  }, {
    start: 65373,
    length: 1,
    convRule: rule5
  }, {
    start: 65374,
    length: 1,
    convRule: rule6
  }, {
    start: 65375,
    length: 1,
    convRule: rule4
  }, {
    start: 65376,
    length: 1,
    convRule: rule5
  }, {
    start: 65377,
    length: 1,
    convRule: rule2
  }, {
    start: 65378,
    length: 1,
    convRule: rule4
  }, {
    start: 65379,
    length: 1,
    convRule: rule5
  }, {
    start: 65380,
    length: 2,
    convRule: rule2
  }, {
    start: 65382,
    length: 10,
    convRule: rule14
  }, {
    start: 65392,
    length: 1,
    convRule: rule91
  }, {
    start: 65393,
    length: 45,
    convRule: rule14
  }, {
    start: 65438,
    length: 2,
    convRule: rule91
  }, {
    start: 65440,
    length: 31,
    convRule: rule14
  }, {
    start: 65474,
    length: 6,
    convRule: rule14
  }, {
    start: 65482,
    length: 6,
    convRule: rule14
  }, {
    start: 65490,
    length: 6,
    convRule: rule14
  }, {
    start: 65498,
    length: 3,
    convRule: rule14
  }, {
    start: 65504,
    length: 2,
    convRule: rule3
  }, {
    start: 65506,
    length: 1,
    convRule: rule6
  }, {
    start: 65507,
    length: 1,
    convRule: rule10
  }, {
    start: 65508,
    length: 1,
    convRule: rule13
  }, {
    start: 65509,
    length: 2,
    convRule: rule3
  }, {
    start: 65512,
    length: 1,
    convRule: rule13
  }, {
    start: 65513,
    length: 4,
    convRule: rule6
  }, {
    start: 65517,
    length: 2,
    convRule: rule13
  }, {
    start: 65529,
    length: 3,
    convRule: rule16
  }, {
    start: 65532,
    length: 2,
    convRule: rule13
  }, {
    start: 65536,
    length: 12,
    convRule: rule14
  }, {
    start: 65549,
    length: 26,
    convRule: rule14
  }, {
    start: 65576,
    length: 19,
    convRule: rule14
  }, {
    start: 65596,
    length: 2,
    convRule: rule14
  }, {
    start: 65599,
    length: 15,
    convRule: rule14
  }, {
    start: 65616,
    length: 14,
    convRule: rule14
  }, {
    start: 65664,
    length: 123,
    convRule: rule14
  }, {
    start: 65792,
    length: 3,
    convRule: rule2
  }, {
    start: 65799,
    length: 45,
    convRule: rule17
  }, {
    start: 65847,
    length: 9,
    convRule: rule13
  }, {
    start: 65856,
    length: 53,
    convRule: rule128
  }, {
    start: 65909,
    length: 4,
    convRule: rule17
  }, {
    start: 65913,
    length: 17,
    convRule: rule13
  }, {
    start: 65930,
    length: 2,
    convRule: rule17
  }, {
    start: 65932,
    length: 3,
    convRule: rule13
  }, {
    start: 65936,
    length: 13,
    convRule: rule13
  }, {
    start: 65952,
    length: 1,
    convRule: rule13
  }, {
    start: 66e3,
    length: 45,
    convRule: rule13
  }, {
    start: 66045,
    length: 1,
    convRule: rule92
  }, {
    start: 66176,
    length: 29,
    convRule: rule14
  }, {
    start: 66208,
    length: 49,
    convRule: rule14
  }, {
    start: 66272,
    length: 1,
    convRule: rule92
  }, {
    start: 66273,
    length: 27,
    convRule: rule17
  }, {
    start: 66304,
    length: 32,
    convRule: rule14
  }, {
    start: 66336,
    length: 4,
    convRule: rule17
  }, {
    start: 66349,
    length: 20,
    convRule: rule14
  }, {
    start: 66369,
    length: 1,
    convRule: rule128
  }, {
    start: 66370,
    length: 8,
    convRule: rule14
  }, {
    start: 66378,
    length: 1,
    convRule: rule128
  }, {
    start: 66384,
    length: 38,
    convRule: rule14
  }, {
    start: 66422,
    length: 5,
    convRule: rule92
  }, {
    start: 66432,
    length: 30,
    convRule: rule14
  }, {
    start: 66463,
    length: 1,
    convRule: rule2
  }, {
    start: 66464,
    length: 36,
    convRule: rule14
  }, {
    start: 66504,
    length: 8,
    convRule: rule14
  }, {
    start: 66512,
    length: 1,
    convRule: rule2
  }, {
    start: 66513,
    length: 5,
    convRule: rule128
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66640,
    length: 78,
    convRule: rule14
  }, {
    start: 66720,
    length: 10,
    convRule: rule8
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 66816,
    length: 40,
    convRule: rule14
  }, {
    start: 66864,
    length: 52,
    convRule: rule14
  }, {
    start: 66927,
    length: 1,
    convRule: rule2
  }, {
    start: 67072,
    length: 311,
    convRule: rule14
  }, {
    start: 67392,
    length: 22,
    convRule: rule14
  }, {
    start: 67424,
    length: 8,
    convRule: rule14
  }, {
    start: 67584,
    length: 6,
    convRule: rule14
  }, {
    start: 67592,
    length: 1,
    convRule: rule14
  }, {
    start: 67594,
    length: 44,
    convRule: rule14
  }, {
    start: 67639,
    length: 2,
    convRule: rule14
  }, {
    start: 67644,
    length: 1,
    convRule: rule14
  }, {
    start: 67647,
    length: 23,
    convRule: rule14
  }, {
    start: 67671,
    length: 1,
    convRule: rule2
  }, {
    start: 67672,
    length: 8,
    convRule: rule17
  }, {
    start: 67680,
    length: 23,
    convRule: rule14
  }, {
    start: 67703,
    length: 2,
    convRule: rule13
  }, {
    start: 67705,
    length: 7,
    convRule: rule17
  }, {
    start: 67712,
    length: 31,
    convRule: rule14
  }, {
    start: 67751,
    length: 9,
    convRule: rule17
  }, {
    start: 67808,
    length: 19,
    convRule: rule14
  }, {
    start: 67828,
    length: 2,
    convRule: rule14
  }, {
    start: 67835,
    length: 5,
    convRule: rule17
  }, {
    start: 67840,
    length: 22,
    convRule: rule14
  }, {
    start: 67862,
    length: 6,
    convRule: rule17
  }, {
    start: 67871,
    length: 1,
    convRule: rule2
  }, {
    start: 67872,
    length: 26,
    convRule: rule14
  }, {
    start: 67903,
    length: 1,
    convRule: rule2
  }, {
    start: 67968,
    length: 56,
    convRule: rule14
  }, {
    start: 68028,
    length: 2,
    convRule: rule17
  }, {
    start: 68030,
    length: 2,
    convRule: rule14
  }, {
    start: 68032,
    length: 16,
    convRule: rule17
  }, {
    start: 68050,
    length: 46,
    convRule: rule17
  }, {
    start: 68096,
    length: 1,
    convRule: rule14
  }, {
    start: 68097,
    length: 3,
    convRule: rule92
  }, {
    start: 68101,
    length: 2,
    convRule: rule92
  }, {
    start: 68108,
    length: 4,
    convRule: rule92
  }, {
    start: 68112,
    length: 4,
    convRule: rule14
  }, {
    start: 68117,
    length: 3,
    convRule: rule14
  }, {
    start: 68121,
    length: 29,
    convRule: rule14
  }, {
    start: 68152,
    length: 3,
    convRule: rule92
  }, {
    start: 68159,
    length: 1,
    convRule: rule92
  }, {
    start: 68160,
    length: 9,
    convRule: rule17
  }, {
    start: 68176,
    length: 9,
    convRule: rule2
  }, {
    start: 68192,
    length: 29,
    convRule: rule14
  }, {
    start: 68221,
    length: 2,
    convRule: rule17
  }, {
    start: 68223,
    length: 1,
    convRule: rule2
  }, {
    start: 68224,
    length: 29,
    convRule: rule14
  }, {
    start: 68253,
    length: 3,
    convRule: rule17
  }, {
    start: 68288,
    length: 8,
    convRule: rule14
  }, {
    start: 68296,
    length: 1,
    convRule: rule13
  }, {
    start: 68297,
    length: 28,
    convRule: rule14
  }, {
    start: 68325,
    length: 2,
    convRule: rule92
  }, {
    start: 68331,
    length: 5,
    convRule: rule17
  }, {
    start: 68336,
    length: 7,
    convRule: rule2
  }, {
    start: 68352,
    length: 54,
    convRule: rule14
  }, {
    start: 68409,
    length: 7,
    convRule: rule2
  }, {
    start: 68416,
    length: 22,
    convRule: rule14
  }, {
    start: 68440,
    length: 8,
    convRule: rule17
  }, {
    start: 68448,
    length: 19,
    convRule: rule14
  }, {
    start: 68472,
    length: 8,
    convRule: rule17
  }, {
    start: 68480,
    length: 18,
    convRule: rule14
  }, {
    start: 68505,
    length: 4,
    convRule: rule2
  }, {
    start: 68521,
    length: 7,
    convRule: rule17
  }, {
    start: 68608,
    length: 73,
    convRule: rule14
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 68858,
    length: 6,
    convRule: rule17
  }, {
    start: 68864,
    length: 36,
    convRule: rule14
  }, {
    start: 68900,
    length: 4,
    convRule: rule92
  }, {
    start: 68912,
    length: 10,
    convRule: rule8
  }, {
    start: 69216,
    length: 31,
    convRule: rule17
  }, {
    start: 69248,
    length: 42,
    convRule: rule14
  }, {
    start: 69291,
    length: 2,
    convRule: rule92
  }, {
    start: 69293,
    length: 1,
    convRule: rule7
  }, {
    start: 69296,
    length: 2,
    convRule: rule14
  }, {
    start: 69376,
    length: 29,
    convRule: rule14
  }, {
    start: 69405,
    length: 10,
    convRule: rule17
  }, {
    start: 69415,
    length: 1,
    convRule: rule14
  }, {
    start: 69424,
    length: 22,
    convRule: rule14
  }, {
    start: 69446,
    length: 11,
    convRule: rule92
  }, {
    start: 69457,
    length: 4,
    convRule: rule17
  }, {
    start: 69461,
    length: 5,
    convRule: rule2
  }, {
    start: 69552,
    length: 21,
    convRule: rule14
  }, {
    start: 69573,
    length: 7,
    convRule: rule17
  }, {
    start: 69600,
    length: 23,
    convRule: rule14
  }, {
    start: 69632,
    length: 1,
    convRule: rule124
  }, {
    start: 69633,
    length: 1,
    convRule: rule92
  }, {
    start: 69634,
    length: 1,
    convRule: rule124
  }, {
    start: 69635,
    length: 53,
    convRule: rule14
  }, {
    start: 69688,
    length: 15,
    convRule: rule92
  }, {
    start: 69703,
    length: 7,
    convRule: rule2
  }, {
    start: 69714,
    length: 20,
    convRule: rule17
  }, {
    start: 69734,
    length: 10,
    convRule: rule8
  }, {
    start: 69759,
    length: 3,
    convRule: rule92
  }, {
    start: 69762,
    length: 1,
    convRule: rule124
  }, {
    start: 69763,
    length: 45,
    convRule: rule14
  }, {
    start: 69808,
    length: 3,
    convRule: rule124
  }, {
    start: 69811,
    length: 4,
    convRule: rule92
  }, {
    start: 69815,
    length: 2,
    convRule: rule124
  }, {
    start: 69817,
    length: 2,
    convRule: rule92
  }, {
    start: 69819,
    length: 2,
    convRule: rule2
  }, {
    start: 69821,
    length: 1,
    convRule: rule16
  }, {
    start: 69822,
    length: 4,
    convRule: rule2
  }, {
    start: 69837,
    length: 1,
    convRule: rule16
  }, {
    start: 69840,
    length: 25,
    convRule: rule14
  }, {
    start: 69872,
    length: 10,
    convRule: rule8
  }, {
    start: 69888,
    length: 3,
    convRule: rule92
  }, {
    start: 69891,
    length: 36,
    convRule: rule14
  }, {
    start: 69927,
    length: 5,
    convRule: rule92
  }, {
    start: 69932,
    length: 1,
    convRule: rule124
  }, {
    start: 69933,
    length: 8,
    convRule: rule92
  }, {
    start: 69942,
    length: 10,
    convRule: rule8
  }, {
    start: 69952,
    length: 4,
    convRule: rule2
  }, {
    start: 69956,
    length: 1,
    convRule: rule14
  }, {
    start: 69957,
    length: 2,
    convRule: rule124
  }, {
    start: 69959,
    length: 1,
    convRule: rule14
  }, {
    start: 69968,
    length: 35,
    convRule: rule14
  }, {
    start: 70003,
    length: 1,
    convRule: rule92
  }, {
    start: 70004,
    length: 2,
    convRule: rule2
  }, {
    start: 70006,
    length: 1,
    convRule: rule14
  }, {
    start: 70016,
    length: 2,
    convRule: rule92
  }, {
    start: 70018,
    length: 1,
    convRule: rule124
  }, {
    start: 70019,
    length: 48,
    convRule: rule14
  }, {
    start: 70067,
    length: 3,
    convRule: rule124
  }, {
    start: 70070,
    length: 9,
    convRule: rule92
  }, {
    start: 70079,
    length: 2,
    convRule: rule124
  }, {
    start: 70081,
    length: 4,
    convRule: rule14
  }, {
    start: 70085,
    length: 4,
    convRule: rule2
  }, {
    start: 70089,
    length: 4,
    convRule: rule92
  }, {
    start: 70093,
    length: 1,
    convRule: rule2
  }, {
    start: 70094,
    length: 1,
    convRule: rule124
  }, {
    start: 70095,
    length: 1,
    convRule: rule92
  }, {
    start: 70096,
    length: 10,
    convRule: rule8
  }, {
    start: 70106,
    length: 1,
    convRule: rule14
  }, {
    start: 70107,
    length: 1,
    convRule: rule2
  }, {
    start: 70108,
    length: 1,
    convRule: rule14
  }, {
    start: 70109,
    length: 3,
    convRule: rule2
  }, {
    start: 70113,
    length: 20,
    convRule: rule17
  }, {
    start: 70144,
    length: 18,
    convRule: rule14
  }, {
    start: 70163,
    length: 25,
    convRule: rule14
  }, {
    start: 70188,
    length: 3,
    convRule: rule124
  }, {
    start: 70191,
    length: 3,
    convRule: rule92
  }, {
    start: 70194,
    length: 2,
    convRule: rule124
  }, {
    start: 70196,
    length: 1,
    convRule: rule92
  }, {
    start: 70197,
    length: 1,
    convRule: rule124
  }, {
    start: 70198,
    length: 2,
    convRule: rule92
  }, {
    start: 70200,
    length: 6,
    convRule: rule2
  }, {
    start: 70206,
    length: 1,
    convRule: rule92
  }, {
    start: 70272,
    length: 7,
    convRule: rule14
  }, {
    start: 70280,
    length: 1,
    convRule: rule14
  }, {
    start: 70282,
    length: 4,
    convRule: rule14
  }, {
    start: 70287,
    length: 15,
    convRule: rule14
  }, {
    start: 70303,
    length: 10,
    convRule: rule14
  }, {
    start: 70313,
    length: 1,
    convRule: rule2
  }, {
    start: 70320,
    length: 47,
    convRule: rule14
  }, {
    start: 70367,
    length: 1,
    convRule: rule92
  }, {
    start: 70368,
    length: 3,
    convRule: rule124
  }, {
    start: 70371,
    length: 8,
    convRule: rule92
  }, {
    start: 70384,
    length: 10,
    convRule: rule8
  }, {
    start: 70400,
    length: 2,
    convRule: rule92
  }, {
    start: 70402,
    length: 2,
    convRule: rule124
  }, {
    start: 70405,
    length: 8,
    convRule: rule14
  }, {
    start: 70415,
    length: 2,
    convRule: rule14
  }, {
    start: 70419,
    length: 22,
    convRule: rule14
  }, {
    start: 70442,
    length: 7,
    convRule: rule14
  }, {
    start: 70450,
    length: 2,
    convRule: rule14
  }, {
    start: 70453,
    length: 5,
    convRule: rule14
  }, {
    start: 70459,
    length: 2,
    convRule: rule92
  }, {
    start: 70461,
    length: 1,
    convRule: rule14
  }, {
    start: 70462,
    length: 2,
    convRule: rule124
  }, {
    start: 70464,
    length: 1,
    convRule: rule92
  }, {
    start: 70465,
    length: 4,
    convRule: rule124
  }, {
    start: 70471,
    length: 2,
    convRule: rule124
  }, {
    start: 70475,
    length: 3,
    convRule: rule124
  }, {
    start: 70480,
    length: 1,
    convRule: rule14
  }, {
    start: 70487,
    length: 1,
    convRule: rule124
  }, {
    start: 70493,
    length: 5,
    convRule: rule14
  }, {
    start: 70498,
    length: 2,
    convRule: rule124
  }, {
    start: 70502,
    length: 7,
    convRule: rule92
  }, {
    start: 70512,
    length: 5,
    convRule: rule92
  }, {
    start: 70656,
    length: 53,
    convRule: rule14
  }, {
    start: 70709,
    length: 3,
    convRule: rule124
  }, {
    start: 70712,
    length: 8,
    convRule: rule92
  }, {
    start: 70720,
    length: 2,
    convRule: rule124
  }, {
    start: 70722,
    length: 3,
    convRule: rule92
  }, {
    start: 70725,
    length: 1,
    convRule: rule124
  }, {
    start: 70726,
    length: 1,
    convRule: rule92
  }, {
    start: 70727,
    length: 4,
    convRule: rule14
  }, {
    start: 70731,
    length: 5,
    convRule: rule2
  }, {
    start: 70736,
    length: 10,
    convRule: rule8
  }, {
    start: 70746,
    length: 2,
    convRule: rule2
  }, {
    start: 70749,
    length: 1,
    convRule: rule2
  }, {
    start: 70750,
    length: 1,
    convRule: rule92
  }, {
    start: 70751,
    length: 3,
    convRule: rule14
  }, {
    start: 70784,
    length: 48,
    convRule: rule14
  }, {
    start: 70832,
    length: 3,
    convRule: rule124
  }, {
    start: 70835,
    length: 6,
    convRule: rule92
  }, {
    start: 70841,
    length: 1,
    convRule: rule124
  }, {
    start: 70842,
    length: 1,
    convRule: rule92
  }, {
    start: 70843,
    length: 4,
    convRule: rule124
  }, {
    start: 70847,
    length: 2,
    convRule: rule92
  }, {
    start: 70849,
    length: 1,
    convRule: rule124
  }, {
    start: 70850,
    length: 2,
    convRule: rule92
  }, {
    start: 70852,
    length: 2,
    convRule: rule14
  }, {
    start: 70854,
    length: 1,
    convRule: rule2
  }, {
    start: 70855,
    length: 1,
    convRule: rule14
  }, {
    start: 70864,
    length: 10,
    convRule: rule8
  }, {
    start: 71040,
    length: 47,
    convRule: rule14
  }, {
    start: 71087,
    length: 3,
    convRule: rule124
  }, {
    start: 71090,
    length: 4,
    convRule: rule92
  }, {
    start: 71096,
    length: 4,
    convRule: rule124
  }, {
    start: 71100,
    length: 2,
    convRule: rule92
  }, {
    start: 71102,
    length: 1,
    convRule: rule124
  }, {
    start: 71103,
    length: 2,
    convRule: rule92
  }, {
    start: 71105,
    length: 23,
    convRule: rule2
  }, {
    start: 71128,
    length: 4,
    convRule: rule14
  }, {
    start: 71132,
    length: 2,
    convRule: rule92
  }, {
    start: 71168,
    length: 48,
    convRule: rule14
  }, {
    start: 71216,
    length: 3,
    convRule: rule124
  }, {
    start: 71219,
    length: 8,
    convRule: rule92
  }, {
    start: 71227,
    length: 2,
    convRule: rule124
  }, {
    start: 71229,
    length: 1,
    convRule: rule92
  }, {
    start: 71230,
    length: 1,
    convRule: rule124
  }, {
    start: 71231,
    length: 2,
    convRule: rule92
  }, {
    start: 71233,
    length: 3,
    convRule: rule2
  }, {
    start: 71236,
    length: 1,
    convRule: rule14
  }, {
    start: 71248,
    length: 10,
    convRule: rule8
  }, {
    start: 71264,
    length: 13,
    convRule: rule2
  }, {
    start: 71296,
    length: 43,
    convRule: rule14
  }, {
    start: 71339,
    length: 1,
    convRule: rule92
  }, {
    start: 71340,
    length: 1,
    convRule: rule124
  }, {
    start: 71341,
    length: 1,
    convRule: rule92
  }, {
    start: 71342,
    length: 2,
    convRule: rule124
  }, {
    start: 71344,
    length: 6,
    convRule: rule92
  }, {
    start: 71350,
    length: 1,
    convRule: rule124
  }, {
    start: 71351,
    length: 1,
    convRule: rule92
  }, {
    start: 71352,
    length: 1,
    convRule: rule14
  }, {
    start: 71360,
    length: 10,
    convRule: rule8
  }, {
    start: 71424,
    length: 27,
    convRule: rule14
  }, {
    start: 71453,
    length: 3,
    convRule: rule92
  }, {
    start: 71456,
    length: 2,
    convRule: rule124
  }, {
    start: 71458,
    length: 4,
    convRule: rule92
  }, {
    start: 71462,
    length: 1,
    convRule: rule124
  }, {
    start: 71463,
    length: 5,
    convRule: rule92
  }, {
    start: 71472,
    length: 10,
    convRule: rule8
  }, {
    start: 71482,
    length: 2,
    convRule: rule17
  }, {
    start: 71484,
    length: 3,
    convRule: rule2
  }, {
    start: 71487,
    length: 1,
    convRule: rule13
  }, {
    start: 71680,
    length: 44,
    convRule: rule14
  }, {
    start: 71724,
    length: 3,
    convRule: rule124
  }, {
    start: 71727,
    length: 9,
    convRule: rule92
  }, {
    start: 71736,
    length: 1,
    convRule: rule124
  }, {
    start: 71737,
    length: 2,
    convRule: rule92
  }, {
    start: 71739,
    length: 1,
    convRule: rule2
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 71904,
    length: 10,
    convRule: rule8
  }, {
    start: 71914,
    length: 9,
    convRule: rule17
  }, {
    start: 71935,
    length: 8,
    convRule: rule14
  }, {
    start: 71945,
    length: 1,
    convRule: rule14
  }, {
    start: 71948,
    length: 8,
    convRule: rule14
  }, {
    start: 71957,
    length: 2,
    convRule: rule14
  }, {
    start: 71960,
    length: 24,
    convRule: rule14
  }, {
    start: 71984,
    length: 6,
    convRule: rule124
  }, {
    start: 71991,
    length: 2,
    convRule: rule124
  }, {
    start: 71995,
    length: 2,
    convRule: rule92
  }, {
    start: 71997,
    length: 1,
    convRule: rule124
  }, {
    start: 71998,
    length: 1,
    convRule: rule92
  }, {
    start: 71999,
    length: 1,
    convRule: rule14
  }, {
    start: 72e3,
    length: 1,
    convRule: rule124
  }, {
    start: 72001,
    length: 1,
    convRule: rule14
  }, {
    start: 72002,
    length: 1,
    convRule: rule124
  }, {
    start: 72003,
    length: 1,
    convRule: rule92
  }, {
    start: 72004,
    length: 3,
    convRule: rule2
  }, {
    start: 72016,
    length: 10,
    convRule: rule8
  }, {
    start: 72096,
    length: 8,
    convRule: rule14
  }, {
    start: 72106,
    length: 39,
    convRule: rule14
  }, {
    start: 72145,
    length: 3,
    convRule: rule124
  }, {
    start: 72148,
    length: 4,
    convRule: rule92
  }, {
    start: 72154,
    length: 2,
    convRule: rule92
  }, {
    start: 72156,
    length: 4,
    convRule: rule124
  }, {
    start: 72160,
    length: 1,
    convRule: rule92
  }, {
    start: 72161,
    length: 1,
    convRule: rule14
  }, {
    start: 72162,
    length: 1,
    convRule: rule2
  }, {
    start: 72163,
    length: 1,
    convRule: rule14
  }, {
    start: 72164,
    length: 1,
    convRule: rule124
  }, {
    start: 72192,
    length: 1,
    convRule: rule14
  }, {
    start: 72193,
    length: 10,
    convRule: rule92
  }, {
    start: 72203,
    length: 40,
    convRule: rule14
  }, {
    start: 72243,
    length: 6,
    convRule: rule92
  }, {
    start: 72249,
    length: 1,
    convRule: rule124
  }, {
    start: 72250,
    length: 1,
    convRule: rule14
  }, {
    start: 72251,
    length: 4,
    convRule: rule92
  }, {
    start: 72255,
    length: 8,
    convRule: rule2
  }, {
    start: 72263,
    length: 1,
    convRule: rule92
  }, {
    start: 72272,
    length: 1,
    convRule: rule14
  }, {
    start: 72273,
    length: 6,
    convRule: rule92
  }, {
    start: 72279,
    length: 2,
    convRule: rule124
  }, {
    start: 72281,
    length: 3,
    convRule: rule92
  }, {
    start: 72284,
    length: 46,
    convRule: rule14
  }, {
    start: 72330,
    length: 13,
    convRule: rule92
  }, {
    start: 72343,
    length: 1,
    convRule: rule124
  }, {
    start: 72344,
    length: 2,
    convRule: rule92
  }, {
    start: 72346,
    length: 3,
    convRule: rule2
  }, {
    start: 72349,
    length: 1,
    convRule: rule14
  }, {
    start: 72350,
    length: 5,
    convRule: rule2
  }, {
    start: 72384,
    length: 57,
    convRule: rule14
  }, {
    start: 72704,
    length: 9,
    convRule: rule14
  }, {
    start: 72714,
    length: 37,
    convRule: rule14
  }, {
    start: 72751,
    length: 1,
    convRule: rule124
  }, {
    start: 72752,
    length: 7,
    convRule: rule92
  }, {
    start: 72760,
    length: 6,
    convRule: rule92
  }, {
    start: 72766,
    length: 1,
    convRule: rule124
  }, {
    start: 72767,
    length: 1,
    convRule: rule92
  }, {
    start: 72768,
    length: 1,
    convRule: rule14
  }, {
    start: 72769,
    length: 5,
    convRule: rule2
  }, {
    start: 72784,
    length: 10,
    convRule: rule8
  }, {
    start: 72794,
    length: 19,
    convRule: rule17
  }, {
    start: 72816,
    length: 2,
    convRule: rule2
  }, {
    start: 72818,
    length: 30,
    convRule: rule14
  }, {
    start: 72850,
    length: 22,
    convRule: rule92
  }, {
    start: 72873,
    length: 1,
    convRule: rule124
  }, {
    start: 72874,
    length: 7,
    convRule: rule92
  }, {
    start: 72881,
    length: 1,
    convRule: rule124
  }, {
    start: 72882,
    length: 2,
    convRule: rule92
  }, {
    start: 72884,
    length: 1,
    convRule: rule124
  }, {
    start: 72885,
    length: 2,
    convRule: rule92
  }, {
    start: 72960,
    length: 7,
    convRule: rule14
  }, {
    start: 72968,
    length: 2,
    convRule: rule14
  }, {
    start: 72971,
    length: 38,
    convRule: rule14
  }, {
    start: 73009,
    length: 6,
    convRule: rule92
  }, {
    start: 73018,
    length: 1,
    convRule: rule92
  }, {
    start: 73020,
    length: 2,
    convRule: rule92
  }, {
    start: 73023,
    length: 7,
    convRule: rule92
  }, {
    start: 73030,
    length: 1,
    convRule: rule14
  }, {
    start: 73031,
    length: 1,
    convRule: rule92
  }, {
    start: 73040,
    length: 10,
    convRule: rule8
  }, {
    start: 73056,
    length: 6,
    convRule: rule14
  }, {
    start: 73063,
    length: 2,
    convRule: rule14
  }, {
    start: 73066,
    length: 32,
    convRule: rule14
  }, {
    start: 73098,
    length: 5,
    convRule: rule124
  }, {
    start: 73104,
    length: 2,
    convRule: rule92
  }, {
    start: 73107,
    length: 2,
    convRule: rule124
  }, {
    start: 73109,
    length: 1,
    convRule: rule92
  }, {
    start: 73110,
    length: 1,
    convRule: rule124
  }, {
    start: 73111,
    length: 1,
    convRule: rule92
  }, {
    start: 73112,
    length: 1,
    convRule: rule14
  }, {
    start: 73120,
    length: 10,
    convRule: rule8
  }, {
    start: 73440,
    length: 19,
    convRule: rule14
  }, {
    start: 73459,
    length: 2,
    convRule: rule92
  }, {
    start: 73461,
    length: 2,
    convRule: rule124
  }, {
    start: 73463,
    length: 2,
    convRule: rule2
  }, {
    start: 73648,
    length: 1,
    convRule: rule14
  }, {
    start: 73664,
    length: 21,
    convRule: rule17
  }, {
    start: 73685,
    length: 8,
    convRule: rule13
  }, {
    start: 73693,
    length: 4,
    convRule: rule3
  }, {
    start: 73697,
    length: 17,
    convRule: rule13
  }, {
    start: 73727,
    length: 1,
    convRule: rule2
  }, {
    start: 73728,
    length: 922,
    convRule: rule14
  }, {
    start: 74752,
    length: 111,
    convRule: rule128
  }, {
    start: 74864,
    length: 5,
    convRule: rule2
  }, {
    start: 74880,
    length: 196,
    convRule: rule14
  }, {
    start: 77824,
    length: 1071,
    convRule: rule14
  }, {
    start: 78896,
    length: 9,
    convRule: rule16
  }, {
    start: 82944,
    length: 583,
    convRule: rule14
  }, {
    start: 92160,
    length: 569,
    convRule: rule14
  }, {
    start: 92736,
    length: 31,
    convRule: rule14
  }, {
    start: 92768,
    length: 10,
    convRule: rule8
  }, {
    start: 92782,
    length: 2,
    convRule: rule2
  }, {
    start: 92880,
    length: 30,
    convRule: rule14
  }, {
    start: 92912,
    length: 5,
    convRule: rule92
  }, {
    start: 92917,
    length: 1,
    convRule: rule2
  }, {
    start: 92928,
    length: 48,
    convRule: rule14
  }, {
    start: 92976,
    length: 7,
    convRule: rule92
  }, {
    start: 92983,
    length: 5,
    convRule: rule2
  }, {
    start: 92988,
    length: 4,
    convRule: rule13
  }, {
    start: 92992,
    length: 4,
    convRule: rule91
  }, {
    start: 92996,
    length: 1,
    convRule: rule2
  }, {
    start: 92997,
    length: 1,
    convRule: rule13
  }, {
    start: 93008,
    length: 10,
    convRule: rule8
  }, {
    start: 93019,
    length: 7,
    convRule: rule17
  }, {
    start: 93027,
    length: 21,
    convRule: rule14
  }, {
    start: 93053,
    length: 19,
    convRule: rule14
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 93824,
    length: 23,
    convRule: rule17
  }, {
    start: 93847,
    length: 4,
    convRule: rule2
  }, {
    start: 93952,
    length: 75,
    convRule: rule14
  }, {
    start: 94031,
    length: 1,
    convRule: rule92
  }, {
    start: 94032,
    length: 1,
    convRule: rule14
  }, {
    start: 94033,
    length: 55,
    convRule: rule124
  }, {
    start: 94095,
    length: 4,
    convRule: rule92
  }, {
    start: 94099,
    length: 13,
    convRule: rule91
  }, {
    start: 94176,
    length: 2,
    convRule: rule91
  }, {
    start: 94178,
    length: 1,
    convRule: rule2
  }, {
    start: 94179,
    length: 1,
    convRule: rule91
  }, {
    start: 94180,
    length: 1,
    convRule: rule92
  }, {
    start: 94192,
    length: 2,
    convRule: rule124
  }, {
    start: 94208,
    length: 6136,
    convRule: rule14
  }, {
    start: 100352,
    length: 1238,
    convRule: rule14
  }, {
    start: 101632,
    length: 9,
    convRule: rule14
  }, {
    start: 110592,
    length: 287,
    convRule: rule14
  }, {
    start: 110928,
    length: 3,
    convRule: rule14
  }, {
    start: 110948,
    length: 4,
    convRule: rule14
  }, {
    start: 110960,
    length: 396,
    convRule: rule14
  }, {
    start: 113664,
    length: 107,
    convRule: rule14
  }, {
    start: 113776,
    length: 13,
    convRule: rule14
  }, {
    start: 113792,
    length: 9,
    convRule: rule14
  }, {
    start: 113808,
    length: 10,
    convRule: rule14
  }, {
    start: 113820,
    length: 1,
    convRule: rule13
  }, {
    start: 113821,
    length: 2,
    convRule: rule92
  }, {
    start: 113823,
    length: 1,
    convRule: rule2
  }, {
    start: 113824,
    length: 4,
    convRule: rule16
  }, {
    start: 118784,
    length: 246,
    convRule: rule13
  }, {
    start: 119040,
    length: 39,
    convRule: rule13
  }, {
    start: 119081,
    length: 60,
    convRule: rule13
  }, {
    start: 119141,
    length: 2,
    convRule: rule124
  }, {
    start: 119143,
    length: 3,
    convRule: rule92
  }, {
    start: 119146,
    length: 3,
    convRule: rule13
  }, {
    start: 119149,
    length: 6,
    convRule: rule124
  }, {
    start: 119155,
    length: 8,
    convRule: rule16
  }, {
    start: 119163,
    length: 8,
    convRule: rule92
  }, {
    start: 119171,
    length: 2,
    convRule: rule13
  }, {
    start: 119173,
    length: 7,
    convRule: rule92
  }, {
    start: 119180,
    length: 30,
    convRule: rule13
  }, {
    start: 119210,
    length: 4,
    convRule: rule92
  }, {
    start: 119214,
    length: 59,
    convRule: rule13
  }, {
    start: 119296,
    length: 66,
    convRule: rule13
  }, {
    start: 119362,
    length: 3,
    convRule: rule92
  }, {
    start: 119365,
    length: 1,
    convRule: rule13
  }, {
    start: 119520,
    length: 20,
    convRule: rule17
  }, {
    start: 119552,
    length: 87,
    convRule: rule13
  }, {
    start: 119648,
    length: 25,
    convRule: rule17
  }, {
    start: 119808,
    length: 26,
    convRule: rule107
  }, {
    start: 119834,
    length: 26,
    convRule: rule20
  }, {
    start: 119860,
    length: 26,
    convRule: rule107
  }, {
    start: 119886,
    length: 7,
    convRule: rule20
  }, {
    start: 119894,
    length: 18,
    convRule: rule20
  }, {
    start: 119912,
    length: 26,
    convRule: rule107
  }, {
    start: 119938,
    length: 26,
    convRule: rule20
  }, {
    start: 119964,
    length: 1,
    convRule: rule107
  }, {
    start: 119966,
    length: 2,
    convRule: rule107
  }, {
    start: 119970,
    length: 1,
    convRule: rule107
  }, {
    start: 119973,
    length: 2,
    convRule: rule107
  }, {
    start: 119977,
    length: 4,
    convRule: rule107
  }, {
    start: 119982,
    length: 8,
    convRule: rule107
  }, {
    start: 119990,
    length: 4,
    convRule: rule20
  }, {
    start: 119995,
    length: 1,
    convRule: rule20
  }, {
    start: 119997,
    length: 7,
    convRule: rule20
  }, {
    start: 120005,
    length: 11,
    convRule: rule20
  }, {
    start: 120016,
    length: 26,
    convRule: rule107
  }, {
    start: 120042,
    length: 26,
    convRule: rule20
  }, {
    start: 120068,
    length: 2,
    convRule: rule107
  }, {
    start: 120071,
    length: 4,
    convRule: rule107
  }, {
    start: 120077,
    length: 8,
    convRule: rule107
  }, {
    start: 120086,
    length: 7,
    convRule: rule107
  }, {
    start: 120094,
    length: 26,
    convRule: rule20
  }, {
    start: 120120,
    length: 2,
    convRule: rule107
  }, {
    start: 120123,
    length: 4,
    convRule: rule107
  }, {
    start: 120128,
    length: 5,
    convRule: rule107
  }, {
    start: 120134,
    length: 1,
    convRule: rule107
  }, {
    start: 120138,
    length: 7,
    convRule: rule107
  }, {
    start: 120146,
    length: 26,
    convRule: rule20
  }, {
    start: 120172,
    length: 26,
    convRule: rule107
  }, {
    start: 120198,
    length: 26,
    convRule: rule20
  }, {
    start: 120224,
    length: 26,
    convRule: rule107
  }, {
    start: 120250,
    length: 26,
    convRule: rule20
  }, {
    start: 120276,
    length: 26,
    convRule: rule107
  }, {
    start: 120302,
    length: 26,
    convRule: rule20
  }, {
    start: 120328,
    length: 26,
    convRule: rule107
  }, {
    start: 120354,
    length: 26,
    convRule: rule20
  }, {
    start: 120380,
    length: 26,
    convRule: rule107
  }, {
    start: 120406,
    length: 26,
    convRule: rule20
  }, {
    start: 120432,
    length: 26,
    convRule: rule107
  }, {
    start: 120458,
    length: 28,
    convRule: rule20
  }, {
    start: 120488,
    length: 25,
    convRule: rule107
  }, {
    start: 120513,
    length: 1,
    convRule: rule6
  }, {
    start: 120514,
    length: 25,
    convRule: rule20
  }, {
    start: 120539,
    length: 1,
    convRule: rule6
  }, {
    start: 120540,
    length: 6,
    convRule: rule20
  }, {
    start: 120546,
    length: 25,
    convRule: rule107
  }, {
    start: 120571,
    length: 1,
    convRule: rule6
  }, {
    start: 120572,
    length: 25,
    convRule: rule20
  }, {
    start: 120597,
    length: 1,
    convRule: rule6
  }, {
    start: 120598,
    length: 6,
    convRule: rule20
  }, {
    start: 120604,
    length: 25,
    convRule: rule107
  }, {
    start: 120629,
    length: 1,
    convRule: rule6
  }, {
    start: 120630,
    length: 25,
    convRule: rule20
  }, {
    start: 120655,
    length: 1,
    convRule: rule6
  }, {
    start: 120656,
    length: 6,
    convRule: rule20
  }, {
    start: 120662,
    length: 25,
    convRule: rule107
  }, {
    start: 120687,
    length: 1,
    convRule: rule6
  }, {
    start: 120688,
    length: 25,
    convRule: rule20
  }, {
    start: 120713,
    length: 1,
    convRule: rule6
  }, {
    start: 120714,
    length: 6,
    convRule: rule20
  }, {
    start: 120720,
    length: 25,
    convRule: rule107
  }, {
    start: 120745,
    length: 1,
    convRule: rule6
  }, {
    start: 120746,
    length: 25,
    convRule: rule20
  }, {
    start: 120771,
    length: 1,
    convRule: rule6
  }, {
    start: 120772,
    length: 6,
    convRule: rule20
  }, {
    start: 120778,
    length: 1,
    convRule: rule107
  }, {
    start: 120779,
    length: 1,
    convRule: rule20
  }, {
    start: 120782,
    length: 50,
    convRule: rule8
  }, {
    start: 120832,
    length: 512,
    convRule: rule13
  }, {
    start: 121344,
    length: 55,
    convRule: rule92
  }, {
    start: 121399,
    length: 4,
    convRule: rule13
  }, {
    start: 121403,
    length: 50,
    convRule: rule92
  }, {
    start: 121453,
    length: 8,
    convRule: rule13
  }, {
    start: 121461,
    length: 1,
    convRule: rule92
  }, {
    start: 121462,
    length: 14,
    convRule: rule13
  }, {
    start: 121476,
    length: 1,
    convRule: rule92
  }, {
    start: 121477,
    length: 2,
    convRule: rule13
  }, {
    start: 121479,
    length: 5,
    convRule: rule2
  }, {
    start: 121499,
    length: 5,
    convRule: rule92
  }, {
    start: 121505,
    length: 15,
    convRule: rule92
  }, {
    start: 122880,
    length: 7,
    convRule: rule92
  }, {
    start: 122888,
    length: 17,
    convRule: rule92
  }, {
    start: 122907,
    length: 7,
    convRule: rule92
  }, {
    start: 122915,
    length: 2,
    convRule: rule92
  }, {
    start: 122918,
    length: 5,
    convRule: rule92
  }, {
    start: 123136,
    length: 45,
    convRule: rule14
  }, {
    start: 123184,
    length: 7,
    convRule: rule92
  }, {
    start: 123191,
    length: 7,
    convRule: rule91
  }, {
    start: 123200,
    length: 10,
    convRule: rule8
  }, {
    start: 123214,
    length: 1,
    convRule: rule14
  }, {
    start: 123215,
    length: 1,
    convRule: rule13
  }, {
    start: 123584,
    length: 44,
    convRule: rule14
  }, {
    start: 123628,
    length: 4,
    convRule: rule92
  }, {
    start: 123632,
    length: 10,
    convRule: rule8
  }, {
    start: 123647,
    length: 1,
    convRule: rule3
  }, {
    start: 124928,
    length: 197,
    convRule: rule14
  }, {
    start: 125127,
    length: 9,
    convRule: rule17
  }, {
    start: 125136,
    length: 7,
    convRule: rule92
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }, {
    start: 125252,
    length: 7,
    convRule: rule92
  }, {
    start: 125259,
    length: 1,
    convRule: rule91
  }, {
    start: 125264,
    length: 10,
    convRule: rule8
  }, {
    start: 125278,
    length: 2,
    convRule: rule2
  }, {
    start: 126065,
    length: 59,
    convRule: rule17
  }, {
    start: 126124,
    length: 1,
    convRule: rule13
  }, {
    start: 126125,
    length: 3,
    convRule: rule17
  }, {
    start: 126128,
    length: 1,
    convRule: rule3
  }, {
    start: 126129,
    length: 4,
    convRule: rule17
  }, {
    start: 126209,
    length: 45,
    convRule: rule17
  }, {
    start: 126254,
    length: 1,
    convRule: rule13
  }, {
    start: 126255,
    length: 15,
    convRule: rule17
  }, {
    start: 126464,
    length: 4,
    convRule: rule14
  }, {
    start: 126469,
    length: 27,
    convRule: rule14
  }, {
    start: 126497,
    length: 2,
    convRule: rule14
  }, {
    start: 126500,
    length: 1,
    convRule: rule14
  }, {
    start: 126503,
    length: 1,
    convRule: rule14
  }, {
    start: 126505,
    length: 10,
    convRule: rule14
  }, {
    start: 126516,
    length: 4,
    convRule: rule14
  }, {
    start: 126521,
    length: 1,
    convRule: rule14
  }, {
    start: 126523,
    length: 1,
    convRule: rule14
  }, {
    start: 126530,
    length: 1,
    convRule: rule14
  }, {
    start: 126535,
    length: 1,
    convRule: rule14
  }, {
    start: 126537,
    length: 1,
    convRule: rule14
  }, {
    start: 126539,
    length: 1,
    convRule: rule14
  }, {
    start: 126541,
    length: 3,
    convRule: rule14
  }, {
    start: 126545,
    length: 2,
    convRule: rule14
  }, {
    start: 126548,
    length: 1,
    convRule: rule14
  }, {
    start: 126551,
    length: 1,
    convRule: rule14
  }, {
    start: 126553,
    length: 1,
    convRule: rule14
  }, {
    start: 126555,
    length: 1,
    convRule: rule14
  }, {
    start: 126557,
    length: 1,
    convRule: rule14
  }, {
    start: 126559,
    length: 1,
    convRule: rule14
  }, {
    start: 126561,
    length: 2,
    convRule: rule14
  }, {
    start: 126564,
    length: 1,
    convRule: rule14
  }, {
    start: 126567,
    length: 4,
    convRule: rule14
  }, {
    start: 126572,
    length: 7,
    convRule: rule14
  }, {
    start: 126580,
    length: 4,
    convRule: rule14
  }, {
    start: 126585,
    length: 4,
    convRule: rule14
  }, {
    start: 126590,
    length: 1,
    convRule: rule14
  }, {
    start: 126592,
    length: 10,
    convRule: rule14
  }, {
    start: 126603,
    length: 17,
    convRule: rule14
  }, {
    start: 126625,
    length: 3,
    convRule: rule14
  }, {
    start: 126629,
    length: 5,
    convRule: rule14
  }, {
    start: 126635,
    length: 17,
    convRule: rule14
  }, {
    start: 126704,
    length: 2,
    convRule: rule6
  }, {
    start: 126976,
    length: 44,
    convRule: rule13
  }, {
    start: 127024,
    length: 100,
    convRule: rule13
  }, {
    start: 127136,
    length: 15,
    convRule: rule13
  }, {
    start: 127153,
    length: 15,
    convRule: rule13
  }, {
    start: 127169,
    length: 15,
    convRule: rule13
  }, {
    start: 127185,
    length: 37,
    convRule: rule13
  }, {
    start: 127232,
    length: 13,
    convRule: rule17
  }, {
    start: 127245,
    length: 161,
    convRule: rule13
  }, {
    start: 127462,
    length: 29,
    convRule: rule13
  }, {
    start: 127504,
    length: 44,
    convRule: rule13
  }, {
    start: 127552,
    length: 9,
    convRule: rule13
  }, {
    start: 127568,
    length: 2,
    convRule: rule13
  }, {
    start: 127584,
    length: 6,
    convRule: rule13
  }, {
    start: 127744,
    length: 251,
    convRule: rule13
  }, {
    start: 127995,
    length: 5,
    convRule: rule10
  }, {
    start: 128e3,
    length: 728,
    convRule: rule13
  }, {
    start: 128736,
    length: 13,
    convRule: rule13
  }, {
    start: 128752,
    length: 13,
    convRule: rule13
  }, {
    start: 128768,
    length: 116,
    convRule: rule13
  }, {
    start: 128896,
    length: 89,
    convRule: rule13
  }, {
    start: 128992,
    length: 12,
    convRule: rule13
  }, {
    start: 129024,
    length: 12,
    convRule: rule13
  }, {
    start: 129040,
    length: 56,
    convRule: rule13
  }, {
    start: 129104,
    length: 10,
    convRule: rule13
  }, {
    start: 129120,
    length: 40,
    convRule: rule13
  }, {
    start: 129168,
    length: 30,
    convRule: rule13
  }, {
    start: 129200,
    length: 2,
    convRule: rule13
  }, {
    start: 129280,
    length: 121,
    convRule: rule13
  }, {
    start: 129402,
    length: 82,
    convRule: rule13
  }, {
    start: 129485,
    length: 135,
    convRule: rule13
  }, {
    start: 129632,
    length: 14,
    convRule: rule13
  }, {
    start: 129648,
    length: 5,
    convRule: rule13
  }, {
    start: 129656,
    length: 3,
    convRule: rule13
  }, {
    start: 129664,
    length: 7,
    convRule: rule13
  }, {
    start: 129680,
    length: 25,
    convRule: rule13
  }, {
    start: 129712,
    length: 7,
    convRule: rule13
  }, {
    start: 129728,
    length: 3,
    convRule: rule13
  }, {
    start: 129744,
    length: 7,
    convRule: rule13
  }, {
    start: 129792,
    length: 147,
    convRule: rule13
  }, {
    start: 129940,
    length: 55,
    convRule: rule13
  }, {
    start: 130032,
    length: 10,
    convRule: rule8
  }, {
    start: 131072,
    length: 42718,
    convRule: rule14
  }, {
    start: 173824,
    length: 4149,
    convRule: rule14
  }, {
    start: 177984,
    length: 222,
    convRule: rule14
  }, {
    start: 178208,
    length: 5762,
    convRule: rule14
  }, {
    start: 183984,
    length: 7473,
    convRule: rule14
  }, {
    start: 194560,
    length: 542,
    convRule: rule14
  }, {
    start: 196608,
    length: 4939,
    convRule: rule14
  }, {
    start: 917505,
    length: 1,
    convRule: rule16
  }, {
    start: 917536,
    length: 96,
    convRule: rule16
  }, {
    start: 917760,
    length: 240,
    convRule: rule92
  }, {
    start: 983040,
    length: 65534,
    convRule: rule200
  }, {
    start: 1048576,
    length: 65534,
    convRule: rule200
  }];
  var checkAttr = function(categories) {
    return function($$char2) {
      var numOfBlocks = function() {
        var $43 = $$char2 < 256;
        if ($43) {
          return numLat1Blocks;
        }
        ;
        return numBlocks;
      }();
      var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex3(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var isSpace = function(c) {
    var uc = fromEnum4(c);
    var $28 = uc <= 823;
    if ($28) {
      return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
    }
    ;
    return uIswspace(uc);
  };
  var isAlphaNum = function($70) {
    return uIswalnum(fromEnum4($70));
  };

  // output/Parsing.String.Basic/index.js
  var show121 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showChar));
  var notElem1 = /* @__PURE__ */ notElem2(eqChar);
  var satisfyCP = function(p2) {
    return satisfy(function($32) {
      return p2(codePointFromChar($32));
    });
  };
  var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
  var noneOf = function(ss) {
    return withLazyErrorMessage(satisfy(flip(notElem1)(ss)))(function(v) {
      return "none of " + show121(ss);
    });
  };
  var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

  // output/Parser.Lexer/index.js
  var bind21 = /* @__PURE__ */ bind(bindParserT);
  var show47 = /* @__PURE__ */ show(showSym);
  var pure20 = /* @__PURE__ */ pure(applicativeParserT);
  var show122 = /* @__PURE__ */ show(showKeyword);
  var elem11 = /* @__PURE__ */ elem(foldableList)(eqString);
  var map40 = /* @__PURE__ */ map(functorList);
  var show216 = /* @__PURE__ */ show(showString);
  var applySecond3 = /* @__PURE__ */ applySecond(applyParserT);
  var alt3 = /* @__PURE__ */ alt(altParserT);
  var parseSymbol = function(sym) {
    return bind21(string(show47(sym)))(function() {
      return pure20(unit);
    });
  };
  var parseKeyword = function(kw) {
    return bind21(string(show122(kw)))(function() {
      return pure20(unit);
    });
  };
  var parseIdentifier = /* @__PURE__ */ function() {
    var lsToStr = function(v) {
      if (v instanceof Nil) {
        return "";
      }
      ;
      if (v instanceof Cons) {
        var rst = lsToStr(v.value1);
        return singleton6(v.value0) + rst;
      }
      ;
      throw new Error("Failed pattern match at Parser.Lexer (line 80, column 5 - line 80, column 35): " + [v.constructor.name]);
    };
    return bind21(map(functorParserT)(function($23) {
      return lsToStr(toList2($23));
    })(many1(alphaNum)))(function(ident) {
      var $18 = elem11(ident)(map40(show122)(allKws));
      if ($18) {
        return fail("identifier cannot be a keyword, got " + show216(ident));
      }
      ;
      return pure20(ident);
    });
  }();
  var parseComment = /* @__PURE__ */ function() {
    return $$try2(bind21(parseSymbol(SymMinus.value))(function() {
      return bind21(parseSymbol(SymMinus.value))(function() {
        return bind21(manyTill(anyChar)($$char("\n")))(function() {
          return pure20(unit);
        });
      });
    }));
  }();
  var sc = /* @__PURE__ */ applySecond3(/* @__PURE__ */ many(/* @__PURE__ */ alt3(/* @__PURE__ */ applySecond3(space)(/* @__PURE__ */ pure20(unit)))(parseComment)))(/* @__PURE__ */ pure20(unit));
  var parseParens = function(p2) {
    return bind21(parseSymbol(SymParensO.value))(function() {
      return bind21(sc)(function() {
        return bind21(p2)(function(a2) {
          return bind21(sc)(function() {
            return bind21(parseSymbol(SymParensC.value))(function() {
              return pure20(a2);
            });
          });
        });
      });
    });
  };
  var sc1 = /* @__PURE__ */ applySecond3(space)(sc);
  var parseCommaSep = /* @__PURE__ */ function() {
    return bind21(parseSymbol(SymComma.value))(function() {
      return sc;
    });
  }();
  var parseAngO = /* @__PURE__ */ function() {
    return alt3(parseSymbol(SymAngO.value))(parseSymbol(SymAngOUnicode.value));
  }();
  var parseAngC = /* @__PURE__ */ function() {
    return alt3(parseSymbol(SymAngC.value))(parseSymbol(SymAngCUnicode.value));
  }();
  var getCurrPos = /* @__PURE__ */ bind21(position)(function(v) {
    return pure20({
      srcCol: v.column,
      srcLine: v.line
    });
  });
  var getCurrLoc = function(startPos) {
    return bind21(getCurrPos)(function(currPos) {
      return pure20({
        locStart: startPos,
        locEnd: currPos
      });
    });
  };

  // output/Parser.Common/index.js
  var bind22 = /* @__PURE__ */ bind(bindParserT);
  var pure21 = /* @__PURE__ */ pure(applicativeParserT);
  var alt4 = /* @__PURE__ */ alt(altParserT);
  var parseXtorname = /* @__PURE__ */ bind22(parseIdentifier)(function(nm) {
    return pure21(new Xtorname(nm));
  });
  var parseVariance = /* @__PURE__ */ function() {
    return alt4(bind22(parseSymbol(SymPlus.value))(function() {
      return pure21(Covariant.value);
    }))(bind22(parseSymbol(SymMinus.value))(function() {
      return pure21(Contravariant.value);
    }));
  }();
  var parseVariable = /* @__PURE__ */ bind22(parseIdentifier)(function(nm) {
    return pure21(new Variable(nm));
  });
  var parseTypevar = /* @__PURE__ */ bind22(parseIdentifier)(function(nm) {
    return pure21(new Typevar(nm));
  });
  var parseVariantVar = /* @__PURE__ */ bind22(parseTypevar)(function(tyv) {
    return bind22(sc)(function() {
      return bind22(parseSymbol(SymColon.value))(function() {
        return bind22(sc)(function() {
          return bind22(parseVariance)(function($$var2) {
            return pure21(new VariantVar({
              variantVar: tyv,
              variantVariance: $$var2
            }));
          });
        });
      });
    });
  });
  var parseTypename = /* @__PURE__ */ bind22(parseIdentifier)(function(nm) {
    return pure21(new Typename(nm));
  });
  var parseModulename = /* @__PURE__ */ bind22(parseIdentifier)(function(nm) {
    return pure21(new Modulename(nm));
  });
  var parseEvaluationOrder = /* @__PURE__ */ function() {
    return alt4(bind22(parseKeyword(KwCBV.value))(function() {
      return pure21(CBV.value);
    }))(bind22(parseKeyword(KwCBN.value))(function() {
      return pure21(CBN.value);
    }));
  }();
  var parseDataCodata = /* @__PURE__ */ function() {
    return alt4(bind22(parseKeyword(KwData.value))(function() {
      return pure21(Data.value);
    }))(bind22(parseKeyword(KwCodata.value))(function() {
      return pure21(Codata.value);
    }));
  }();

  // output/Parser.Types/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var bind23 = /* @__PURE__ */ bind(bindParserT);
  var pure22 = /* @__PURE__ */ pure(applicativeParserT);
  var alt5 = /* @__PURE__ */ alt(altParserT);
  var parseTyVar = /* @__PURE__ */ bind23(parseTypevar)(function($$var2) {
    return pure22(new TyVar4($$var2));
  });
  var parseTyArgs = /* @__PURE__ */ function() {
    return alt5(bind23(parseSymbol(SymParensO.value))(function() {
      return bind23(sepBy(parseVariantVar)(parseCommaSep))(function(vars) {
        return bind23(parseSymbol(SymParensC.value))(function() {
          return pure22(vars);
        });
      });
    }))(pure22(Nil.value));
  }();
  var parseFunTy = function(ty1) {
    return bind23(sc)(function() {
      return bind23(parseSymbol(SymMinus.value))(function() {
        return bind23(parseSymbol(SymAngC.value))(function() {
          return bind23(sc)(function() {
            return bind23($lazy_parseTy(38))(function(ty2) {
              return pure22(new TyFun(ty1, ty2));
            });
          });
        });
      });
    });
  };
  var $lazy_parseTyShift = /* @__PURE__ */ $runtime_lazy5("parseTyShift", "Parser.Types", function() {
    return bind23(parseSymbol(SymBrackO.value))(function() {
      return bind23(sc)(function() {
        return bind23($lazy_parseTy(80))(function(ty) {
          return bind23(sc)(function() {
            return bind23(parseSymbol(SymBrackC.value))(function() {
              return pure22(new TyShift4(ty));
            });
          });
        });
      });
    });
  });
  var $lazy_parseTy = /* @__PURE__ */ $runtime_lazy5("parseTy", "Parser.Types", function() {
    return bind23(alt5(function(v) {
      return $lazy_parseTyParens(24);
    }(unit))(alt5(function(v) {
      return $lazy_parseTyForall(25);
    }(unit))(alt5(function(v) {
      return $lazy_parseTyShift(26);
    }(unit))(alt5(function(v) {
      return $lazy_parseTyCo(27);
    }(unit))(alt5(function(v) {
      return $$try2($lazy_parseTyDecl(28));
    }(unit))(/* @__PURE__ */ function(v) {
      return parseTyVar;
    }(unit)))))))(function(ty) {
      return alt5($$try2(parseFunTy(ty)))(pure22(ty));
    });
  });
  var $lazy_parseTyCo = /* @__PURE__ */ $runtime_lazy5("parseTyCo", "Parser.Types", function() {
    return bind23(alt5(parseKeyword(KwCo.value))(parseKeyword(Kwco.value)))(function() {
      return bind23(sc)(function() {
        return bind23($lazy_parseTy(89))(function(ty) {
          return pure22(new TyCo4(ty));
        });
      });
    });
  });
  var $lazy_parseTyDecl = /* @__PURE__ */ $runtime_lazy5("parseTyDecl", "Parser.Types", function() {
    return bind23(parseTypename)(function(tyn) {
      return bind23(parseSymbol(SymParensO.value))(function() {
        return bind23(sepBy($lazy_parseTy(67))(parseCommaSep))(function(args) {
          return bind23(parseSymbol(SymParensC.value))(function() {
            return pure22(new TyDecl4(tyn, args));
          });
        });
      });
    });
  });
  var $lazy_parseTyForall = /* @__PURE__ */ $runtime_lazy5("parseTyForall", "Parser.Types", function() {
    return bind23(alt5(parseKeyword(KwForall.value))(parseKeyword(Kwforall.value)))(function() {
      return bind23(sc)(function() {
        return bind23(sepBy(parseTypevar)(many1(space)))(function(args) {
          return bind23(sc)(function() {
            return bind23(parseSymbol(SymDot.value))(function() {
              return bind23(sc)(function() {
                return bind23($lazy_parseTy(59))(function(ty) {
                  return pure22(new TyForall4(args, ty));
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseTyParens = /* @__PURE__ */ $runtime_lazy5("parseTyParens", "Parser.Types", function() {
    return bind23(parseSymbol(SymParensO.value))(function() {
      return bind23(sc)(function() {
        return bind23($lazy_parseTy(46))(function(ty) {
          return bind23(sc)(function() {
            return bind23(parseSymbol(SymParensC.value))(function() {
              return pure22(ty);
            });
          });
        });
      });
    });
  });
  var parseTy = /* @__PURE__ */ $lazy_parseTy(22);

  // output/Parser.Terms/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var bind24 = /* @__PURE__ */ bind(bindParserT);
  var pure23 = /* @__PURE__ */ pure(applicativeParserT);
  var alt6 = /* @__PURE__ */ alt(altParserT);
  var parseVar = /* @__PURE__ */ bind24(getCurrPos)(function(startPos) {
    return bind24(parseVariable)(function(v) {
      return bind24(getCurrLoc(startPos))(function(loc) {
        return pure23(new Var4(loc, v));
      });
    });
  });
  var parseErr = /* @__PURE__ */ function() {
    var charlsToStr = function(v) {
      if (v instanceof Nil) {
        return "";
      }
      ;
      if (v instanceof Cons) {
        return singleton6(v.value0) + charlsToStr(v.value1);
      }
      ;
      throw new Error("Failed pattern match at Parser.Terms (line 295, column 5 - line 295, column 39): " + [v.constructor.name]);
    };
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseKeyword(KwError.value))(function() {
        return bind24(sc)(function() {
          return bind24(parseSymbol(SymQuot.value))(function() {
            return bind24(many(noneOf(['"'])))(function(msg) {
              return bind24(parseSymbol(SymQuot.value))(function() {
                return bind24(getCurrLoc(startPos))(function(loc) {
                  return pure23(new Err4(loc, charlsToStr(msg)));
                });
              });
            });
          });
        });
      });
    });
  }();
  var parseDone = /* @__PURE__ */ bind24(getCurrPos)(function(startPos) {
    return bind24(parseKeyword(KwDone.value))(function() {
      return bind24(getCurrLoc(startPos))(function(loc) {
        return pure23(new Done5(loc));
      });
    });
  });
  var parseCutAnnot = /* @__PURE__ */ alt6(/* @__PURE__ */ $$try2(/* @__PURE__ */ bind24(parseTy)(function(ty) {
    return bind24(sc)(function() {
      return bind24(parseSymbol(SymColon.value))(function() {
        return bind24(sc)(function() {
          return bind24(parseEvaluationOrder)(function(eo) {
            return pure23(new Tuple(eo, new Just(ty)));
          });
        });
      });
    });
  })))(/* @__PURE__ */ bind24(parseEvaluationOrder)(function(eo) {
    return pure23(new Tuple(eo, Nothing.value));
  }));
  var parseSeq = function(t1) {
    return bind24(sc)(function() {
      return bind24(getCurrPos)(function(startPos) {
        return bind24(parseSymbol(SymSemi.value))(function() {
          return bind24(sc)(function() {
            return bind24($lazy_parseTerm(38))(function(t2) {
              return bind24(getCurrLoc(startPos))(function(loc) {
                return pure23(new Seq(loc, t1, t2));
              });
            });
          });
        });
      });
    });
  };
  var parseOr = function(t1) {
    return bind24(sc)(function() {
      return bind24(getCurrPos)(function(startPos) {
        return bind24(parseSymbol(SymBar.value))(function() {
          return bind24(parseSymbol(SymBar.value))(function() {
            return bind24(sc)(function() {
              return bind24($lazy_parseTerm(72))(function(t2) {
                return bind24(getCurrLoc(startPos))(function(loc) {
                  return pure23(new OrBool(loc, t1, t2));
                });
              });
            });
          });
        });
      });
    });
  };
  var parseApp = function(t1) {
    return bind24(sc)(function() {
      return bind24(getCurrPos)(function(startPos) {
        return bind24(parseSymbol(SymSqBrackO.value))(function() {
          return bind24(sc)(function() {
            return bind24($lazy_parseTerm(48))(function(t2) {
              return bind24(sc)(function() {
                return bind24(parseSymbol(SymSqBrackC.value))(function() {
                  return bind24(getCurrLoc(startPos))(function(loc) {
                    return pure23(new App2(loc, t1, t2));
                  });
                });
              });
            });
          });
        });
      });
    });
  };
  var parseAnd = function(t1) {
    return bind24(sc)(function() {
      return bind24(getCurrPos)(function(startPos) {
        return bind24(parseSymbol(SymAmper.value))(function() {
          return bind24(parseSymbol(SymAmper.value))(function() {
            return bind24(sc)(function() {
              return bind24($lazy_parseTerm(61))(function(t2) {
                return bind24(getCurrLoc(startPos))(function(loc) {
                  return pure23(new AndBool(loc, t1, t2));
                });
              });
            });
          });
        });
      });
    });
  };
  var $lazy_parseXtor = /* @__PURE__ */ $runtime_lazy6("parseXtor", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseXtorname)(function(nm) {
        return bind24(sc)(function() {
          return bind24(parseSymbol(SymParensO.value))(function() {
            return bind24(sc)(function() {
              return bind24(sepBy($lazy_parseTerm(185))(parseCommaSep))(function(args) {
                return bind24(sc)(function() {
                  return bind24(parseSymbol(SymParensC.value))(function() {
                    return bind24(getCurrLoc(startPos))(function(loc) {
                      return pure23(new Xtor4(loc, nm, args));
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseC = /* @__PURE__ */ $runtime_lazy6("parseC", "Parser.Terms", function() {
    return alt6(/* @__PURE__ */ function(v) {
      return parseErr;
    }(unit))(alt6(/* @__PURE__ */ function(v) {
      return parseDone;
    }(unit))(alt6(function(v) {
      return $lazy_parseCut(232);
    }(unit))(alt6(function(v) {
      return $lazy_parseCaseOf(233);
    }(unit))(alt6(function(v) {
      return $$try2($lazy_parseCutCBV(234));
    }(unit))(alt6(function(v) {
      return $$try2($lazy_parseCutCBN(235));
    }(unit))(alt6(function(v) {
      return $$try2($lazy_parsePrint(236));
    }(unit))(function(v) {
      return $$try2($lazy_parsePrintAnnot(237));
    }(unit))))))));
  });
  var $lazy_parseCaseOf = /* @__PURE__ */ $runtime_lazy6("parseCaseOf", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwCase.value))(parseKeyword(Kwcase.value)))(function() {
        return bind24(sc1)(function() {
          return bind24($lazy_parseTerm(244))(function(t) {
            return bind24(sc1)(function() {
              return bind24(alt6(parseKeyword(KwOf.value))(parseKeyword(Kwof.value)))(function() {
                return bind24(sc)(function() {
                  return bind24(parseSymbol(SymBrackO.value))(function() {
                    return bind24(sc)(function() {
                      return bind24(sepBy($lazy_parsePattern(250))(parseCommaSep))(function(pts) {
                        return bind24(sc)(function() {
                          return bind24(parseSymbol(SymBrackC.value))(function() {
                            return bind24(getCurrLoc(startPos))(function(loc) {
                              return pure23(new CaseOf(loc, t, pts));
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseCommand = /* @__PURE__ */ $runtime_lazy6("parseCommand", "Parser.Terms", function() {
    return alt6(parseParens(function(v) {
      return $lazy_parseC(226);
    }(unit)))(function(v) {
      return $lazy_parseC(226);
    }(unit));
  });
  var $lazy_parseCut = /* @__PURE__ */ $runtime_lazy6("parseCut", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseAngO)(function() {
        return bind24(sc)(function() {
          return bind24($lazy_parseTerm(261))(function(t) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymBar.value))(function() {
                return bind24(sc)(function() {
                  return bind24(parseCutAnnot)(function(v) {
                    return bind24(sc)(function() {
                      return bind24(parseSymbol(SymBar.value))(function() {
                        return bind24(sc)(function() {
                          return bind24($lazy_parseTerm(269))(function(u2) {
                            return bind24(sc)(function() {
                              return bind24(parseAngC)(function() {
                                return bind24(getCurrLoc(startPos))(function(loc) {
                                  if (v.value1 instanceof Nothing) {
                                    return pure23(new Cut4(loc, t, v.value0, u2));
                                  }
                                  ;
                                  if (v.value1 instanceof Just) {
                                    return pure23(new CutAnnot2(loc, t, v.value1.value0, v.value0, u2));
                                  }
                                  ;
                                  throw new Error("Failed pattern match at Parser.Terms (line 273, column 3 - line 275, column 46): " + [v.value1.constructor.name]);
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseCutCBN = /* @__PURE__ */ $runtime_lazy6("parseCutCBN", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24($lazy_parseTerm(348))(function(t) {
        return bind24(sc)(function() {
          return bind24(parseAngO)(function() {
            return bind24(parseAngO)(function() {
              return bind24(sc)(function() {
                return bind24(optionMaybe($$try2(bind24(parseTy)(function(ty) {
                  return bind24(sc)(function() {
                    return bind24(parseAngO)(function() {
                      return bind24(parseAngO)(function() {
                        return bind24(sc)(function() {
                          return pure23(ty);
                        });
                      });
                    });
                  });
                }))))(function(mty) {
                  return bind24($lazy_parseTerm(360))(function(u2) {
                    return bind24(getCurrLoc(startPos))(function(loc) {
                      if (mty instanceof Nothing) {
                        return pure23(new Cut4(loc, t, CBN.value, u2));
                      }
                      ;
                      if (mty instanceof Just) {
                        return pure23(new CutAnnot2(loc, t, mty.value0, CBN.value, u2));
                      }
                      ;
                      throw new Error("Failed pattern match at Parser.Terms (line 362, column 3 - line 364, column 46): " + [mty.constructor.name]);
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseCutCBV = /* @__PURE__ */ $runtime_lazy6("parseCutCBV", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24($lazy_parseTerm(327))(function(t) {
        return bind24(sc)(function() {
          return bind24(parseAngC)(function() {
            return bind24(parseAngC)(function() {
              return bind24(sc)(function() {
                return bind24(optionMaybe($$try2(bind24(parseTy)(function(ty) {
                  return bind24(sc)(function() {
                    return bind24(parseAngC)(function() {
                      return bind24(parseAngC)(function() {
                        return bind24(sc)(function() {
                          return pure23(ty);
                        });
                      });
                    });
                  });
                }))))(function(mty) {
                  return bind24($lazy_parseTerm(339))(function(u2) {
                    return bind24(getCurrLoc(startPos))(function(loc) {
                      if (mty instanceof Nothing) {
                        return pure23(new Cut4(loc, t, CBV.value, u2));
                      }
                      ;
                      if (mty instanceof Just) {
                        return pure23(new CutAnnot2(loc, t, mty.value0, CBV.value, u2));
                      }
                      ;
                      throw new Error("Failed pattern match at Parser.Terms (line 341, column 5 - line 343, column 48): " + [mty.constructor.name]);
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseIf = /* @__PURE__ */ $runtime_lazy6("parseIf", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwIf.value))(parseKeyword(Kwif.value)))(function() {
        return bind24(sc1)(function() {
          return bind24($lazy_parseTerm(94))(function(b2) {
            return bind24(sc1)(function() {
              return bind24(alt6(parseKeyword(KwThen.value))(parseKeyword(Kwthen.value)))(function() {
                return bind24(sc1)(function() {
                  return bind24($lazy_parseTerm(98))(function(t1) {
                    return bind24(sc1)(function() {
                      return bind24(alt6(parseKeyword(KwElse.value))(parseKeyword(Kwelse.value)))(function() {
                        return bind24(sc1)(function() {
                          return bind24($lazy_parseTerm(102))(function(t2) {
                            return bind24(getCurrLoc(startPos))(function(loc) {
                              return pure23(new IfThenElse(loc, b2, t1, t2));
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseLam = /* @__PURE__ */ $runtime_lazy6("parseLam", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseSymbol(SymBackSl.value))(parseSymbol(SymLambda.value)))(function() {
        return bind24(parseVariable)(function(v) {
          return bind24(sc)(function() {
            return bind24(parseSymbol(SymDot.value))(function() {
              return bind24(sc)(function() {
                return bind24($lazy_parseTerm(199))(function(t) {
                  return bind24(getCurrLoc(startPos))(function(loc) {
                    return pure23(new Lam(loc, v, t));
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseLst = /* @__PURE__ */ $runtime_lazy6("parseLst", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseSymbol(SymSqBrackO.value))(function() {
        return bind24(sc)(function() {
          return bind24(sepBy($lazy_parseTerm(130))(parseCommaSep))(function(ts) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymSqBrackC.value))(function() {
                return bind24(getCurrLoc(startPos))(function(loc) {
                  return pure23(new Lst(loc, ts));
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseMu = /* @__PURE__ */ $runtime_lazy6("parseMu", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwMu.value))(alt6(parseKeyword(Kwmu.value))(parseSymbol(SymMu.value))))(function() {
        return bind24(sc)(function() {
          return bind24(parseVariable)(function(v) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymDot.value))(function() {
                return bind24(sc)(function() {
                  return bind24($lazy_parseCommand(145))(function(c) {
                    return bind24(getCurrLoc(startPos))(function(loc) {
                      return pure23(new Mu4(loc, v, c));
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseNot = /* @__PURE__ */ $runtime_lazy6("parseNot", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseSymbol(SymExcl.value))(function() {
        return bind24($lazy_parseTerm(110))(function(t) {
          return bind24(getCurrLoc(startPos))(function(loc) {
            return pure23(new NotBool(loc, t));
          });
        });
      });
    });
  });
  var $lazy_parsePattern = /* @__PURE__ */ $runtime_lazy6("parsePattern", "Parser.Terms", function() {
    return bind24(parseXtorname)(function(nm) {
      return bind24(sc)(function() {
        return bind24(optionMaybe(parseParens(sepBy(parseVariable)(parseCommaSep))))(function(args) {
          return bind24(sc)(function() {
            return bind24(parseSymbol(SymEq.value))(function() {
              return bind24(parseAngC)(function() {
                return bind24(sc)(function() {
                  return bind24($lazy_parseCommand(220))(function(c) {
                    if (args instanceof Nothing) {
                      return pure23(new Pattern4({
                        ptxt: nm,
                        ptv: Nil.value,
                        ptcmd: c
                      }));
                    }
                    ;
                    if (args instanceof Just) {
                      return pure23(new Pattern4({
                        ptxt: nm,
                        ptv: args.value0,
                        ptcmd: c
                      }));
                    }
                    ;
                    throw new Error("Failed pattern match at Parser.Terms (line 221, column 3 - line 223, column 61): " + [args.constructor.name]);
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parsePrint = /* @__PURE__ */ $runtime_lazy6("parsePrint", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwPrint.value))(parseKeyword(Kwprint.value)))(function() {
        return bind24(sc)(function() {
          return bind24($lazy_parseTerm(319))(function(t) {
            return bind24(sc)(function() {
              return bind24(getCurrLoc(startPos))(function(loc) {
                return pure23(new Print4(loc, t));
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parsePrintAnnot = /* @__PURE__ */ $runtime_lazy6("parsePrintAnnot", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwPrint.value))(parseKeyword(Kwprint.value)))(function() {
        return bind24(sc)(function() {
          return bind24($lazy_parseTerm(305))(function(t) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymColon.value))(function() {
                return bind24(parseSymbol(SymColon.value))(function() {
                  return bind24(sc)(function() {
                    return bind24(parseTy)(function(ty) {
                      return bind24(getCurrLoc(startPos))(function(loc) {
                        return pure23(new PrintAnnot2(loc, t, ty));
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseShift = /* @__PURE__ */ $runtime_lazy6("parseShift", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseSymbol(SymBrackO.value))(function() {
        return bind24(sc)(function() {
          return bind24($lazy_parseTerm(167))(function(t) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymColon.value))(function() {
                return bind24(sc)(function() {
                  return bind24(parseEvaluationOrder)(function(eo) {
                    return bind24(parseSymbol(SymBrackC.value))(function() {
                      return bind24(getCurrLoc(startPos))(function(loc) {
                        if (eo instanceof CBV) {
                          return pure23(new ShiftCBV4(loc, t));
                        }
                        ;
                        if (eo instanceof CBN) {
                          return pure23(new ShiftCBN4(loc, t));
                        }
                        ;
                        throw new Error("Failed pattern match at Parser.Terms (line 174, column 3 - line 176, column 33): " + [eo.constructor.name]);
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseT = /* @__PURE__ */ $runtime_lazy6("parseT", "Parser.Terms", function() {
    return alt6(function(v) {
      return $lazy_parseMu(78);
    }(unit))(alt6(function(v) {
      return $lazy_parseXCase(79);
    }(unit))(alt6(function(v) {
      return $lazy_parseShift(80);
    }(unit))(alt6(function(v) {
      return $lazy_parseLam(81);
    }(unit))(alt6(function(v) {
      return $lazy_parseLst(82);
    }(unit))(alt6(function(v) {
      return $lazy_parseNot(83);
    }(unit))(alt6(function(v) {
      return $lazy_parseIf(84);
    }(unit))(alt6(function(v) {
      return $$try2($lazy_parseTup(85));
    }(unit))(alt6(function(v) {
      return $$try2($lazy_parseXtor(86));
    }(unit))(/* @__PURE__ */ function(v) {
      return parseVar;
    }(unit))))))))));
  });
  var $lazy_parseTerm = /* @__PURE__ */ $runtime_lazy6("parseTerm", "Parser.Terms", function() {
    return bind24(alt6(function(v) {
      return $lazy_parseT(29);
    }(unit))(parseParens(function(v) {
      return $lazy_parseT(29);
    }(unit))))(function(t) {
      return alt6($$try2(parseSeq(t)))(alt6($$try2(parseApp(t)))(alt6($$try2(parseAnd(t)))(alt6($$try2(parseOr(t)))(pure23(t)))));
    });
  });
  var $lazy_parseTup = /* @__PURE__ */ $runtime_lazy6("parseTup", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(parseSymbol(SymParensO.value))(function() {
        return bind24(sc)(function() {
          return bind24(sepBy($lazy_parseTerm(119))(parseCommaSep))(function(ts) {
            return bind24(sc)(function() {
              return bind24(parseSymbol(SymParensC.value))(function() {
                return bind24(getCurrLoc(startPos))(function(loc) {
                  return pure23(new Tup(loc, ts));
                });
              });
            });
          });
        });
      });
    });
  });
  var $lazy_parseXCase = /* @__PURE__ */ $runtime_lazy6("parseXCase", "Parser.Terms", function() {
    return bind24(getCurrPos)(function(startPos) {
      return bind24(alt6(parseKeyword(KwCase.value))(parseKeyword(Kwcase.value)))(function() {
        return bind24(sc)(function() {
          return bind24(parseSymbol(SymBrackO.value))(function() {
            return bind24(sc)(function() {
              return bind24(sepBy($lazy_parsePattern(156))(parseCommaSep))(function(pts) {
                return bind24(sc)(function() {
                  return bind24(parseSymbol(SymBrackC.value))(function() {
                    return bind24(getCurrLoc(startPos))(function(loc) {
                      return pure23(new XCase4(loc, pts));
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var parseCommand = /* @__PURE__ */ $lazy_parseCommand(225);
  var parseTerm = /* @__PURE__ */ $lazy_parseTerm(27);

  // output/Syntax.Parsed.Program/index.js
  var show48 = /* @__PURE__ */ show(showXtorname);
  var intercalate17 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map41 = /* @__PURE__ */ map(functorList);
  var show123 = /* @__PURE__ */ show(showTy4);
  var show217 = /* @__PURE__ */ show(showVariable);
  var show311 = /* @__PURE__ */ show(showTerm4);
  var show49 = /* @__PURE__ */ show(showModulename);
  var show53 = /* @__PURE__ */ show(showDeclTy);
  var show63 = /* @__PURE__ */ show(showTypename);
  var show73 = /* @__PURE__ */ show(showVariantVar);
  var eq29 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqCommand3));
  var show83 = /* @__PURE__ */ show(/* @__PURE__ */ showList(showCommand4));
  var XtorSig4 = /* @__PURE__ */ function() {
    function XtorSig5(value0) {
      this.value0 = value0;
    }
    ;
    XtorSig5.create = function(value0) {
      return new XtorSig5(value0);
    };
    return XtorSig5;
  }();
  var VarDecl4 = /* @__PURE__ */ function() {
    function VarDecl5(value0) {
      this.value0 = value0;
    }
    ;
    VarDecl5.create = function(value0) {
      return new VarDecl5(value0);
    };
    return VarDecl5;
  }();
  var Import = /* @__PURE__ */ function() {
    function Import2(value0) {
      this.value0 = value0;
    }
    ;
    Import2.create = function(value0) {
      return new Import2(value0);
    };
    return Import2;
  }();
  var DataDecl3 = /* @__PURE__ */ function() {
    function DataDecl4(value0) {
      this.value0 = value0;
    }
    ;
    DataDecl4.create = function(value0) {
      return new DataDecl4(value0);
    };
    return DataDecl4;
  }();
  var AnnotDecl = /* @__PURE__ */ function() {
    function AnnotDecl2(value0) {
      this.value0 = value0;
    }
    ;
    AnnotDecl2.create = function(value0) {
      return new AnnotDecl2(value0);
    };
    return AnnotDecl2;
  }();
  var Program3 = /* @__PURE__ */ function() {
    function Program4(value0) {
      this.value0 = value0;
    }
    ;
    Program4.create = function(value0) {
      return new Program4(value0);
    };
    return Program4;
  }();
  var showXtorSig2 = {
    show: function(v) {
      if ($$null(v.value0.sigArgs)) {
        return show48(v.value0.sigName);
      }
      ;
      return show48(v.value0.sigName) + ("(" + (intercalate17(", ")(map41(show123)(v.value0.sigArgs)) + ")"));
    }
  };
  var show93 = /* @__PURE__ */ show(showXtorSig2);
  var showVarDecl2 = {
    show: function(v) {
      if (v.value0.varIsRec) {
        return "rec " + (show217(v.value0.varName) + (" := " + (show311(v.value0.varBody) + ";")));
      }
      ;
      return show217(v.value0.varName) + (":=" + (show311(v.value0.varBody) + ";"));
    }
  };
  var showImport = {
    show: function(v) {
      return show49(v.value0.importName);
    }
  };
  var showDataDecl2 = {
    show: function(v) {
      if ($$null(v.value0.declArgs)) {
        return show53(v.value0.declType) + (" " + (show63(v.value0.declName) + ("{" + (intercalate17(", ")(map41(show93)(v.value0.declXtors)) + "}"))));
      }
      ;
      return show53(v.value0.declType) + (" " + (show63(v.value0.declName) + ("(" + (intercalate17(", ")(map41(show73)(v.value0.declArgs)) + (")  {" + (intercalate17(",  ")(map41(show93)(v.value0.declXtors)) + "}"))))));
    }
  };
  var showAnnotDecl = {
    show: function(v) {
      return show217(v.value0.annotName) + (" :: " + show123(v.value0.annotType));
    }
  };
  var showList3 = function(dictShow) {
    var show102 = show(dictShow);
    return function(ls) {
      return intercalate17("\n		")(map41(show102)(ls));
    };
  };
  var showList1 = /* @__PURE__ */ showList3(showImport);
  var showList22 = /* @__PURE__ */ showList3(showDataDecl2);
  var showList32 = /* @__PURE__ */ showList3(showVarDecl2);
  var showList4 = /* @__PURE__ */ showList3(showAnnotDecl);
  var showProgram3 = {
    show: function(v) {
      if (eq29(Nil.value)(v.value0.progMain)) {
        return "	module " + (show49(v.value0.progName) + ("\n	Imports:\n		" + (showList1(v.value0.progImports) + ("\n	Declarations:\n		" + (showList22(v.value0.progDecls) + ("\n	Variables:\n		" + (showList32(v.value0.progVars) + ("\n	Annotations:\n		" + showList32(v.value0.progVars)))))))));
      }
      ;
      return "	module " + (show49(v.value0.progName) + ("\n	Imports:\n		" + (showList1(v.value0.progImports) + ("\n	Declarations:\n		" + (showList22(v.value0.progDecls) + ("\n	Variables:\n		" + (showList32(v.value0.progVars) + ("\n	Annotations:\n		" + (showList4(v.value0.progAnnots) + ("\n	Main: " + show83(v.value0.progMain)))))))))));
    }
  };
  var emptyProg3 = function(mn) {
    return function(src9) {
      return new Program3({
        progName: mn,
        progDecls: Nil.value,
        progVars: Nil.value,
        progAnnots: Nil.value,
        progImports: Nil.value,
        progMain: Nil.value,
        progSrc: src9
      });
    };
  };
  var addVarProgram3 = function($$var2) {
    return function(v) {
      return new Program3({
        progName: v.value0.progName,
        progDecls: v.value0.progDecls,
        progAnnots: v.value0.progAnnots,
        progImports: v.value0.progImports,
        progMain: v.value0.progMain,
        progSrc: v.value0.progSrc,
        progVars: new Cons($$var2, v.value0.progVars)
      });
    };
  };
  var addMainProgram = function(c) {
    return function(v) {
      return new Program3({
        progName: v.value0.progName,
        progDecls: v.value0.progDecls,
        progVars: v.value0.progVars,
        progAnnots: v.value0.progAnnots,
        progImports: v.value0.progImports,
        progSrc: v.value0.progSrc,
        progMain: new Cons(c, v.value0.progMain)
      });
    };
  };
  var addImportProgram = function(imp) {
    return function(v) {
      return new Program3({
        progName: v.value0.progName,
        progDecls: v.value0.progDecls,
        progVars: v.value0.progVars,
        progAnnots: v.value0.progAnnots,
        progMain: v.value0.progMain,
        progSrc: v.value0.progSrc,
        progImports: new Cons(imp, v.value0.progImports)
      });
    };
  };
  var addDeclProgram3 = function(decl) {
    return function(v) {
      return new Program3({
        progName: v.value0.progName,
        progVars: v.value0.progVars,
        progAnnots: v.value0.progAnnots,
        progImports: v.value0.progImports,
        progMain: v.value0.progMain,
        progSrc: v.value0.progSrc,
        progDecls: new Cons(decl, v.value0.progDecls)
      });
    };
  };
  var addAnnotProgram = function(annot) {
    return function(v) {
      return new Program3({
        progName: v.value0.progName,
        progDecls: v.value0.progDecls,
        progVars: v.value0.progVars,
        progImports: v.value0.progImports,
        progMain: v.value0.progMain,
        progSrc: v.value0.progSrc,
        progAnnots: new Cons(annot, v.value0.progAnnots)
      });
    };
  };

  // output/Parser.Program/index.js
  var bind25 = /* @__PURE__ */ bind(bindParserT);
  var pure24 = /* @__PURE__ */ pure(applicativeParserT);
  var applyFirst2 = /* @__PURE__ */ applyFirst(applyParserT);
  var alt7 = /* @__PURE__ */ alt(altParserT);
  var map42 = /* @__PURE__ */ map(functorParserT);
  var foldr9 = /* @__PURE__ */ foldr(foldableList);
  var parseXtorSig = /* @__PURE__ */ bind25(getCurrPos)(function(startPos) {
    return bind25(parseXtorname)(function(nm) {
      return bind25(optionMaybe(parseParens(sepBy(parseTy)(parseCommaSep))))(function(args) {
        return bind25(getCurrLoc(startPos))(function(loc) {
          if (args instanceof Nothing) {
            return pure24(new XtorSig4({
              sigPos: loc,
              sigName: nm,
              sigArgs: Nil.value
            }));
          }
          ;
          if (args instanceof Just) {
            return pure24(new XtorSig4({
              sigPos: loc,
              sigName: nm,
              sigArgs: args.value0
            }));
          }
          ;
          throw new Error("Failed pattern match at Parser.Program (line 132, column 3 - line 134, column 73): " + [args.constructor.name]);
        });
      });
    });
  });
  var parseVarDecl = /* @__PURE__ */ bind25(getCurrPos)(function(startPos) {
    return bind25(optionMaybe(applyFirst2(parseKeyword(KwRec.value))(sc1)))(function(isRec) {
      return bind25(sc)(function() {
        return bind25(parseVariable)(function(nm) {
          return bind25(sc)(function() {
            return bind25(parseSymbol(SymColon.value))(function() {
              return bind25(parseSymbol(SymEq.value))(function() {
                return bind25(sc)(function() {
                  return bind25(parseTerm)(function(t) {
                    return bind25(sc)(function() {
                      return bind25(getCurrLoc(startPos))(function(loc) {
                        return pure24(new VarDecl4({
                          varPos: loc,
                          varName: nm,
                          varIsRec: isJust(isRec),
                          varBody: t
                        }));
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var parseTypeAnnot = /* @__PURE__ */ bind25(getCurrPos)(function(startPos) {
    return bind25(parseVariable)(function(nm) {
      return bind25(sc)(function() {
        return bind25(parseSymbol(SymColon.value))(function() {
          return bind25(parseSymbol(SymColon.value))(function() {
            return bind25(sc)(function() {
              return bind25(parseTy)(function(ty) {
                return bind25(sc)(function() {
                  return bind25(getCurrLoc(startPos))(function(loc) {
                    return pure24(new AnnotDecl({
                      annotPos: loc,
                      annotName: nm,
                      annotType: ty
                    }));
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var parseModuleDecl = /* @__PURE__ */ function() {
    return alt7(bind25(sc)(function() {
      return bind25(parseKeyword(KwModule.value))(function() {
        return bind25(sc1)(function() {
          return parseModulename;
        });
      });
    }))(pure24(new Modulename("")));
  }();
  var parseMain = /* @__PURE__ */ function() {
    return bind25(alt7(parseKeyword(KwMain.value))(parseKeyword(Kwmain.value)))(function() {
      return bind25(sc)(function() {
        return bind25(parseSymbol(SymColon.value))(function() {
          return bind25(parseSymbol(SymEq.value))(function() {
            return bind25(sc)(function() {
              return bind25(parseCommand)(function(c) {
                return bind25(sc)(function() {
                  return pure24(c);
                });
              });
            });
          });
        });
      });
    });
  }();
  var parseImport = /* @__PURE__ */ bind25(getCurrPos)(function(startPos) {
    return bind25(parseKeyword(KwImport.value))(function() {
      return bind25(sc1)(function() {
        return bind25(parseModulename)(function(mn) {
          return bind25(sc)(function() {
            return bind25(getCurrLoc(startPos))(function(loc) {
              return pure24(new Import({
                importPos: loc,
                importName: mn
              }));
            });
          });
        });
      });
    });
  });
  var parseDataDecl = /* @__PURE__ */ bind25(getCurrPos)(function(startPos) {
    return bind25(parseDataCodata)(function(isco) {
      return bind25(sc1)(function() {
        return bind25(parseTypename)(function(nm) {
          return bind25(sc)(function() {
            return bind25(parseTyArgs)(function(args) {
              return bind25(sc)(function() {
                return bind25(parseSymbol(SymBrackO.value))(function() {
                  return bind25(sc)(function() {
                    return bind25(sepBy(parseXtorSig)(parseCommaSep))(function(xtors) {
                      return bind25(sc)(function() {
                        return bind25(parseSymbol(SymBrackC.value))(function() {
                          return bind25(getCurrLoc(startPos))(function(loc) {
                            return pure24(new DataDecl3({
                              declPos: loc,
                              declName: nm,
                              declArgs: args,
                              declType: isco,
                              declXtors: xtors
                            }));
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
  var parseDecl = /* @__PURE__ */ function() {
    return alt7(map42(MkI.create)(parseImport))(alt7(map42(MkD.create)(parseDataDecl))(alt7(map42(MkM.create)(parseMain))(alt7(map42(MkV.create)($$try2(parseVarDecl)))(map42(MkA.create)($$try2(parseTypeAnnot))))));
  }();
  var parseProgram = function(src9) {
    var foldFun = function(v) {
      if (v instanceof MkD) {
        return addDeclProgram3(v.value0);
      }
      ;
      if (v instanceof MkV) {
        return addVarProgram3(v.value0);
      }
      ;
      if (v instanceof MkA) {
        return addAnnotProgram(v.value0);
      }
      ;
      if (v instanceof MkI) {
        return addImportProgram(v.value0);
      }
      ;
      if (v instanceof MkM) {
        return addMainProgram(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser.Program (line 44, column 5 - line 44, column 47): " + [v.constructor.name]);
    };
    return bind25(parseModuleDecl)(function(nm) {
      return bind25(sc)(function() {
        return bind25(manyTill(bind25(parseDecl)(function(decl) {
          return bind25(sc)(function() {
            return pure24(decl);
          });
        }))(eof))(function(decls) {
          var progRes = foldr9(foldFun)(emptyProg3(nm)(src9))(decls);
          return pure24(progRes);
        });
      });
    });
  };

  // output/SolveConstraints.Errors/index.js
  var show50 = /* @__PURE__ */ show(showTypename);
  var show124 = /* @__PURE__ */ show(showTy2);
  var show218 = /* @__PURE__ */ show(showKind);
  var ErrTyArity2 = /* @__PURE__ */ function() {
    function ErrTyArity3(value0) {
      this.value0 = value0;
    }
    ;
    ErrTyArity3.create = function(value0) {
      return new ErrTyArity3(value0);
    };
    return ErrTyArity3;
  }();
  var ErrTyNeq = /* @__PURE__ */ function() {
    function ErrTyNeq2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTyNeq2.create = function(value0) {
      return function(value1) {
        return new ErrTyNeq2(value0, value1);
      };
    };
    return ErrTyNeq2;
  }();
  var ErrKindNeq2 = /* @__PURE__ */ function() {
    function ErrKindNeq4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrKindNeq4.create = function(value0) {
      return function(value1) {
        return new ErrKindNeq4(value0, value1);
      };
    };
    return ErrKindNeq4;
  }();
  var ErrTypeKindNeq = /* @__PURE__ */ function() {
    function ErrTypeKindNeq2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTypeKindNeq2.create = function(value0) {
      return function(value1) {
        return new ErrTypeKindNeq2(value0, value1);
      };
    };
    return ErrTypeKindNeq2;
  }();
  var ErrOther8 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var errorSolverError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrTyArity2) {
          return "Wrong number of type arguments for type " + show50(v.value0);
        }
        ;
        if (v instanceof ErrTyNeq) {
          return "Types " + (show124(v.value0) + (" and " + (show124(v.value1) + " are not equal")));
        }
        ;
        if (v instanceof ErrKindNeq2) {
          return "Kinds " + (show218(v.value0) + (" and " + (show218(v.value1) + " are not equal")));
        }
        ;
        if (v instanceof ErrTypeKindNeq) {
          return "Kinds of types " + (show124(v.value0) + (" and " + (show124(v.value1) + " are not equal")));
        }
        ;
        if (v instanceof ErrOther8) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at SolveConstraints.Errors (line 19, column 1 - line 28, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrOther8) {
          return v.value0;
        }
        ;
        return defaultLoc;
      },
      toError: ErrOther8.create
    };
  }();

  // output/SolveConstraints.Definition/index.js
  var toUnfoldable11 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var map43 = /* @__PURE__ */ map(functorList);
  var show51 = /* @__PURE__ */ show(showTypevar);
  var show125 = /* @__PURE__ */ show(showTy2);
  var intercalate18 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var monadExceptT14 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var monadStateStateT3 = /* @__PURE__ */ monadStateStateT(monadExceptT14);
  var gets7 = /* @__PURE__ */ gets(monadStateStateT3);
  var bind26 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadExceptT14));
  var pure25 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadExceptT14));
  var modify10 = /* @__PURE__ */ modify2(monadStateStateT3);
  var insert8 = /* @__PURE__ */ insert(ordTypevar);
  var throwError14 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity)));
  var MkSolverState = /* @__PURE__ */ function() {
    function MkSolverState2(value0) {
      this.value0 = value0;
    }
    ;
    MkSolverState2.create = function(value0) {
      return new MkSolverState2(value0);
    };
    return MkSolverState2;
  }();
  var showSubst = function(varmap) {
    var substList = toUnfoldable11(varmap);
    var shownSubsts = map43(function(v) {
      return show51(v.value0) + (" -> " + show125(v.value1));
    })(substList);
    return intercalate18("\n	")(shownSubsts);
  };
  var initialSolverState = function(constrs) {
    return new MkSolverState({
      slvTyVars: empty2,
      remConstrs: constrs
    });
  };
  var runSolveM = function(constrs) {
    return function(m) {
      var v = runExcept(runStateT(m)(initialSolverState(constrs)));
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return new Right(new Tuple(v.value0.value0, v.value0.value1.value0.slvTyVars));
      }
      ;
      throw new Error("Failed pattern match at SolveConstraints.Definition (line 45, column 23 - line 47, column 69): " + [v.constructor.name]);
    };
  };
  var getSlvTyVars = /* @__PURE__ */ gets7(function(v) {
    return v.value0.slvTyVars;
  });
  var getNextConstr = /* @__PURE__ */ bind26(/* @__PURE__ */ gets7(function(v) {
    return v.value0.remConstrs;
  }))(function(constrs) {
    if (constrs instanceof Nil) {
      return pure25(Nothing.value);
    }
    ;
    if (constrs instanceof Cons) {
      return bind26(modify10(function(v) {
        return new MkSolverState({
          slvTyVars: v.value0.slvTyVars,
          remConstrs: constrs.value1
        });
      }))(function() {
        return pure25(new Just(constrs.value0));
      });
    }
    ;
    throw new Error("Failed pattern match at SolveConstraints.Definition (line 60, column 3 - line 64, column 21): " + [constrs.constructor.name]);
  });
  var addSlvTyVar = function(v) {
    return function(ty) {
      return bind26(modify10(function(v1) {
        return new MkSolverState({
          remConstrs: v1.value0.remConstrs,
          slvTyVars: insert8(v)(ty)(v1.value0.slvTyVars)
        });
      }))(function() {
        return pure25(unit);
      });
    };
  };
  var addConstraint2 = function(constr) {
    return bind26(gets7(function(v) {
      return v.value0.remConstrs;
    }))(function(constrs) {
      return bind26(modify10(function(v) {
        return new MkSolverState({
          slvTyVars: v.value0.slvTyVars,
          remConstrs: new Cons(constr, constrs)
        });
      }))(function() {
        return pure25(unit);
      });
    });
  };
  var addConstraintsArgs = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Nil && v2 instanceof Nil) {
          return pure25(unit);
        }
        ;
        if (v1 instanceof Nil) {
          return throwError14(new ErrTyArity2(v));
        }
        ;
        if (v2 instanceof Nil) {
          return throwError14(new ErrTyArity2(v));
        }
        ;
        if (v1 instanceof Cons && v2 instanceof Cons) {
          return bind26(addConstraint2(new MkTyEq(v1.value0, v2.value0)))(function() {
            return bind26(addConstraintsArgs(v)(v1.value1)(v2.value1))(function() {
              return pure25(unit);
            });
          });
        }
        ;
        throw new Error("Failed pattern match at SolveConstraints.Definition (line 80, column 1 - line 80, column 69): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/SolveConstraints.Solver/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadExceptT15 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind27 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadExceptT15));
  var lookup8 = /* @__PURE__ */ lookup(ordTypevar);
  var pure26 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadExceptT15));
  var notEq4 = /* @__PURE__ */ notEq(eqTypename);
  var throwError15 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity)));
  var unifyTypeConstraint = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v instanceof TyVar2 && v1 instanceof TyVar2) {
          $tco_done = true;
          return bind27(getSlvTyVars)(function(vars) {
            var v3 = new Tuple(lookup8(v.value0)(vars), lookup8(v1.value0)(vars));
            if (v3.value0 instanceof Just && v3.value1 instanceof Just) {
              return bind27(addConstraint2(new MkTyEq(v3.value0.value0, v3.value1.value0)))(function() {
                return pure26(unit);
              });
            }
            ;
            if (v3.value0 instanceof Nothing && v3.value1 instanceof Just) {
              return addSlvTyVar(v.value0)(v3.value1.value0);
            }
            ;
            if (v3.value0 instanceof Just && v3.value1 instanceof Nothing) {
              return addSlvTyVar(v1.value0)(v3.value0.value0);
            }
            ;
            if (v3.value0 instanceof Nothing && v3.value1 instanceof Nothing) {
              return pure26(unit);
            }
            ;
            throw new Error("Failed pattern match at SolveConstraints.Solver (line 35, column 3 - line 41, column 41): " + [v3.constructor.name]);
          });
        }
        ;
        if (v instanceof TyVar2) {
          $tco_done = true;
          return bind27(getSlvTyVars)(function(vars) {
            var v3 = lookup8(v.value0)(vars);
            if (v3 instanceof Nothing) {
              return addSlvTyVar(v.value0)(v1);
            }
            ;
            if (v3 instanceof Just) {
              return addConstraint2(new MkTyEq(v3.value0, v1));
            }
            ;
            throw new Error("Failed pattern match at SolveConstraints.Solver (line 44, column 3 - line 46, column 46): " + [v3.constructor.name]);
          });
        }
        ;
        if (v1 instanceof TyVar2) {
          $tco_var_v = v1;
          $copy_v1 = v;
          return;
        }
        ;
        if (v instanceof TyDecl2 && v1 instanceof TyDecl2) {
          if (notEq4(v.value0)(v1.value0)) {
            $tco_done = true;
            return throwError15(new ErrTyNeq(v, v1));
          }
          ;
          if (otherwise) {
            $tco_done = true;
            return bind27(addConstraintsArgs(v.value0)(v.value1)(v1.value1))(function() {
              return pure26(unit);
            });
          }
          ;
        }
        ;
        if (v instanceof TyShift2 && v1 instanceof TyShift2) {
          $tco_var_v = v.value0;
          $copy_v1 = v1.value0;
          return;
        }
        ;
        if (v instanceof TyCo2 && v1 instanceof TyCo2) {
          $tco_var_v = v.value0;
          $copy_v1 = v1.value0;
          return;
        }
        ;
        $tco_done = true;
        return throwError15(new ErrTyNeq(v, v1));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var $lazy_solve = /* @__PURE__ */ $runtime_lazy7("solve", "SolveConstraints.Solver", function() {
    return bind27(getNextConstr)(function(mctr) {
      if (mctr instanceof Nothing) {
        return pure26(unit);
      }
      ;
      if (mctr instanceof Just && mctr.value0 instanceof MkTyEq) {
        return bind27(unifyTypeConstraint(mctr.value0.value0)(mctr.value0.value1))(function() {
          return $lazy_solve(26);
        });
      }
      ;
      if (mctr instanceof Just && mctr.value0 instanceof MkKindEq) {
        return throwError15(new ErrOther8(defaultLoc, "should not happen"));
      }
      ;
      if (mctr instanceof Just && mctr.value0 instanceof MkKindNeq) {
        return throwError15(new ErrOther8(defaultLoc, "should not happen"));
      }
      ;
      throw new Error("Failed pattern match at SolveConstraints.Solver (line 22, column 3 - line 28, column 81): " + [mctr.constructor.name]);
    });
  });
  var solve = /* @__PURE__ */ $lazy_solve(19);

  // output/ImportLibs/foreign.js
  var streamSrc = `
module Stream

import Bool

codata Stream(a:-){
  Head(a),
  Tail(Stream(a))
}

constTrue :: Stream(Bool)
rec constTrue := mu a. 
  <case { Head(b) => <True | CBV |b>, Tail(str) => < constTrue | CBV | str >  } | CBV | a>

`;
  var natSrc = `
module Nat 

import Fun

data Nat{ 
  Z,
  S(Nat)
}


succ :: Fun(Nat,Nat)
succ := case { Ap(n,a) => <S(n) | CBV | a> }

pred :: Fun(Nat,Nat)
rec pred := case { Ap(n,a) => 
  <  case {
    Z => <Z|CBV|a>,
    S(m) => <mu b. <pred | CBV | Ap(m,b)> |CBV|a>
  } | CBV | n> 
}

main := <pred | CBV | Ap(S(S(S(Z))),mu x.Print x)>
`;
  var boolSrc = `
module Bool

import Fun

data Bool{ 
  True,
  False
}

not :: Fun(Bool,Bool)
not := case { Ap(b,a) => 
  <case {
    True  => <False | CBV | a>,
    False => <True  | CBV | a>
  } | CBV | b>
}

and :: Fun(Bool,Fun(Bool,Bool))
and := case { Ap(b1,a) =>
  < case { Ap(b2,b) => 
    < case {
      True => <b2|CBV|b>,
      False => <False|CBV|b>
    } | CBV | b1 > 
  } | CBV | a>
}
 
or :: Fun(Bool,Fun(Bool,Bool))
or := case { Ap(b1,a) => 
  < case { Ap(b2, b) => 
    < case { 
      True  => <True|CBV|b>,
      False => <b2  |CBV|b> 
    } | CBV | b1> 
  } | CBV | a> 
}

ifthenelse :: forall X. Bool -> X -> X 
ifthenelse := case { Ap(b1,a) => 
  < case { Ap(t1,b) => 
    < case { Ap(t2,c) =>  
      < case { 
        True => <t1 | CBV | b>,
        False => <t2 | CBV | c> 
        } | CBV | b1> 
    } | CBV | b>
  } | CBV | a>
}

printCons :: Forall X. X
printCons := mu x.Print x

andEx :: Bool
andEx := True && False 

orEx :: Bool
orEx := True || True 

notEx :: Bool
notEx := !True 

ifTrue :: Bool
ifTrue := if True then True else False

main := case True of { True => Done, False => error ""} 
`;
  var lpairSrc = `
module LPair 

codata LPair(a:-,b:-){ 
  fst(a),
  snd(b)
}
`;
  var listSrc = `
module List

import Fun
import Nat
import Unit

-- Lists
data List(a:+){
  Cons(a,List(a)),
  Nil
}

brackEx :: List(Unit)
brackEx := [MkUnit,MkUnit,MkUnit]

brackEx2 :: List(Unit)
brackEx2 := [MkUnit,MkUnit,MkUnit,MkUnit,MkUnit]

tail :: forall X. Fun(List(X),List(X))
tail := case { Ap(ls,a) => 
  < case { 
    Nil         => <Nil | CBV | a>,
    Cons(hd,rs) => <rs  | CBV | a>
  } | CBV | ls> 
}

head :: forall X. Fun(List(X),X)
head := case { Ap(ls,a) => 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | CBV | a>
  } | CBV | ls> 
}


len :: forall X. Fun(List(X),Nat)
rec len := case { Ap(ls,a) => 
  < case {
    Nil => <Z|CBV|a>,
    Cons(l1,lrs) => 
     <len | CBV | Ap(lrs,mu x.<S(x)|CBV|a>)>
  } | CBV | ls>
}

printCons :: Forall X. X
printCons := mu x. Print x;mu x.Done

main := <len | Fun(List(Nat),Nat):CBV | Ap(Cons(Z,Nil),printCons)>
`;
  var pairSrc = `
module Pair

import Fun 
import Unit

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. Fun(X,Pair(X,X))
diag := case { Ap(x,a) => <Tup(x,x) | CBV | a> }

pairSugar :: Pair(Pair(Unit,Unit),Pair(Unit,Unit))
pairSugar := (MkUnit,MkUnit,MkUnit,MkUnit)

pairSugar2 :: Pair(Unit,Pair(Unit,Unit))
pairSugar2 := (MkUnit,MkUnit,MkUnit)
`;
  var funSrc = `
module Fun

import Unit

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. Fun(X,X)
id := case { Ap(x,a) => <x | CBV | a> }

id2 :: Forall X. Fun(X,X)
id2 := \\x. x

main := <id2 [MkUnit] | CBV | mu x.Print x>
`;
  var unitSrc = `
module Unit

data Unit { MkUnit }

cbvU :: {Unit}
cbvU := {MkUnit:CBV}

cbnU :: {Unit}
cbnU := {MkUnit:CBN} 

cutCBV :: Unit 
cutCBV := Mu y. (MkUnit >> Unit >> Mu x. Done)

cutCBN :: Unit
cutCBN := Mu y. (MkUnit << Unit << Mu x. Done) 
`;

  // output/ImportLibs/index.js
  var libSources = /* @__PURE__ */ function() {
    return [new Tuple("stream", streamSrc), new Tuple("nat", natSrc), new Tuple("bool", boolSrc), new Tuple("lpair", lpairSrc), new Tuple("list", listSrc), new Tuple("pair", pairSrc), new Tuple("fun", funSrc), new Tuple("unit", unitSrc)];
  }();

  // output/StandardLib/index.js
  var libMap = /* @__PURE__ */ function() {
    var fstUpper = function(str) {
      var v = uncons2(str);
      if (v instanceof Nothing) {
        return "";
      }
      ;
      if (v instanceof Just) {
        return toUpper(singleton7(v.value0.head)) + v.value0.tail;
      }
      ;
      throw new Error("Failed pattern match at StandardLib (line 26, column 7 - line 28, column 67): " + [v.constructor.name]);
    };
    var toModule = function(str) {
      return new Modulename(fstUpper(str));
    };
    var libSourcesCap = map(functorArray)(lmap(bifunctorTuple)(toModule))(libSources);
    return fromFoldable(ordModulename)(foldableArray)(libSourcesCap);
  }();

  // output/TypeCheck.Errors/index.js
  var show54 = /* @__PURE__ */ show(showTerm2);
  var show126 = /* @__PURE__ */ show(showCommand2);
  var show219 = /* @__PURE__ */ show(showVariable);
  var show312 = /* @__PURE__ */ show(showTypevar);
  var show410 = /* @__PURE__ */ show(showTy2);
  var show55 = /* @__PURE__ */ show(showTerm3);
  var show64 = /* @__PURE__ */ show(showTy);
  var show74 = /* @__PURE__ */ show(showTypename);
  var show84 = /* @__PURE__ */ show(showXtorname);
  var intercalate19 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map44 = /* @__PURE__ */ map(functorList);
  var ErrNoAnnot = /* @__PURE__ */ function() {
    function ErrNoAnnot2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrNoAnnot2.create = function(value0) {
      return function(value1) {
        return new ErrNoAnnot2(value0, value1);
      };
    };
    return ErrNoAnnot2;
  }();
  var ErrUndefinedVar2 = /* @__PURE__ */ function() {
    function ErrUndefinedVar3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrUndefinedVar3.create = function(value0) {
      return function(value1) {
        return new ErrUndefinedVar3(value0, value1);
      };
    };
    return ErrUndefinedVar3;
  }();
  var ErrNotSubsumed = /* @__PURE__ */ function() {
    function ErrNotSubsumed2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrNotSubsumed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrNotSubsumed2(value0, value1, value22);
        };
      };
    };
    return ErrNotSubsumed2;
  }();
  var ErrUndefinedTyVar2 = /* @__PURE__ */ function() {
    function ErrUndefinedTyVar3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrUndefinedTyVar3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrUndefinedTyVar3(value0, value1, value22);
        };
      };
    };
    return ErrUndefinedTyVar3;
  }();
  var ErrFreeTyVar = /* @__PURE__ */ function() {
    function ErrFreeTyVar2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrFreeTyVar2.create = function(value0) {
      return function(value1) {
        return new ErrFreeTyVar2(value0, value1);
      };
    };
    return ErrFreeTyVar2;
  }();
  var ErrTyCoForShift = /* @__PURE__ */ function() {
    function ErrTyCoForShift2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrTyCoForShift2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrTyCoForShift2(value0, value1, value22);
        };
      };
    };
    return ErrTyCoForShift2;
  }();
  var ErrKindNeq3 = /* @__PURE__ */ function() {
    function ErrKindNeq4(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrKindNeq4.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrKindNeq4(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrKindNeq4;
  }();
  var ErrKindUnclear = /* @__PURE__ */ function() {
    function ErrKindUnclear2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrKindUnclear2.create = function(value0) {
      return function(value1) {
        return new ErrKindUnclear2(value0, value1);
      };
    };
    return ErrKindUnclear2;
  }();
  var ErrTypeNeq = /* @__PURE__ */ function() {
    function ErrTypeNeq2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrTypeNeq2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrTypeNeq2(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrTypeNeq2;
  }();
  var ErrNotTyDecl = /* @__PURE__ */ function() {
    function ErrNotTyDecl2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrNotTyDecl2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrNotTyDecl2(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrNotTyDecl2;
  }();
  var ErrTypeArity = /* @__PURE__ */ function() {
    function ErrTypeArity2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrTypeArity2.create = function(value0) {
      return function(value1) {
        return new ErrTypeArity2(value0, value1);
      };
    };
    return ErrTypeArity2;
  }();
  var ErrXtorArity3 = /* @__PURE__ */ function() {
    function ErrXtorArity4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrXtorArity4.create = function(value0) {
      return function(value1) {
        return new ErrXtorArity4(value0, value1);
      };
    };
    return ErrXtorArity4;
  }();
  var ErrBadPattern2 = /* @__PURE__ */ function() {
    function ErrBadPattern3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrBadPattern3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrBadPattern3(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrBadPattern3;
  }();
  var ErrCutKind = /* @__PURE__ */ function() {
    function ErrCutKind2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    ErrCutKind2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new ErrCutKind2(value0, value1, value22, value32);
          };
        };
      };
    };
    return ErrCutKind2;
  }();
  var ErrBadType = /* @__PURE__ */ function() {
    function ErrBadType2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ErrBadType2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ErrBadType2(value0, value1, value22);
        };
      };
    };
    return ErrBadType2;
  }();
  var ErrUnclearType = /* @__PURE__ */ function() {
    function ErrUnclearType2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrUnclearType2.create = function(value0) {
      return function(value1) {
        return new ErrUnclearType2(value0, value1);
      };
    };
    return ErrUnclearType2;
  }();
  var ErrList = /* @__PURE__ */ function() {
    function ErrList2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrList2.create = function(value0) {
      return function(value1) {
        return new ErrList2(value0, value1);
      };
    };
    return ErrList2;
  }();
  var ErrOther9 = /* @__PURE__ */ function() {
    function ErrOther10(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrOther10.create = function(value0) {
      return function(value1) {
        return new ErrOther10(value0, value1);
      };
    };
    return ErrOther10;
  }();
  var whileTerm = function(t) {
    return "while type checking " + show54(t);
  };
  var whileCmd = function(c) {
    return "while type checking " + show126(c);
  };
  var errorCheckerError = /* @__PURE__ */ function() {
    return {
      getMessage: function(v) {
        if (v instanceof ErrNoAnnot) {
          return "No annotation for " + (show219(v.value1) + ", cannot type check.");
        }
        ;
        if (v instanceof ErrUndefinedVar2) {
          return "Variable " + (show219(v.value1) + " was not defined ");
        }
        ;
        if (v instanceof ErrUndefinedTyVar2) {
          return "Type Variable " + (show312(v.value1) + (" was not defined " + whileTerm(v.value2)));
        }
        ;
        if (v instanceof ErrFreeTyVar) {
          return "Type Variable " + (show312(v.value1) + " cannot appear free");
        }
        ;
        if (v instanceof ErrTyCoForShift) {
          return "Cannot use co-type of " + (show410(v.value2) + (" for shift term " + show55(v.value1)));
        }
        ;
        if (v instanceof ErrKindNeq3) {
          return "Kinds of types " + (show410(v.value1) + (" and " + (show410(v.value2) + (" are not equal " + whileTerm(v.value3)))));
        }
        ;
        if (v instanceof ErrNotSubsumed) {
          return "Type " + (show410(v.value1) + (" should be subsumed by " + show410(v.value2)));
        }
        ;
        if (v instanceof ErrKindUnclear) {
          return "Kind of type " + (show64(v.value1) + " is unclear");
        }
        ;
        if (v instanceof ErrTypeNeq) {
          return "Types " + (show410(v.value1) + (" and " + (show410(v.value2) + (" should be equal " + whileTerm(v.value3)))));
        }
        ;
        if (v instanceof ErrNotTyDecl) {
          return "Type " + (show410(v.value2) + (" should be " + (show74(v.value1) + (" " + whileTerm(v.value3)))));
        }
        ;
        if (v instanceof ErrTypeArity) {
          return "Wrong number of arguments for type " + show74(v.value1);
        }
        ;
        if (v instanceof ErrXtorArity3) {
          return "Wrong number of arguments for xtor " + show84(v.value1);
        }
        ;
        if (v instanceof ErrBadPattern2) {
          return "Malformed case: found patterns for " + (intercalate19(", ")(map44(show84)(v.value1)) + (", expected " + (intercalate19(", ")(map44(show84)(v.value2)) + (" " + whileTerm(v.value3)))));
        }
        ;
        if (v instanceof ErrCutKind) {
          return "Kind of types " + (show410(v.value1) + (" and " + (show410(v.value2) + (" in cut are not equal " + whileCmd(v.value3)))));
        }
        ;
        if (v instanceof ErrBadType) {
          return "Cannot typecheck " + (show54(v.value1) + (" with type " + show410(v.value2)));
        }
        ;
        if (v instanceof ErrUnclearType) {
          return "Type of term " + (show126(v.value1) + " is unclear");
        }
        ;
        if (v instanceof ErrList) {
          return intercalate19("\n ")(map44(getMessage(errorCheckerError))(v.value1));
        }
        ;
        if (v instanceof ErrOther9) {
          return v.value1;
        }
        ;
        throw new Error("Failed pattern match at TypeCheck.Errors (line 43, column 1 - line 82, column 21): " + [v.constructor.name]);
      },
      getLocation: function(v) {
        if (v instanceof ErrNoAnnot) {
          return v.value0;
        }
        ;
        if (v instanceof ErrUndefinedVar2) {
          return v.value0;
        }
        ;
        if (v instanceof ErrUndefinedTyVar2) {
          return v.value0;
        }
        ;
        if (v instanceof ErrFreeTyVar) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTyCoForShift) {
          return v.value0;
        }
        ;
        if (v instanceof ErrKindNeq3) {
          return v.value0;
        }
        ;
        if (v instanceof ErrNotSubsumed) {
          return v.value0;
        }
        ;
        if (v instanceof ErrKindUnclear) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTypeNeq) {
          return v.value0;
        }
        ;
        if (v instanceof ErrNotTyDecl) {
          return v.value0;
        }
        ;
        if (v instanceof ErrTypeArity) {
          return v.value0;
        }
        ;
        if (v instanceof ErrXtorArity3) {
          return v.value0;
        }
        ;
        if (v instanceof ErrBadPattern2) {
          return v.value0;
        }
        ;
        if (v instanceof ErrCutKind) {
          return v.value0;
        }
        ;
        if (v instanceof ErrBadType) {
          return v.value0;
        }
        ;
        if (v instanceof ErrUnclearType) {
          return v.value0;
        }
        ;
        if (v instanceof ErrList) {
          return v.value0;
        }
        ;
        if (v instanceof ErrOther9) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at TypeCheck.Errors (line 43, column 1 - line 82, column 21): " + [v.constructor.name]);
      },
      toError: ErrOther9.create
    };
  }();

  // output/TypeCheck.Definition/index.js
  var monadExceptT16 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var monadStateReaderT5 = /* @__PURE__ */ monadStateReaderT(/* @__PURE__ */ monadStateStateT(monadExceptT16));
  var gets8 = /* @__PURE__ */ gets(monadStateReaderT5);
  var bind28 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT16)));
  var lookupMVar3 = /* @__PURE__ */ lookupMVar(errorCheckerError)(/* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity))))(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT16)));
  var lookup9 = /* @__PURE__ */ lookup(ordVariable);
  var pure27 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT16)));
  var throwError16 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var modify11 = /* @__PURE__ */ modify2(monadStateReaderT5);
  var union5 = /* @__PURE__ */ union(ordVariable);
  var insert9 = /* @__PURE__ */ insert(ordVariable);
  var MkCheckState = /* @__PURE__ */ function() {
    function MkCheckState2(value0) {
      this.value0 = value0;
    }
    ;
    MkCheckState2.create = function(value0) {
      return new MkCheckState2(value0);
    };
    return MkCheckState2;
  }();
  var initialCheckerState = /* @__PURE__ */ function() {
    return new MkCheckState({
      checkVars: empty2,
      checkTyVars: Nil.value
    });
  }();
  var runCheckM = function(env) {
    return function(m) {
      var v = runExcept(runStateT(runReaderT(m)(env))(initialCheckerState));
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return new Right(v.value0.value0);
      }
      ;
      throw new Error("Failed pattern match at TypeCheck.Definition (line 42, column 19 - line 44, column 31): " + [v.constructor.name]);
    };
  };
  var getCheckerVars = /* @__PURE__ */ gets8(function(v) {
    return v.value0.checkVars;
  });
  var getMTypeVar = function(v) {
    return bind28(getCheckerVars)(function(vars) {
      return bind28(lookupMVar3(v))(function(mvar) {
        var v1 = new Tuple(lookup9(v)(vars), mvar);
        if (v1.value0 instanceof Nothing && v1.value1 instanceof Nothing) {
          return pure27(Nothing.value);
        }
        ;
        if (v1.value0 instanceof Just) {
          return pure27(new Just(v1.value0.value0));
        }
        ;
        if (v1.value1 instanceof Just) {
          return pure27(new Just(embedType2(v1.value1.value0.value0.varTy)));
        }
        ;
        throw new Error("Failed pattern match at TypeCheck.Definition (line 74, column 3 - line 77, column 76): " + [v1.constructor.name]);
      });
    });
  };
  var getTypeVar = function(loc) {
    return function(v) {
      return bind28(getMTypeVar(v))(function(mty) {
        if (mty instanceof Nothing) {
          return throwError16(new ErrUndefinedVar2(loc, v));
        }
        ;
        if (mty instanceof Just) {
          return pure27(mty.value0);
        }
        ;
        throw new Error("Failed pattern match at TypeCheck.Definition (line 82, column 3 - line 84, column 23): " + [mty.constructor.name]);
      });
    };
  };
  var withCheckerVars = function(newVars) {
    return function(fun) {
      return bind28(getCheckerVars)(function(currVars) {
        return bind28(modify11(function(v) {
          return new MkCheckState({
            checkTyVars: v.value0.checkTyVars,
            checkVars: union5(currVars)(newVars)
          });
        }))(function() {
          return bind28(fun)(function(res) {
            return bind28(modify11(function(v) {
              return new MkCheckState({
                checkTyVars: v.value0.checkTyVars,
                checkVars: currVars
              });
            }))(function() {
              return pure27(res);
            });
          });
        });
      });
    };
  };
  var getCheckerTyVars = /* @__PURE__ */ gets8(function(v) {
    return v.value0.checkTyVars;
  });
  var addCheckerVar = function(v) {
    return function(ty) {
      return bind28(modify11(function(v1) {
        return new MkCheckState({
          checkTyVars: v1.value0.checkTyVars,
          checkVars: insert9(v)(ty)(v1.value0.checkVars)
        });
      }))(function() {
        return pure27(unit);
      });
    };
  };
  var addCheckerTyVar = function(tyv) {
    return bind28(modify11(function(v) {
      return new MkCheckState({
        checkVars: v.value0.checkVars,
        checkTyVars: new Cons(tyv, v.value0.checkTyVars)
      });
    }))(function() {
      return pure27(unit);
    });
  };

  // output/TypeCheck.Types/index.js
  var monadExceptT17 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind29 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT17)));
  var elem13 = /* @__PURE__ */ elem(foldableList)(eqTypevar);
  var applicativeReaderT15 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT17));
  var pure28 = /* @__PURE__ */ pure(applicativeReaderT15);
  var throwError17 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var monadErrorReaderT6 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity)));
  var lookupDecl3 = /* @__PURE__ */ lookupDecl(errorCheckerError)(monadErrorReaderT6)(/* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT17)));
  var zipWithErrorM3 = /* @__PURE__ */ zipWithErrorM(errorCheckerError)(monadErrorReaderT6);
  var $$for14 = /* @__PURE__ */ $$for(applicativeReaderT15)(traversableList);
  var checkType = function(v) {
    return function(v1) {
      if (v1 instanceof TyVar) {
        return bind29(getCheckerTyVars)(function(tyVars) {
          var $31 = elem13(v1.value0)(tyVars);
          if ($31) {
            return pure28(new TyVar2(v1.value0));
          }
          ;
          return throwError17(new ErrFreeTyVar(v, v1.value0));
        });
      }
      ;
      if (v1 instanceof TyDecl) {
        return bind29(lookupDecl3(v)(v1.value0))(function(v2) {
          return bind29(zipWithErrorM3(v1.value1)(v2.value0.declArgs)(new ErrTypeArity(v, v1.value0)))(function() {
            return bind29($$for14(v1.value1)(checkType(v)))(function(args$prime) {
              return pure28(new TyDecl2(v1.value0, args$prime));
            });
          });
        });
      }
      ;
      if (v1 instanceof TyCo) {
        return bind29(checkType(v)(v1.value0))(function(ty$prime) {
          return pure28(new TyCo2(ty$prime));
        });
      }
      ;
      if (v1 instanceof TyShift) {
        return bind29(checkType(v)(v1.value0))(function(ty$prime) {
          return pure28(new TyShift2(ty$prime));
        });
      }
      ;
      if (v1 instanceof TyForall) {
        return bind29($$for14(v1.value0)(addCheckerTyVar))(function() {
          return bind29(checkType(v)(v1.value1))(function(ty$prime) {
            return pure28(new TyForall2(v1.value0, ty$prime));
          });
        });
      }
      ;
      throw new Error("Failed pattern match at TypeCheck.Types (line 22, column 1 - line 22, column 40): " + [v.constructor.name, v1.constructor.name]);
    };
  };

  // output/TypeCheck.Terms/index.js
  var monadExceptT18 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var bind30 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT18)));
  var applicativeReaderT16 = /* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT18));
  var $$for15 = /* @__PURE__ */ $$for(applicativeReaderT16)(traversableList);
  var pure29 = /* @__PURE__ */ pure(applicativeReaderT16);
  var map45 = /* @__PURE__ */ map(/* @__PURE__ */ functorReaderT(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity))));
  var lookup10 = /* @__PURE__ */ lookup(ordVariable);
  var monadErrorReaderT7 = /* @__PURE__ */ monadErrorReaderT(/* @__PURE__ */ monadErrorStateT(/* @__PURE__ */ monadErrorExceptT(monadIdentity)));
  var monadReaderReaderT5 = /* @__PURE__ */ monadReaderReaderT(/* @__PURE__ */ monadStateT(monadExceptT18));
  var lookupMVar4 = /* @__PURE__ */ lookupMVar(errorCheckerError)(monadErrorReaderT7)(monadReaderReaderT5);
  var map114 = /* @__PURE__ */ map(functorMaybe);
  var throwError18 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var lookupXtorDecl3 = /* @__PURE__ */ lookupXtorDecl(errorCheckerError)(monadErrorReaderT7)(monadReaderReaderT5);
  var lookupXtor3 = /* @__PURE__ */ lookupXtor(errorCheckerError)(monadErrorReaderT7)(monadReaderReaderT5);
  var unless3 = /* @__PURE__ */ unless(applicativeReaderT16);
  var eq30 = /* @__PURE__ */ eq(eqTypename);
  var map210 = /* @__PURE__ */ map(functorList);
  var zipWithErrorM4 = /* @__PURE__ */ zipWithErrorM(errorCheckerError)(monadErrorReaderT7);
  var substTyvars4 = /* @__PURE__ */ substTyvars(substituteTypevarsTy);
  var fromFoldable9 = /* @__PURE__ */ fromFoldable(ordTypevar)(foldableList);
  var foldr10 = /* @__PURE__ */ foldr(foldableList);
  var insert10 = /* @__PURE__ */ insert(ordVariable);
  var eq111 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqXtorname));
  var fromFoldable1 = /* @__PURE__ */ fromFoldable2(foldableList)(ordXtorname);
  var getLoc4 = /* @__PURE__ */ getLoc(hasLocTerm2);
  var eq210 = /* @__PURE__ */ eq(eqTy2);
  var getLoc12 = /* @__PURE__ */ getLoc(hasLocCommand2);
  var checkTerm = function(v) {
    return function(v1) {
      if (v1 instanceof TyForall2) {
        return bind30($$for15(v1.value0)(addCheckerTyVar))(function() {
          return bind30(checkTerm(v)(v1.value1))(function(t$prime) {
            var newTy = new TyForall2(v1.value0, getType2(t$prime));
            return pure29(setType(t$prime)(newTy));
          });
        });
      }
      ;
      if (v1 instanceof TyCo2) {
        return bind30(checkTerm(v)(v1.value0))(function(t$prime) {
          var newTy = new TyCo2(getType2(t$prime));
          return pure29(setType(t$prime)(newTy));
        });
      }
      ;
      if (v instanceof Var2) {
        return bind30(map45(lookup10(v.value1))(getCheckerVars))(function(checkerTy) {
          return bind30(lookupMVar4(v.value1))(function(vardecl) {
            var mvarty = map114(function(v3) {
              return embedType2(v3.value0.varTy);
            })(vardecl);
            return bind30(function() {
              var v3 = new Tuple(checkerTy, mvarty);
              if (v3.value0 instanceof Nothing && v3.value1 instanceof Nothing) {
                return throwError18(new ErrUndefinedVar2(v.value0, v.value1));
              }
              ;
              if (v3.value1 instanceof Just) {
                return pure29(v3.value1.value0);
              }
              ;
              if (v3.value0 instanceof Just) {
                return pure29(v3.value0.value0);
              }
              ;
              throw new Error("Failed pattern match at TypeCheck.Terms (line 51, column 10 - line 54, column 41): " + [v3.constructor.name]);
            }())(function(ty$prime) {
              var $83 = isSubsumed(v1)(ty$prime);
              if ($83) {
                return pure29(new Var3(v.value0, v.value1, v1));
              }
              ;
              return throwError18(new ErrNotSubsumed(v.value0, v1, ty$prime));
            });
          });
        });
      }
      ;
      if (v instanceof Mu2) {
        return bind30(addCheckerVar(v.value1)(v1))(function() {
          return bind30(checkCommand(v.value2))(function(c$prime) {
            return pure29(new Mu3(v.value0, v.value1, c$prime, v1));
          });
        });
      }
      ;
      if (v instanceof Xtor2 && v1 instanceof TyDecl2) {
        return bind30(lookupXtorDecl3(v.value0)(v.value1))(function(v2) {
          return bind30(lookupXtor3(v.value0)(v.value1))(function(v3) {
            return bind30(unless3(eq30(v1.value0)(v2.value0.declName))(throwError18(new ErrNotTyDecl(v.value0, v2.value0.declName, v1, v))))(function() {
              var argVars = map210(function(v4) {
                return v4.value0.variantVar;
              })(v2.value0.declArgs);
              return bind30(zipWithErrorM4(argVars)(v1.value1)(new ErrTypeArity(v.value0, v1.value0)))(function(varSubsts) {
                var argsEmbedded = map210(embedType2)(v3.value0.sigArgs);
                var xtArgTys = map210(substTyvars4(fromFoldable9(varSubsts)))(argsEmbedded);
                return bind30(zipWithErrorM4(v.value2)(xtArgTys)(new ErrXtorArity3(v.value0, v.value1)))(function(argsToCheck) {
                  return bind30($$for15(argsToCheck)(function(v4) {
                    return checkTerm(v4.value0)(v4.value1);
                  }))(function(argsChecked) {
                    return pure29(new Xtor3(v.value0, v.value1, argsChecked, v1));
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof XCase2 && (v.value1 instanceof Cons && v1 instanceof TyDecl2)) {
        var checkPattern = function(v2) {
          return function(varmap) {
            return bind30(lookupXtor3(v.value0)(v2.value0.ptxt))(function(v3) {
              var argsEmbedded = map210(embedType2)(v3.value0.sigArgs);
              var xtargs$prime = map210(substTyvars4(varmap))(argsEmbedded);
              return bind30(zipWithErrorM4(v2.value0.ptv)(xtargs$prime)(new ErrXtorArity3(v.value0, v3.value0.sigName)))(function(argsZipped) {
                return bind30(getCheckerVars)(function(currVars) {
                  var newVars = foldr10(function(v4) {
                    return function(m) {
                      return insert10(v4.value0)(v4.value1)(m);
                    };
                  })(currVars)(argsZipped);
                  return bind30(withCheckerVars(newVars)(checkCommand(v2.value0.ptcmd)))(function(c$prime) {
                    return pure29(new Pattern3({
                      ptxt: v2.value0.ptxt,
                      ptv: v2.value0.ptv,
                      ptcmd: c$prime
                    }));
                  });
                });
              });
            });
          };
        };
        return bind30(lookupXtorDecl3(v.value0)(v.value1.value0.value0.ptxt))(function(v2) {
          return bind30(unless3(eq30(v1.value0)(v2.value0.declName))(throwError18(new ErrNotTyDecl(v.value0, v2.value0.declName, v1, v))))(function() {
            var ptxtns = map210(function(v3) {
              return v3.value0.ptxt;
            })(v.value1);
            var declxtns = map210(function(v3) {
              return v3.value0.sigName;
            })(v2.value0.declXtors);
            return bind30(unless3(eq111(fromFoldable1(ptxtns))(fromFoldable1(declxtns)))(throwError18(new ErrBadPattern2(v.value0, ptxtns, declxtns, v))))(function() {
              return bind30(zipWithErrorM4(map210(function(v3) {
                return v3.value0.variantVar;
              })(v2.value0.declArgs))(v1.value1)(new ErrTypeArity(v.value0, v1.value0)))(function(varmap) {
                return bind30($$for15(v.value1)(function(x) {
                  return checkPattern(x)(fromFoldable9(varmap));
                }))(function(pts$prime) {
                  return pure29(new XCase3(v.value0, pts$prime, v1));
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof ShiftCBV2 && v1 instanceof TyShift2) {
        return bind30(checkTerm(v.value1)(v1.value0))(function(t$prime) {
          var newTy = new TyShift2(getType2(t$prime));
          return pure29(new ShiftCBV3(v.value0, t$prime, newTy));
        });
      }
      ;
      if (v instanceof ShiftCBN2 && v1 instanceof TyShift2) {
        return bind30(checkTerm(v.value1)(v1.value0))(function(t$prime) {
          var newTy = new TyShift2(getType2(t$prime));
          return pure29(new ShiftCBN3(v.value0, t$prime, newTy));
        });
      }
      ;
      return throwError18(new ErrBadType(getLoc4(v), v, v1));
    };
  };
  var checkCommand = function(v) {
    if (v instanceof Cut2 && (v.value1 instanceof Var2 && v.value3 instanceof Var2)) {
      return bind30(getMTypeVar(v.value1.value1))(function(mty1) {
        return bind30(getMTypeVar(v.value3.value1))(function(mty2) {
          var v3 = new Tuple(mty1, mty2);
          if (v3.value0 instanceof Nothing && v3.value1 instanceof Nothing) {
            return throwError18(new ErrUnclearType(v.value0, v));
          }
          ;
          if (v3.value0 instanceof Just && v3.value1 instanceof Nothing) {
            return pure29(new Cut3(v.value0, new Var3(v.value1.value0, v.value1.value1, v3.value0.value0), v.value2, new Var3(v.value3.value0, v.value3.value1, v3.value0.value0)));
          }
          ;
          if (v3.value0 instanceof Nothing && v3.value1 instanceof Just) {
            return pure29(new Cut3(v.value0, new Var3(v.value1.value0, v.value1.value1, v3.value1.value0), v.value2, new Var3(v.value3.value0, v.value3.value1, v3.value1.value0)));
          }
          ;
          if (v3.value0 instanceof Just && v3.value1 instanceof Just) {
            var $142 = eq210(v3.value0.value0)(v3.value1.value0);
            if ($142) {
              return pure29(new Cut3(v.value0, new Var3(v.value1.value0, v.value1.value1, v3.value0.value0), v.value2, new Var3(v.value3.value0, v.value3.value1, v3.value1.value0)));
            }
            ;
            return throwError18(new ErrUnclearType(v.value0, v));
          }
          ;
          throw new Error("Failed pattern match at TypeCheck.Terms (line 119, column 3 - line 123, column 156): " + [v3.constructor.name]);
        });
      });
    }
    ;
    if (v instanceof Cut2 && v.value1 instanceof Var2) {
      return bind30(getTypeVar(v.value1.value0)(v.value1.value1))(function(ty) {
        return bind30(checkTerm(v.value3)(ty))(function(u$prime) {
          return pure29(new Cut3(v.value0, new Var3(v.value1.value0, v.value1.value1, ty), v.value2, u$prime));
        });
      });
    }
    ;
    if (v instanceof Cut2 && v.value3 instanceof Var2) {
      return bind30(getTypeVar(v.value3.value0)(v.value3.value1))(function(ty) {
        return bind30(checkTerm(v.value1)(ty))(function(t$prime) {
          return pure29(new Cut3(v.value0, t$prime, v.value2, new Var3(v.value3.value0, v.value3.value1, ty)));
        });
      });
    }
    ;
    if (v instanceof Cut2) {
      return throwError18(new ErrUnclearType(getLoc12(v), v));
    }
    ;
    if (v instanceof CutAnnot) {
      return bind30(checkType(v.value0)(v.value2))(function(ty1) {
        return bind30(checkType(v.value0)(v.value2))(function(ty2) {
          return bind30(checkTerm(v.value1)(ty1))(function(t$prime) {
            return bind30(checkTerm(v.value4)(ty2))(function(u$prime) {
              return pure29(new Cut3(v.value0, t$prime, v.value3, u$prime));
            });
          });
        });
      });
    }
    ;
    if (v instanceof Done3) {
      return pure29(new Done4(v.value0));
    }
    ;
    if (v instanceof Err2) {
      return pure29(new Err3(v.value0, v.value1));
    }
    ;
    if (v instanceof Print2 && v.value1 instanceof Var2) {
      return bind30(getTypeVar(v.value1.value0)(v.value1.value1))(function(ty) {
        return pure29(new Print3(v.value0, new Var3(v.value1.value0, v.value1.value1, ty)));
      });
    }
    ;
    if (v instanceof Print2) {
      return throwError18(new ErrUnclearType(getLoc12(v), v));
    }
    ;
    if (v instanceof PrintAnnot) {
      return bind30(checkType(v.value0)(v.value2))(function(ty$prime) {
        return bind30(checkTerm(v.value1)(ty$prime))(function(t$prime) {
          return pure29(new Print3(v.value0, t$prime));
        });
      });
    }
    ;
    throw new Error("Failed pattern match at TypeCheck.Terms (line 115, column 1 - line 115, column 46): " + [v.constructor.name]);
  };

  // output/TypeCheck.Program/index.js
  var throwError19 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowReaderT(/* @__PURE__ */ monadThrowStateT(/* @__PURE__ */ monadThrowExceptT(monadIdentity))));
  var monadExceptT19 = /* @__PURE__ */ monadExceptT(monadIdentity);
  var pure30 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeReaderT(/* @__PURE__ */ applicativeStateT(monadExceptT19)));
  var bind31 = /* @__PURE__ */ bind(/* @__PURE__ */ bindReaderT(/* @__PURE__ */ bindStateT(monadExceptT19)));
  var getTypeAnnot = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return throwError19(new ErrNoAnnot(v, v1));
        }
        ;
        if (v2 instanceof Just) {
          return pure30(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at TypeCheck.Program (line 20, column 1 - line 20, column 61): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var checkVarDecl = function(v) {
    return bind31(getTypeAnnot(v.value0.varPos)(v.value0.varName)(v.value0.varTy))(function(ty) {
      return bind31(checkType(v.value0.varPos)(ty))(function(ty$prime) {
        return bind31(function() {
          if (v.value0.varIsRec) {
            return addCheckerVar(v.value0.varName)(ty$prime);
          }
          ;
          return pure30(unit);
        }())(function() {
          return bind31(checkTerm(v.value0.varBody)(ty$prime))(function(t$prime) {
            return pure30(new VarDecl2({
              varPos: v.value0.varPos,
              varName: v.value0.varName,
              varIsRec: v.value0.varIsRec,
              varBody: t$prime,
              varTy: ty$prime
            }));
          });
        });
      });
    });
  };

  // output/Driver.Driver/index.js
  var monadStateT4 = /* @__PURE__ */ monadStateT(monadIdentity);
  var applicativeExceptT2 = /* @__PURE__ */ applicativeExceptT(monadStateT4);
  var pure31 = /* @__PURE__ */ pure(applicativeExceptT2);
  var bindExceptT2 = /* @__PURE__ */ bindExceptT(monadStateT4);
  var bind32 = /* @__PURE__ */ bind(bindExceptT2);
  var gets9 = /* @__PURE__ */ gets(/* @__PURE__ */ monadStateExceptT(/* @__PURE__ */ monadStateStateT(monadIdentity)));
  var show56 = /* @__PURE__ */ show(showCommand);
  var liftErr2 = /* @__PURE__ */ liftErr(errorEvalError);
  var liftErr1 = /* @__PURE__ */ liftErr(errorParserErr);
  var show127 = /* @__PURE__ */ show(showProgram3);
  var show220 = /* @__PURE__ */ show(showVariable);
  var liftErr22 = /* @__PURE__ */ liftErr(errorKindError);
  var liftErr3 = /* @__PURE__ */ liftErr(errorCheckerError);
  var show313 = /* @__PURE__ */ show(showTy2);
  var throwError20 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowExceptT(monadStateT4));
  var liftErr4 = /* @__PURE__ */ liftErr(errorGenerateError);
  var intercalate20 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var map46 = /* @__PURE__ */ map(functorList);
  var show411 = /* @__PURE__ */ show(showTypevar);
  var liftErr5 = /* @__PURE__ */ liftErr(errorSolverError);
  var substTyvars5 = /* @__PURE__ */ substTyvars(substituteTypevarsVarDecl);
  var show57 = /* @__PURE__ */ show(showTypename);
  var liftErr6 = /* @__PURE__ */ liftErr(errorInferDeclError);
  var substTyvars13 = /* @__PURE__ */ substTyvars(substituteTypevarsCommand);
  var show65 = /* @__PURE__ */ show(showModulename);
  var liftErr7 = /* @__PURE__ */ liftErr(errorDepError);
  var applySecond4 = /* @__PURE__ */ applySecond(/* @__PURE__ */ applyExceptT(monadStateT4));
  var compare4 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(ordInt));
  var elemIndex4 = /* @__PURE__ */ elemIndex(eqModulename);
  var lookup11 = /* @__PURE__ */ lookup(ordModulename);
  var unless4 = /* @__PURE__ */ unless(applicativeExceptT2);
  var $$for16 = /* @__PURE__ */ $$for(applicativeExceptT2);
  var for12 = /* @__PURE__ */ $$for16(traversableList);
  var liftErr8 = /* @__PURE__ */ liftErr(errorDesugarError);
  var ifM2 = /* @__PURE__ */ ifM(bindExceptT2);
  var show75 = /* @__PURE__ */ show(showProgram);
  var for22 = /* @__PURE__ */ $$for16(traversableMap);
  var elemIndex1 = /* @__PURE__ */ elemIndex(eqVariable);
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var fromFoldable10 = /* @__PURE__ */ fromFoldable(ordVariable)(foldableList);
  var for3 = /* @__PURE__ */ $$for16(traversableMaybe);
  var map115 = /* @__PURE__ */ map(/* @__PURE__ */ functorExceptT(/* @__PURE__ */ functorStateT(functorIdentity)));
  var runProgramTrace = function(v) {
    if (isNothing(v.value0.progMain)) {
      return pure31(emptyTrace(new Done2(defaultLoc)));
    }
    ;
    var main3 = fromMaybe(new Done2(defaultLoc))(v.value0.progMain);
    return bind32(gets9(function(v1) {
      return v1.value0.drvEnv;
    }))(function(env) {
      return bind32(debug("evaluating " + show56(main3)))(function() {
        var evaled = runEvalM(env)(evalWithTrace(main3)(emptyTrace(main3)));
        return liftErr2(evaled)(v.value0.progName)("evaluation (with trace)");
      });
    });
  };
  var runProgram = function(v) {
    if (isNothing(v.value0.progMain)) {
      return pure31(new Done2(defaultLoc));
    }
    ;
    var main3 = fromMaybe(new Done2(defaultLoc))(v.value0.progMain);
    return bind32(gets9(function(v1) {
      return v1.value0.drvEnv;
    }))(function(env) {
      return bind32(debug("evaluating main " + show56(main3)))(function() {
        var evaled = runEvalM(env)($$eval(main3));
        return liftErr2(evaled)(v.value0.progName)("evaluation");
      });
    });
  };
  var parseProg = function(mn) {
    return function(src9) {
      return bind32(debug("------ Parsing ------"))(function() {
        var srcStripped = trim(src9);
        var progTextShort = take3(fromMaybe(10)(indexOf2("\n")(srcStripped)))(srcStripped);
        return bind32(debug('parsing program from string "' + (progTextShort + '..."')))(function() {
          var progParsed = runSourceParser(src9)(parseProgram(src9));
          return bind32(liftErr1(progParsed)(mn)("parsing"))(function(prog) {
            return bind32(debug("parsed program \n" + (show127(prog) + "\n")))(function() {
              return pure31(prog);
            });
          });
        });
      });
    };
  };
  var kindVarDecl = function(mn) {
    return function(v) {
      return bind32(debug("inferring kind of variable " + show220(v.value0.varName)))(function() {
        return bind32(gets9(function(v2) {
          return v2.value0.drvEnv;
        }))(function(env) {
          var var$prime = runKindM(env)(kindVariable(v));
          return bind32(liftErr22(var$prime)(mn)("kind vardecl"))(function(var$prime$prime) {
            return bind32(addVarDecl(mn)(var$prime$prime))(function() {
              return pure31(var$prime$prime);
            });
          });
        });
      });
    };
  };
  var inferVarDecl = function(v) {
    return function(v1) {
      if (v1.value0.varTy instanceof Just) {
        return bind32(debug("type checking variable " + show220(v1.value0.varName)))(function() {
          return bind32(gets9(function(v3) {
            return v3.value0.drvEnv;
          }))(function(env) {
            var v$prime = runCheckM(env)(checkVarDecl(v1));
            if (v$prime instanceof Left) {
              return bind32(debug("type checking for variable " + (show220(v1.value0.varName) + " failed, inferring type instead")))(function() {
                var annotTy = runCheckM(env)(checkType(v1.value0.varPos)(v1.value0.varTy.value0));
                return bind32(liftErr3(annotTy)(v)("checking type annotation"))(function(annotTy$prime) {
                  return bind32(inferVarDecl(v)(new VarDecl({
                    varPos: v1.value0.varPos,
                    varName: v1.value0.varName,
                    varIsRec: v1.value0.varIsRec,
                    varBody: v1.value0.varBody,
                    varTy: Nothing.value
                  })))(function(v3) {
                    var inferTy = getType2(v3.value0.varBody);
                    var $119 = isSubsumed(annotTy$prime)(inferTy);
                    if ($119) {
                      return bind32(debug("Annotated type " + (show313(annotTy$prime) + (" is correctly subsumed by inferred type " + (show313(inferTy) + "\n")))))(function() {
                        return pure31(v3);
                      });
                    }
                    ;
                    return throwError20(new ErrAnnotMismatch(v1.value0.varPos, v1.value0.varName, annotTy$prime, inferTy));
                  });
                });
              });
            }
            ;
            if (v$prime instanceof Right) {
              return bind32(debug("Successfully checked type " + (show313(getType2(v$prime.value0.value0.varBody)) + (" for " + (show220(v1.value0.varName) + "\n")))))(function() {
                return pure31(v$prime.value0);
              });
            }
            ;
            throw new Error("Failed pattern match at Driver.Driver (line 196, column 3 - line 209, column 17): " + [v$prime.constructor.name]);
          });
        });
      }
      ;
      return bind32(debug("inferring type for " + show220(v1.value0.varName)))(function() {
        return bind32(gets9(function(v3) {
          return v3.value0.drvEnv;
        }))(function(env) {
          var constr = runGenM(env)(genConstraintsVarDecl(v1));
          return bind32(liftErr4(constr)(v)("generate constraints"))(function(v3) {
            return bind32(debug("generated typevars\n	" + intercalate20(", ")(map46(show411)(v3.value1.value0))))(function() {
              return bind32(debug("generated constraints\n" + showConstrs(v3.value1.value1)))(function() {
                var slv = runSolveM(v3.value1.value1)(solve);
                return bind32(liftErr5(slv)(v)("solve constraints"))(function(v4) {
                  return bind32(debug("solved constraints and got substitution\n " + showSubst(v4.value1)))(function() {
                    var v5 = substTyvars5(v4.value1)(v3.value0);
                    return bind32(debug("Final type for variable " + (show220(v1.value0.varName) + (": " + (show313(getType2(v5.value0.varBody)) + "\n")))))(function() {
                      return pure31(v5);
                    });
                  });
                });
              });
            });
          });
        });
      });
    };
  };
  var inferDataDecl = function(mn) {
    return function(v) {
      return bind32(debug("infering declaration " + show57(v.value0.declName)))(function() {
        var decl$prime = runDeclM(inferDecl(v));
        return bind32(liftErr6(decl$prime)(mn)("inferring declaration"))(function(decl$prime$prime) {
          return bind32(addDecl(mn)(decl$prime$prime))(function() {
            return pure31(decl$prime$prime);
          });
        });
      });
    };
  };
  var inferCommand = function(mn) {
    return function(c) {
      return bind32(gets9(function(v) {
        return v.value0.drvEnv;
      }))(function(env) {
        var ctr = runGenM(env)(genConstraintsCmd(c));
        return bind32(liftErr4(ctr)(mn)("generate constraints command"))(function(v) {
          var vm = runSolveM(v.value1.value1)(solve);
          return bind32(liftErr5(vm)(mn)("solving constraints command"))(function(v1) {
            var c$prime$prime = substTyvars13(v1.value1)(v.value0);
            var ck = runKindM(env)(kindCommand(c$prime$prime));
            return liftErr22(ck)(mn)("kinding command (after infer)");
          });
        });
      });
    };
  };
  var getVarOrder2 = function(v) {
    if (isEmpty(v.value0.progVars)) {
      return pure31(Nil.value);
    }
    ;
    return bind32(debug("ordering variables in " + show65(v.value0.progName)))(function() {
      return bind32(gets9(function(v1) {
        return v1.value0.drvEnv;
      }))(function(env) {
        var progOrder = runDepM(env)(depOrderProgram(v));
        return bind32(liftErr7(progOrder)(v.value0.progName)("dependency order (variables)"))(function(progOrder$prime) {
          var orderStr = intercalate20(", ")(map46(show220)(progOrder$prime));
          return bind32(debug("ordered variables: " + orderStr))(function() {
            return pure31(progOrder$prime);
          });
        });
      });
    });
  };
  var getInferOrder = function(v) {
    return function(v1) {
      if (v1 instanceof Nil) {
        return applySecond4(debug("No imports for " + (show65(v.value0.progName) + ", skipping import order")))(pure31(Nil.value));
      }
      ;
      return bind32(debug("Ordering Imports for " + show65(v.value0.progName)))(function() {
        return bind32(gets9(function(v2) {
          return v2.value0.drvEnv;
        }))(function(env) {
          var order = runDepM(env)(depOrderModule(v)(v1));
          return bind32(liftErr7(order)(v.value0.progName)("dependency order (modules)"))(function(order$prime) {
            var indexFun = function(v2) {
              return function(v3) {
                return compare4(elemIndex4(v2.value0.progName)(order$prime))(elemIndex4(v3.value0.progName)(order$prime));
              };
            };
            var impsSorted = sortBy(indexFun)(v1);
            return bind32(debug("ordered imports: " + intercalate20(", ")(map46(function(v2) {
              return show65(v2.value0.progName);
            })(impsSorted))))(function() {
              return pure31(impsSorted);
            });
          });
        });
      });
    };
  };
  var getImports = function(v) {
    var splitImps = function(v1) {
      if (v1 instanceof Nil) {
        return new Tuple(Nil.value, Nil.value);
      }
      ;
      if (v1 instanceof Cons && v1.value0.value1 instanceof Nothing) {
        var v2 = splitImps(v1.value1);
        return new Tuple(new Cons(v1.value0.value0, v2.value0), v2.value1);
      }
      ;
      if (v1 instanceof Cons && v1.value0.value1 instanceof Just) {
        var v2 = splitImps(v1.value1);
        return new Tuple(v2.value0, new Cons(new Tuple(v1.value0.value0, v1.value0.value1.value0), v2.value1));
      }
      ;
      throw new Error("Failed pattern match at Driver.Driver (line 99, column 5 - line 99, column 116): " + [v1.constructor.name]);
    };
    return bind32(debug("loading standard library imports"))(function() {
      var imps = map46(function(v12) {
        return v12.value0.importName;
      })(v.value0.progImports);
      var maybeSrcs = map46(function(mn) {
        return new Tuple(mn, lookup11(mn)(libMap));
      })(imps);
      var v1 = splitImps(maybeSrcs);
      return bind32(unless4($$null(v1.value0))(throwError20(new ErrNotStdLib(v1.value0))))(function() {
        var impNames = map46(fst)(v1.value1);
        return bind32(debug("Imports to load: " + intercalate20(", ")(map46(show65)(impNames))))(function() {
          return bind32(for12(v1.value1)(function(v2) {
            return withDebug(false)(parseProg(v2.value0))(v2.value1);
          }))(function(impsParsed) {
            return pure31(impsParsed);
          });
        });
      });
    });
  };
  var desugarProg = function(v) {
    return bind32(debug("desugaring program " + show65(v.value0.progName)))(function() {
      return bind32(gets9(function(v1) {
        return v1.value0.drvEnv;
      }))(function(env) {
        var prog$prime = runDesugarM(env)(v.value0.progName)(desugarProgram(v));
        return liftErr8(prog$prime)(v.value0.progName)("desugaring");
      });
    });
  };
  var inferProgram = function(v) {
    return ifM2(inEnv(v.value0.progName))(getProg(v.value0.progName))(bind32(debug("------ Loading Imports ------"))(function() {
      return bind32(getImports(v))(function(imports) {
        return bind32(getInferOrder(v)(imports))(function(impsOrdered) {
          return bind32(inferImportsOrdered(impsOrdered))(function() {
            return bind32(debug(""))(function() {
              return bind32(debug("------ Desugaring Program ------"))(function() {
                return bind32(desugarProg(v))(function(v1) {
                  return bind32(debug("Succcessfully desugared program\n" + show75(v1)))(function() {
                    return bind32(debug(""))(function() {
                      return bind32(debug("------ Type inference ------"))(function() {
                        return bind32(debug("checking type declarations in " + show65(v1.value0.progName)))(function() {
                          return bind32(for22(v1.value0.progDecls)(inferDataDecl(v1.value0.progName)))(function(decls$prime) {
                            return bind32(debug(""))(function() {
                              return bind32(getVarOrder2(v1))(function(progOrder) {
                                var indexFun = function(v2) {
                                  return function(v3) {
                                    return compare4(elemIndex1(v2.value0.varName)(progOrder))(elemIndex1(v3.value0.varName)(progOrder));
                                  };
                                };
                                var varsSorted = sortBy(indexFun)(map46(snd)(toUnfoldable12(v1.value0.progVars)));
                                return bind32(debug("inferring types of variables\n"))(function() {
                                  return bind32(for12(varsSorted)(inferVarDecl(v1.value0.progName)))(function(typedVars) {
                                    return bind32(debug("------ Kind inference ------"))(function() {
                                      return bind32(for12(typedVars)(kindVarDecl(v1.value0.progName)))(function(kindedVars) {
                                        var varmap = fromFoldable10(map46(function(v2) {
                                          return new Tuple(v2.value0.varName, v2);
                                        })(kindedVars));
                                        return bind32(for3(v1.value0.progMain)(inferCommand(v.value0.progName)))(function(main$prime) {
                                          return pure31(new Program2({
                                            progName: v.value0.progName,
                                            progDecls: decls$prime,
                                            progVars: varmap,
                                            progMain: main$prime,
                                            progSrc: v.value0.progSrc
                                          }));
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    }));
  };
  var inferImportsOrdered = function(v) {
    if (v instanceof Nil) {
      return applySecond4(debug("No imports to infer"))(pure31(unit));
    }
    ;
    return bind32(gets9(function(v1) {
      return v1.value0.drvEnv;
    }))(function(v1) {
      var imports$prime = filter(function(v2) {
        return isNothing(lookup11(v2.value0.progName)(v1));
      })(v);
      var impNames = map46(function(v2) {
        return v2.value0.progName;
      })(imports$prime);
      return bind32(debug("Loading imports not found in environment: " + intercalate20(", ")(map46(show65)(impNames))))(function() {
        return bind32(for12(imports$prime)(function(x) {
          return withDebug(false)(inferProgram)(x);
        }))(function() {
          return bind32(debug("Successfully loaded imports"))(function() {
            return pure31(unit);
          });
        });
      });
    });
  };
  var runStr = function(mn) {
    return function(progText) {
      return function(withTrace) {
        return bind32(parseProg(mn)(progText))(function(progParsed$prime) {
          return bind32(inferProgram(progParsed$prime))(function(prog) {
            if (withTrace) {
              return map115(Right.create)(runProgramTrace(prog));
            }
            ;
            return map115(Left.create)(runProgram(prog));
          });
        });
      };
    };
  };

  // output/Definitions/index.js
  var map47 = /* @__PURE__ */ map(functorList);
  var toUnfoldable13 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var show58 = /* @__PURE__ */ show(showVariable);
  var show128 = /* @__PURE__ */ show(showTy3);
  var intercalate21 = /* @__PURE__ */ intercalate(foldableList)(monoidString);
  var showInSrc2 = /* @__PURE__ */ showInSrc(errorDriverError);
  var show221 = /* @__PURE__ */ show(showCommand);
  var show314 = /* @__PURE__ */ show(/* @__PURE__ */ showList(showCommand));
  var intercalate1 = /* @__PURE__ */ intercalate(foldableMaybe)(monoidString);
  var lookup14 = /* @__PURE__ */ lookup(ordModulename);
  var ResErr = /* @__PURE__ */ function() {
    function ResErr2(value0) {
      this.value0 = value0;
    }
    ;
    ResErr2.create = function(value0) {
      return new ResErr2(value0);
    };
    return ResErr2;
  }();
  var ResSucc = /* @__PURE__ */ function() {
    function ResSucc2(value0) {
      this.value0 = value0;
    }
    ;
    ResSucc2.create = function(value0) {
      return new ResSucc2(value0);
    };
    return ResSucc2;
  }();
  var ProgramInput = /* @__PURE__ */ function() {
    function ProgramInput2(value0) {
      this.value0 = value0;
    }
    ;
    ProgramInput2.create = function(value0) {
      return new ProgramInput2(value0);
    };
    return ProgramInput2;
  }();
  var RunProg = /* @__PURE__ */ function() {
    function RunProg2() {
    }
    ;
    RunProg2.value = new RunProg2();
    return RunProg2;
  }();
  var stateOutput = function(v) {
    var getEnvTrace = function(v1) {
      var progList = map47(snd)(toUnfoldable13(v1));
      var progVars = concatMap(function(v2) {
        return map47(snd)(toUnfoldable13(v2.value0.progVars));
      })(progList);
      var varsTys = map47(function(v2) {
        return new Tuple(v2.value0.varName, v2.value0.varTy);
      })(progVars);
      var varsShown = map47(function(v2) {
        return show58(v2.value0) + (" :: " + show128(v2.value1));
      })(varsTys);
      return intercalate21("\n")(varsShown);
    };
    return {
      debugTr: intercalate21("\n")(v.value0.drvDebug),
      typesTr: getEnvTrace(v.value0.drvEnv)
    };
  };
  var toRunResult = function(v) {
    return function(v1) {
      if (v1.value0 instanceof Left) {
        var v2 = stateOutput(v1.value1);
        return new ResErr({
          errMsg: showInSrc2(v1.value0.value0)(v),
          errDebug: v2.debugTr,
          errTypes: v2.typesTr
        });
      }
      ;
      if (v1.value0 instanceof Right && v1.value0.value0 instanceof Left) {
        var v2 = stateOutput(v1.value1);
        return new ResSucc({
          succCmd: show221(v1.value0.value0.value0),
          succTrace: "",
          succDebug: v2.debugTr,
          succTypes: v2.typesTr
        });
      }
      ;
      if (v1.value0 instanceof Right && v1.value0.value0 instanceof Right) {
        var v2 = stateOutput(v1.value1);
        return new ResSucc({
          succCmd: show221(v1.value0.value0.value0.value0),
          succTrace: show314(v1.value0.value0.value0.value1),
          succDebug: v2.debugTr,
          succTypes: v2.typesTr
        });
      }
      ;
      throw new Error("Failed pattern match at Definitions (line 42, column 1 - line 42, column 104): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var runProg = function(progSource) {
    return toRunResult(progSource)(runDriverM(initialDriverState)(runStr(new Modulename("WebInput"))(progSource)(true)));
  };
  var initialState = function(v) {
    return {
      progSrc: intercalate1("\n")(lookup14(new Modulename("Bool"))(libMap)),
      runRes: new ResSucc({
        succCmd: "",
        succTrace: "",
        succDebug: "",
        succTypes: ""
      })
    };
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail3 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step4 = bhead(step4);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step4)) {
                status = RETURN;
                fail3 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util.fromRight(step4);
              }
              break;
            case CONTINUE:
              switch (step4.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step4._2;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step4 = util.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util.left, util.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util.left, step4._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step4 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util.left(step4._1);
                  step4 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential3(util, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail3 || step4;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step4 = attempt._2(util.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util.fromRight(step4);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util.fromRight(step4);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step4 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step4 = attempt._1.failed(util.fromLeft(fail3))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step4));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util.fromLeft(fail3);
                }, 0);
              } else if (util.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step4);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail3 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head5 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step4.tag) {
              case FORKED:
                if (step4._3 === EMPTY) {
                  tmp = fibers[step4._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head5 === null) {
                  break loop;
                }
                step4 = head5._2;
                if (tail2 === null) {
                  head5 = null;
                } else {
                  head5 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step4 = step4._2;
                break;
              case APPLY:
              case ALT:
                if (head5) {
                  tail2 = new Aff2(CONS, head5, tail2);
                }
                head5 = step4;
                step4 = step4._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head5, tail2) {
        var fail3, step4, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail3 = result;
          step4 = null;
        } else {
          step4 = result;
          fail3 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head5 === null) {
              cb(fail3 || step4)();
              return;
            }
            if (head5._3 !== EMPTY) {
              return;
            }
            switch (head5.tag) {
              case MAP:
                if (fail3 === null) {
                  head5._3 = util.right(head5._1(util.fromRight(step4)));
                  step4 = head5._3;
                } else {
                  head5._3 = fail3;
                }
                break;
              case APPLY:
                lhs = head5._1._3;
                rhs = head5._2._3;
                if (fail3) {
                  head5._3 = fail3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail3 === lhs ? head5._2 : head5._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail3, null, null);
                      } else {
                        join3(fail3, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head5._3 = step4;
                }
                break;
              case ALT:
                lhs = head5._1._3;
                rhs = head5._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail3 = step4 === lhs ? rhs : lhs;
                  step4 = null;
                  head5._3 = fail3;
                } else {
                  head5._3 = step4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step4 === lhs ? head5._2 : head5._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step4, null, null);
                      } else {
                        join3(step4, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head5 = null;
            } else {
              head5 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step4 = par;
        var head5 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step4.tag) {
                  case MAP:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                    step4 = step4._2;
                    break;
                  case APPLY:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  case ALT:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step4;
                    step4 = new Aff2(FORKED, fid, new Aff2(CONS, head5, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step4)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head5 === null) {
                  break loop;
                }
                if (head5._1 === EMPTY) {
                  head5._1 = step4;
                  status = CONTINUE;
                  step4 = head5._2;
                  head5._2 = EMPTY;
                } else {
                  head5._2 = step4;
                  step4 = head5;
                  if (tail2 === null) {
                    head5 = null;
                  } else {
                    head5 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_7 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_14 = traverse_7(dictFoldable);
        return function(f) {
          var $51 = traverse_14(function($53) {
            return parallel4(f($53));
          });
          return function($52) {
            return sequential3($51($52));
          };
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictApplicative) {
      var parTraverse_2 = parTraverse_1(dictApplicative);
      return function(dictFoldable) {
        return parTraverse_2(dictFoldable)(identity11);
      };
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy8 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure32 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map48 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map116 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy8("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure210 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure210(unit))($$const(fin))($$const(a2));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Apply0: function() {
      return applyAff;
    },
    Apply1: function() {
      return applyParAff;
    }
  };
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var applicativeParAff = {
    pure: function($76) {
      return parallel2(pure210($76));
    },
    Apply0: function() {
      return applyParAff;
    }
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($77) {
    return Canceler($$const(liftEffect2($77)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map48(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map116(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure32(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map48(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped4(function($83) {
        return liftEffect2(k($83));
      })($$try3(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
    };
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure210(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure210(unit));

  // output/Events/index.js
  var lookup15 = /* @__PURE__ */ lookup(ordModulename);
  var selectExample = function(nm) {
    var v = lookup15(new Modulename(nm))(libMap);
    if (v instanceof Just) {
      return new ProgramInput(v.value0);
    }
    ;
    if (v instanceof Nothing) {
      return new ProgramInput("");
    }
    ;
    throw new Error("Failed pattern match at Events (line 22, column 20 - line 24, column 29): " + [v.constructor.name]);
  };
  var runSrc = function(v) {
    return RunProg.value;
  };
  var handleAction = function(dictMonadState) {
    var modify_3 = modify_2(dictMonadState);
    var bind36 = bind(dictMonadState.Monad0().Bind1());
    var gets10 = gets(dictMonadState);
    return function(inp) {
      if (inp instanceof ProgramInput) {
        return modify_3(function(st) {
          var $14 = {};
          for (var $15 in st) {
            if ({}.hasOwnProperty.call(st, $15)) {
              $14[$15] = st[$15];
            }
            ;
          }
          ;
          $14.progSrc = inp.value0;
          return $14;
        });
      }
      ;
      if (inp instanceof RunProg) {
        return bind36(gets10(function(st) {
          return st.progSrc;
        }))(function(src9) {
          var res = runProg(src9);
          return modify_3(function(st) {
            var $18 = {};
            for (var $19 in st) {
              if ({}.hasOwnProperty.call(st, $19)) {
                $18[$19] = st[$19];
              }
              ;
            }
            ;
            $18.runRes = res;
            return $18;
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Events (line 27, column 20 - line 32, column 37): " + [inp.constructor.name]);
    };
  };
  var getSrc = function(newSrc) {
    return new ProgramInput(newSrc);
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map49 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map49(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map50 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map50(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Halogen.Aff.Util/index.js
  var bind33 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure33 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map51 = /* @__PURE__ */ map(functorEffect);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var throwError21 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind33(liftEffect3(bindFlipped5(composeKleisliFlipped2(function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document))(windowImpl)))(function(mel) {
      return pure33(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped5(readyState)(bindFlipped5(document)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map51(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard3(bindAff)(awaitLoad)(function() {
    return bind33(selectElement("body"))(function(body2) {
      return maybe(throwError21(error("Could not find body")))(pure33)(body2);
    });
  });

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Halogen.Data.Slot/index.js
  var foreachSlot = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_7(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty4 = empty2;

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step3 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map52 = /* @__PURE__ */ map(functorArray);
  var map117 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap3 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map52(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map52(map117(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap3(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy9 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy9("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step3(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy9("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy9("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step3(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy9("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step3(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy9("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail2 = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton8($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure111 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail2(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure111(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object/index.js
  var lookup16 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy10 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup16("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener2(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy10("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text5 = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity12 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift2.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure39 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure39(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift2) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply3(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure39 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure39(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift2) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton8(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity12);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons4 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc3 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null4 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty6 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc3(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr11 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl4 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons4(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl4(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons5 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null4(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr11(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty7 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append5 = link;
  var semigroupCatList = {
    append: append5
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append5(cat)(new CatCons(a2, empty6));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy11 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append6 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append6(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons5(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty7);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc4(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy11("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure34 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure34($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map119 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map119(Done.create)(pure111(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map119(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM5(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var bind34 = /* @__PURE__ */ bind(bindEffect);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void4(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_(function(v) {
            return append7(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind34(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift3 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var functorHalogenM = freeFunctor;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize2(value0) {
      this.value0 = value0;
    }
    ;
    Initialize2.create = function(value0) {
      return new Initialize2(value0);
    };
    return Initialize2;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy12 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy12("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step3(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map53 = /* @__PURE__ */ map(functorHalogenM);
  var pure35 = /* @__PURE__ */ pure(applicativeHalogenM);
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map53(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure35(unit)),
      handleQuery: $$const(pure35(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var h1 = /* @__PURE__ */ element2("h1");
  var h1_ = /* @__PURE__ */ h1([]);
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var option2 = /* @__PURE__ */ element2("option");
  var option_ = /* @__PURE__ */ option2([]);
  var select3 = /* @__PURE__ */ element2("select");
  var textarea = function(es) {
    return element2("textarea")(es)([]);
  };
  var div3 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div3([]);
  var button = /* @__PURE__ */ element2("button");
  var br = function(props) {
    return element2("br")(props)([]);
  };
  var br_ = /* @__PURE__ */ br([]);
  var body = /* @__PURE__ */ element2("body");

  // output/Halogen.HTML.Properties/index.js
  var unwrap5 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var readOnly3 = /* @__PURE__ */ prop1("readOnly");
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var id2 = /* @__PURE__ */ prop22("id");
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap5($37));
    };
  }();
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();
  var style = /* @__PURE__ */ attr2("style");

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_7(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component2) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty4)();
            var childrenOut = $$new(empty4)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component: component2,
              state: component2.initialState(input3),
              refs: empty2,
              children: empty4,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup18 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind110 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard4(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure36 = /* @__PURE__ */ pure(applicativeAff);
  var map55 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map118 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map211 = /* @__PURE__ */ map(functorMaybe);
  var insert13 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete7 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert14 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup19 = /* @__PURE__ */ lookup(ordForkId);
  var lookup23 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped6(lookup18(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind110(liftEffect4(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind110(liftEffect4(f))(function(result) {
          return bind110(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure36(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind110(liftEffect4(read(ref2)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render2) {
    return function(ref2) {
      return function(q2) {
        return bind110(liftEffect4(read(ref2)))(function(v) {
          return evalM(render2)(ref2)(v["component"]["eval"](new Query(map55(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render2) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind110(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind110(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render2)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map118(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind110(liftEffect4(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure36(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers,
                    state: v3.value1
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render2(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure36(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind110(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind110(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render2)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind110(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_(map211(insert13(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure36(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure36(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift3) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind110(liftEffect4(read(ref2)))(function(v2) {
                return bind110(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure36(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render2)(ref2);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind110(fresh(ForkId)(ref2))(function(fid) {
                return bind110(liftEffect4(read(ref2)))(function(v2) {
                  return bind110(liftEffect4($$new(false)))(function(doneRef) {
                    return bind110(fork3($$finally(liftEffect4(function __do2() {
                      modify_($$delete7(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert14(fid)(fiber))(v2.forks))))(function() {
                        return pure36(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind110(liftEffect4(read(ref2)))(function(v2) {
                return bind110(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup19(v1.value0)(forkMap)))(function() {
                    return pure36(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind110(liftEffect4(read(ref2)))(function(v2) {
                return bind110(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup19(v1.value0)(forkMap)))(function() {
                    return pure36(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind110(liftEffect4(read(ref2)))(function(v2) {
                return pure36(v1.value1(lookup23(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render2) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers,
              refs: alter2($$const(v.value1))(v.value0)(st.refs)
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind110(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render2)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind35 = /* @__PURE__ */ bind(bindEffect);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork(monadForkAff);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard5(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure37 = /* @__PURE__ */ pure(applicativeEffect);
  var map56 = /* @__PURE__ */ map(functorEffect);
  var pure110 = /* @__PURE__ */ pure(applicativeAff);
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void5 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM4 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind111 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do2() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)(function() {
        var $59 = traverse_5(fork4);
        return function($60) {
          return handleAff($59(reverse($60)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped7(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped7(traverse_33(function() {
        var $61 = killFiber(error("finalized"));
        return function($62) {
          return handleAff($61($62));
        };
      }()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component2) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render2)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped7(unDriverStateX(function() {
                    var $63 = render2(lchs);
                    return function($64) {
                      return $63(function(v) {
                        return v.selfRef;
                      }($64));
                    };
                  }()))(read($$var2))();
                  bindFlipped7(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot) {
                  return function __do2() {
                    var childrenIn = map56(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure110(unit))(handler3);
                              return function($66) {
                                return $65(slot.output($66));
                              };
                            }())();
                            return handleAff(evalM(render2)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure110(unit))(handler3);
                          return function($68) {
                            return $67(slot.output($68));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map56(function($69) {
                      return isJust(slot.get($69));
                    })(read(childrenOutRef))();
                    when4(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot.set($$var2))(childrenOutRef)();
                    return bind35(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure37(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render2 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map56(isNothing)(read(v.pendingHandlers))();
              when4(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty4)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render2)(v.selfRef);
                return function($72) {
                  return $70($$void5($71($72)));
                };
              }();
              var childHandler = function() {
                var $73 = queueOrRun(v.pendingQueries);
                return function($74) {
                  return $73(handler3(Action.create($74)));
                };
              }();
              var rendering = renderSpec2.render(function($75) {
                return handleAff(handler3($75));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers,
                  rendering: new Just(rendering),
                  children: children2
                };
              }))();
              return when4(shouldProcessHandlers)(flip(tailRecM4)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $76 = traverse_5(fork4);
                    return function($77) {
                      return handleAff($76(reverse($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null)(mmore);
                  if ($52) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind111(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure110(Nothing.value);
                }
                ;
                return evalQ(render2)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind111(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind111(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create3();
              var dsx = bindFlipped7(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect5($78($79));
                };
              }())(i2)(component2))();
              return unDriverStateX(function(st) {
                return pure37({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map57 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map57(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map57(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy13 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
  var pure38 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap6 = /* @__PURE__ */ unwrap();
  var when5 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity13 = /* @__PURE__ */ identity(categoryFn);
  var bind112 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map58 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void6(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void6(appendChild(v)(v2.value0));
        }
        ;
        return pure38(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document2) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap6)(spec);
          var $lazy_patch = $runtime_lazy13("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step3(st.value0, slot.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy13("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step4 = buildThunk2(slot.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy13("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render2 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render2;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document2
        };
      };
    };
  };
  var renderSpec = function(document2) {
    return function(container) {
      var render2 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document2);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void6(appendChild(node)(toNode(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step3(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when5(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render2,
        renderChild: identity13,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component2) {
    return function(i2) {
      return function(element3) {
        return bind112(liftEffect6(map58(toDocument)(bindFlipped8(document)(windowImpl))))(function(document2) {
          return runUI(renderSpec(document2)(element3))(component2)(i2);
        });
      };
    };
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value14) {
    return value14 == null ? f : s(value14[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail3 = fail2(dictMonad);
    var pure39 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail3(new TypeMismatch("object", typeOf(value14))), pure39, k, value14);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map59 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map59(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop3))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onValueChange = /* @__PURE__ */ addForeignPropHandler(change)("value")(readString2);

  // output/Layout/index.js
  var value13 = /* @__PURE__ */ value12(isPropString);
  var show59 = /* @__PURE__ */ show(showInt);
  var show129 = /* @__PURE__ */ show(showModulename);
  var progDiv = function(src9) {
    return div3([class_("prog")])([textarea([id2("progInput"), value13(src9), onValueChange(getSrc)]), br_, button([id2("runButton"), onClick(runSrc)])([text5("Run")])]);
  };
  var getArea = function(contents) {
    return function(cl) {
      return function(htmlId) {
        var nlines = length(split("\n")(contents));
        return textarea([class_(cl), id2(htmlId), readOnly3(true), value13(contents), style("height:" + (show59(nlines) + "em;"))]);
      };
    };
  };
  var resDiv = function(v) {
    if (v instanceof ResErr) {
      return div3([class_("results")])([h1_([text5("Results")]), h2_([text5("Output")]), getArea("Error: " + v.value0.errMsg)("evalError")("evalRes"), br_, h2_([text5("Inferred Types")]), getArea(v.value0.errTypes)("results")("typesStr"), h2_([text5("Debug Trace")]), getArea(v.value0.errDebug)("results")("debugStr")]);
    }
    ;
    if (v instanceof ResSucc) {
      return div3([class_("results")])([h1_([text5("Results")]), h2_([text5("Output")]), getArea(v.value0.succCmd)("evalSucc")("evalRes"), br_, h2_([text5("Inferred Types")]), getArea(v.value0.succTypes)("results")("typesStr"), h2_([text5("Evaluation Trace")]), br_, textarea([id2("traceStr"), readOnly3(true), value13(v.value0.succTrace)]), h2_([text5("Debug Trace")]), getArea(v.value0.succDebug)("results")("debugStr")]);
    }
    ;
    throw new Error("Failed pattern match at Layout (line 37, column 1 - line 37, column 45): " + [v.constructor.name]);
  };
  var exSelect = /* @__PURE__ */ div_([/* @__PURE__ */ text5("Choose Example "), /* @__PURE__ */ select3([/* @__PURE__ */ id2("exampleSelect"), /* @__PURE__ */ onValueChange(selectExample)])(/* @__PURE__ */ map(functorArray)(function(v) {
    return option_([text5(show129(v.value0))]);
  })(/* @__PURE__ */ toUnfoldable2(unfoldableArray)(libMap))), br_]);
  var layout = function(src9) {
    return function(res) {
      return body([])([h1_([text5("One Sided Sequent Calculus")]), exSelect, progDiv(src9), resDiv(res)]);
    };
  };
  var render = function(v) {
    return layout(v.progSrc)(v.runRes);
  };

  // output/Runner/index.js
  var $$eval2 = /* @__PURE__ */ function() {
    return mkEval({
      handleQuery: defaultEval.handleQuery,
      receive: defaultEval.receive,
      initialize: defaultEval.initialize,
      finalize: defaultEval.finalize,
      handleAction: handleAction(monadStateHalogenM)
    });
  }();
  var component = /* @__PURE__ */ mkComponent({
    initialState,
    render,
    "eval": $$eval2
  });
  var uiRunner = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component)(RunProg.value)(body2);
  }));

  // output/Main/index.js
  var main2 = uiRunner;

  // <stdin>
  main2();
})();
