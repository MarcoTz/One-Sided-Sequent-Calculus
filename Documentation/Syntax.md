# Syntax 

## Terms 

* Variables are identified by their names `x`.
* Mu-abstractions use the keyword `mu` or `Mu` or unicode mu in `mu x. c`
* Cases: `case  { xtn1(x11,...,x1n) => c1,... xtnm(xm1,...,xmn) => cm }`
* Xtor terms: `xtn(t1,...,tn)`
* Shifts: `{ t : K }` with `K` either `CBV` or `CBN`

## Sugar 

In addition to the core language terms, there is also a number of syntactic sugar constructs 

* Lambda abstractions: `\x.t`
* Lists: `[t1,t2,...,tn]`
* Boolean negations: `!t`
* If-branches: `If t1 then t2 else t3`
* Pairs: `(t1,t2,...,tn)`
* Sequences (i.e. `(\_.t2) t1`): `t1;t2`
* Boolean And: `t1 && t2`
* Boolean Or: `t1 || t2`
* Function application `t1 t2`

## Commands 

* Errors: `error "msg"`
* Done: `Done` or `done`
* Cuts: `< t1 | K | t2 >` with `K` wither `CBV` or `CBN`
* Print: `Print t` or with a type annotation for `t`:  `Print t:ty`

### Sugar

Additionally, there is sugar for some additional commands 

* `CBV` cuts: `t1 >> t2`
* `CBN` cuts: `t1 << t2`
* Case-of: `case t of {...}` where `...` are the usual patterns


## Types 

* generalized types: `forall x1 x2 ... xn. ty`
* shifts: `{ ty }`
* Cotypes: `co ty`
* Algebraic types are identified by their names `tyn(ty1,...)`
* Variables are identified by their names `x` 

### Sugar 

The function type can also be written using sugar `ty1 -> ty2`

## Definitions 

Definitions are written the same way as in the grammar.
