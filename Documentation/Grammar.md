# Grammar 

## Kinds 

```
K :: =          Kinds
          CBV   Call by value  
        | CBN   Call by name 
        | CBA   Call by any 
```

## Terms 

All defined types define a set of xtor names `xtn`. 
We additionally assume a finite set of variables `x`.

```
p ::=   xtn(x,...) => c   Patterns
```

```
t ::=                           Terms 
          x                     Variables 
        | mu x. c               Mu-abstractions 
        | case  { p, ... }      XCases 
        | { t : K }             Shifts
        | xtn(t,...)            Xtors
```

## Commands 

Error messages are simply strings `str`

```
c ::=                       Commands 
        error str           Errors 
        | Done              Done
        | < t | K | t >     Cuts
        | Print t           Print 

```

## Types 

All defined types are defined by their type name `tyn`

```
ty ::=                          Types 
        tyn(ty,...)             Declared Types 
        | x                     Type variables  
        | Forall x ... . ty     Generalized Type
        | {ty}                  Shifts           
        | co ty                 Cotypes
```


## Definitions 

module modulename
import modulename
(co)data tyn(x1:+/-,...,xn:+/-) { xtn(xi1,...,xin) } 
main := c
var := t
v
