# Grammar 

## Definitions 

Modules have to start with a line defining the module name, which is then used for imports

```
module modulename
``` 

Modules can be imported with an import statement.

```
import modulename
```

In order for a module to be successfully imported, it needs to be in the standard library (`one-sided-examples/stdlib`).

Variables are defined with the `var` keyword, followed by `:=` and a term `t` and finished with `;`. 
When a variable is recursive, it also has a `rec` keyword.

```
(rec) var := t;
```

Additionally, a `main` function can be defined, which is the only definition with right-hand side a command instead of a term.

```
main := c;
```

### Type Definitions 

Type definitions are used to define all algebraic data types that can be used within a program. 

```
(co)data tyn(x1:+/-,...,xn:+/-) { xtni(tyi1,...,tyin) } 
```

Both data and codata types can be defined with the `codata` and `data` keywords, followed by the type name `tyn`, which is then used to refer to the type.
A type can have any number of type arguments `xi` that are all either covariant (`+`) or contravariant (`-`). 
The last part of the definition is then a list of xtors (constructors/destructors) with a list of argument types for the xtor. 
The previously defined type variables can be used in these types.

## Kinds 

Kinds are just the calling conventions, call-by-any is used for terms that can be used with either.

```
K :: =          Kinds
          CBV   Call by value  
        | CBN   Call by name 
        | CBA   Call by any 
```

## Terms 

Patterns have an xtor with variables as arguemnt on the left-hand side, and a command in which these variables can be used on the right hand side.

```
p ::=   xtn(x,...) => c   Patterns
```

Using these patterns, we have the following terms
 
```
t ::=                           Terms 
          x                     Variables 
        | mu x. c               Mu-abstractions 
        | case  { p, ... }      XCases 
        | { t : K }             Shifts
        | xtn(t,...)            Xtors
```

## Commands 

Commands representing computations have the following syntax

```
c ::=                       Commands 
        error str           Errors 
        | Done              Done
        | < t | K | t >     Cuts
        | Print t           Print 

```

Error messages in the `error` command are simply strings `str`.

## Types 

The following types are possible for terms
```
ty ::=                          Types 
        tyn(ty,...)             Declared Types 
        | x                     Type variables  
        | Forall x ... . ty     Generalized Type
        | {ty}                  Shifts           
        | co ty                 Cotypes
```
