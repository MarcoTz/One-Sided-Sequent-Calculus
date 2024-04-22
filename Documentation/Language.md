# Programming in the One Sided Calculus 

As in the usual sequent calculus, there are commands as well as producer and consumer terms. 
Commands are one of the following 
``` 
Done 
Print Term
error String 
< Term | CBV/CBN | Term>
< Term | Type : CBV/CBN | Term >
Term >> Term
Term >> Type >> Term
Term << Term
Term << Type << Term
``` 
The `Done` and `Print` commands indicate a finished computation, with `Print` outputting the result, while `error` aborts computation and returns an error message. 
All other commands are cuts between two terms, where one has to be a producer and one a consumer, both of the same type.
Such a cut always needs an evaluation order annotation, which is either `CBV` or `CBN`. 
Additionally, a cut can be annotated with a type that both sides need to have.
During type checking, such annotations are sometimes needed, and if a term is annotated, all cuts within the term need to be annotated as well with the exception where one side of the cut is a variable with a clear type (for example, if the variable was bound by a `mu` abstraction with a type annotation.
Instead of always annotating `CBV` or `CBN`, the alternative notation of `>>` or `<<` can be used to define a cut, in which case `>>` will always be evaluated as `CBV` and `<<<` as CBN.
The same way as regular cuts can be type annotated, these can be as well.

In the one-sided calculus, there are no producer and consumer terms.
Instead, a producer is a term with a positive type and a consumer is a term with negative type.

Terms are one of the following 

``` 
x
Mu x. c 
xtn(t_i)
case { xtni(xij) => ci }
{t:CBV/CBN} 
```
There are variables `x`, Mu-abstractions where `c` is a command, xtors (that is constructor or destructors), xcases (that is cases or cocases) and shifts, where `{t:CBV/CBN}` forces `t` to have the specified evaluation order,.
Xtors and XCases can be used depending on their data definitions.

## Data Definitions 

To define a data or codata type, use the following syntax.

``` 
(co-)data name(xi:+/-) { 
 xtor(tyi),
 ...
} 
```

Each type definition begins with keyword `data` or `codata` indicating if a type is a data or codata type by default. 
In the data case, all xtors are constructors while in the codata case they are destructors, and similarly for xcases.
For each data type, there is then the corresponding codata type, with the same xtors, denoted by `co tyn` and the same works for codata types.
A type definition can have any number of type arguments, denoted by type variables `xi` with a polarity `+/-` which indicates variance (`+` co- and `-` contravariance). 
Within the brackets the corresponding constructors or destructors of the type are defined, using xtor names with any number of argument types `tyi`. 
These can be any type as long as it has been defined previously.
The number and types of arguments defined here are the ones that need to be used in a xtor or xcase expression,

## Variable Definitions 

To define a variable for a term, use the syntax 

```
var name := t;
```

Anything after the `:=` will be the value of the variable, which is any of the terms above.
The definition is then ended with a `;`.
To add a type annotation to a variable use the syntax 
```
name :: ty;
```
Where after the `::` use some of the possible types.

Additionally, each program can contain a single `main` function, which cannot have a type annotation and needs to be a command.
If the program can be successfully typed, this command is then evaluated.

## Types 

The following types can be used in programs (and in particular for type annotations in cuts or for variables)
 
```
X
Name(tyi)
{ty}
co ty
Forall Xi . ty
```

Names with arguments require the corresponding data type to be defined, shifts `{ty}` force a polarity for `ty`, that is, if `ty` is the type of `t` then `{t}` has type `ty` with positive polarity, and if `c` type checks with `x:ty` then `{x}.c` forces `x` to have negative type in `c`.
The `co` type of a type is what makes the one-sidedness possible, it changes a data type to a codata type and vice versa.
For example, given a data type `Bool` of booleans with constructors `True` and `False`, the corresponding codata type `co Bool` is defined by destructors `True` and `False`.
This way, a case of `Bool` used as a producer is actually a cocase of the dual type `co Bool`.

When a type is used to annotate a term or cut, it always needs to be followed by `: +/-` to indicate whether a type or its dual type is used.
The polarity after a cut annotation refers to the polarity of the first term in the cut, that is `<t|ty:+|+|u>` requires `t` to be a producer and `<t|ty:=|+|u>` requires `t` to be a comsumer.

## Programs 

A program has the form 

``` 
module name 

import name'
...

data ...
...

var ...
...

main :=...
```

A program requires a module name at the beginning, followed by any number of type and variable definitions, and possibly a number of import statements.
Optionally, a program can also contain a main function to be evaluated. 
If none is provided, evaluating a program just amounts to type check all its definitions.
