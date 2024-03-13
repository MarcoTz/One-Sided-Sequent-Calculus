# Programming in the One Sided Calculus 

As in the usual sequent calculus, there are commands as well as producer and consumer terms. 
Commands are either `Done` indicating a finished computation or a cut which has the following syntax: 

```
< Term | +/- | Term>
< Term | Type | +/- | Term >
< Term | +/- | Type | Term > 
```

The `+/-` indicated the evaluation order, `+` standing for call-by-value and `-` standing for call-by-name.
Before or after the evaluation order an optional type annotation can be provided. 
Since for now only type checking is implemented, this annotation is necessarry, unless at least one of the terms is a variable for which the type is clear.

In the one-sided calculus, there are no producer and consumer terms.
Instead, a producer is a term with a positive type and a consumer is a term with negative type.
Since it is not clear, which term in a cut is the producer and which is the consumer without type inference, currently type checking uses the evaluation order to determine which is which.
That is, a cut `<t|+|u>` requires `t` to be the producer, and `<t|-|u>` requires `u` to be the producer.

Terms are one of the following 

``` 
x
Mu x. c 
xtn(t_i)
case { xtni(xij) => ci }
{t} 
{x}.c
```
There are variables `x`, Mu-abstractions where `c` is a command, xtors (that is constructor or destructors), xcases (that is cases or cocases) and shifts, where `{t}` forces `t` to have a positive type (that is, be a producer, and `{x}.c` forces `x` to only appear negative (that is as a consumer) in the command `c`.
Xtors and XCases can be used depending on their data definitions.

## Data Definitions 

To define a data or codata type, use the following syntax.

``` 
data name(xi:+/-) : +/- { 
 xtor(tyi),
 ...
} 
```

A type definition can have any number of type arguments, denoted by type variables `xi` with a polarity `+/-` which indicates arguments of this type are data or codata types.
The polarity annotation of the type itself denotes whether the type itself is a data or codata type.
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
```

A program requires a module name at the beginning, followed by any number of type and variable definitions, and possibly a number of import statements.
These require the module name of the program to be imported and the file containing the module has to have the same name with the extension `.os`.

