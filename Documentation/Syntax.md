# Syntax 

## Terms 

x 
mu x. c 
Mu x. c
case  { xtn1(x11,...,x1n) => c1,... xtnm(xm1,...,xmn) => cm }
{ t : CBV } 
{ t : CBN }
xtn(t1,...,tn) 

## Sugar 

\x.t 
[t1,t2,...,tn]
!t
If t1 then t2 else t3 
(t1,t2,...,tn)
t1;t2
t1 && t2 
t1 || t2 
t1 t2 

## Commands 
error "" 
Done 
< t1 | CBV | t2 > 
< t1 | CBN | t2 >
Print t 
Print t:ty 

### Sugar 

t1 >> t2 
case t of { }

## Types 
( ty ) 
forall x1 x2 ... xn. ty 
{ ty } 
co ty 
tyn 
x 

### Sugar 

ty1 -> ty2 

## Definitions 

module modulename
import modulename
(co)data tyn(x1:+/-,...,xn:+/-) { xtn(xi1,...,xin) } 
main := c
var := t
var ;: ty
