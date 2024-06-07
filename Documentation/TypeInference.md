# Type Inference 

## Terms 

``` 
 v:ty in Gamma
----------------
Gamma => v : ty
```

```
 v not in Gamma, X=frehTyVar
 ---------------------------
    Gamma => v : X  
```


``` 
   X=freshTyvar, Gamma, x:X => c 
-----------------------------------
      Gamma => mu x.c : X 
``` 


```
tyn(argTys) in Gamma,    Gamma => args:argTys
---------------------------------------------
     Gamma => xtn(args) : tyn(argTys)
```

```
tyn(argTys) in Gamma,    Gamma, args:argTys => c
------------------------------------------------
        Gamma => (xtn(args) => c) 
```

```
tyn(argTys) in Gamma, Gamma => pts
------------------------------------
Gamma => case { pts } : tyn(argTys)
```
```
   Gamma => t:ty 
--------------------
Gamma => {t:K} : ty
```

## Commands 

``` 
Gamma => t:ty,   Gamma => u:ty 
------------------------------
    Gamma => <t | K | u> 
``` 

```
Gamma => t:ty,   Gamma => u:ty 
-------------------------------
  Gamma => < t | K | ty | u > 
```

```
-------------
Gamma => Done 
```

```

------------------
Gamma => error str
```

```
 Gamma => t : ty 
-------------------
 Gamma => Print(t) 
```

```
   Gamma => t :ty 
--------------------
Gamma => Print(t:ty) 
```
