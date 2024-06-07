# Kinding

## Types 

```
--------------
Gamma => X : K 
```

```
tyn(args':+,args'':-) in Gamma,  Gamma => args in args' : K, Gamma => args in args'' : -K
------------------------------------------------------------------------------------------
                            Gamma => tyn(args) : K
```

```
 Gamma => ty : -K 
------------------
Gamma => {ty} : K 
```

```
Gamma => ty : K 
-----------------
Gamma => co ty : K
```

```
     Gamma => ty : K 
---------------------------
Gamma => forall Xi. ty : K 
```

### Terms 

```
  Gamma => ty : K 
----------------------
Gamma => prdcns v :: (ty : K)
```

```
   Gamma => c,  Gamma => ty : K
----------------------------------
Gamma => prdcns mu x.c :: (ty : K)
```

```
cdt tyn(...){ ... xtn(argTys) ... } in Gamma, Gamma => args :: (argTys : K) 
       cdt == Data && prdcns=prd || cdt == Codata && prdcns=cns 
---------------------------------------------------------------------------
            Gamma => prdcns xtn(args) :: (tyn(tyArgs) : K)
```

```
cdt tyn(...){ ... xtn(argTys) ... } in Gamma, Gamma => args :: (argTys : K) 
       cdt == Codata && prdcns=prd || cdt == Data && prdcns=cns 
---------------------------------------------------------------------------
            Gamma => prdcns xtn(args) :: (co tyn(tyArgs) : K)
```

```
         cdt tyn(...)  in Gamma, Gamma => pts
cdt == Data && prdcns == cns || cdt == Codata && prdcns == prd
---------------------------------------------------------------
       Gamma => prdcns case { pts } :: (tyn(args) : K)
```
```
         cdt tyn(...)  in Gamma, Gamma => pts
cdt == Codata && prdcns == cns || cdt == Data && prdcns == prd
---------------------------------------------------------------
       Gamma => prdcns case { pts } :: (co tyn(args) : K)
```

```
      Gamma => c 
------------------------
Gamma => (xtn(args) => c)  
```

```
K != CBN,    Gamma => t :: ty : CBV,     Gamma => ty : CBV
-----------------------------------------------------------
       Gamma => prdcns {t:CBV} :: (ty : K)
```
```
K != CBV,    Gamma => t :: ty : CBN,     Gamma => ty : CBN
-----------------------------------------------------------
       Gamma => prdcns {t:CBN} :: (ty : K)
```


## Commands 

```
Gamma => prdcns t :: (ty:K),    Gamma => -prdcns u::(ty:K)
----------------------------------------------------------
                Gamma -> < t | K | u> 
```

```
--------------
Gamma => Done 
```

``` 
------------------
Gamma => error str 
``` 

```
Gamma => t :: (ty : K) 
----------------------
   Gamma => Print t 
```
