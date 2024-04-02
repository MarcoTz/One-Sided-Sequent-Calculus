module cex16

import Unit;

-- polarities of terms being cut needs to be different 
x :: Unit :+;
x := mu y.<MkUnit | + | MkUnit>;
