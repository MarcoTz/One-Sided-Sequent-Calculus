module cex16

import Unit;

-- polarities of terms being cut needs to be different 
x :: Unit :CBV;
x := mu y.<MkUnit | CBV | MkUnit>;
