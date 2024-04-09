module cex05 

import Unit;

wrongPol :: Unit;
wrongPol := case {MkUnit => Done };

main := <wrongPol | CBV | wrongPol>;
