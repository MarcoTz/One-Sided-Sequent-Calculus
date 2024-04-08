module cex05 

import Unit;

-- case of unit needs to be -
wrongPol :: Unit:CBV;
wrongPol := case { MkUnit => Done };
