module cex05 

import Unit;

-- case of unit needs to be -
wrongPol :: Unit:+;
wrongPol := case { MkUnit => Done };
