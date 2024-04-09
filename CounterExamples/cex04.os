module cex04

import Unit;

-- mutual recursion
test :: Unit;
test := test2;
test2 := test;
