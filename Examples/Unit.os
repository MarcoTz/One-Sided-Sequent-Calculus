module Unit

data Unit : + { MkUnit }

test :: Unit : +;
test := mu a. error "test";
