SPAGO = spago
PACKAGE = one-sided-sc
FFISH = one-sided-examples/stdlib/libToJs.sh

.PHONY: examples build test  

examples: 
	$(FFISH)

build: examples  
	$(SPAGO) bundle -p $(PACKAGE) 

test: examples
	$(SPAGO) test
