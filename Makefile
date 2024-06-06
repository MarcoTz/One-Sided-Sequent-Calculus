SPAGO = spago
PACKAGE = one-sided-sc
FFISH = one-sided-examples/stdlib/libToJs.sh

.PHONY: examples build test  

examples: 
	echo "Compiling .os files to javascript"
	$(FFISH)

build: examples  
	echo "building web app"
	$(SPAGO) bundle-app -t ./web-app/index.js -p browser
#Command for spago@next, currently not working
#	$(SPAGO) bundle -p $(PACKAGE) 

test: examples
	echo "running test suite"
	$(SPAGO) test

full: test build
