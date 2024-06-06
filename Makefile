SPAGO = spago
PACKAGE = one-sided-sc
FFISH = one-sided-examples/stdlib/libToJs.sh

.PHONY: examples build test  

examples: 
	echo "Compiling .os files to javascript"
	$(FFISH)

build: examples 
	echo "building web app"
	echo "trying to use spago legacy"
	if $(SPAGO) bundle-app -t ./web-app/index.js -p browser; then \
		echo "build succeeded using spago legacy"; \
	else \
		echo "Could not build using spago legacy, trying spago@next instead"; \
		$(SPAGO) bundle -p $(PACKAGE); \
	fi

test: examples
	echo "running test suite";
	$(SPAGO) test

full: test build
