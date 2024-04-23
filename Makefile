SPAGO = spago
MAIN = ./web-app/index.html
PACKAGE = one-sided-sc

.PHONY: build test  

build: 
	$(SPAGO) bundle -p $(PACKAGE) 

test: 
	$(SPAGO) test
