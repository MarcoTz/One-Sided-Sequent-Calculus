SPAGO = spago
MAIN = ./web-app/index.html
PACKAGE = one-sided-sc

.PHONY: web-app

build: 
	$(SPAGO) bundle -p $(PACKAGE) 

test: 
	$(SPAGO) test
