SPAGO = spago bundle
MAIN = ./web-app/index.html
PACKAGE = one-sided-web-app

.PHONY: web-app

build: 
	$(SPAGO) -p $(PACKAGE) --platform browser

run: build 
	xdg-open $(MAIN) & true
