SPAGO = spago bundle
PACKAGE = one-sided-web-app

.PHONY: web-app

web-app: 
	$(SPAGO) -p $(PACKAGE) --platform browser

