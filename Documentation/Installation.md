# Installation

The current version of this language was programmed in [purescript](https://www.purescript.org/), in order to allow direct compilation to javascript.
The project is managed using the purescript package manager [spago](https://github.com/purescript/spago), which needs to be installed in addition to purescript itself.
When running `spago build` or `spago run`, all required purescript packages are installed  and the modules are compiled in order.
However, this does not provide a single javascript file that can be loaded in HTML. 
Instead, purescript has to be bundled with the `spago bundle` command (see Makefile), which additionally requires [esbuild](https://esbuild.github.io/) to be installed.
Calling `spago bundle` (or `make build`) will then create an `index.js` file in the `web-app` directory, and the web-app can be run by simply opening the `index.html` file in any browser.
