# Installation

The current version of this language was programmed in [purescript](https://www.purescript.org/), in order to allow direct compilation to javascript.
The project is managed using the purescript package manager [spago](https://github.com/purescript/spago), which needs to be installed in addition to purescript itself.
When running `spago build` or `spago run`, all required purescript packages are installed  and the modules are compiled in order.
However, this does not provide a single javascript file that can be loaded in HTML. 
Instead, purescript has to be bundled with the `spago bundle` command (see Makefile), which additionally requires [esbuild](https://esbuild.github.io/) to be installed.
Calling `spago bundle` (or `make build`) will then create an `index.js` file in the `web-app` directory, and the web-app can be run by simply opening the `index.html` file in any browser.

## Spago and Spago Next

There are two different versions of spago, one usually called `spago-legecy` and the other being the current version, also called `spago@next` (in npm).
The legacy version uses `.dhall` files, while the new version uses `.yaml` files. 
Since there sometimes are difficulties installing the current version and legacy has to be used, this project contains both `yaml` and `dhall` files for each included package.

Except for the difference in configuration files, building and running works the same way with both, but bundling has been changed in the new version. This is why in the Makefile, there is a branch depending on the intstalled spago version installed.
