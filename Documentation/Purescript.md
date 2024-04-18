This repo is currently being migrated to purescript.
In order to build the modules, both purescript and spago need to be installed 

For running in the browser you additionally need to have esbuild installed.
Then the web-app can be bundled into main.js with the command

`spago bundle -p one-sided-web-app --platform browser`
