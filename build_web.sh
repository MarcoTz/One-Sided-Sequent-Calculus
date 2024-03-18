#requires ghc wasm backend
# https://gitlab.haskell.org/ghc/ghc-wasm-meta
# https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
source ~/.ghc-wasm/env

if [ $1 -eq "test-suite"] 
then
  cabal --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs build test-suite 
  wasmtime run --mapdir /::$PWD dist-newstyle/build/wasm32-wasi/ghc-9.9.20240309/one-sided-sequent-0.1/x/test-suite/build/test-suite/test-suite.wasm
else
  cabal --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs build web-app 
  wasmtime run dist-newstyle/build/wasm32-wasi/ghc-9.9.20240309/one-sided-sequent-0.1/x/web-app/build/web-app/web-app.wasm
fi
