#! /bin/sh

mkdir -p dist
docker run -v $(pwd):/src -v ~/.stack:/root/.stack timhabermaas/haskell-build stack install --local-bin-path ./dist --system-ghc
