FROM fpco/stack-build:lts-11.11

# Use system ghc for all stack commands, otherwise stack tries to download
# another GHC version.
RUN stack config set system-ghc --global true

WORKDIR /cubemania

COPY api.cabal /cubemania/api.cabal
COPY stack.yaml /cubemania/stack.yaml
RUN stack install --only-dependencies

COPY . /cubemania

RUN stack install api:exe:api-exe --local-bin-path dist


FROM ubuntu

RUN apt-get update -y && apt-get install -y libpq-dev netbase

COPY --from=0 /cubemania/dist/api-exe /app/api-exe
WORKDIR /app
CMD /app/api-exe
