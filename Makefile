build: configure
	@cabal build all

configure:
	@cabal configure --enable-tests

clean:
	@cabal clean

test: build
	@cabal test all

cabal-file:
	@cabal-fmt --Werror -i llvm-codegen.cabal

.PHONY: build configure clean test cabal-file
