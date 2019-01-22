PROJECT = lang
NIX_SHELL = nix-shell --attr env $(PROJECT).nix

delete-cabal:
	rm -f *.cabal

generate-nix-files: delete-cabal
	cabal2nix --hpack . > default.nix

shell: generate-nix-files
	$(NIX_SHELL)

build: delete-cabal
	stack install --local-bin-path=. -j 8 --fast

repl: delete-cabal
	stack repl

ghcid: delete-cabal
	ghcid \
	--reload=./lib \
	--command='stack repl --ghci-options=-ignore-dot-ghci' \
	--warnings \
	--test=Dev.main

test: delete-cabal
	stack test

run: build
	./lang

format:
	fd -e hs -x hlint {} --refactor --refactor-options='--inplace'
	fd -e hs -x stylish-haskell -i {}
	fd -e hs -x brittany --write-mode=inplace {} --columns 100
