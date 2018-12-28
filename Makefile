PROJECT = lang
NIX_SHELL = nix-shell --attr env $(PROJECT).nix

generate-nix-files:
	hpack
	cabal2nix --hpack . > default.nix

shell: generate-nix-files
	$(NIX_SHELL)

build:
	stack install --local-bin-path=. -j 8 --fast

repl:
	stack repl

ghcid:
	ghcid \
	--reload=./lib \
	--command='stack repl --ghci-options=-ignore-dot-ghci' \
	--warnings \
	--test=Dev.main

test:
	stack test

run: build
	./lang

format:
	fd -e hs -x hlint {} --refactor --refactor-options='--inplace'
	fd -e hs -x stylish-haskell -i {}
	fd -e hs -x brittany --write-mode=inplace {} --columns 100

lint:
	fd -e hs | xargs hlint
