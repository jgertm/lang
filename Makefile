PROJECT = lang
NIX_SHELL = nix-shell --attr env $(PROJECT).nix

generate-nix-files:
	hpack
	cabal2nix --hpack . > default.nix

shell: generate-nix-files
	$(NIX_SHELL)

build: generate-nix-files
	$(NIX_SHELL) --run "cabal new-build"
	cp dist-newstyle/build/x86_64-linux/ghc-8.4.3/lang-0.1.0.0/x/lang/build/lang/lang .

repl: generate-nix-files
	$(NIX_SHELL) --run "cabal new-repl"

ghcid: generate-nix-files
	$(NIX_SHELL) --run \
		"ghcid \
		--reload=./src \
		--reload=./exe \
		--reload=./test \
		--reload=./examples \
		--command='cabal new-repl lib:$(PROJECT) --ghc-options=-ignore-dot-ghci' \
		--warnings \
		--test=Dev.main"

test: generate-nix-files
	$(NIX_SHELL) --run "fd -e hs | entr cabal new-test"

run: generate-nix-files
	$(NIX_SHELL) --run "cabal run"

format:
	fd -e hs -x brittany --write-mode=inplace

lint:
	fd -e hs | xargs hlint
