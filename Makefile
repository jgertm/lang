PROJECT = lang
NIX_SHELL = nix-shell --attr env $(PROJECT).nix

generate-nix-files: 
	hpack
	cabal2nix --hpack . > default.nix

shell: generate-nix-files
	$(NIX_SHELL)


build: generate-nix-files
	$(NIX_SHELL) --run "cabal new-build"

repl: generate-nix-files
	$(NIX_SHELL) --run "cabal new-repl"

ghcid: generate-nix-files
	$(NIX_SHELL) --run \
		"ghcid \
		--reload=./src \
		--reload=./exe \
		--reload=./test \
		--command='cabal new-repl lib:$(PROJECT)' \
		--warnings \
		--test=Dev.main"

test: generate-nix-files
	$(NIX_SHELL) --run "fd -e hs | entr cabal new-test" 

run: generate-nix-files
	$(NIX_SHELL) --run "cabal run"

format:
	fd -e hs -x hindent
	fd -e hs -x stylish-haskell -i
