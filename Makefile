generate-nix-files: 
	hpack
	cabal2nix --hpack . > default.nix

shell: generate-nix-files 
	nix-shell --attr env lang.nix

format:
	fd -e hs | xargs hindent
	fd -e hs | xargs stylish-haskell -i 

lint:
	fd -e hs | xargs hlint | less

build: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal new-build"

repl: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal new-repl"

ghcid: generate-nix-files
	nix-shell --attr env lang.nix --command \
		"ghcid \
		--reload=./src \
		--reload=./examples \
		--command='cabal new-repl' \
		--test='Main.main'"

test: generate-nix-files
	nix-shell --attr env lang.nix --command "fd -e hs | entr cabal new-test" 

run: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal run"
