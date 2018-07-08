generate-nix-files: 
	cabal2nix --hpack . > default.nix

shell: generate-nix-files 
	nix-shell --attr env lang.nix

format:
	fd -e hs | xargs hindent
	fd -e hs | xargs stylish-haskell -i

build: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal new-build"

repl: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal new-repl"

ghcid: generate-nix-files
	nix-shell --attr env lang.nix --command "ghcid"

run: generate-nix-files
	nix-shell --attr env lang.nix --command "cabal run"
