delete-cabal:
	rm -f *.cabal

build: delete-cabal
	stack install --local-bin-path=. -j 8 --fast

repl: delete-cabal
	stack repl

ghcid: delete-cabal
	ghcid \
	--reload=./lib \
	--reload=./examples \
	--command='stack repl --ghci-options=-ignore-dot-ghci' \
	--warnings \
	--test=Dev.main

test: delete-cabal
	stack test -j 8 --fast

run: build
	./lang

format:
	fd -e hs -x hlint {} --refactor --refactor-options='--inplace'
	fd -e hs -x stylish-haskell -i {}
	fd -e hs -x brittany --write-mode=inplace {} --columns 100
