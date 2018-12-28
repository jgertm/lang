((haskell-mode
  . ((haskell-process-type . cabal-repl)
     (haskell-process-wrapper-function
      . (lambda (argv) (append (list "nix-shell" "--attr" "env" "lang.nix" "--command")
                          (list (mapconcat 'identity argv " ")))))
     (haskell-process-args-cabal-new-repl . ("--ghc-option=-ferror-spans" "--ghc-option=-w"))
     (haskell-process-args-cabal-repl . ("lib:lang" "--ghc-option=-ferror-spans" "--ghc-option=-w")))))
