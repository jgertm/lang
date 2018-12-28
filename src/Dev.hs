module Dev where

import qualified Data.Map                      as Map
import           Text.Pretty.Simple

import qualified Builtins
import qualified Syntax.Atom                   as Syntax
import qualified Syntax.Common                 as Syntax
import qualified Syntax.Term                   as Syntax
import           Type
import           Type.Expression
import           Type.Monad
import           Type.Synthesis
import           Type.Types

main :: IO ()
main = do
  let a   = Var "alpha"
      a'  = UniversalVariable a
      typ = Forall a Type (Function a' a')
  pPrint $ inferWith (Map.singleton (Syntax.Single "id") typ) application
  pPrint $ infer lambda
  pPrint $ inferWith (Map.singleton (Syntax.Single "+") (Builtins.typ Builtins.plus)) addition
  pPrint $ inferWith (Map.singleton (Syntax.Single "+") (Builtins.typ Builtins.plus)) successor
  -- pPrint $ inferWith (Map.singleton (Syntax.Single "+") (Builtins.typ Builtins.plus)) successorAnno


application =
  Syntax.Application () (Syntax.Symbol () (Syntax.Single "id")) [Syntax.Atom () Syntax.Unit]

lambda = Syntax.Lambda () (Syntax.Single "x") x where x = Syntax.Symbol () (Syntax.Single "x")

addition = Syntax.Application
  ()
  (Syntax.Symbol () (Syntax.Single "+"))
  [Syntax.Atom () (Syntax.Integer 1), Syntax.Atom () (Syntax.Integer 1)]

successor =
  let i = Syntax.Single "i"
  in  Syntax.Lambda
        ()
        i
        (Syntax.Application ()
                            (Syntax.Symbol () (Syntax.Single "+"))
                            [Syntax.Atom () (Syntax.Integer 1), Syntax.Symbol () i]
        )

-- successorAnno = Syntax.Annotation () successor (fn [integer, integer, integer])
