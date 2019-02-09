module Dev where

import           Data.Text.Prettyprint.Doc
import           Text.Pretty.Simple
import qualified Universum.Unsafe              as Unsafe

import           Classes
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Parser
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import qualified Type
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Synthesis                as Synthesis


main :: IO ()
main = do
  putStrLn ""
  print $ eval $ parse factorial

eval = fromRight undefined . Interpreter.eval
parse :: Text -> Term.Term Empty
parse = meta (const ()) . fromRight undefined . Parser.parse Parser.term "<dev>"


unit = "nil"
two = "2"
three = "3"
true = "true"
lambda = "(fn [x] x)"
application = "((fn [x] x) nil)"

constantly =
  let x = Reference.Local "x"
      y = Reference.Local "y"
  in  Term.Lambda () x (Term.Lambda () y (Term.Symbol () x))

application2 = Term.Application () constantly [two, unit]
ifstatement = "(if true 2 3)"
wrongIfstatement = "(if true 2 nil)"
letBinding = "(let [[foo nil]] foo)"
matchStatement = "(match 2 (2 nil))"
addition = "(+ 2 3)"
specializedApply = "(fn [f] (f nil))"
apply = "(fn [f x] (f x))"
compose = "(fn [g f] (fn [x] (g (f x))))"

recursion = "((recur f (fn [x] (match x (3 nil) (n (f (+ 1 n)))))) 1)"
factorialFn = "(recur fac (fn [x] (match x (0 1) (n (* n (fac (- n 1)))))))"
factorial = "((recur fac (fn [x] (match x (0 1) (n (* n (fac (- n 1))))))) 4)"
