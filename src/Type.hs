module Type
  ( module Type
  , Type(..)
  , Context(..)
  )
where

import qualified Builtins
import           Classes
import           Error
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Term
import qualified Type.Context                  as Ctx
import           Type.Monad
import           Type.Rules
import           Type.Synthesis                 ( synthesize )
import           Type.Types                     ( Type(..) )


infer :: Term.Term phase -> Either Error Type
infer = inferWith builtins

builtins = map Builtins.typ Builtins.builtins

inferTrace :: Term.Term phase -> (Either Error Type, [Judgement])
inferTrace = inferWithTrace mempty

inferWith :: Map Syntax.Value Type -> Term.Term phase -> Either Error Type
inferWith = fst ... inferWithTrace

inferWithTrace :: Map Syntax.Value Type -> Term.Term phase -> (Either Error Type, [Judgement])
inferWithTrace bindings e =
  let gamma              = Ctx.initialize bindings
      (result, _, rules) = run $ synthesize gamma $ meta (const ()) e
      output             = case result of
        Left  err             -> Left $ Type err
        Right ((typ, _), ctx) -> Right $ Ctx.apply ctx typ
  in  (output, rules)
