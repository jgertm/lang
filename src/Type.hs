module Type
  ( module Type
  , Type(..)
  , Context(..)
  , synthesize
  )
where

import qualified Application                   as App
import qualified Builtins
import           Classes                        ( meta )
import           Error
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Term
import qualified Type.Context                  as Ctx
import           Type.Synthesis                 ( synthesize )
import           Type.Types                     ( Context(..)
                                                , Type(..)
                                                )


infer :: Term.Term phase -> Either Error Type
infer = inferWith $ map Builtins.typ Builtins.builtins

inferWith :: Map Syntax.Value Type -> Term.Term phase -> Either Error Type
inferWith bindings e =
  let gamma  = Ctx.initialize bindings
      result = App.run $ App.App $ synthesize gamma $ meta (const ()) e
      output = case result of
        Left  err             -> Left err
        Right ((typ, _), ctx) -> Right $ Ctx.apply ctx typ
  in  output
