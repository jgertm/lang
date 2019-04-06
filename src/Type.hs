module Type
  ( module Type
  , Type(..)
  , Context(..)
  , synthesize
  )
where

import           Application                    ( Compile )
import qualified Builtins
import           Classes                        ( meta )
import           Error
import qualified Syntax.Reference              as Ref
import qualified Syntax.Term                   as Term
import qualified Type.Context                  as Ctx
import qualified Type.Expression               as Type
import           Type.Monad                     ( Infer )
import           Type.Synthesis                 ( synthesize )
import           Type.Types                     ( Context(..)
                                                , Type(..)
                                                )


infer :: Term.Term phase -> Compile Type
infer expr =
  let nativeTypedefs = Type.natives
      nativeBindings = map Builtins.typ Builtins.builtins
  in  inferWith nativeTypedefs nativeBindings expr

inferWith :: Map Ref.Type Type -> Map Ref.Value Type -> Term.Term phase -> Compile Type
inferWith typedefs bindings expr = do
  let gamma = Ctx.initialize typedefs bindings
      expr' = meta (const ()) expr
  ((typ, _), ctx) <- synthesize gamma expr'
  pure $ Ctx.apply ctx typ
