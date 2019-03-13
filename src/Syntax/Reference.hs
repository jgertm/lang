module Syntax.Reference
  ( Value(..)
  , Module(..)
  , Keyword(..)
  , Type(..)
  , nest
  , append
  )
where

import qualified Data.Text                     as Text
import           Data.Text.Prettyprint.Doc
                                         hiding ( nest )


data Value
  = Local Text
  | Remote Module Text
  deriving (Generic, Show, Eq, Ord)

instance Pretty Value where
  pretty (Local name)        = pretty name
  pretty (Remote modul name) = pretty modul <> "/" <> pretty name

nest :: Text -> Value -> Value
nest outer (Local name                 ) = Remote (Module [outer]) name
nest outer (Remote (Module outers) name) = Remote (Module $ outer : outers) name

append :: Value -> Text -> Value
append (Local name        ) suffix = Local $ name <> suffix
append (Remote outers name) suffix = Remote outers $ name <> suffix


newtype Module = Module [Text] deriving (Generic, Show, Eq, Ord)

instance Pretty Module where
  pretty (Module path) = pretty $ Text.intercalate "." path


newtype Keyword = Keyword Value deriving (Generic, Show, Eq, Ord)

instance Pretty Keyword where
  pretty (Keyword name) = ":" <> pretty name


newtype Type = Type Text deriving (Generic, Show, Eq, Ord)

instance Pretty Type where
  pretty (Type name) = pretty name
