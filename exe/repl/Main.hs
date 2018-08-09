module Main
  ( main
  ) where

import qualified Data.Map.Strict          as M
import           System.Console.Haskeline hiding (handle)
import qualified Text.Parsec              as P

import           Builtins
import           Inference
import           Interpreter
import           Parser
import           Renaming
import           Syntax

data Flags = Flags
  { showAST         :: Bool
  , showConstraints :: Bool
  , showType        :: Bool
  , showContext     :: Bool
  } deriving (Generic)

data State = State
  { flags       :: Flags
  , definitions :: Map Binding Term
  } deriving (Generic)

defaultState :: State
defaultState = State {flags = defaultFlags, definitions = natives}
  where
    defaultFlags =
      Flags
        { showAST = False
        , showConstraints = False
        , showContext = False
        , showType = False
        }
    natives = map (\bi -> ENative mempty bi []) builtins

newtype REPL a = REPL
  { unRepl :: StateT State (InputT IO) a
  } deriving (Functor, Applicative, Monad, MonadState State, MonadIO)

runREPL :: REPL a -> IO a
runREPL = runInputT defaultSettings . evaluatingStateT defaultState . unRepl

main :: IO ()
main = runREPL loop
  where
    loop :: REPL ()
    loop = do
      line <- REPL . lift $ getInputLine "> "
      case line >>= (rightToMaybe . parse input "<repl>" . toText) of
        Nothing    -> pass -- FIXME: exit on EOF, loop on SPC
        Just input -> handle input *> loop

handle :: (MonadState State m, MonadIO m) => Input -> m ()
handle (Control c) = apply c
handle (Expression e) = do
  State {definitions, flags} <- get
  let Right ast = renameWith (map (const 0) definitions) e
  putStrLn $
    unlines $
    catMaybes $
    case interpretWith definitions ast of
      Left err -> [Just $ "ERROR: " <> show err]
      Right exp@(EAtom _ atom) ->
        let value = showAtom atom
            typ = ": " <> show (typeOf exp)
         in [ Just value
            , if showType flags then Just typ else Nothing
            , if showAST flags then Just $ show $ stripContext ast else Nothing
            ]
      Right exp ->
        let typ = ": " <> show (typeOf exp)
            (_, constraints) = runConstrain exp
         in [ Just (show exp)
            , if showType flags then Just typ else Nothing
            , if showConstraints flags then Just (unlines $ map show constraints) else Nothing
            , if showAST flags then Just $ show $ stripContext ast else Nothing
            ]
handle (Definition d) =
  case d of
    DConstant _ name body -> do
      modify (over #definitions $ M.insert (Single name) body)
      putStrLn name
    DFunction _ name args body -> do
      let lambda = ELambda mempty args body
      modify (over #definitions $ M.insert (Single name) lambda)
      putStrLn name

apply :: (MonadState State m, MonadIO m) => Control -> m ()
apply =
  let toggle lens = modify (over (#flags . lens) not)
   in \case
        PrintAST -> toggle #showAST
        IncludeContexts -> toggle #showContext
        PrintType -> toggle #showConstraints *> toggle #showType
        Quit -> exitSuccess

data Input -- TODO: expose as native call
  = Expression Term
  | Definition Definition
  | Control Control
  deriving (Show, Eq, Ord)

input :: Parser Input
input = cases [control, Definition <$> definition, Expression <$> expr]
  where
    control =
      map Control . cases $
      map
        (\(pat, tgl) -> P.string (":" <> pat) $> tgl)
        [ ("meta", IncludeContexts)
        , ("ast", PrintAST)
        , ("type", PrintType)
        , ("quit", Quit)
        ]

data Control
  = PrintAST
  | IncludeContexts
  | PrintType
  | Load FilePath
  | Quit
  deriving (Show, Eq, Ord)
