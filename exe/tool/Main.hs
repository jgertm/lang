module Main
  ( main
  )
where

import           Data.Text.Prettyprint.Doc
import           Options.Applicative     hiding ( action )

import qualified Application                   as App
import qualified Module
import qualified Parser
import qualified Syntax.Term                   as Term


main :: IO ()
main = do
  options <- execParser opts
  case action options of
    Typecheck file -> do
      content <- readFile file
      case App.run $ Module.load Module.empty =<< Parser.parse Parser.file file content of
        Right mod -> putTextLn $ show $ pretty mod
        Left  err -> do
          putStrLn "ERROR"
          print err
    Run file -> do
      content <- readFile file
      let modul = App.run $ Module.load Module.native =<< Parser.parse Parser.file file content
      guard $ isRight modul
      case Module.run =<< modul of
        Left  err                -> print err
        Right (Term.Atom _ atom) -> putTextLn $ show $ pretty atom
        Right term               -> print term
    Repl _ -> undefined

greet :: IO ()
greet = putTextLn $ unlines
  [" _             ", "| |___ ___ ___ ", "| | .'|   | . |", "|_|__,|_|_|_  |", "          |___|"]

data Options = Options {action :: Command} deriving (Generic, Show)

data Command
  = Run FilePath
  | Typecheck FilePath
  | Repl (Maybe FilePath)
  deriving (Generic, Show)

opts :: ParserInfo Options
opts =
  flip info mempty
    $    (do
           action <- hsubparser $ mconcat
             [ command "run" $ info run (progDesc "Execute a .lang file")
             , command "type" $ info typecheck (progDesc "Typecheck a .lang file")
             , command "repl" $ info repl (progDesc "Launch a lang REPL")
             ]
           pure Options {action }
         )
    <**> helper
 where
  run              = Run <$> filepath
  typecheck        = Typecheck <$> filepath
  repl             = Repl <$> optionalFilepath
  filepath         = argument str (metavar "FILE")
  optionalFilepath = argument (Just <$> str) (metavar "FILE" <> value Nothing)
