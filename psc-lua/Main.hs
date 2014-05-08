module Main where

import Control.Applicative
import Control.Monad

import Data.Version (showVersion)

import System.Console.CmdTheLine
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>))
import System.Exit (exitSuccess, exitFailure)

import Text.Parsec (ParseError)

import Language.PureScript.Lua.Options
import Language.PureScript.Lua.Compiler

import qualified Language.PureScript as P
import qualified Paths_psc_lua as Paths
import qualified System.IO.UTF8 as U

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: Maybe [FilePath] -> IO (Either ParseError [(FilePath, P.Module)])
readInput Nothing = do
  text <- getContents
  return $ map ((,) undefined) <$> P.runIndentParser "" P.parseModules text
readInput (Just input) = fmap collect $ forM input $ \inputFile -> do
  text <- U.readFile inputFile
  return $ (inputFile, P.runIndentParser inputFile P.parseModules text)
  where
  collect :: [(FilePath, Either ParseError [P.Module])] -> Either ParseError [(FilePath, P.Module)]
  collect = fmap concat . sequence . map (\(fp, e) -> fmap (map ((,) fp)) e)

compile :: Options -> Maybe [FilePath] -> IO ()
compile opts input = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.print err
      exitFailure
    Right ms -> do
      case compileToLua opts (map snd ms) of
        Left err -> do
          U.putStrLn err
          exitFailure
        Right luaModules -> do
          forM_ luaModules $ \(moduleName, luaCode) ->
            U.writeFile (moduleName <.> "lua") luaCode
          exitSuccess

useStdIn :: Term Bool
useStdIn = value . flag $ (optInfo [ "s", "stdin" ])
     { optDoc = "Read from standard input" }

inputFiles :: Term [FilePath]
inputFiles = value $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .lua file" }

performRuntimeTypeChecks :: Term Bool
performRuntimeTypeChecks = value $ flag $ (optInfo [ "runtime-type-checks" ])
     { optDoc = "Generate runtime type checks" }

runMain :: Term (Maybe String)
runMain = value $ defaultOpt (Just "Main") Nothing $ (optInfo [ "main" ])
     { optDoc = "Generate code to run the main method in the specified module." }

verboseErrors :: Term Bool
verboseErrors = value $ flag $ (optInfo [ "v", "verbose-errors" ])
     { optDoc = "Display verbose error messages" }

options :: Term Options
options = Options <$> performRuntimeTypeChecks <*> runMain <*> verboseErrors

stdInOrInputFiles :: FilePath -> Term (Maybe [FilePath])
stdInOrInputFiles prelude = combine <$> useStdIn <*> inputFiles
  where
  combine False input = Just (prelude : input)
  combine True _ = Nothing

term :: FilePath -> Term (IO ())
term prelude = compile <$> options <*> stdInOrInputFiles prelude

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = showVersion Paths.version
  , termDoc  = "Compiles PureScript to Lua"
  }

main :: IO ()
main = do
  prelude <- preludeFilename
  run (term prelude, termInfo)

