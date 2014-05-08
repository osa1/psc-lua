module Language.PureScript.Lua.Options where

import qualified Language.PureScript.Options as P

-- | The data type of compiler options
data Options = Options

  -- | Perform type checks at runtime
  { optionsPerformRuntimeTypeChecks :: Bool

  -- | When specified, checks the type of `main` in the module, and
  -- generate a call to run main after the module definitions.
  , optionsMain :: Maybe String

  -- | Verbose error message
  , optionsVerboseErrors :: Bool
  } deriving Show

-- | Default compiler options
defaultOptions :: Options
defaultOptions = Options False Nothing False

-- | Convert Lua backend options to PureScript options -- to be used for
-- psc internal functions.
toPscOpts :: Options -> P.Options
toPscOpts opts =
    P.defaultOptions{ P.optionsPerformRuntimeTypeChecks = optionsPerformRuntimeTypeChecks opts
                    , P.optionsMain = optionsMain opts
                    , P.optionsVerboseErrors = optionsVerboseErrors opts
                    }

