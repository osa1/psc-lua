{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.CodeGen.Lua.Monad where

import Control.Applicative
import Control.Monad.State.Strict

import Language.PureScript.Names
import Language.PureScript.Environment

import Language.PureScript.Lua.Options

data CGState = CGState
    { cgOpts     :: Options
    , cgEnv      :: Environment
    , cgModName  :: ModuleName
    , cgFreshVar :: Int
    } deriving (Show)

newtype CG a = CG { runCG :: State CGState a }
    deriving (Functor, Applicative, Monad, MonadState CGState)

fresh :: CG String
fresh = do
    v <- gets cgFreshVar
    let ret = "fresh_" ++ show v
    modify $ \s -> s{cgFreshVar = v + 1}
    return ret

