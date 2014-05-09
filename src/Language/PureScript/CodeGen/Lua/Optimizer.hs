{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.PureScript.CodeGen.Lua.Optimizer (optimize) where

import qualified Language.Lua.Syntax as L

import Language.PureScript.CodeGen.Lua.Utils

import Data.Generics

optimize :: L.Block -> L.Block
optimize = removeIfTrues . removeDollars

-- | Replace `Prelude.$` calls with direct function applications.
-- This assumes using standard Prelude.
removeDollars :: Data a => a -> a
removeDollars = everywhere (mkT removeDollarExp)
  where
    removeDollarExp
      (L.PrefixExp (L.PEFunCall (L.NormalFunCall
        (L.PEFunCall (L.NormalFunCall (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String "_dollar")))
                                      (L.Args [arg1])))
        (L.Args [arg2])))) =
      L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (expToPexp arg1) $ L.Args [arg2]
    removeDollarExp e = e

-- | Replace `if true then stats end` with `stats`. If `stats` has a return
-- statement, then remove all following statements.
removeIfTrues :: Data a => a -> a
removeIfTrues = everywhere (mkT removeIfTrue)
  where
    removeIfTrue
      b@(L.Block stats ret) =
        case searchIfTrue stats of
          Nothing -> b
          Just (prevStats, ifBody, Left restStats) ->
            -- `if true then stats end` found, without return statement in `stats`
            L.Block (prevStats ++ ifBody ++ restStats) ret
          Just (prevStats, ifBody, Right ret') ->
            -- `if true then stats end` found, `stats` have a return statement
            L.Block (prevStats ++ ifBody) (Just ret')

    searchIfTrue [] = Nothing
    searchIfTrue (L.If [(L.Bool True, L.Block ifBody Nothing)] Nothing : rest) =
      return ([], ifBody, Left rest)
    searchIfTrue (L.If [(L.Bool True, L.Block ifBody (Just ret))] Nothing : _) =
      return ([], ifBody, Right ret)
    searchIfTrue (stat : stats) = do
      (prevs, ifBody, rest) <- searchIfTrue stats
      return (stat : prevs, ifBody, rest)

