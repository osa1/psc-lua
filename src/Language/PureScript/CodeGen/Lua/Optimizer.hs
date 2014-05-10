{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.PureScript.CodeGen.Lua.Optimizer (optimize) where

import qualified Language.Lua.Syntax as L

import Language.PureScript.CodeGen.Lua.Utils

import Data.Generics
import Data.List (foldl')
import Prelude hiding (exp)

optimize :: L.Block -> L.Block
optimize = inlineCommonOperators . removeIfTrues . removeDollars

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
    removeDollarExp exp = exp

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

applyAll :: Data a => [a -> a] -> a -> a
applyAll = foldl' (.) id

inlineCommonOperators :: Data a => a -> a
inlineCommonOperators = everywhere (mkT inlineCommonOperators')

inlineCommonOperators' :: L.Exp -> L.Exp
inlineCommonOperators' = applyAll
    [ binary "numNumber" "_plus" L.Add
    , binary "numNumber" "_minus" L.Sub
    , binary "numNumber" "_times" L.Mul
    , binary "numNumber" "_div" L.Div
    , binary "numNumber" "_percent" L.Mod

    , binary "ordNumber" "_less" L.LT
    , binary "ordNumber" "_greater" L.GT
    , binary "ordNumber" "_greater_eq" L.GTE
    , binary "ordNumber" "_less_eq" L.LTE

    , binary "eqNumber" "_eq_eq" L.EQ
    , binary "eqNumber" "_div_eq" L.NEQ
    , binary "eqString" "_eq_eq" L.EQ
    , binary "eqString" "_div_eq" L.NEQ
    , binary "eqBoolean" "_eq_eq" L.EQ
    , binary "eqBoolean" "_div_eq" L.NEQ

    , binary "semigroupString" "_plus_plus" L.Concat

    , binary "boolLikeBoolean" "_amp_amp" L.And
    , binary "boolLikeBoolean" "_bar_bar" L.Or

    , unary "numNumber" "_minus" L.Neg
    , unary "boolLikeBoolean" "__not" L.Not
    ]
  where
    binary :: String -> String -> L.Binop -> L.Exp -> L.Exp
    binary typecls method binop exp =
      case exp of
        (L.PrefixExp (L.PEFunCall (L.NormalFunCall
          (L.PEFunCall (L.NormalFunCall
            (L.PEFunCall (L.NormalFunCall
              (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method')))
              (L.Args [L.PrefixExp (L.PEFunCall
                         (L.NormalFunCall
                           (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String typecls')))
                           (L.Args [L.TableConst []])))])))
            (L.Args [arg1])))
          (L.Args [arg2]))))
          | method' == method && typecls' == typecls -> L.Binop binop arg1 arg2
          | otherwise -> exp
        _ -> exp

    unary :: String -> String -> L.Unop -> L.Exp -> L.Exp
    unary typecls method unop exp =
      case exp of
        (L.PrefixExp (L.PEFunCall (L.NormalFunCall
          (L.PEFunCall (L.NormalFunCall
            (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method')))
            (L.Args [L.PrefixExp (L.PEFunCall
                       (L.NormalFunCall
                         (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String typecls')))
                         (L.Args [L.TableConst []])))])))
          (L.Args [arg]))))
          | method' == method && typecls' == typecls -> L.Unop unop arg
          | otherwise -> exp
        _ -> exp

