module Language.PureScript.CodeGen.Lua.Utils where

import qualified Language.Lua.Syntax as L
import qualified Language.Lua.PrettyPrinter as L

import Data.List (foldl')

var :: String -> L.Exp
var = L.PrefixExp . L.PEVar . L.VarName

funcall :: L.Exp -> [L.Exp] -> L.Exp
funcall f args =
    L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (expToPexp f) (L.Args args)

funcallS :: String -> [L.Exp] -> L.Stat
funcallS f args = L.FunCall $ L.NormalFunCall (L.PEVar $ L.VarName f) (L.Args args)

funcallStat :: L.Exp -> [L.Exp] -> L.Stat
funcallStat f args =
    L.FunCall $ L.NormalFunCall (expToPexp f) (L.Args args)

pprint :: L.LPretty l => [l] -> String
pprint ls =
    foldl' (\str ss -> "\n" ++ ss str) "" $ map (L.displayS . L.renderPretty 0.8 100 . L.pprint) ls

expToPexp :: L.Exp -> L.PrefixExp
expToPexp (L.PrefixExp pexp) = pexp
expToPexp e = L.Paren e

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace lhs rhs (x : xs) =
    (if x == lhs then rhs else x) : replace lhs rhs xs

