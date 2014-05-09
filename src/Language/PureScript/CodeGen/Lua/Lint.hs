{-# LANGUAGE ViewPatterns #-}

-- | Some basic simplifications on generated Lua modoules
module Language.PureScript.CodeGen.Lua.Lint (lint) where

import qualified Language.Lua.Syntax as L

import Language.PureScript.CodeGen.Lua.Utils

import Data.Generics
import qualified Data.Set as S

lint :: L.Block -> L.Block
lint = removeForwardDecls

-- | Replace a variable declaration immediately followed by it's definition
-- with just definition. NOTE: `local fun; fun = function ...` is not same
-- with `local fun = function ...`, instead it should be replaced with
-- `local function fun ...` otherwise self-recursion won't work.
removeForwardDecls :: Data d => d -> d
removeForwardDecls = everywhere (mkT removeForwardDecl)
  where
    removeForwardDecl :: [L.Stat] -> [L.Stat]
    removeForwardDecl [] = []
    removeForwardDecl [stat] = [stat]
    removeForwardDecl (s1@(L.LocalAssign (S.fromList -> vars) Nothing) : s2@(L.Assign [L.VarName var] [rhs]) : rest)
      | var `S.member` vars =
          (if S.size vars > 1 then [L.LocalAssign (S.toList $ S.delete var vars) Nothing]
                              else [])
          ++ (case rhs of
                L.EFunDef funbody -> L.LocalFunAssign var funbody
                _ -> L.LocalAssign [var] (Just [rhs]))
          : removeForwardDecl rest
      | otherwise = s1 : s2 : removeForwardDecl rest
    removeForwardDecl (stat : stats) = stat : removeForwardDecl stats

