{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

-- | This module provides a limited form of capture-avoiding substitution
-- under some assumptions. Only variables are substituted. The assumption
-- should hold: Only lexical scoping is used. e.g. everything declared as
-- `local`, no global variables etc. is used.
module Language.PureScript.CodeGen.Lua.Subst where

import qualified Language.Lua.Syntax as L

import Language.PureScript.CodeGen.Lua.Utils

import Control.Applicative
import Control.Monad.State
import Prelude hiding (exp)

type SubstState = Int

initSubstState :: SubstState
initSubstState = 0

newtype Subst a = Subst { unwrapS :: State SubstState a }
  deriving (Functor, Applicative, Monad, MonadState SubstState)

fresh :: Subst L.Name
fresh = do
    st <- get
    put (st + 1)
    return $ "__fresh_" ++ show (st + 1)

runSubst :: SubstState -> Subst a -> a
runSubst st s = evalState (unwrapS s) st

runSubst' :: Subst a -> Subst a
runSubst' s = flip runSubst s <$> get

substBlock :: L.Name -> L.Name -> L.Block -> Subst L.Block
substBlock name exp (L.Block stats ret) = uncurry L.Block <$> iter stats
  where
    iter :: [L.Stat] -> Subst ([L.Stat], Maybe [L.Exp])

    iter [] = ([],) <$> substRets name exp ret

    iter (L.Assign vars exps : rest) = do
      -- since we should be enforcing lexical scoping using only local
      -- vars, this should be a mutation of an already-bound variable or
      -- table field etc.
      vars' <- mapM (runSubst' . substVar name exp) vars
      exps' <- mapM (runSubst' . substExp name exp) exps
      (ss, ret') <- iter rest
      return (L.Assign vars' exps' : ss, ret')

    -- TODO: make sure RHSs can't refer to LHS vars here
    -- (e.g. no mutually recursive definitions)
    iter (L.LocalAssign names exps : rest) = do
      exps' <- maybe (return Nothing)
                     (liftM Just . mapM (runSubst' . substExp name exp)) exps

      if name `elem` names then
        -- `name` is not free in rest of the statements and in return statement
        return (L.LocalAssign names exps' : rest, ret)
      else
        if exp `elem` names then do
          -- we're capturing a variable
          freshVar <- fresh
          let names' = replace exp freshVar names
          L.Block ss' ret' <- runSubst' $ substBlock exp freshVar (L.Block rest ret)
          L.Block ss'' ret'' <- substBlock name exp (L.Block ss' ret')
          return (L.LocalAssign names' exps' : ss'', ret'')
        else do
          (ss, ret') <- iter rest
          return (L.LocalAssign names exps' : ss, ret')

    iter ss@(L.LocalFunAssign funname funbody : rest)
      | name == funname =
          -- `name` is not free in rest of the statements and in return statement
          return (ss, ret)
      | exp == funname = do
          -- `name` is free, but we are capturing the function name
          funname' <- fresh
          funbody' <- runSubst' $ substFunBody funname funname' funbody
          L.Block ss' ret' <- substBlock funname funname' (L.Block rest ret)

          -- fixed capturing, continue with substitution
          funbody'' <- runSubst' $ substFunBody name exp funbody'
          L.Block ss'' ret'' <- substBlock name exp (L.Block ss' ret')
          return (L.LocalFunAssign funname' funbody'' : ss'', ret'')

      | otherwise = do
          funbody' <- runSubst' $ substFunBody name exp funbody
          (ss', ret') <- iter rest
          return (L.LocalFunAssign funname funbody' : ss', ret')

    -- rest of the cases should be trivial
    iter (stat : ss) = do
      (ss', ret') <- iter ss

      stat' <- case stat of
                 L.FunCall fc ->
                   L.FunCall <$> (runSubst' $ substFunCall name exp fc)
                 L.Do block ->
                   L.Do <$> (runSubst' $ substBlock name exp block)
                 L.While exp' block ->
                   L.While <$> (runSubst' $ substExp name exp exp')
                           <*> (runSubst' $ substBlock name exp block)
                 L.Repeat block exp' ->
                   L.Repeat <$> (runSubst' $ substBlock name exp block)
                            <*> (runSubst' $ substExp name exp exp')
                 L.If conds else_ ->
                   L.If <$> (mapM (\(exp', block) -> (,) <$> (runSubst' $ substExp name exp exp')
                                                         <*> (runSubst' $ substBlock name exp block)) conds)
                        <*> maybe (return Nothing)
                                  (liftM Just . runSubst' . substBlock name exp) else_
                 L.ForRange name' e1 e2 e3 block ->
                   L.ForRange name' <$> (runSubst' $ substExp name exp e1)
                                    <*> (runSubst' $ substExp name exp e2)
                                    <*> maybe (return Nothing)
                                              (liftM Just . runSubst' . substExp name exp) e3
                                    <*> (runSubst' $ substBlock name exp block)
                 L.ForIn names exps block ->
                   L.ForIn names <$> mapM (runSubst' . substExp name exp) exps
                                 <*> (runSubst' $ substBlock name exp block)
                 L.FunAssign funname funbody ->
                   -- should be safe for same reasons with L.Assign case above
                   L.FunAssign funname <$> (runSubst' $ substFunBody name exp funbody)
                 _ -> return stat

      return (stat' : ss', ret')

substRets :: L.Name -> L.Name -> Maybe [L.Exp] -> Subst (Maybe [L.Exp])
substRets _ _ Nothing = return Nothing
substRets name exp (Just rets) = Just <$> mapM (substExp name exp) rets

substExp :: L.Name -> L.Name -> L.Exp -> Subst L.Exp
substExp name exp (L.EFunDef fb) = L.EFunDef <$> substFunBody name exp fb
substExp name exp (L.PrefixExp pexp) = L.PrefixExp <$> substPexp name exp pexp
substExp name exp (L.TableConst fs) = L.TableConst <$> mapM (runSubst' . substField name exp) fs
substExp name exp (L.Binop op e1 e2) =
    L.Binop op <$> runSubst' (substExp name exp e1) <*> runSubst' (substExp name exp e2)
substExp name exp (L.Unop op e) = L.Unop op <$> runSubst' (substExp name exp e)
substExp _ _ e = return e

substFunBody :: L.Name -> L.Name -> L.FunBody -> Subst L.FunBody
substFunBody name exp fb@(L.FunBody args b block)
  | name `elem` args =
      -- `name` is not free in `funbody`
      return fb
  | exp `elem` args = do
      -- name capturing
      freshVar <- fresh
      block' <- runSubst' $ substBlock exp freshVar block
      let args' = replace exp freshVar args

      -- name capturing is resolved, continue with substitution
      block'' <- substBlock name exp block'
      return $ L.FunBody args' b block''
  | otherwise = L.FunBody args b <$> substBlock name exp block

substVar :: L.Name -> L.Name -> L.Var -> Subst L.Var
substVar name exp (L.VarName name')
  | name == name' = return $ L.VarName exp
  | otherwise = return $ L.VarName name'
substVar name exp (L.Select pexp exp') =
    L.Select <$> runSubst' (substPexp name exp pexp) <*> runSubst' (substExp name exp exp')
substVar name exp (L.SelectName pexp name') =
    L.SelectName <$> runSubst' (substPexp name exp pexp) <*> pure name'

substFunCall :: L.Name -> L.Name -> L.FunCall -> Subst L.FunCall
substFunCall name exp (L.NormalFunCall pexp arg) =
    L.NormalFunCall <$> (runSubst' $ substPexp name exp pexp)
                    <*> (runSubst' $ substFunArg name exp arg)
substFunCall name exp (L.MethodCall pexp mname arg) =
    L.MethodCall <$> (runSubst' $ substPexp name exp pexp)
                 <*> pure mname
                 <*> (runSubst' $ substFunArg name exp arg)

substFunArg :: L.Name -> L.Name -> L.FunArg -> Subst L.FunArg
substFunArg name exp (L.Args exps) = L.Args <$> mapM (runSubst' . substExp name exp) exps
substFunArg name exp (L.TableArg fields) = L.TableArg <$> mapM (runSubst' . substField name exp) fields
substFunArg _ _ s@L.StringArg{} = return s

substField :: L.Name -> L.Name -> L.TableField -> Subst L.TableField
substField name exp (L.ExpField e1 e2) =
    L.ExpField <$> runSubst' (substExp name exp e1) <*> runSubst' (substExp name exp e2)
substField name exp (L.NamedField name' exp') =
    L.NamedField name' <$> substExp name exp exp'
substField name exp (L.Field exp') = L.Field <$> substExp name exp exp'

substPexp :: L.Name -> L.Name -> L.PrefixExp -> Subst L.PrefixExp
substPexp name exp (L.PEVar v) = L.PEVar <$> substVar name exp v
substPexp name exp (L.PEFunCall fc) = L.PEFunCall <$> substFunCall name exp fc
substPexp name exp (L.Paren exp') = L.Paren <$> substExp name exp exp'

