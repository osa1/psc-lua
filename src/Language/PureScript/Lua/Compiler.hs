module Language.PureScript.Lua.Compiler where

import Language.PureScript.Types as P
import Language.PureScript.Declarations as P
import Language.PureScript.Names as P
import qualified Language.PureScript.CodeGen.Lua as L
import qualified Language.PureScript.CodeGen.Lua.Utils as L (pprint)
import Language.PureScript.CodeGen.Lua.Optimizer
import Language.PureScript.CodeGen.Lua.Lint
import Language.PureScript.TypeChecker as P
import Language.PureScript.Sugar as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P

import qualified Language.PureScript.Constants as C

import Language.PureScript.Lua.Options

import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.State.Lazy
import Control.Applicative ((<$>))

compileToLua :: Options -> [Module] -> Either String [(String, String)]
compileToLua = compileToLua' initEnvironment

compileToLua' :: Environment -> Options -> [Module] -> Either String [(String, String)]
compileToLua' env opts ms = do
    (sorted, _) <- sortModules $ map importPrelude ms
    desugared <- stringifyErrorStack True $ desugar sorted
    (elaborated, env') <- runCheck' (toPscOpts opts) env $ forM desugared $ typeCheckModule mainModuleIdent
    regrouped <- stringifyErrorStack True $ createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
    let
      mainModule :: Maybe String
      mainModule = optionsMain opts

      compileModule :: Module -> (String, String)
      compileModule m =
        let (mname, lua) = L.moduleToLua opts m env'
            codeText = L.pprint $ map (optimize . lint)
              [if Just mname == mainModule then L.generateMain env' opts lua else lua]
        in (mname, codeText)

    return $ map compileModule regrouped
  where
    mainModuleIdent = moduleNameFromString <$> optionsMain opts

typeCheckModule :: Maybe ModuleName -> Module -> Check Module
typeCheckModule mainModuleName (Module mn decls exps) = do
  modify (\s -> s { checkCurrentModule = Just mn })
  decls' <- typeCheckAll mainModuleName mn decls
  mapM_ checkTypesAreExported exps'
  return $ Module mn decls' exps
  where

  exps' = fromMaybe (error "exports should have been elaborated") exps

  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> Check ()
  checkTypesAreExported (ValueRef name) = do
    ty <- lookupVariable mn (Qualified (Just mn) name)
    case find isTconHidden (findTcons ty) of
      Just hiddenType -> throwError . strMsg $
        "Error in module '" ++ show mn ++ "':\n\
        \Exporting declaration '" ++ show name ++ "' requires type '" ++ show hiddenType ++ "' to be exported as well"
      Nothing -> return ()
  checkTypesAreExported _ = return ()

  -- Find the type constructors exported from the current module used in a type
  findTcons :: Type -> [ProperName]
  findTcons = everythingOnTypes (++) go
    where
    go (TypeConstructor (Qualified (Just mn') name)) | mn' == mn = [name]
    go _ = []

  -- Checks whether a type constructor is not being exported from the current module
  isTconHidden :: ProperName -> Bool
  isTconHidden tyName = all go exps'
    where
    go (TypeRef tyName' _) = tyName' /= tyName
    go _ = True

-- | Add an import declaration for the Prelude to a module if it does not
-- already explicitly import it.
importPrelude :: Module -> Module
importPrelude m@(Module mn decls exps)  =
  if isPreludeImport `any` decls || mn == prelude then m
  else Module mn (preludeImport : decls) exps
  where
  prelude = ModuleName [ProperName C.prelude]
  isPreludeImport (ImportDeclaration (ModuleName [ProperName mn']) _ _) | mn' == C.prelude = True
  isPreludeImport (PositionedDeclaration _ d) = isPreludeImport d
  isPreludeImport _ = False
  preludeImport = ImportDeclaration prelude Nothing Nothing

