{-# LANGUAGE TupleSections, RecordWildCards #-}

module Language.PureScript.CodeGen.Lua (moduleToLua, generateMain, mnameToStr) where

import qualified Language.Lua.Syntax as L
import qualified Language.Lua.Parser as L

import Data.Char
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict hiding (state, guard)
import Control.Applicative
import Control.Arrow ((***))
import Data.List (intercalate)
import Data.Function (on)

import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Types
import Language.PureScript.Environment

import qualified Language.PureScript.CodeGen.JS.AST as JS
import qualified Language.PureScript.Constants as C

import Language.PureScript.Lua.Options
import Language.PureScript.CodeGen.Lua.Utils
import Language.PureScript.CodeGen.Lua.Monad

-- | Compile a module to Lua and return (file name, Lua chunk)
moduleToLua :: Options -> Module -> Environment -> (String, L.Block)
moduleToLua opts (Module mname decls (Just exports)) env =
    -- trace (unlines [ "compiling module: " ++ show mname
    --                , "with exports: " ++ show exports ]) $
    ( mnameToStr mname
    , L.Block (mkEmptyModule : implicitImports ++ luaDecls ++ exportAssigns)
              (Just [L.PrefixExp moduleTable]) )
  where
    state = CGState opts env mname 0
    mkEmptyModule = L.LocalAssign [mnameToStr mname] (Just [L.TableConst []])
    moduleTable = L.PEVar $ L.VarName $ mnameToStr mname
    implicitImports =
      let imports' = S.toList $ S.delete mname (S.fromList $ concatMap imports decls)
      in flip map imports' $ \m ->
           let m' = mnameToStr m
           in L.LocalAssign [m'] (Just [funcall (var "require") [L.String m']])
    luaDecls = concatMap (flip evalState state . runCG . declToLua) decls
    exportAssigns = concatMap (assignDeclToTable moduleTable) exports
moduleToLua _ _ _ = error "Exports should have been elaborated in name desugaring"

-- TODO: This is copied from JS backend
imports :: Declaration -> [ModuleName]
imports =
    let (f, _, _, _, _) =
          everythingOnValues (++) (const []) collect (const []) (const []) (const [])
    in f
  where
    collect :: Value -> [ModuleName]
    collect (Var (Qualified (Just mn) _)) = [mn]
    collect (Constructor (Qualified (Just mn) _)) = [mn]
    collect _ = []

-- TODO: this function is mostly same as PureScript.generateMain, maybe
-- move error checking to it's own function.
generateMain :: Environment -> Options -> L.Block -> L.Block
generateMain env opts lua@(L.Block stats ret) =
  case moduleNameFromString <$> optionsMain opts of
    Just mmi -> do
      if ((mmi, Ident C.main) `M.notMember` names env)
        then error $ show mmi ++ "." ++ C.main ++ " is undefined"
        else L.Block (stats ++ [funcallS "main" []]) ret
    Nothing -> lua

assignDeclToTable :: L.PrefixExp -> DeclarationRef -> [L.Stat]
assignDeclToTable tbl (TypeRef _ (Just dctors)) =
  map (\n -> L.Assign [L.Select tbl (L.String $ runProperName n)]
                      [var $ runProperName n]) dctors
assignDeclToTable tbl (ValueRef name) =
  return $ L.Assign [L.Select tbl (L.String $ identToStr name)] [var $ identToEscapedStr name]
assignDeclToTable tbl (TypeInstanceRef name) =
  return $ L.Assign [L.Select tbl (L.String $ identToStr name)] [var $ identToEscapedStr name]
assignDeclToTable tbl (PositionedDeclarationRef _ decl) = assignDeclToTable tbl decl
assignDeclToTable _ _ = []

-- Mutually recursive definitions or definitions that are compiled to
-- mutually recursive Lua functions should have variable declarations
-- before actual definitions. `declToLua` returns a list of variables that
-- are just introduced and list of statements generated for Declaration.
-- This list of variables should be declared before Lua statements.

declToLua :: Declaration -> CG [L.Stat]
declToLua decl = do
    (vars, stats) <- declToLua' decl
    return $ case vars of
               [] -> stats
               _  -> L.LocalAssign vars Nothing : stats

declToLua' :: Declaration -> CG ([String], [L.Stat])
declToLua' (ValueDeclaration ident _ _ _ val) = do
    val' <- valueToLua val
    let ident' = identToEscapedStr ident
    return ([ident'], [L.Assign [L.VarName ident'] [val']])
declToLua' (BindingGroupDeclaration vals) =
    liftM ((concat *** concat) . unzip) $ forM vals $ \(ident, _, val) -> do
      val' <- valueToLua val
      let ident' = identToEscapedStr ident
      return ([ident'], [L.Assign [L.VarName ident'] [val']])
declToLua' (DataDeclaration _ _ ctors) =
    liftM ([],) $ forM ctors $ \(pn@(ProperName ctor), tys) ->
      L.LocalAssign [ctor] . Just . (: []) <$> mkCtor pn 0 tys []
  where
    mkCtor :: ProperName -> Int -> [Type] -> [L.Exp] -> CG L.Exp
    mkCtor pn _ [] values = do
      mp <- gets cgModName
      return $ L.TableConst [ L.NamedField "ctor" (L.String $ show (Qualified (Just mp) pn))
                            , L.NamedField "values" (L.TableConst $ map L.Field $ reverse values)
                            ]
    mkCtor pn index (_ : tys') values = do
      ret <- mkCtor pn (index + 1) tys' ((L.PrefixExp $ L.PEVar $ L.VarName $ "value" ++ show index) : values)
      return $ L.EFunDef $ L.FunBody ["value" ++ show index] False (L.Block [] (Just [ret]))
declToLua' (DataBindingGroupDeclaration ds) =
    liftM ((concat *** concat) . unzip) $ mapM declToLua' ds
declToLua' (ExternDeclaration _ _ (Just (JS.JSRaw code)) _) =
    -- TODO: ExternDeclaration contains JS syntax, which is not useful
    -- for other backends. For now I'm forcing the compiler to only
    -- have JSRaw ExternDeclarations because we can put whatever code
    -- we want as a string to that constructor. All other constructors
    -- used in ExternDeclarations are errors in Lua backend.
    --
    -- Instead of pasting extern declaration to the generated code, I'm
    -- parsing the code and returning the AST. This is mostly because
    -- language-lua doesn't have a syntax node for raw code but I think
    -- this may also be useful for some optimizations. i.e. we can inline
    -- extern declarations etc.
    case L.parseText L.stat code of
      Left parseErr ->
        error $ unlines [ "Error while parsing extern declaration:"
                        , show code
                        , show parseErr ]
      Right ast -> return ([], [ast])
declToLua' (ExternDeclaration _ _ (Just js) _) =
    error $ "ExternDeclaration in Lua backend with JS code: " ++ show js
declToLua' (PositionedDeclaration _ d) = declToLua' d
declToLua' (ImportDeclaration mname _ Nothing) = do
    let varName = mnameToStr mname
    return ([], [L.LocalAssign [varName] (Just [funcall (var "require") [L.String varName]])])
declToLua' _ = return ([], [])

valueToLua :: Value -> CG L.Exp
valueToLua (NumericLiteral (Left i)) = return $ L.Number (show i)
valueToLua (NumericLiteral (Right d)) = return $ L.Number (show d)
valueToLua (BooleanLiteral b) = return $ L.Bool b
valueToLua (StringLiteral s) = return $ L.String s
valueToLua (ArrayLiteral xs) = L.TableConst <$> mapM (liftM L.Field . valueToLua) xs
valueToLua (ObjectLiteral ps) = L.TableConst <$> mapM (\(n, v) -> L.ExpField (L.String n) <$> valueToLua v) ps
valueToLua (ObjectUpdate o ps) = do
    o' <- valueToLua o
    ps' <- mapM (\(i, e) -> (i,) <$> valueToLua e) ps
    extendObj o' ps'
valueToLua (Constructor name) = constr name
valueToLua (Case values alts) = do
    fun <- compilePatternMatch alts (length values)
    values' <- mapM valueToLua values
    return $ L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (L.Paren $ L.EFunDef $ fun)
                                                         (L.Args values')
valueToLua (IfThenElse cond th el) = do
    body' <- body
    return $ funcall (L.EFunDef $ L.FunBody [] False body') []
  where
    body = do
      c <- guard
      return $ L.Block [c] Nothing
    guard = do
      cond' <- valueToLua cond
      th' <- valueToLua th
      el' <- valueToLua el
      return $ L.If [(cond', L.Block [] (Just [th']))] (Just $ L.Block [] (Just [el']))
valueToLua (Accessor prop val) = do
    val' <- valueToLua val
    return $ L.PrefixExp $ L.PEVar $ L.Select (expToPexp val') (L.String prop)
valueToLua (App v1 v2) = funcall <$> valueToLua v1 <*> liftM (: []) (valueToLua v2)
valueToLua (Let ds val) = L.PrefixExp . L.PEFunCall . flip L.NormalFunCall (L.Args []) <$> body
  where
    body :: CG L.PrefixExp
    body = do
      ds' <- liftM concat $ mapM declToLua ds
      val' <- valueToLua val
      return $ L.Paren $ L.EFunDef $ L.FunBody [] False $ L.Block ds' (Just [val'])
valueToLua (Abs (Left arg) body) = L.EFunDef . L.FunBody [identToEscapedStr arg] False <$> ret
  where
    ret = do
      body' <- valueToLua body
      return $ L.Block [] (Just [body'])
valueToLua (TypedValue _ v@(Abs (Left arg) val) ty) = do
    opts <- gets cgOpts
    if optionsPerformRuntimeTypeChecks opts
      then do
        let arg' = identToEscapedStr arg
            stats = runtimeTypeChecks arg' ty
        ret <- valueToLua val
        return $ L.EFunDef $ L.FunBody [arg'] False (L.Block stats $ Just [ret])
      else
        valueToLua v
valueToLua (TypedValue _ val _) = valueToLua val
valueToLua (Var ident) = qIdentToLua ident
valueToLua (PositionedValue _ val) = valueToLua val
valueToLua TypeClassDictionary{} = error "Type class dictionary was not replaced"
valueToLua v = error $ "Invalid argument to valueToJs: " ++ show v

runtimeTypeChecks :: String -> Type -> [L.Stat]
runtimeTypeChecks arg ty =
    let argTy = getFunctionArgumentType ty
    in maybe [] (argumentCheck (L.PrefixExp $ L.PEVar $ L.VarName arg)) argTy
  where
    getFunctionArgumentType :: Type -> Maybe Type
    getFunctionArgumentType (TypeApp (TypeApp t funArg) _)
      | t == tyFunction = Just funArg
    getFunctionArgumentType (ForAll _ ty' _) = getFunctionArgumentType ty'
    getFunctionArgumentType _ = Nothing

    argumentCheck :: L.Exp -> Type -> [L.Stat]
    argumentCheck val t
      | t == tyNumber = [typeCheck val "number"]
      | t == tyString = [typeCheck val "string"]
      | t == tyBoolean = [typeCheck val "boolean"]
    argumentCheck _val (TypeApp t _)
      | t == tyArray = [] -- [arrayCheck val] -- TODO: how to check for arrays?
    argumentCheck val (TypeApp t row)
      | t == tyObject =
        let (pairs, _) = rowToList row
        in typeCheck val "table"
           : concatMap (\(field, ty') ->
               argumentCheck (fieldAccessor val field) ty') pairs
    argumentCheck val (TypeApp (TypeApp t _) _)
      | t == tyFunction = [typeCheck val "function"]
    argumentCheck val (ForAll _ ty' _) = argumentCheck val ty'
    argumentCheck _ _ = []

    typeCheck :: L.Exp -> String -> L.Stat
    typeCheck e tyName =
      let guard = L.Binop L.NEQ (funcall (var "type") [e]) (L.String tyName)
          body = funcallS "error" [L.String $ tyName ++ " expected."]
      in L.If [(guard, L.Block [body] Nothing)] Nothing

    fieldAccessor :: L.Exp -> String -> L.Exp
    fieldAccessor e fname =
      L.PrefixExp $ L.PEVar $ L.Select (L.Paren e) (L.String fname)

    -- arrayCheck :: L.Exp -> L.Stat
    -- arrayCheck _ = L.EmptyStat

-- | Pattern matching constructs are compiled to Lua functions that get
-- values being pattern matched as arguments.
compilePatternMatch
  :: [CaseAlternative] -- ^ cases in pattern matching
  -> Int -- ^ number of values being pattern matched
  -> CG L.FunBody -- ^ generated Lua function
compilePatternMatch alts nvals = do
    argNames <- replicateM nvals fresh
    stats <- concat <$> mapM (compilePatternCase argNames) alts
    let errStat = funcallS "error" [L.String "Failed pattern match"]
    return $ L.FunBody argNames False $ L.Block (stats ++ [errStat]) Nothing

compilePatternCase :: [String] -> CaseAlternative -> CG [L.Stat]
compilePatternCase argNames (CaseAlternative binders guard rhs) = do
    -- compile rhs using update environment with argNames
    CGState{..} <- get
    let cgEnv' = bindNames cgModName (concatMap binderNames binders) cgEnv
        (rhs', CGState _ _ _ fv) =
          runState (runCG $ valueToLua rhs) (CGState cgOpts cgEnv' cgModName cgFreshVar)
    modify $ \st -> st{cgFreshVar=fv}

    -- compile the guard
    guard' <- case guard of { Nothing -> return Nothing; Just g -> Just <$> valueToLua g }

    -- generate conditional statement for the case
    generateCond (zip argNames binders) guard' rhs'

generateCond :: [(String, Binder)] -> Maybe L.Exp -> L.Exp -> CG [L.Stat]
generateCond [] Nothing rhs =
    -- this if statement should be removed by the optimizer
    return [L.If [(L.Bool True, L.Block [] (Just [rhs]))] Nothing]
generateCond [] (Just guard) rhs =
    return [L.If [(guard, L.Block [] (Just [rhs]))] Nothing]
generateCond ((bname, binder) : bs) guard rhs = do
    cond <- generateCond bs guard rhs
    generateBinderStats bname binder cond

generateBinderStats :: String -> Binder -> [L.Stat] -> CG [L.Stat]
generateBinderStats _ NullBinder rest = return rest
generateBinderStats bname (StringBinder str) rest =
    return [L.If [(L.Binop L.EQ (var bname) (L.String str), L.Block rest Nothing)] Nothing]
generateBinderStats bname (NumberBinder num) rest =
    let lnum = L.Number (either show show num)
    in return [L.If [(L.Binop L.EQ (var bname) lnum, L.Block rest Nothing)] Nothing]
generateBinderStats bname (BooleanBinder True) rest =
    return [L.If [(var bname, L.Block rest Nothing)] Nothing]
generateBinderStats bname (BooleanBinder False) rest =
    return [L.If [(L.Unop L.Not $ var bname, L.Block rest Nothing)] Nothing]
generateBinderStats bname (VarBinder ident) rest = do
    return (L.LocalAssign [identToEscapedStr ident] (Just [var bname]) : rest)
generateBinderStats bname (ConstructorBinder ctor binders) rest = do
    stats <- bindVals binders 1 rest
    onlyctor <- isOnlyConstructor ctor
    if onlyctor
      then return stats
      else
        let cond =
              L.Binop L.EQ (L.PrefixExp $ L.PEVar $ L.SelectName (L.PEVar $ L.VarName bname) "ctor")
                           (L.String $ show ctor)
        in return [L.If [(cond, L.Block stats Nothing)] Nothing]
  where
    bindVals :: [Binder] -> Int -> [L.Stat] -> CG [L.Stat]
    bindVals [] _ rest' = return rest'
    bindVals (b : bs) idx rest' = do
      elVar <- fresh
      rest'' <- bindVals bs (idx + 1) rest'
      stats <- generateBinderStats elVar b rest''
      let accessor =
            -- `bname.values[`idx]
            L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.SelectName (L.PEVar $ L.VarName bname) "values")
                                             (L.Number $ show idx)
          assignment =
            -- local `elVar = `bname.values[`idx]
            L.LocalAssign [elVar] (Just [accessor])
      return (assignment : stats)
generateBinderStats bname (ObjectBinder binders) rest = bindFields binders rest
  where
    bindFields :: [(String, Binder)] -> [L.Stat] -> CG [L.Stat]
    bindFields [] rest' = return rest'
    bindFields ((fname, binder) : bs) rest' = do
      fVar <- fresh
      rest'' <- bindFields bs rest'
      stats <- generateBinderStats fVar binder rest''
      let accessor =
            -- `bname.`fname
            L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.VarName bname) (L.String fname)
      return (L.LocalAssign [fVar] (Just [accessor]) : stats)
generateBinderStats bname (ConsBinder headb tailb) rest = do
    headVar <- fresh
    tailVar <- fresh
    headBStat <- generateBinderStats headVar headb rest
    tailBStat <- generateBinderStats tailVar tailb headBStat
    let cond =
          -- #`bname > 0
          L.Binop L.GT (L.Unop L.Len (var bname)) (L.Number "0")
        body =
          -- local `headVar = bname[1]
          -- local `tailVar = {}
          -- for i=2, #`bname do
          --   `tailVar[i-1] = `bname[i]
          -- end
          let headVarRhs =
                L.PrefixExp $ L.PEVar $
                  L.Select (L.PEVar $ L.VarName bname) (L.Number "1")
              tailVarRhs = L.TableConst []
          in
          [ L.LocalAssign [headVar, tailVar] (Just [headVarRhs, tailVarRhs])
          , L.ForRange "i" (L.Number "2") (L.Unop L.Len $ var bname) Nothing $ L.Block
              [ L.Assign [L.Select (L.PEVar $ L.VarName tailVar) (L.Binop L.Sub (var "i") (L.Number "1"))]
                         [L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.VarName bname) (var "i")]
              ] Nothing
          ]
    return [L.If [(cond, L.Block (body ++ tailBStat) Nothing)] Nothing]
generateBinderStats bname (ArrayBinder binders) rest = do
    stats <- bindElems binders 1 rest
    let cond = L.Binop L.EQ (L.Unop L.Len (var bname)) (L.Number $ show $ length binders)
    return [L.If [(cond, L.Block stats Nothing)] Nothing]
  where
    bindElems :: [Binder] -> Int -> [L.Stat] -> CG [L.Stat]
    bindElems [] _ rest' = return rest'
    bindElems (b : bs) idx rest' = do
      elVar <- fresh
      rest'' <- bindElems bs (idx + 1) rest'
      stats <- generateBinderStats elVar b rest''
      let indexer =
            -- `bname[`idx]
            L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.VarName bname) (L.Number $ show idx)
      return (L.LocalAssign [elVar] (Just [indexer]) : stats)
generateBinderStats bname (NamedBinder ident binder) rest = do
    let ident' = identToEscapedStr ident
    binder' <- generateBinderStats bname binder rest
    return (L.LocalAssign [ident'] (Just [var bname]) : binder')
generateBinderStats bname (PositionedBinder _ binder) rest =
    generateBinderStats bname binder rest

isOnlyConstructor :: Qualified ProperName -> CG Bool
isOnlyConstructor ctor = do
    e <- gets cgEnv
    let ty = fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e
    return $ numConstructors e (ctor, ty) == 1
  where
    numConstructors e ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors e
    typeConstructor (Qualified (Just moduleName) _, (tyCtor, _)) = (moduleName, tyCtor)
    typeConstructor _ = error "Invalid argument to isOnlyConstructor"

extendObj :: L.Exp -> [(String, L.Exp)] -> CG L.Exp
extendObj obj extensions = do
    -- local `fresh` = {}
    -- for k, v in pairs(obj) do
    --   `fresh`[k] = v
    -- end
    -- for k, v in `extensions` do
    --   `fresh`[k] = `extensions`[k]
    -- end
    -- return `fresh`
    newObjName <- fresh
    let newObj = L.LocalAssign [newObjName] (Just [L.TableConst []])
        copyObj = L.ForIn ["k", "v"] [funcall (var "pairs") [obj]] $
                    let lhs = L.Select (expToPexp $ var newObjName) (var "k")
                        rhs = var ("v")
                    in L.Block [L.Assign [lhs] [rhs]] Nothing
        asgnNewFields = map (\(fname, rhs) ->
                              L.Assign [L.Select (expToPexp $ var newObjName) (L.String fname)]
                                       [rhs]) extensions
        fun = L.EFunDef $ L.FunBody [newObjName] False $ L.Block (newObj : copyObj : asgnNewFields)
                                                                 (Just [var newObjName])
    return $ L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (L.Paren fun) (L.Args [obj])

constr :: Qualified ProperName -> CG L.Exp
constr (Qualified Nothing (ProperName c)) = return $ L.PrefixExp $ L.PEVar $ L.VarName c
constr (Qualified (Just ns) (ProperName c)) = do
    curModule <- gets cgModName
    return $ if curModule == ns
               then
                 L.PrefixExp $ L.PEVar $ L.VarName c
               else
                 -- m1_m2_m3.c
                 L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.VarName $ mnameToStr ns) (L.String c)

qIdentToLua :: Qualified Ident -> CG L.Exp
qIdentToLua (Qualified Nothing ident) = return $ L.PrefixExp $ L.PEVar $ L.VarName $ identToEscapedStr ident
qIdentToLua (Qualified (Just ns) ident) = do
    curModule <- gets cgModName
    return $ if curModule == ns
               then
                 L.PrefixExp $ L.PEVar $ L.VarName (identToEscapedStr ident)
               else
                 L.PrefixExp $ L.PEVar $ L.Select (L.PEVar $ L.VarName $ mnameToStr ns)
                                                  (L.String $ identToStr ident)

mnameToStr :: ModuleName -> String
mnameToStr (ModuleName pns) = intercalate "_" (runProperName `map` pns)

nameIsLuaReserved :: String -> Bool
nameIsLuaReserved s = s `S.member` reserved
  where
    reserved = S.fromList
      [ "and", "break", "do", "else", "elseif", "end", "false", "for", "function"
      , "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then"
      , "true", "until", "while" ]

identToStr :: Ident -> String
identToStr (Ident name) = name
identToStr (Op op) = op

identToEscapedStr :: Ident -> String
identToEscapedStr (Ident name) | nameIsLuaReserved name = "__" ++ name
identToEscapedStr (Ident name) = concatMap identCharToString name
identToEscapedStr (Op op) = concatMap identCharToString op

bindNames :: ModuleName -> [Ident] -> Environment -> Environment
bindNames m idents env =
    env{ names = M.fromList [ ((m, ident), (noType, LocalVariable)) | ident <- idents ] `M.union` names env }
  where
    noType = error "Temporary lambda variable type was read"

identCharToString :: Char -> String
identCharToString c
  | isAlphaNum c = [c]
  | otherwise =
      case c of
        '_' -> "_"
        '.' -> "_dot"
        '$' -> "_dollar"
        '~' -> "_tilde"
        '=' -> "_eq"
        '<' -> "_less"
        '>' -> "_greater"
        '!' -> "_bang"
        '#' -> "_hash"
        '%' -> "_percent"
        '^' -> "_up"
        '&' -> "_amp"
        '|' -> "_bar"
        '*' -> "_times"
        '/' -> "_div"
        '+' -> "_plus"
        '-' -> "_minus"
        ':' -> "_colon"
        '\\' -> "_bslash"
        '?' -> "_qmark"
        '@' -> "_at"
        '\'' -> "_prime"
        _ -> '_' : show (ord c)

