-- Autor: Kamil Zwierzchowski
-- Interpreter

module Evaluation where
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Fix
import Data.Maybe
import qualified Data.Map as Map
import AbsExp





---------------------- DATA TYPES -------------------------

type Name = String

data Value = IntVal Int
           | BoolVal Bool
           | StringVal String
           | Location Loc
           deriving (Show)


data Flags = BreakWhile
           | ContinueWhile
           | Return 
           deriving (Show, Eq, Ord)

type Loc = Int 
type Store = Map.Map Loc Value 
type VarEnv = Map.Map Name Loc
data FunVal = FunData 
  { env :: Env,
    args :: [(Name, Bool)],   
    body :: Block,
    ret :: Type }
type FunEnv = Map.Map Name FunVal 
type FlagsEnv = Map.Map Flags Value 

data Env = EnvData 
  { var :: VarEnv, 
    fun :: FunEnv, 
    flags :: FlagsEnv}
    



type Eval a = ReaderT Env (ExceptT String (StateT Store IO)) a


---------------------- SPECIAL FUNCTION TYPES (FOR PRINING) --------------------

printFunction :: BNFC'Position -> Name -> Value -> Eval ()
printFunction a fun arg = case arg of
  IntVal i -> liftIO $ print i 
  BoolVal b -> liftIO $ print b 
  StringVal s -> liftIO $ print s 
  _ -> printError ("Function provided wrong type of argument") a


initPrintCheck :: BNFC'Position -> Name -> [Expr] -> Eval Value
initPrintCheck a fun args = 
  case length args == 1 of 
    False -> printError ("Function provided wrong number of arguments") a
    True -> do 
      val <- evalExpr $ head args 
      printFunction a fun val
      return val


specialPrintingFunctions = 
  Map.fromList [(show $ Ident "printInt", initPrintCheck), 
                (show $ Ident "printString", initPrintCheck),
                (show $ Ident "printBoolean", initPrintCheck)]


------------------------ SPECIAL FUNCTION MAP -----------------------------------

type SpecialFunction = BNFC'Position -> Name -> [Expr] -> Eval Value

specialFunctions :: Map.Map Name SpecialFunction
specialFunctions = specialPrintingFunctions



----------------------- TYPES FUNCTIONS ----------------------------------------

-- Checks whether types agree. It ignores tuples and different array dimensions.
checkType :: Type -> Value -> Eval () 
checkType t val = case (t, val) of 
  (Int _, IntVal _) -> return ()
  (Str _, StringVal _) -> return ()
  (Bool _, BoolVal _) -> return ()
  (Array a t' _, Location loc) -> do 
    val' <- getFromLoc a loc 
    checkType t' val'
  _ -> printError ("Types does not match") Nothing


getInitialValue :: Type -> Value
getInitialValue (Int _) = IntVal 0
getInitialValue (Str _) = StringVal ""
getInitialValue (Bool _) = BoolVal False 
getInitialValue (Array _ t' _) = getInitialValue t' 


------------------------ LOCATION FUNCTIONS -----------------------------------

-- Adds new location and returns it.
addNewLoc :: Value -> Eval Loc
addNewLoc val = do 
  locations <- get
  let s = Map.size locations
  put $ Map.insert s val locations
  return $ s 


addNewLocBlock :: BNFC'Position -> Value -> Int -> Eval [Loc]
addNewLocBlock a val l = case l <= 0 of 
  True -> printError ("Tried to allocate non positive length array") a
  False -> mapM addNewLoc (take l (repeat val))


-- Gets value from given location.
getFromLoc :: BNFC'Position -> Loc -> Eval Value  
getFromLoc a loc = do 
  locations <- get 
  case Map.lookup loc locations of
    Nothing -> printError ("Incorrect read from location" ++ (show loc)) a
    Just val -> return val 


-- Puts value to given location
putToLoc :: BNFC'Position -> Loc -> Value -> Eval Value 
putToLoc a loc val = do
  locations <- get 
  case Map.member loc locations of
    False -> printError ("Incorrect subscribing to location" ++ (show loc)) a
    True -> do 
      put $ Map.insert loc val locations
      return val



-- Allocates array for a type and variable location.
allocateArray :: Type -> Value -> Loc -> Eval [Loc]
allocateArray t val loc = case t of 
    Array a t' i -> let 
        allocateBlock :: Loc -> Eval [Loc]   -- allocates new block and tie it to location
        allocateBlock loc' = do 
          locations' <- addNewLocBlock a val (fromIntegral i) 
          putToLoc a loc' (Location $ head locations')
          return locations'
      in do 
        locations <- allocateArray t' val loc       -- allocates previous layer
        -- foldM (\li loc' -> ((addNewLocBlock val (fromIntegral i))++li)) [] locations
        locations' <- mapM allocateBlock locations 
        return $ concat locations'  
    _ -> return [loc]                         -- variable pointing at some point 


copyLocation :: Loc -> Eval Loc 
copyLocation loc = do
  val <- getFromLoc Nothing loc 
  addNewLoc val


duplicateEnv :: Eval Env 
duplicateEnv = do 
  env <- ask 
  let varEnv = var env 
  varEnv' <- mapM copyLocation varEnv
  return $ EnvData varEnv' (fun env) (flags env)




-- Takes the type and initial value.
allocate :: BNFC'Position -> Type -> Value -> Eval Loc
allocate a t val = case (t, val) of    -- if simple type, allocate one cell, otherwise allocate
  (Int a', IntVal i) -> addNewLoc val 
  (Bool a', BoolVal b) -> addNewLoc val 
  (Str a', StringVal s) -> addNewLoc val 
  (Array a' t' i, _) -> do 
    loc <- addNewLoc val 
    allocateArray t val loc 
    return loc 
  _ -> printError "Type does not agree with initial value" a

---------------------------- ENVIRONMENT FUNCTIONS -----------------------------

composeStmt :: Env -> Stmt -> Eval Env  
composeStmt env stmt = local (\_ -> env) $ evalStmt stmt


----------------------- VARIABLE ENVIRONMENT FUNCTIONS --------------------------


-- Get location kept by variable.
getVarLoc :: BNFC'Position -> Name -> Eval Loc
getVarLoc a n = do 
  varEnv <- asks var
  case Map.lookup n varEnv of 
    Nothing -> printError ("Variable not defined") a
    Just loc -> return loc 

-- Get value kept by variable. 
getFromVar :: BNFC'Position -> Name -> Eval Value 
getFromVar a n = do
  loc <- getVarLoc a n 
  getFromLoc a loc


putToVar :: Name -> Loc -> Eval Env
putToVar n loc = do
  EnvData varEnv funEnv flgasEnv <- ask 
  return $ EnvData (Map.insert n loc varEnv) funEnv flgasEnv

-- Get the location variable is pointing at.
getVariableLoc :: BNFC'Position -> Lval -> Eval Loc 
getVariableLoc a (SmplVal b ident) = getVarLoc a $ show ident 
getVariableLoc a (CmplVal b x e) = do
  val <- evalExpr (EVar b x)
  case val of
    Location loc -> do            -- memory is allocated, no control on memory
      val' <- evalExpr e 
      case val' of 
        IntVal i -> 
          return $ loc + i      -- offset + index
        _ -> printError ("Index provided to array is not a number") a
    _ -> printError ("Provided variable is not an array") a



----------------------- FUNCTIONS ENVIRONMNET FUNCTION ----------------------

addFunction :: Name -> FunVal -> Env -> Env 
addFunction n fun' env = EnvData (var env) (Map.insert n fun' (fun env)) (flags env)

findFunction :: Name -> Env -> Maybe FunVal
findFunction n = (Map.lookup n) . fun 

findMain :: Env -> Maybe FunVal
findMain = findFunction $ show (Ident "main")



----------------------- FLAGS ENVIRONMENT FUNCTIONS ---------------------------

extractReturn :: BNFC'Position -> Env -> Eval Value 
extractReturn a env@(EnvData _ _ flagsEnv) = 
  case isNotReturnFlag env of 
    True -> printError ("Break or continue used outside of a loop") a
    False -> case Map.lookup Return flagsEnv of 
      Nothing -> printError ("Function did not returned its value") a
      Just x -> return x

putReturn :: Value -> Eval Env 
putReturn val = do 
  EnvData varEnv funEnv flagsEnv <- ask 
  return $ EnvData varEnv funEnv (Map.insert Return val flagsEnv)


putContinue :: Eval Env 
putContinue = do 
  EnvData varEnv funEnv flagsEnv <- ask 
  return $ EnvData varEnv funEnv (Map.insert ContinueWhile (IntVal 0) flagsEnv)

putBreak :: Eval Env 
putBreak = do 
  EnvData varEnv funEnv flagsEnv <- ask 
  return $ EnvData varEnv funEnv (Map.insert BreakWhile (IntVal 0) flagsEnv)

anyFlag :: Env -> Bool 
anyFlag env = (not . Map.null . flags) env

isNotReturnFlag :: Env -> Bool 
isNotReturnFlag env = (isFlag env ContinueWhile) || (isFlag env BreakWhile)

isNotContinueFlag :: Env -> Bool 
isNotContinueFlag env = (isFlag env BreakWhile) || (isFlag env Return)

isFlag :: Env -> Flags -> Bool 
isFlag env flag = ((Map.member flag) . flags) env


------------------------ PROGRAM EVALUATIONS ---------------------------------

emptyEnv :: Env 
emptyEnv = EnvData Map.empty Map.empty Map.empty

composeTopDef :: Env -> TopDef -> Eval Env  
composeTopDef env def = case findMain env of 
  Nothing -> local (\_ -> env) $ evalTopDef def
  Just _ -> return env

evalProgram :: Program -> Eval Value
evalProgram (Program a defs) = do 
  env <- foldM composeTopDef emptyEnv defs 
  case findMain env of 
    Nothing -> throwError ("main function not declared.")
    Just f -> 
      local (\_ -> env) $ evalExpr (EApp Nothing (Ident "main") [])


printErrorLoc :: BNFC'Position -> String 
printErrorLoc Nothing = "."
printErrorLoc (Just (line, column)) = " in line " ++ (show line) ++ " in column " ++ (show column) ++ "."

printError :: String -> BNFC'Position -> Eval a 
printError err a = throwError $ err ++ (printErrorLoc a) 


----------------------- BLOCK EVALUATIONS --------------------------------------

evalBlock :: Block -> Eval Env
evalBlock (Block a stmts) = do 
  env <- ask 
  env' <- foldM composeStmt env stmts
  return env'


----------------------- STATEMENT EVALUATIONS ----------------------------------

argToPair :: Arg -> (Name, Bool)
argToPair (Arg a t ident) = (show ident, False)
argToPair (ArgRef a t ident) = (show ident, True)


evalTopDef :: TopDef -> Eval Env 
evalTopDef (FnDef a t ident args body) = do 
  env' <- duplicateEnv
  let args' = map argToPair args
  let fun' = FunData env' args' body t
  env <- ask
  let fun'' = FunData (addFunction (show ident) fun'' env') args' body t 
  return $ addFunction (show ident) fun'' env




evalItem :: Type -> Item -> Eval Env
evalItem t (NoInit a ident) = do 
  loc <- allocate a t (getInitialValue t)
  putToVar (show ident) loc 
evalItem t (Init a ident e) = do
  val <- evalExpr e 
  loc <- allocate a t val 
  putToVar (show ident) loc


propagateFlags :: Env -> Env -> Eval Env 
propagateFlags env env' = 
  return $ EnvData (var env) (fun env) (flags env')

propagateReturn :: Env -> Env -> Eval Env 
propagateReturn env env' = 
  return $ EnvData (var env) (fun env)
                   ((Map.delete ContinueWhile) $ (Map.delete BreakWhile) (flags env'))
           


evalStmt :: Stmt -> Eval Env 
evalStmt stmt = do 
  env <- ask
  let b = anyFlag env 
  if b then return env else evalStmt' stmt
  

evalStmt' :: Stmt -> Eval Env
evalStmt' (Empty a) = ask
evalStmt' (BStmt a block) = do
  env <- ask
  env' <- evalBlock block 
  propagateFlags env env'
evalStmt' (Decl a t items) = let 
    composeItem :: Env -> Item -> Eval Env  
    composeItem env item = local (\_ -> env) $ evalItem t item  
  in do
    env <- ask
    foldM composeItem env items
evalStmt' (LocFun a def) = evalTopDef def
evalStmt' (Ass a lval e) = do 
  loc <- getVariableLoc a lval
  val <- evalExpr e
  putToLoc a loc val 
  ask
evalStmt' (Incr a lval) = do 
  loc <- getVariableLoc a lval 
  val <- getFromLoc a loc
  case val of 
    IntVal i -> do 
      putToLoc a loc $ IntVal (i + 1)
      ask
    _ -> printError ("Provided non-int to increment") a
evalStmt' (Decr a lval) = do 
  loc <- getVariableLoc a lval 
  val <- getFromLoc a loc 
  case val of 
    IntVal i -> do 
      putToLoc a loc $ IntVal (i - 1)
      ask
    _ -> printError ("Provided non-int to decrement") a
evalStmt' (Ret a e) = do 
  val <- evalExpr e 
  putReturn val 
evalStmt' (Cont a) = do 
  env <- putContinue
  return env
evalStmt' (Break a) = do 
  env <- putBreak
  return env
evalStmt' (Cond a expr stmt) = do 
  val <- evalExpr expr 
  case val of 
    BoolVal False -> ask 
    BoolVal True -> do
      env <- ask
      env' <- evalStmt stmt 
      propagateFlags env env'
    _ -> printError ("Provided non-boolean value to if condition") a
evalStmt' (CondElse a expr stmt1 stmt2) = do 
  val <- evalExpr expr 
  case val of 
    BoolVal True -> do
      env <- ask
      env' <- evalStmt stmt1
      propagateFlags env env' 
    BoolVal False -> do 
      env <- ask
      env' <- evalStmt stmt2 
      propagateFlags env env' 
evalStmt' loop@(While a expr stmt) = do
  val <- evalExpr expr 
  case val of 
    BoolVal False -> ask 
    BoolVal True -> do 
      env' <- evalStmt stmt 

      case isNotContinueFlag env' of 
        True -> do
          env <- ask 
          propagateReturn env env'
        False -> do
          env <- ask
          env'' <- evalStmt loop
          propagateReturn env env'' 
    _ -> printError ("Provided non-boolean value to while condition") a
evalStmt' (SExp a expr) = do 
  evalExpr expr 
  ask





----------------------- EXPRESIONS EVALUATIONS  ---------------------------------




evalExpr :: Expr -> Eval Value
evalExpr (EVar a x) = do 
  loc <- getVariableLoc a x
  getFromLoc a loc
evalExpr (ELitInt _ i) = return $ IntVal (fromIntegral i) 
evalExpr (ELitTrue _) = return $ BoolVal True
evalExpr (ELitFalse _) = return $ BoolVal False 
evalExpr (EApp a ident xs) =
  case Map.lookup (show ident) specialFunctions of    -- checking whether special function is used
    Just f -> f a (show ident) xs 
    Nothing -> do
      EnvData _ fun _ <- ask                   -- taking fun environment 
      case Map.lookup (show ident) fun of
        Nothing -> printError ("Function not defined") a
        Just f -> if length (args f) /= (length xs)       -- function exists
          then printError ("Function provided wrong number of arguments") a
          else let 
              evalArg :: ((Name, Bool), Expr) -> Eval (Name, Loc) -- evaluate arguments
              evalArg ((n, r), expr) = 
                case r of                   -- not reference -> just evaluate
                  False -> do
                    val <- evalExpr expr 
                    loc <- addNewLoc val 
                    return (n, loc)
                  True ->                   -- a reference -> check for indent
                    case expr of
                      EVar b lval -> do      -- the variable -> find its location
                        loc <- getVariableLoc a lval     
                        return (n, loc)  
                      _ -> printError ("Argument provided to function is not an lvalue") a

              appArg :: (Name, Loc) -> (Env -> Env) -> (Env -> Env)
              appArg (n, loc) fun env = let 
                  EnvData varEnv funEnv flgasEnv = fun env 
                  varEnv' = Map.insert n loc varEnv
                in EnvData varEnv' funEnv flgasEnv


              app :: Env -> [(Name, Loc)] -> (Env -> Env)  -- applicate argument to variables
              app env' args = foldr appArg (const env') args


            in do
              evalArgs <- mapM evalArg (zip (args f) xs) 
              env' <- local (app (env f) evalArgs) (evalBlock $ body f)
              retVal <- extractReturn a env'
              checkType (ret f) retVal
              return retVal



evalExpr (EString a s) = return $ StringVal s 
evalExpr (Neg a expr) = do 
  val <- evalExpr expr 
  case val of 
    IntVal i -> return $ IntVal (-i)
    _ -> printError ("Provided expresion does not return an Int type") a
evalExpr (Not a expr) = do 
  val <- evalExpr expr 
  case val of 
    BoolVal b -> return $ BoolVal (not b)
    _ -> printError ("Provided expresion does not return a Boolean type") a
evalExpr (EMul a e1 (Div _) e2) = do 
  val1 <- evalExpr e1 
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (IntVal i1, IntVal i2) -> case i2 of 
      0 -> printError "Division by 0" a
      _ -> return $ IntVal (div i1 i2)
    _ -> printError ("Provided expresion does not return an Int type") a

evalExpr (EMul a e1 op e2) = do 
  val1 <- evalExpr e1  
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (IntVal i1, IntVal i2) -> return $ IntVal (mulToFun op i1 i2)
    _ -> printError ("Provided expresion does not return an Int type") a
evalExpr (EAdd a e1 op e2) = do 
  val1 <- evalExpr e1 
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (IntVal i1, IntVal i2) -> return $ IntVal (addToFun op i1 i2)
    _ -> printError ("Provided expresion does not return an Int type") a
evalExpr (ERel a e1 op e2) = do 
  val1 <- evalExpr e1 
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (IntVal i1, IntVal i2) -> return $ BoolVal (relToFun op i1 i2)
    _ -> printError ("Provided expresion does not return an Int type") a
evalExpr (EAnd a e1 e2) = do 
  val1 <- evalExpr e1 
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 && b2)
    _ -> printError ("Provided expresion does not return an Boolean type") a
evalExpr (EOr a e1 e2) = do 
  val1 <- evalExpr e1 
  val2 <- evalExpr e2 
  case (val1, val2) of 
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 || b2)
    _ -> printError ("Provided expresion does not return an Boolean type") a


-------------------------- OPERATOR TO FUNCTION -----------------------------

-- Converts the MulOp operator to the real function.
mulToFun :: MulOp -> (Int -> Int -> Int)
mulToFun (Times _) = (*)
mulToFun (Div _) = div
mulToFun (Mod _) = mod

-- Converts the AddOp operator to the real function.
addToFun :: AddOp -> (Int -> Int -> Int)
addToFun (Plus _) = (+)
addToFun (Minus _) = (-)

-- Converts the RelOp operator to the real function.
relToFun :: RelOp -> (Int -> Int -> Bool)
relToFun (LTH _) = (<)
relToFun (LE _) = (<=)
relToFun (GTH _) = (>)
relToFun (GE _) = (>=)
relToFun (EQU _) = (==)
relToFun (NE _) = (/=)              





---------------------------- TESTING ENVIRONMENT ----------------------------


runEval :: Eval a -> IO (Either String a, Store)
runEval ev = runStateT (runExceptT (runReaderT ev (EnvData Map.empty Map.empty Map.empty))) Map.empty

runExpr :: Expr -> IO (Either String Value, Store)
runExpr expr = do
  (val, store) <- (runEval . evalExpr) expr
  return (val, store)

example1 = ELitInt Nothing 10
example2 = ELitTrue Nothing


