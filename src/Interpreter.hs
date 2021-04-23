{-# LANGUAGE TemplateHaskell #-}
module Interpreter where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.RWS.Strict
    ( MonadReader(local), RWST(..), asks, MonadWriter (tell) )
import Control.Monad.Except
    ( MonadError(throwError), ExceptT, runExceptT )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Lens ( (%~), makeLenses, (^.), (&), (.~) )
import Control.Arrow ((>>>))
import Data.Functor (($>))

data Expr
    = Var String
    | Add Expr Expr
    | Eq Expr Expr
    | Int Int
    | Unit
    | Let String Expr Expr
    | Lambda String Expr
    | App Expr Expr
    | If Expr Expr Expr
    | TryHandle Expr String Expr
    | Perform Expr
    | Print Expr
    | Seq Expr Expr

infixr 1 \-> 
(\->) :: String -> Expr -> Expr
(\->) = Lambda
infixr 2 \>>
(\>>) :: Expr -> Expr -> Expr
(\>>) = Seq
infixr 3 \$
(\$) :: Expr -> Expr -> Expr
(\$) = App
infix 6 \==
(\==) :: Expr -> Expr -> Expr
(\==) = Eq
infixl 7 \+
(\+) :: Expr -> Expr -> Expr
(\+) = Add

lets :: [(String, Expr)] -> Expr -> Expr
lets bindings body = foldr (uncurry Let) body bindings

newtype Interpreter a = Interpreter
    { getInterpreter :: 
        ExceptT String
        (
        RWST Env [Value] () 
        Identity
        )
        a
    }
    deriving
        (
            Functor,
            Applicative,
            Monad,
            MonadReader Env,
            MonadError String,
            MonadWriter [Value]
        )

defaultHandler :: Value
defaultHandler = VBuiltinFun $ \v -> throwError ("Unhandled effect: " ++ show v)

runInterpreter :: Interpreter a -> (Either String a, [Value])
runInterpreter =
    getInterpreter
    >>> runExceptT
    >>> (\m -> runRWST m (Env mempty defaultHandler) ())
    >>> fmap (\(a,_,w) -> (a,w))
    >>> runIdentity

execInterpreter :: Interpreter a -> Either String a
execInterpreter = runInterpreter >>> fst

-- For VLambda, normal lambdas have Nothing for the value, and handler lambdas have Just their parent handler
data Value = VInt Int | VUnit | VLambda (Map String Value) (Maybe Value) String Expr | VBuiltinFun (Value -> Interpreter Value)

instance Show Value where
    show = \case
        VInt n -> show n
        VLambda{} -> "<lambda>"
        VBuiltinFun{} -> "<builtin>"
        VUnit -> "()"

instance Eq Value where
    VInt l == VInt r = l == r
    VInt{} == _ = False
    VUnit == VUnit = True
    VUnit == _ = False
    VLambda{} == _ = False
    VBuiltinFun{} == _ = False

data Env = Env { _vars :: Map String Value, _handler :: Value }

makeLenses ''Env

withVar :: String -> Value -> Interpreter a -> Interpreter a
withVar x v = local (vars %~ Map.insert x v)

lookupVar :: String -> Interpreter Value
lookupVar x = do
    asks (\e -> e ^. vars & Map.lookup x) >>= \case
        Nothing -> throwError ("Unbound var: "++x)
        Just v -> return v

withHandler :: Value -> Interpreter a -> Interpreter a
withHandler h = local (handler .~ h)

printValue :: Value -> Interpreter ()
printValue = tell . pure

app :: Value -> Expr -> Interpreter Value
app vf ex = case vf of
    VBuiltinFun f -> f =<< evalExpr ex
    VLambda vs h x body -> do
        vx <- evalExpr ex
        -- If we're applying a handler lambda, run the body using its parent. Otherwise, don't change the handler
        local (vars .~ Map.insert x vx vs >>> maybe id (handler .~) h) (evalExpr body)
    _ -> throwError "applied non-function"

mkLambda :: String -> Expr -> Interpreter Value
mkLambda x body = do
    closure <- asks (^.vars)
    return $ VLambda closure Nothing x body

mkHandler :: String -> Expr -> Interpreter Value
mkHandler x body = do
    closure <- asks (^.vars)
    h <- asks (^.handler)
    return $ VLambda closure (Just h) x body

evalExpr :: Expr -> Interpreter Value
evalExpr = \case
    Var x -> lookupVar x
    Int n -> return $ VInt n
    Unit -> return VUnit
    Print e -> (printValue =<< evalExpr e) $> VUnit
    Seq l r -> evalExpr l >> evalExpr r
    App ef ex -> do
        vf <- evalExpr ef
        app vf ex
    Perform eff -> do
        h <- asks (^.handler)
        app h eff
    Eq l r -> do
        vl <- evalExpr l
        vr <- evalExpr r
        case (vl,vr) of
            (VInt nl, VInt nr) -> return . VInt $ if nl == nr then 1 else 0
            (VUnit, VUnit) -> return (VInt 1)
            _ -> return (VInt 0)
    Add l r -> do
        vl <- evalExpr l
        vr <- evalExpr r
        case (vl, vr) of
            (VInt nl, VInt nr) -> return . VInt $ nl + nr
            _ -> throwError "+ expects two ints"
    If cnd thn els -> evalExpr cnd >>= \case
        VInt 0 -> evalExpr els
        VInt{} -> evalExpr thn
        _ -> throwError "if expected int"
    Let x rhs body -> do
        vrhs <- evalExpr rhs
        withVar x vrhs (evalExpr body)
    Lambda x body -> mkLambda x body
    TryHandle thrower e h -> do
        h' <- mkHandler e h
        withHandler h' (evalExpr thrower)

runEvalExpr :: Expr -> Either String Value
runEvalExpr = evalExpr >>> execInterpreter
