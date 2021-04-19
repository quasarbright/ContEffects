{-# LANGUAGE TemplateHaskell #-}
module Interpreter where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Lens
import Control.Lens.Combinators

data Expr
    = Var String
    | Add Expr Expr
    | Eq Expr Expr
    | Int Int
    | Let String Expr Expr
    | Lambda String Expr
    | If Expr Expr Expr
    | TryHandle Expr String Expr
    | Perform Expr

newtype Interpreter a = Interpreter { runInterpreter :: ReaderT Env (ExceptT String Identity) a}
    deriving
        (
            Functor,
            Applicative,
            Monad,
            MonadReader Env,
            MonadError String
        )

data Value = VInt Int | VLambda Env String Expr | VBuiltinFun (Value -> Interpreter Value)

data Env = Env { _vars :: Map String Value, _handler :: () }

makeLenses ''Env

withVar :: String -> Value -> Interpreter a -> Interpreter a
withVar x v = local (vars %~ Map.insert x v)

