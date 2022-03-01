{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - Tipos de datos con SS
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables
-}

module Lang where

import Common ( Pos )
import Data.List (delete)

-- Constructores Básicos
type Name = String

data Const = CNat !Int
  deriving Show

data UnaryOp = Succ | Pred | Print
  deriving Show

data BinaryOp = Add | Sub
  deriving Show

-- | Shallow Types AST
data STy = 
      SNatTy
    | SFunTy STy STy
    | SAliasTy Name  -- Para cuando se utiliza el alias en lugar del tipo
    deriving Show

-- | Tipo de datos de las declaraciones con SS
data SDecl a =
    STypeAlias { aliasPos :: Pos, aliasName :: Name, aliasTy :: STy } -- Para definir alias de tipos
  | SLetDec { letDeclPos :: Pos, letDeclName :: Name, letDeclParams :: [(Name, STy)], letDeclTy :: STy, letDeclBody :: a }
  | SLetRecDec { letRecDeclPos :: Pos, letRecDeclName :: Name, letRecDeclParams :: [(Name, STy)], letRecDeclTy :: STy, letRecDeclBody :: a }
  deriving (Show,Functor)

-- | Shallow AST para soportar azúcar sintáctico
data STm info var =
    SV info var
  | SConst info Const
  | SLet info Name [(Name, STy)] STy (STm info var) (STm info var)
  | SLetRec info Name [(Name, STy)] STy (STm info var) (STm info var)
  | SLam info [(Name, STy)] (STm info var)
  | SApp info (STm info var) (STm info var)
  | SUnaryOp info UnaryOp (Maybe (STm info var))-- El parseo de un unaryOp aplicado a un valor se hace con una aplicación
  | SBinaryOp info BinaryOp  -- El parseo de un binaryOp aplicado a un valor se hace con una aplicación
  | SFix info [(Name, STy)] (STm info var)
  | SIfZ info (STm info var) (STm info var) (STm info var)
  deriving (Show, Functor)

type STerm = STm Pos Name   -- Los términos con SS guardan la posición y tienen variables con nombres

-- | AST de Tipos
data Ty = 
      NatTy 
    | FunTy Ty Ty
    deriving (Show,Eq)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a =
    Decl { declPos :: Pos, declName :: Name, declBody :: a }
  deriving (Show,Functor)

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var = 
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | Let info Name Ty (Tm info var) (Tm info var) -- implementacion let-binding interna
  | App info (Tm info var) (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  -- | UnaryOp info UnaryOp (Tm info var) -- VER Esta para el print de llvm
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  deriving (Show, Functor)

type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición

type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición

data Var = 
    Bound !Int
  | Free Name
  deriving Show

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _) = i
getInfo (Const i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _ ) = i
getInfo (BinaryOp i _ _ _) = i
-- getInfo (UnaryOp i _ _ ) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i
getInfo (Let i _ _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))     = [v]
freeVars (V _ _)            = []
freeVars (Lam _ _ _ t)      = freeVars t
freeVars (App _ l r)        = freeVars l ++ freeVars r
freeVars (BinaryOp _ _ l r) = freeVars l ++ freeVars r
-- freeVars (UnaryOp _ _ t)    = freeVars t
freeVars (Fix _ _ _ _ _ t)  = freeVars t
freeVars (IfZ _ c t e)      = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)        = []
freeVars (Let _ x _ v t)    = freeVars v ++ (delete x (freeVars t))
