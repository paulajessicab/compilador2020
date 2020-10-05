{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )

-- | AST de Tipos
data Ty = 
      NatTy 
    | FunTy Ty Ty
    deriving (Show,Eq)

type Name = String

data Const = CNat !Int
  deriving Show

data UnaryOp = Succ | Pred
  deriving Show

data STy = 
      SNatTy
    | SFunTy Ty Ty
    | SAliasTy Name

data SDecl a =
    SAliasType { aliasPos :: Pos, aliasName :: Name, aliasTy :: Ty }
  | SLetDec { letDeclPos :: Pos, letDeclName :: Name, letDeclParams :: [(Name, Ty)], letDeclTy :: Ty, letDeclBody :: a }
  | SLetRecDec { letRecDeclPos :: Pos, letRecDeclName :: Name, letRecDeclParams :: [(Name, Ty)], letRecDeclTy :: Ty, letRecDeclBody :: a }
  deriving (Show,Functor)

-- | Shallow AST para soportar azúcar sintáctico
--
data STm info var =
    SV info var
  | SConst info Const
  | SLet info Name [(Name, Ty)] Ty (STm info var) (STm info var)
  | SLetRec info Name [(Name, Ty)] Ty (STm info var) (STm info var)
  | SLam info [(Name, Ty)] (STm info var)
  | SApp info (STm info var) (STm info var)
  | SUnaryOp info UnaryOp (STm info var)
  | SFix info [(Name, Ty)] (STm info var)
  | SIfZ info (STm info var) (STm info var) (STm info var)
  deriving (Show, Functor)

type STerm = STm Pos Name   -- ^ 'STm' tiene 'Name's como variables ligadas y libres, guarda posición

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
  | App info (Tm info var) (Tm info var)
  | UnaryOp info UnaryOp (Tm info var)
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
getInfo (UnaryOp i _ _) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))    = [v]
freeVars (V _ _)           = []
freeVars (Lam _ _ _ t)     = freeVars t
freeVars (App _ l r)       = freeVars l ++ freeVars r
freeVars (UnaryOp _ _ t)   = freeVars t
freeVars (Fix _ _ _ _ _ t) = freeVars t
freeVars (IfZ _ c t e)     = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)       = []
