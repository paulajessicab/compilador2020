{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : CEK
Description : Define la máquina CEK.
License     : GPL-3
Stability   : experimental

Este módulo define la máquina CEK.
C: términos a ejecutar - el Control de la computación
E: entorno que asocia variables a valores para evitar sustituciones
K: continuaciones
La máquina va ejecutando las funciones mutuamente recursivas search y
destroy desde un estado inicial hasta un estado final.
Estado inicial de la forma <t, [], []>
Estado final de la forma <<v,[]>>
-}

module CEK where

import Lang 
import MonadPCF (lookupDecl, MonadPCF, failPosPCF)

type Env = [Val]

data Val = Cons Int | VClos Clos

data Clos = ClosFun Env Name Term | ClosFix Env Name Name Term

data Frame  = KArg Env Term 
            | KClos Clos
            | KIfZ Env Term Term
            | KSucc 
            | KPred

type Kont = [Frame]

-- La única razón por la que necesitamos MonadPCF es para buscar el valor de variables globales que aparezcan en el término.
-- En esos casos, simplemente se expanden y se continúa la evaluación

-- | Fase de búsqueda
-- | Toma un estado <t,env,k> y, analizando el término t, va construyendo la continuación hasta encontrar un valor
search :: MonadPCF m => Term -> Env -> Kont -> m Val
search (UnaryOp _ Pred t) env k = search t env (KPred : k) 
search (UnaryOp _ Succ t) env k = search t env (KSucc : k) 
search (IfZ _ c t e) env k = search c env ((KIfZ env t e) : k)
search (App _ t u) env k = search t env ((KArg env u) : k)
search (V _ (Bound i)) env k = destroy (env!!i) k
search (V p (Free n)) env k = do
                                v <- lookupDecl n
                                case v of 
                                  Just t -> search t env k
                                  Nothing -> failPosPCF p "No se pudo encontrar el nombre dentro de las declaraciones globales" 
search (Const _ (CNat n)) _ k = destroy (Cons n) k
search (Lam _ x _ t) env k = destroy (VClos $ ClosFun env x t) k
search (Fix _ f _ x _ t) env k = destroy (VClos $ ClosFix env f x t) k

-- hacer caso bound (que creo que tengo que sacar el valor del entorno)
-- hacer el caso free (que creo que tengo que buscar el valor global de la mónada)

-- | Fase de reducción
-- | Toma un estado <<v, k>> y, dependiendo de la continuación, opera sobre este valor
destroy :: MonadPCF m => Val -> Kont -> m Val
destroy v [] = return v -- caso base -> estado final
destroy (Cons 0) (KPred:k) = destroy (Cons 0)  k
destroy (Cons n) (KPred:k) = destroy (Cons (n-1)) k
destroy (Cons n) (KSucc:k) = destroy (Cons (n+1)) k
destroy (Cons 0) ((KIfZ env t _):k) = search t env k
destroy (VClos c) ((KArg env t):k) = search t env ((KClos c):k)
destroy v (KClos (ClosFun env _ t):k) = search t (v:env) k
destroy v (KClos(ClosFix env f x t):k) = search t (VClos (ClosFix env f x t):v:env) k

