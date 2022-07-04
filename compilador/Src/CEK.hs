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
import Common ( Pos(NoPos) )
import MonadPCF (liftIO,  MonadState(get), failPosPCF, lookupDecl, MonadPCF, printPCF )
import TypeChecker ( tc )
import Global ( GlEnv(tyEnv) ) 
import Subst ( substN )

type Env = [Val]

data Val = Cons Int | VClos Clos
    deriving Show

data Clos = ClosFun Env Name Term | ClosFix Env Name Ty Name Ty Term
      deriving Show

data Frame  = KArg Env Term 
            | KClos Clos
            | KIfZ Env Term Term
            | KSucc 
            | KPred
            | KLet Env Term
            | KAddR Env Term
            | KAddL Env Int
            | KSubR Env Term
            | KSubL Env Int

type Kont = [Frame]

-- La única razón por la que necesitamos MonadPCF es para buscar el valor de variables globales que aparezcan en el término.
-- En esos casos, simplemente se expanden y se continúa la evaluación

-- | Fase de búsqueda
-- | Toma un estado <t,env,k> y, analizando el término t, va construyendo la continuación hasta encontrar un valor
search :: MonadPCF m => Term -> Env -> Kont -> m Val
search (Let _ x ty v t) env k = search v env ((KLet env t) : k) -- ver
search (IfZ _ c t e) env k = search c env ((KIfZ env t e) : k)
search (BinaryOp _ Add m n) env k = search m env ((KAddR env n) : k)
search (BinaryOp _ Sub m n) env k = search m env ((KSubR env n) : k)
search (App _ t u) env k = search t env ((KArg env u) : k)
search (V _ (Bound i)) env k = destroy (env!!i) k
search (V p (Free n)) env k = do
                                v <- lookupDecl n
                                case v of 
                                  Just t -> search t env k
                                  Nothing -> failPosPCF p "No se pudo encontrar el nombre dentro de las declaraciones globales" 
search (Const _ (CNat n)) _ k = destroy (Cons n) k
search (Lam _ x _ t) env k = destroy (VClos $ ClosFun env x t) k
search kk@(Fix _ f fty x xty t) env k = destroy (VClos $ ClosFix env f fty x xty t) k

-- | Fase de reducción
-- | Toma un estado <<v, k>> y, dependiendo de la continuación, opera sobre este valor
destroy :: MonadPCF m => Val -> Kont -> m Val
destroy v [] = return v -- caso base -> estado final
destroy (Cons 0) (KPred:k) = destroy (Cons 0)  k
destroy (Cons n) (KPred:k) = destroy (Cons (n-1)) k
destroy (Cons n) (KSucc:k) = destroy (Cons (n+1)) k
destroy (Cons 0) ((KIfZ env t _):k) = search t env k
destroy (Cons _) ((KIfZ env _ e):k) = search e env k
destroy (Cons m) ((KAddR env n):k) = search n env ((KAddL env m) : k)
destroy (Cons m) ((KSubR env n):k) = search n env ((KSubL env m) : k)
destroy (Cons n) ((KAddL env m):k) = destroy (Cons (m+n)) k
destroy (Cons n) ((KSubL env m):k) = destroy (Cons (m-n)) k
destroy pp@(VClos c) ((KArg env t):k) = search t env ((KClos c):k)
destroy v (KClos (ClosFun env _ t):k) = search t (v:env) k
destroy v (KClos(ClosFix env f fty x xty t):k) = search t (v:VClos (ClosFix env f fty x xty t):env) k
destroy v ((KLet env t) : k) = search t (v:env) k

-- | Evaluación de un término usando la máquina CEK
evalCEK :: MonadPCF m => Term -> m Val
evalCEK t = search t [] []

-- | Convierte Val a Term
valToTerm :: MonadPCF m => Val -> m Term
valToTerm (Cons n) = return $ Const NoPos (CNat n)
valToTerm (VClos (ClosFun e n t)) = do
                                    liftIO . putStrLn $ show e
                                    s <- get
                                    ty <- tc t (tyEnv s)
                                    case e of
                                      [] -> return $ Lam NoPos n ty t
                                      (s:xs) -> do
                                        re <- mapM valToTerm (s:xs)
                                        return $ Lam NoPos n ty (substN (reverse re) t)
valToTerm (VClos (ClosFix e f fty x xty t)) = do
                                        liftIO . putStrLn $ show e
                                        case e of
                                          [] -> return $ Fix NoPos f fty x xty t
                                          (s:xs) -> do 
                                                re <- mapM valToTerm (s:xs)
                                                return $ Fix NoPos f fty x xty (substN (reverse re) t)

