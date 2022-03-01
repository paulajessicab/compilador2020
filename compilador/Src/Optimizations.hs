{-|
Module      : Optimizations
Description : Provee la implementacion de las optimizaciones para el compilador
Copyright   : 
License     : GPL-3
Maintainer  : 
Stability   : experimental

-}
module Optimizations (optimize) where

import Data.Map (Map, empty, lookup, adjust, insertWith, delete)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace
import MonadPCF (MonadPCF, failPCF, lookupDecl, printPCF)
import Lang
import Data.List(foldl')
import Subst(subst, openN, substN)

debug = flip trace

optimizationLimit = 10

-- | Optimize
-- | Aplica una serie de optimizaciones optimizationLimit veces. 
optimize :: MonadPCF m => [Decl Term] -> m [Decl Term] 
optimize decls = optimize' optimizationLimit decls

optimize' :: MonadPCF m => Int -> [Decl Term] -> m [Decl Term]
optimize' 0 decls = return decls
optimize' 1 decls = inline decls >>= deadCodeElimination
optimize' n decls = optimize' 1 decls >>= optimize' (n-1)


-- | Inline Optimization
-- | 
inline :: MonadPCF m => [Decl Term] -> m [Decl Term]
inline decls = mapM (inlineDecl names) decls --inlineN names decls
               where names = Map.filter (== 1) $ countFunctionRefs decls

inlineDecl :: MonadPCF m => Map Name Int -> Decl Term -> m (Decl Term)
inlineDecl names (Decl p n t) = do  inlined <- inlineT names t
                                    return $ Decl p n inlined

-- Si la variable libre aparece en el map, reemplazo
-- Si es una constante o una variable -> reemplazo..
-- Para el caso del App, trabajar en la sustitución
inlineT :: MonadPCF m => Map Name Int -> Term -> m (Term)
inlineT names fv@(V _ (Free n)) = case Data.Map.lookup n names of 
                                      Just 1 ->  do d <- lookupDecl n
                                                    case d of 
                                                        Just t -> return t
                                                        Nothing -> failPCF "No encontrado"
                                      Nothing -> do d <- lookupDecl n 
                                                    case d of 
                                                      Just (Const i (CNat c)) -> return (Const i (CNat c))
                                                      Just t -> return fv
                                                      Nothing -> failPCF "No encontrado"
inlineT names v@(V _ (Bound _)) = return v
inlineT names c@(Const _ _) = return c
inlineT names (App i fn@(Lam _ x tx t) b) = do case b of -- Todo acomodar esto, tengo que reemplazar el bound 0 por la variable?
                                                  (Const _ _) -> return $ subst b t
                                                  (V _ _) -> return $ subst b t
                                                  _ -> return $ Let i "__opt_dummy_name" tx b t
inlineT names (Lam i v tv t) = do ti <- inlineT names t
                                  return $ Lam i v tv ti
inlineT names (Let i n ty a b) = do ai <- inlineT names a
                                    bi <- inlineT names b
                                    return $ Let i n ty ai bi
inlineT names (App i a b) = do ai <- inlineT names a
                               bi <- inlineT names b
                               return (App i ai bi)
inlineT names (BinaryOp i op a b) = do ai <- inlineT names a
                                       bi <- inlineT names b
                                       return $ BinaryOp i op ai bi
inlineT names (Fix i n ty n2 ty2 t) = do ti <- inlineT names t -- Ver si no hay que hacer un tratamiento especial
                                         return $ Fix i n ty n2 ty2 ti
inlineT names (IfZ i c a b) = do  ci <- inlineT names c
                                  ai <- inlineT names a
                                  bi <- inlineT names b
                                  return $ IfZ i ci ai bi

-- | Obtiene nombres de variables frescas - Se podría mejorar (ver si hay que cortar en algun nro)
--fresh :: MonadPCF m => m Name
--fresh = fresh' 0
--
--fresh' :: MonadPCF m => Int -> m Name
--fresh' n    = do d <- lookupDecl name
--                 case d of
--                    Just _  -> fresh' (n+1)
--                    Nothing -> return name
--                 where name = "__opt" ++ (show n)

-- | Dead code elimination 
deadCodeElimination :: MonadPCF m => [Decl Term] ->  m [Decl Term]
deadCodeElimination decls = deadCodeElimination' (Map.filter (== 0) names) decls
                            where (Decl _ n _) = last decls
                                  refs = countRefs decls
                                  names = Data.Map.delete n refs -- el ultimo elemento es el retorno

deadCodeElimination' :: MonadPCF m => Map Name Int -> [Decl Term] ->  m [Decl Term]
deadCodeElimination' names [] = return []
deadCodeElimination' names d@[(Decl _ n _)] = case Map.lookup n names of
                                                Just i -> return []
                                                Nothing -> return d
deadCodeElimination' names (d:ds) = do
                                      e <- deadCodeElimination' names [d]
                                      es <- deadCodeElimination' names ds
                                      return $ e ++ es  

-- | Cuenta la cantidad de veces que se desreferencia una variable de tipo funcion
countFunctionRefs :: [Decl Term] -> Map Name Int
countFunctionRefs decls = foldl' countFunctionRefsDecl Data.Map.empty decls
    
countFunctionRefsDecl :: Map Name Int -> Decl Term -> Map Name Int
countFunctionRefsDecl onceApplied (Decl p n t) = countRefsTerm True (Map.insertWith (+) n 0 onceApplied) t

-- | Count decls
countRefs :: [Decl Term] -> Map Name Int
countRefs decls = foldl' ctReferencesDecl Data.Map.empty decls
                  where ctReferencesDecl names (Decl p n t) = countRefsTerm False (Map.insertWith (+) n 0 names) t

-- | Cuenta la cantidad de veces que se desreferencia una variable
-- | justFnCalls = true => Solo cuenta desreferencias de variables de tipo funcion
countRefsTerm :: Bool -> Map Name Int -> Term -> Map Name Int
countRefsTerm justFnCalls refs (App _ (V i (Free n)) b) = let c = Data.Map.adjust (1+) n refs
                                                          in countRefsTerm justFnCalls c b
countRefsTerm justFnCalls refs (V i (Free n)) = if justFnCalls
                                                then refs
                                                else Data.Map.adjust (1+) n refs
countRefsTerm justFnCalls refs (Lam i v tv t) = countRefsTerm justFnCalls refs t
countRefsTerm justFnCalls refs (Let i n ty a b) = countRefsTerm justFnCalls (countRefsTerm justFnCalls refs a) b
countRefsTerm justFnCalls refs (App i a b) = countRefsTerm justFnCalls (countRefsTerm justFnCalls refs a) b
countRefsTerm justFnCalls refs (BinaryOp _ _ a b) = countRefsTerm justFnCalls (countRefsTerm justFnCalls refs a) b
countRefsTerm justFnCalls refs (Fix _ _ _ _ _ t) = countRefsTerm justFnCalls refs t
countRefsTerm justFnCalls refs (IfZ _ c a b) =  let  cc = countRefsTerm justFnCalls refs c
                                                     ca = countRefsTerm justFnCalls cc a
                                                in countRefsTerm justFnCalls ca b
countRefsTerm justFnCalls refs t = refs