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
import Subst(subst)

debug = flip trace

optimizationLimit = 3

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
inline decls = inlineN names decls
               where names = Map.filter (== 1) $ ctFunctionCalls decls

inlineN :: MonadPCF m => Map Name Int -> [Decl Term] -> m [Decl Term]
inlineN names decls = mapM (inlineD names) decls

inlineD :: MonadPCF m => Map Name Int -> Decl Term -> m (Decl Term)
inlineD names (Decl p n t) = do inlined <- inlineT names t
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
inlineT names (App i (Lam _ x tx t) b) = do case b of
                                              (Const _ _) -> return $ subst b t
                                              (V _ _) -> return $ subst b t
                                              _ -> return $ Let i "_prueba" tx b t --TODO cambiar nombre
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
inlineT names (UnaryOp i op t) = do ti <- inlineT names t
                                    return $ UnaryOp i op ti
inlineT names (Fix i n ty n2 ty2 t) = do ti <- inlineT names t -- Ver si no hay que hacer un tratamiento especial
                                         return $ Fix i n ty n2 ty2 ti
inlineT names (IfZ i c a b) = do  ci <- inlineT names c
                                  ai <- inlineT names a
                                  bi <- inlineT names b
                                  return $ IfZ i ci ai bi

-- | Dead code elimination 
deadCodeElimination :: MonadPCF m => [Decl Term] ->  m [Decl Term]
deadCodeElimination decls = deadCodeElimination' (Map.filter (== 0) names) decls  --`debug` ("--"++show names ++ "Decls: " ++ show decls)
                            where (Decl _ n _) = last decls
                                  refs = ctReferences decls
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
-- | Se ignora la última declaracion
ctFunctionCalls :: [Decl Term] -> Map Name Int
ctFunctionCalls decls = foldl' ctFnCallsDecl Data.Map.empty decls
    
ctFnCallsDecl :: Map Name Int -> Decl Term -> Map Name Int
ctFnCallsDecl onceApplied (Decl p n t) = countUsage True (Map.insertWith (+) n 0 onceApplied) t

-- | Count decls
ctReferences decls = foldl' ctReferencesDecl Data.Map.empty decls
                        
ctReferencesDecl names (Decl p n t) = countUsage False (Map.insertWith (+) n 0 names) t

-- | Cuenta la cantidad de veces que se desreferencia una variable
-- | justFnCalls = true => Solo cuenta desreferencias de variables de tipo funcion
countUsage :: Bool -> Map Name Int -> Term -> Map Name Int
countUsage justFnCalls refs (App _ (V i (Free n)) b) = let c = Data.Map.adjust (1+) n refs
                                                       in countUsage justFnCalls c b --`debug` ("fncall: " ++ show refs)
countUsage justFnCalls refs (V i (Free n)) = if justFnCalls
                                             then refs --`debug` ("Sin cambios variable: " ++ show refs)
                                             else Data.Map.adjust (1+) n refs --`debug` ("variable: " ++ show refs)
countUsage justFnCalls refs (Lam i v tv t) = countUsage justFnCalls refs t
countUsage justFnCalls refs (Let i n ty a b) = countUsage justFnCalls (countUsage justFnCalls refs a) b
countUsage justFnCalls refs (App i a b) = countUsage justFnCalls (countUsage justFnCalls refs a) b
countUsage justFnCalls refs (BinaryOp _ _ a b) = countUsage justFnCalls (countUsage justFnCalls refs a) b
countUsage justFnCalls refs (UnaryOp _ _ a) = countUsage justFnCalls refs a
countUsage justFnCalls refs (Fix _ _ _ _ _ t) = countUsage justFnCalls refs t
countUsage justFnCalls refs (IfZ _ c a b) = let cc = countUsage justFnCalls refs c
                                                ca = countUsage justFnCalls cc a
                                            in countUsage justFnCalls ca b
countUsage justFnCalls refs t = refs --`debug` ("Sin cambios: " ++ show refs) --const + bound