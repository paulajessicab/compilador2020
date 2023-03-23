{-|
Module      : Optimizations
Description : Provee la implementacion de las optimizaciones para el compilador
Copyright   : 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Optimizaciones implementadas:

- Constant folding 
Si los operandos son conocidos en tiempo de compilacion, realizar la operacion de forma estatica

- Simplificacion algebraica
Tomar ventaja de reglas  de simplificacion matematica.

- Constant propagation
Si se sabe que el valor de una variable es constante, reemplazar el uso de esa variable por la constante.

- Inlining (Inline expansion)
Reemplazar la llamada a una funcion por el cuerpo de la funcion (reescribiendo argumentos para que sean variables locales)

Code explosion:
Hacer inline expansion hace crecer el tamaño del código. Si se hace de forma indiscriminada, el tamaño del programa explota.
Heurísticas para controlar inlining:
1. Expandir solo las llamadas a funciones que se ejecutan muy frecuentemente
2. Expandir funciones con cuerpos muy pequeños
3. Expandir funciones que se llaman solo una vez. De este modo, luego se puede hacer una pasada eliminando el código muerto resultante.

Se tomó la opción 3.

-}
module Optimizations (optimize) where

import Data.Map (Map, empty, lookup, adjust, insertWith, delete, member)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace
import MonadPCF (MonadPCF, failPCF, lookupDecl, printPCF, addDecl)
import Lang
import Data.List(foldl')
import Subst(subst, openN, substN)

debug = flip trace

optimizationLimit :: Int
optimizationLimit = 5

-- | Optimize
-- | Aplica una serie de optimizaciones optimizationLimit veces. 
optimize :: MonadPCF m => [Decl Term] -> m [Decl Term] 
optimize decls = optimize' optimizationLimit decls

-- | 
optimize' :: MonadPCF m => Int -> [Decl Term] -> m [Decl Term]
optimize' 0 decls = return decls
optimize' 1 decls = inline decls >>= deadCodeElimination
optimize' n decls = optimize' 1 decls >>= optimize' (n-1) --`debug` ("\n OPT:\n "++ show decls)


-- | Inline Optimization
inline :: MonadPCF m => [Decl Term] -> m [Decl Term]
inline decls = mapM (\d -> inlineDecl d (Map.filter (== 1) (countFunctionRefs decls))) decls --`debug` ("\n once Declared:\n "++ show (countFunctionRefs decls) )

inlineDecl :: MonadPCF m => Decl Term -> Map Name Int -> m (Decl Term)
inlineDecl (Decl p n t) onceRef = do inlined <- inlineT t onceRef
                                     addDecl $ Decl p n inlined -- Actualiza las declaraciones globales
                                     return $ Decl p n inlined

inlineT :: MonadPCF m => Term  -> Map Name Int -> m (Term)
inlineT fv@(V _ (Free n)) onceRef = do def <- lookupDecl n 
                                       case def of 
                                          Just c@(Const _ _)    -> return c  --`debug` ("\n lookup:\n "++ show n ++ ": " ++ show def ) -- Constant propagation
                                          Just t@(V _ (Free _)) -> return t  --`debug` ("\n lookup:\n "++ show n ++ ": " ++ show def )-- Copy propagation 
                                          Just t@(V _ _) -> return t
                                          Just t@(Lam _ _ _ m) | Data.Map.member n onceRef == True -> return t --`debug` ("\n lookup:\n "++ show n ++ ": " ++ show def ) --Esta tengo que validar cuanto se llama
                                          Just t@(Fix _ _ _ _ _ m) | Data.Map.member n onceRef == True -> return t
                                          _  -> return fv --`debug` ("\n lookup:\n "++ show n ++ ": " ++ show def )
inlineT (App i fn@(Lam _ x tx t) b) onceRef = do case b of
                                                        (Const _ _) -> return $ subst b t --`debug` ("\n Subs Constant:\n "++ show b ++ " EN " ++ show t)
                                                        (V _ (Free _)) -> return $ subst b t --`debug` ("\n Subs Variable:\n "++ show b ++ " EN " ++ show t )
                                                        _ -> return $ Let i "__opt_dummy_name" tx b t --`debug` ("\n Subs Constant:\n "++ show b ++ " EN " ++ show t )
inlineT (Lam i v tv t) onceRef = do ti <- inlineT t onceRef
                                    return $ Lam i v tv ti
inlineT (Let i n ty a b) onceRef = do ai <- inlineT a onceRef
                                      bi <- inlineT b onceRef
                                      return $ Let i n ty ai bi
inlineT (App i a b) onceRef = do ai <- inlineT a onceRef
                                 bi <- inlineT b onceRef
                                 return (App i ai bi)
inlineT (BinaryOp i op a b) onceRef = do  case (op, a, b) of
                                                -- Constant folding
                                                (Add, Const _ (CNat a'), Const _ (CNat b')) -> return $ Const i (CNat (a' + b'))
                                                (Sub, Const _ (CNat a'), Const _ (CNat b')) -> if b' > a'
                                                                                               then return $ Const i (CNat 0)
                                                                                               else return $ Const i (CNat (a' - b'))
                                                -- Simplificacion algebraica
                                                (Add, Const _ (CNat 0), b) -> inlineT b onceRef
                                                (Add, a, Const _ (CNat 0)) -> inlineT a onceRef
                                                (Sub, a, Const _ (CNat 0)) -> inlineT a onceRef
                                                (Sub, Const p (CNat 0), b) -> inlineT (Const p (CNat 0)) onceRef
                                                _ ->  do ai <- inlineT a onceRef
                                                         bi <- inlineT b onceRef
                                                         return $ BinaryOp i op ai bi
inlineT (Fix i n ty n2 ty2 t) onceRef = do ti <- inlineT t onceRef -- Ver si no hay que hacer un tratamiento especial
                                           return $ Fix i n ty n2 ty2 ti
inlineT (IfZ i c a b) onceRef = do  ci <- inlineT c onceRef
                                    ai <- inlineT a onceRef
                                    bi <- inlineT b onceRef
                                    return $ IfZ i ci ai bi
inlineT t _ = return t -- bound and const

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




