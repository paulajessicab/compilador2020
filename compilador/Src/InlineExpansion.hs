{-|
Module      : InlineExpansion
Description : Provee la implementacion de la optimizacion Inline Expansion para el compilador
Copyright   : 
License     : GPL-3
Maintainer  : 
Stability   : experimental

-}
module InlineExpansion where

import Data.Map (Map, empty, lookup, adjust, insertWith)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace
import MonadPCF (MonadPCF, failPCF, lookupDecl, printPCF)
--import ClosureConversion(IrDecl, IrTm(IrConst,IrAccess, IrBinaryOp, IrCall, MkClosure, IrIfZ, IrLet, IrUnaryOp, IrVar), IrDecl(IrVal, IrFun))
--import Lang(Const(CNat), Name, UnaryOp( Pred, Succ ), BinaryOp(Add, Sub) )
import Lang
import Data.List(foldl')

debug = flip trace

optimizationLimit = 3

-- Inlining: Reemplazo ciertas llamadas a funciones por sus definiciones.
-- Heurísitica elegida:
--  Se reemplazaran las funciones que solo tengan una llamada (para poder hacer dead code elimination posteriormente)
--  El proceso termina cuando ya no quedan funciones que son llamadas desde un único lugar (ver)

-- Pasos
-- 1- Contar llamadas a funciones (countFunctionCalls)
-- 2- Buscar declaraciones de funciones con una sola llamada (Map.filter (==1) y buscar definiciones en MkClosure - tal vez necesito el nombre de la clausura)
-- 3- Aplicar inlining (con los cuidados necesarios)
-- 4?- Aplicar dead code elimination (ver)

-- Inlining
-- Se aplica optimizationLimit veces
-- Es una mezcla de inlining, constant folding (y aplicacion?)
-- Se reemplaza cuando está en el diccionario con llamadas únicas o cuando es una constante


-- Guarda el mapeo de funciones que fueron llamadas una única vez y el entero que indica si se aplicaron expansiones
--type CalledOnceStorage = (Map Name IrTm, Int)

-- Toma una [IrDecl] y devuelve una [IrDecl]

--inlineExpansion :: [IrDecl] -> [IrDecl]
--inlineExpansion decls = runInlineExpansion decls 0 --`debug` (show (decls))





-- Cuenta la cantidadd de veces que se llamó a cada funcion -- Ver si hacerla para todas las funciones o solo para las que tengan llamadas y si el orden importa
-- Toma una lista de declaraciones
-- Devuelve un diccionario con los nombres de las funciones como clave y la cantidad de llamadas como valor 



countFunctionCalls :: [Decl Term] -> Map Name Int
countFunctionCalls decls = let t = foldl' countFnCallsDecl Data.Map.empty decls
                           in t


countFnCallsDecl :: Map Name Int -> Decl Term -> Map Name Int
countFnCallsDecl onceApplied (Decl p n (Lam i v tv t)) = countFnCallsTerm (Map.insertWith (+) n 0 onceApplied) t -- Si encuentro una función la agrego al mapeo, tengo que aplicarlo a t primero
countFnCallsDecl onceApplied (Decl p n t) = countFnCallsTerm onceApplied t

countFnCallsTerm :: Map Name Int -> Term -> Map Name Int
-- cuento solo las llamadas a funciones
countFnCallsTerm onceApplied (App _ (V i (Free n)) b) = let c = Data.Map.adjust (1+) n onceApplied
                                                        in countFnCallsTerm c b
countFnCallsTerm onceApplied (Lam i v tv t) = countFnCallsTerm onceApplied t
countFnCallsTerm onceApplied (Let i n ty a b) = countFnCallsTerm (countFnCallsTerm onceApplied a) b
countFnCallsTerm onceApplied (App i a b) = countFnCallsTerm (countFnCallsTerm onceApplied a) b
countFnCallsTerm onceApplied (BinaryOp _ _ a b) = countFnCallsTerm (countFnCallsTerm onceApplied a) b
countFnCallsTerm onceApplied (UnaryOp _ _ a) = countFnCallsTerm onceApplied a
countFnCallsTerm onceApplied (Fix _ _ _ _ _ t) = countFnCallsTerm onceApplied t
countFnCallsTerm onceApplied (IfZ _ c a b) = let cc = countFnCallsTerm onceApplied c
                                                 ca = countFnCallsTerm cc a
                                             in countFnCallsTerm ca b
countFnCallsTerm onceApplied t = onceApplied --constante

-- Toma una lista de declaraciones y una lista de nombres de funciones a reemplazar
-- Devuelve una lista de declaraciones a las que se le aplico el inlining
inline :: MonadPCF m => [Decl Term] -> Map Name Int -> m [Decl Term]
inline decls names = inline' optimizationLimit names decls


inline' :: MonadPCF m => Int -> Map Name Int -> [Decl Term] -> m [Decl Term]
inline' 1 names decls = inlineN names decls
inline' n names decls = inlineN names decls >>= inline' (n-1) names
                          --do d <- inlineN names decls
                          -- inline' (n-1) names d

inlineN :: MonadPCF m => Map Name Int -> [Decl Term] -> m [Decl Term]
inlineN names decls = mapM (inlineD names) decls

inlineD :: MonadPCF m => Map Name Int -> Decl Term -> m (Decl Term)
inlineD names (Decl p n t) = do inlined <- inlineT names t
                                return $ Decl p n inlined
-- Toma una declaracion y una lista de nombres de funciones a reemplazar
-- Devuelve una declaracion a las que se le aplico el inlining

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












{-
countFunctionCalls :: [IrDecl] -> Map Name Int
countFunctionCalls ds = Map.unionsWith (+) $ map (\x -> countFnCallsDecl x Map.empty) ds-- unir los diccionarios



countFnCallsDecl :: IrDecl -> Map Name Int -> Map Name Int
countFnCallsDecl (IrVal name (MkClosure cloName _)) fs = Map.insertWith (+) name 0 fs--let c = Data.Map.lookup name fs
                                                         --case c of 
                                                           -- Nothing -> Map.insertWith (+) name 0 fs
                                                           -- Just i -> fs-- ver entornos de closure- Seguramente no los necesite, pero ver porque son tm --Error? Ya definida
countFnCallsDecl (IrVal name t) fs = countFnCallsTerm t fs
countFnCallsDecl (IrFun name ar args t) fs = countFnCallsTerm t fs


countFnCallsTerm :: IrTm -> Map Name Int -> Map Name Int
countFnCallsTerm (IrLet _ (IrVar fnName) (IrCall (IrAccess (IrVar _) 0) params)) fns = Map.insertWith (+) fnName 1 fns
countFnCallsTerm (IrVar n) fns = fns
countFnCallsTerm (IrCall body params) fns = let fns' = Map.unionsWith (+) $ map (\x -> countFnCallsTerm x Map.empty) params-- TODO acomodar
                                            in Map.unionWith (+) fns fns'
countFnCallsTerm (IrConst c) fns = fns
countFnCallsTerm (IrUnaryOp _ t) fns = countFnCallsTerm t fns
countFnCallsTerm (IrBinaryOp _ t1 t2) fns = let fns' = countFnCallsTerm t1 fns
                                            in countFnCallsTerm t2 fns'
countFnCallsTerm (IrLet n t1 t2) fns = let fns' = countFnCallsTerm t1 fns
                                       in countFnCallsTerm t2 fns'
countFnCallsTerm (IrIfZ c tt tf) fns =  let fnsc = countFnCallsTerm c fns
                                            fnst = countFnCallsTerm tt fnsc
                                        in countFnCallsTerm tf fnst
countFnCallsTerm (MkClosure n params) fns = let f = Map.unionsWith (+) $ map (\x -> countFnCallsTerm x Map.empty) params -- TODO así está bien?
                                            in Map.unionWith (+) f fns
                                            --foldl (\acc t -> countFnCallsTerm t acc) fns params
countFnCallsTerm (IrAccess t i) fns = countFnCallsTerm t fns -- TODO acomodar?
-}