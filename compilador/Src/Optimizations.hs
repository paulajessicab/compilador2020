{-|
Module      : Optimizations
Description : Provee metodos de optimizacion para el compilador
Copyright   : 
License     : GPL-3
Maintainer  : 
Stability   : experimental

-}
module Optimizations where

import Data.Map
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace

import ClosureConversion(IrDecl, IrTm(IrConst,IrAccess, IrBinaryOp, IrCall, MkClosure, IrIfZ, IrLet, IrUnaryOp, IrVar), IrDecl(IrVal, IrFun))
import Lang(Const(CNat), Name, UnaryOp( Pred, Succ ), BinaryOp(Add, Sub) )

debug = flip trace



-- Constant Propagation: Reemplazo las variables que representan constantes por las constantes en sí
-- Constant Folding: Si todos los términos de una operación son constantes, opero y reemplazo la operación por el resultado
-- Constant Conditions: Si llego a una constante en la condición del ifz, evaluo si es 0 y dejo solo el branch que corresponde.
-- Unreachable code elimination: Saco el código que inalcanzable.
--                               Por ejemplo, si elimino una de las ramas, puede que haya código al que se llamaba solo desde ahí, y ya no es necesario.
-- Inlining: Reemplazo ciertas llamadas a funciones por sus definiciones.


-- Cuándo paro?



 -- Hacer inlining + esto. Tomar como criterio, por ejemplo, cuando deja de tener cambios o una cota máxima.
 -- Criterio para decidir cuándo hacer inlining?

-- Esto vendría a ser similar a Sparse conditional constant propagation?







-- Guarda el mapeo de constantes computadas y el entero que indica si en la iteración pasada se guardó una nueva constante
type ConstantsStorage = (Map Name IrTm, Int)

-- Toma una [IrDecl] y devuelve una [IrDecl]

constantFolding :: [IrDecl] -> [IrDecl]
constantFolding decls = runConstantFolding decls 0 --`debug` (show (decls))

--Para cada elemento de la lista, 
-- * Si es canonVal, me guardo el valor
-- * Si es CanonFun y es una operación aritmética, intento buscar si sus operandos están definidos. Si llego a las constantes, opero (y pongo el canonval?)
runConstantFolding :: [IrDecl] -> Int -> [IrDecl]
runConstantFolding decls n = let (v, s) = runState (constantFolding'  decls) (Data.Map.empty, 0)
                        in if (snd s) > n -- Si guardé una variable nueva en la iteración anterior
                           then runConstantFolding v (snd s)  --`debug` ("Then " ++ show (v)) 
                           else v --`debug` ("Else " ++ show (snd s)) 

constantFolding' :: [IrDecl] -> State ConstantsStorage [IrDecl] 
constantFolding' ds = mapM constantFoldDecl ds


constantFoldDecl :: IrDecl -> State ConstantsStorage IrDecl
constantFoldDecl (IrVal name t) = do t' <- constantFoldTerm name t
                                     return $ IrVal name t'
constantFoldDecl (IrFun name ar args t) = do t' <- constantFoldTerm "" t
                                             return $ IrFun name ar args t'


constantFoldTerm :: Name -> IrTm -> State ConstantsStorage IrTm
constantFoldTerm _ t@(IrVar n) = do 
                                    s <- get
                                    let c = Data.Map.lookup n (fst s)
                                    case c of 
                                        Nothing -> return t
                                        Just (IrConst (CNat c)) -> return $ IrConst (CNat c)
constantFoldTerm "" t@(IrConst (CNat c)) = do return t
constantFoldTerm name t@(IrConst (CNat c)) = do 
                                                s <- get  
                                                put $ (insert name (IrConst (CNat c)) (fst s), (snd s) + 1)
                                                return t
constantFoldTerm _ (IrUnaryOp op t) = do 
                                            t' <- constantFoldTerm "" t
                                            case (op, t') of
                                                (Succ, IrConst (CNat n)) -> return $ IrConst (CNat (n+1))
                                                (Pred, IrConst (CNat 0)) -> return $ IrConst (CNat 0) -- throw error
                                                (Pred, IrConst (CNat n)) -> return $ IrConst (CNat (n-1))
                                                _ -> return $ IrUnaryOp op t'
constantFoldTerm _ (IrCall t ts) = do 
                                        t' <- constantFoldTerm "" t
                                        ts' <- mapM (constantFoldTerm "") ts
                                        return $ IrCall t' ts'
constantFoldTerm _ (IrBinaryOp op a b) = do 
                                            a' <- constantFoldTerm "" a
                                            b' <- constantFoldTerm "" b
                                            case (op, a', b') of
                                                (Add, IrConst (CNat n), IrConst (CNat m))  -> return $ IrConst (CNat (n+m))
                                                (Sub, IrConst (CNat n), IrConst (CNat m))  -> return $ IrConst (CNat (n-m))
                                                _ -> return $ IrBinaryOp op a' b'
constantFoldTerm _ (IrLet n t b) = do 
                                      t' <- constantFoldTerm "" t
                                      b' <- constantFoldTerm "" b
                                      return $ IrLet n t' b' 
constantFoldTerm _ (IrIfZ c a b) = do 
                                      c' <- constantFoldTerm "" c
                                      case c' of
                                            IrConst (CNat 0) -> do a' <- constantFoldTerm "" a
                                                                   return a'
                                            IrConst (CNat _) -> do b' <- constantFoldTerm "" b
                                                                   return b'
                                            _ -> do 
                                                    a' <- constantFoldTerm "" a
                                                    b' <- constantFoldTerm "" b
                                                    return $ IrIfZ c' a' b'
constantFoldTerm _ (MkClosure n ts) = do 
                                        ts' <- mapM (constantFoldTerm "") ts
                                        return $ MkClosure n ts'
constantFoldTerm _ (IrAccess t n) = do  -- VER
                                        t' <- constantFoldTerm "" t
                                        return $ IrAccess t' n