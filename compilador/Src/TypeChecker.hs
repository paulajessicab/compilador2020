{-|
Module      : Typechecker
Description : Chequeo de tipos de términos y declaraciones.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module TypeChecker (
   tc,
   tcDecl,
   tyEnv
   ) where

import Lang
import Global
import MonadPCF
import PPrint
import Subst
import Debug.Trace

debug = flip trace

-- | 'tc' chequea y devuelve el tipo de un término 
-- | Si el término no está bien tipado, lanza un error
-- | usando la interfaz de las mónadas @MonadPCF@.
tc :: MonadPCF m => Term         -- ^ término a chequear
                 -> [(Name,Ty)]  -- ^ entorno de tipado
                 -> m Ty         -- ^ tipo del término
tc (V p (Bound _)) _ = failPosPCF p ">> Typecheck Error: No deberia haber variables Bound"
tc (V p (Free n)) bs = case lookup n bs of
                           Nothing -> failPosPCF p $ "Variable no declarada "++ppName n
                           Just ty -> return ty
tc (Const _ (CNat n)) _ = return NatTy
tc (IfZ p c t t') bs = do
       tyc  <- tc c bs
       expect NatTy tyc c
       tyt  <- tc t bs
       tyt' <- tc t' bs
       expect tyt tyt' t'
tc (Let p x ty v t) bs = do
    tyv <- tc v bs
    expect ty tyv v
    tyt <- tc (open x t) ((x,ty):bs)
    return tyt 
tc (Lam p v ty t) bs = do
         ty' <- tc (open v t) ((v,ty):bs)
         return (FunTy ty ty')
tc (App p t u) bs = do
         tyt <- tc t bs
         (dom,cod) <- domCod t tyt
         tyu <- tc u bs
         expect dom tyu u
         return cod
tc term@(Fix p f fty x xty t) bs = do
         (dom, cod) <- domCod (V p (Free f)) fty
         when (dom /= xty) $ do
            typeError term "El tipo del argumento de un fixpoint debe coincidir con el \
                            \dominio del tipo de la función"
         let t' = openN [f, x] t
         ty' <- tc t' ((x,xty):(f,fty):bs)
         expect cod ty' t'
         return fty
tc (BinaryOp p u t t') bs = do 
                        ty <- tc t bs
                        expect NatTy ty t
                        ty' <- tc t' bs
                        expect NatTy ty' t'
                        return NatTy

-- | @'typeError' t s@ lanza un error de tipo para el término @t@ 
typeError :: MonadPCF m => Term   -- ^ término que se está chequeando  
                        -> String -- ^ mensaje de error
                        -> m a
typeError t s = failPosPCF (getInfo t) $ ">> Typecheck Error: Error de tipo en "++pp t++"\n"++s
 
-- | 'expect' chequea que el tipo esperado sea igual al que se obtuvo
-- | y lanza un error si no lo es.
expect :: MonadPCF m => Ty    -- ^ tipo esperado
                     -> Ty    -- ^ tipo que se obtuvo
                     -> Term  -- ^ término que se está chequeando
                     -> m Ty
expect ty ty' t = if ty == ty' then return ty 
                               else typeError t $ 
              "Tipo esperado: "++ ppTy ty
            ++"\npero se obtuvo: "++ ppTy ty'

-- | 'domCod chequea que un tipo sea función
-- | devuelve un par con el tipo del dominio y el codominio de la función
domCod :: MonadPCF m => Term -> Ty -> m (Ty, Ty)
domCod t (FunTy d c) = return (d, c)
domCod t ty = typeError t $ "Se esperaba un tipo función, pero se obtuvo: " ++ ppTy ty

-- | 'tcDecl' chequea el tipo de una declaración y la agrega al entorno de tipado de declaraciones globales
-- | No se valida que la declaracion de tipos de SDecl sea correcta porque se pierde al hacer desugar. 
-- | Para validarlo debería cambiar todos los usos de Decl para preservar el tipo internamente.
tcDecl :: MonadPCF m  => Decl Term -> m ()
tcDecl (Decl p n t) = do
    --chequear si el nombre ya está declarado
    mty <- lookupTy n
    case mty of
        Nothing -> do  --no está declarado 
                  s <- get
                  ty <- tc t (tyEnv s)                 
                  addTy n ty
        Just _  -> failPosPCF p $ ">> Typecheck Error: "++ n ++" ya está declarado"
