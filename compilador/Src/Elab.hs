{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, desugarTy, desugar, elab', desugarDec, elabDecl, elabDeclModule ) where

import Lang
import Subst
import MonadPCF

-- | 'desugar' remueve el azúcar sintáctico en un término dado.
desugar :: MonadPCF m => STerm -> m NTerm
desugar (SV p v)                   = return $ V p v
desugar (SConst p c)               = return $ Const p c
--desugar (SUnaryOp p op)            = return $ Lam p "x" NatTy (UnaryOp p op (V p "x"))
desugar (SUnaryOp p op)            = case op of
                                        Succ -> return $ Lam p "x" NatTy $ Lam p "y" NatTy (BinaryOp p Add (V p "x") (Const p (CNat 1)))
                                        _    -> return $ Lam p "x" NatTy $ Lam p "y" NatTy (BinaryOp p Sub (V p "x") (Const p (CNat 1)))
desugar (SBinaryOp p op)           = return $ Lam p "x" NatTy $ Lam p "y" NatTy (BinaryOp p op (V p "x") (V p "y"))
desugar (SLam p [] _)              = failPosPCF p "Numero de argumentos incorrecto para Lam"
desugar (SLam p [b] t)             = do 
                                    ty <- desugarTy (snd b)
                                    tt <- desugar t
                                    return $ Lam p (fst b) ty tt
desugar (SLam p (b:bs) t)          = do 
                                    ty <- desugarTy $ snd b
                                    term <- desugar $ SLam p bs t
                                    return $ Lam p (fst b) ty term
--desugar (SApp p (SUnaryOp _ op) a)= return $ UnaryOp p op t                                   
desugar (SApp p (SUnaryOp _ op) a)= do   
                                      t <- desugar a
                                      case op of
                                        Succ -> return $ BinaryOp p Add t (Const p (CNat 1))
                                        _    -> return $ BinaryOp p Sub t (Const p (CNat 1))
                                      
desugar (SApp p h a)               = do
                                      dh <- desugar h
                                      da <- desugar a
                                      return $ App p dh da
desugar (SFix p [f, x] t)          = do
                                      tyf <- desugarTy $ snd f
                                      tyx <- desugarTy $ snd x 
                                      dt <- desugar t
                                      return $ Fix p (fst f) tyf (fst x) tyx dt
desugar (SFix p (f:x:bs) t)        = do
                                      tyf <- desugarTy $ snd f
                                      tyx <- desugarTy $ snd x
                                      dt <- desugar $ SFix p bs t
                                      return $ Fix p (fst f) tyf (fst x) tyx dt
desugar (SIfZ p c t e)             =  do
                                      dc <- desugar c
                                      dt <- desugar t
                                      de <- desugar e
                                      return $ IfZ p dc dt de
desugar (SLet p v [] _ t t')      = do
                                     -- dty <- desugarTy ty
                                      dt' <- desugar t'
                                      dt <- desugar t
                                      return $ Let p v dt dt' -- implementacion let-binding interno 
desugar (SLet p f xs ty t t')      = desugar $ SLet p f [] (foldr (\x -> SFunTy (snd x)) ty xs) (SLam p xs t) t'-- Esto creo que queda igual
desugar (SLetRec p _ [] _ _ _ ) = failPosPCF p "Error: LetRec debe tener al menos 1 argumento"
desugar (SLetRec p f [(x, xty)] ty t t') = desugar $ SLet p f [] (SFunTy xty ty) (SFix p [(f, SFunTy xty ty), (x, xty)] t) t'
desugar (SLetRec p f (x:xs) ty t t') = desugar $ SLetRec p f [x] (foldr (\x -> SFunTy (snd x)) ty xs) (SLam p xs t) t'
desugar _ = failPCF "Error en la etapa de desugar"

-- | Quita el syntactic sugar de una declaración
desugarDec :: MonadPCF m => SDecl STerm -> m (Maybe (Decl NTerm))
desugarDec (STypeAlias p n t)           =  do
                                            dt <- desugarTy t
                                            mty <- lookupTy n
                                            case mty of
                                              Nothing -> do
                                                          addSynTy n dt
                                                          return Nothing
                                              Just _  -> failPosPCF p "ya está declarado"
desugarDec (SLetDec p f [] _ t)         = do
                                          dt <- desugar t
                                          return $ Just $ Decl p f dt
desugarDec (SLetDec p f [x] ty t)       = desugarDec $ SLetDec p f [] (SFunTy (snd x) ty) (SLam p [x] t)
desugarDec (SLetDec p f (x:xs) ty t)    = desugarDec $ SLetDec p f [] (foldr (\x -> SFunTy (snd x)) ty (x:xs)) (SLam p (x:xs) t)
desugarDec (SLetRecDec p _ [] _ _)      = failPosPCF p "Let Rec debe tener al menos 1 argumento"
desugarDec (SLetRecDec p f [x] ty t)    = desugarDec $ SLetDec p f [] (SFunTy (snd x) ty) (SFix p [(f, SFunTy (snd x) ty), x] t)
desugarDec (SLetRecDec p f (x:xs) ty t) = desugarDec $ SLetRecDec p f [x] (foldr (\x -> SFunTy (snd x)) ty xs) (SLam p xs t)

-- | Quita el syntactic sugar de un tipo
desugarTy :: MonadPCF m => STy -> m Ty
desugarTy SNatTy = return NatTy
desugarTy (SFunTy x y) = do 
                            tx <- desugarTy x
                            ty <- desugarTy y
                            return $ FunTy tx ty
desugarTy (SAliasTy n) = do 
                            ty <- lookupSynTy n
                            case ty of 
                              Just t -> return t
                              Nothing -> failPCF "Alias de tipo no definido"

-- | 'elab' transforma variables ligadas en índices de de Bruijn en un término dado. 
elab' :: NTerm -> Term
elab' (V p v)               = V p (Free v)
elab' (Const p c)           = Const p c
elab' (Lam p v ty t)        = Lam p v ty (close v (elab' t))
elab' (BinaryOp p op a b)   = BinaryOp p op (elab' a) (elab' b)
elab' (App p h a)           = App p (elab' h) (elab' a)
elab' (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' t))
elab' (IfZ p c t e)         = IfZ p (elab' c) (elab' t) (elab' e)
--elab' (UnaryOp i o t)       = UnaryOp i o (elab' t)
elab' (Let p v e1 e2)       = Let p v (elab' e1) (close v (elab' e2)) 

{-
El original era equivalente a
elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab'

fmap :: (a -> b) -> f a -> fb
Es decir, en este caso sería (Decl NTerm -> Decl Term) -> Decl NTerm -> Decl Term

Ahora desugarDec debería poder devolver: Nothing, Just Decl NTerm o Error
Como tengo que seguir teniendo en cuenta los errores que pueden surgir en desugarDec,
tengo que usar la MonadError
-}
elabDecl :: MonadPCF m => SDecl STerm -> m (Maybe (Decl Term))
elabDecl sd = do 
                nd <- desugarDec sd
                case nd of
                  Just d -> return $ Just $ fmap elab' d
                  Nothing -> return Nothing

elabDeclModule :: MonadPCF m => [SDecl STerm] -> m [Decl Term]
elabDeclModule [] = failPCF "No code to elab"
elabDeclModule [x] = do
                        dx <- elabDecl x
                        case dx of
                          Nothing -> failPCF "Error al armar modulo"
                          Just d -> return [d]  --do --addDecl fmap elab' 
elabDeclModule (x:xs) = do
                          elabx <- elabDeclModule [x]
                          elabxs <- elabDeclModule xs
                          return $ elabx ++ elabxs

elab :: MonadPCF m => STerm -> m Term
elab st = do
            nt <- desugar st
            return $ elab' nt