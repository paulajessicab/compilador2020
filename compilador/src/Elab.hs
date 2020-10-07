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

module Elab ( elab, elab_decl, desugar, elab', desugarDec ) where

import Lang
import Subst

-- | 'desugar' remueve el azúcar sintáctico en un término dado.
desugar :: STerm -> NTerm
desugar (SV p v)                   = V p v
desugar (SConst p c)               = Const p c
--v1desugar (SUnaryOp p op t)  = UnaryOp p op (desugar t)
desugar (SUnaryOp p op)            = Lam p "x" NatTy (UnaryOp p op (V p "x"))
--v3desugar (SUnaryOp p op t) = UnaryOp p op (desugar t)
--v3desugar (SUnaryOpNotApp p op) = Lam p "x" NatTy (UnaryOp p op (V p "x"))
desugar (SLam p [b] t)             = Lam p (fst b) (snd b) (desugar t) --todo ver 0
desugar (SLam p (b:bs) t)          = Lam p (fst b) (snd b) $ desugar $ SLam p bs t
desugar (SApp p (SUnaryOp p' op) a) = UnaryOp p op (desugar a)
desugar (SApp p h a)               = App p (desugar h) (desugar a)
desugar (SFix p [f, x] t)          = Fix p (fst f) (snd f) (fst x) (snd x) (desugar t)
desugar (SFix p (f:x:bs) t)        = Fix p (fst f) (snd f) (fst x) (snd x) $ desugar $ SFix p bs t
desugar (SIfZ p c t e)             =  IfZ p (desugar c) (desugar t) (desugar e)
desugar (SLet p v [] ty t t')      = App p (Lam p v ty (desugar t')) (desugar t)
--v1desugar (SLet p v [(x, tx)] ty t t') = desugar $ SLet p v [] (FunTy tx ty) (SLam p x tx t) t'
desugar (SLet p f xs ty t t')      = desugar $ SLet p f [] (foldr (\x -> FunTy (snd x)) ty xs) (SLam p xs t) t'
--TODO Error letrec con 0 args
desugar (SLetRec p f [(x, xty)] ty t t') = desugar $ SLet p f [] (FunTy xty ty) (SFix p [(f, FunTy xty ty), (x, xty)] t) t'
desugar (SLetRec p f (x:xs) ty t t') = desugar $ SLetRec p f [x] (foldr (\x -> FunTy (snd x)) ty xs) (SLam p xs t) t'

--Hacer declaraciones de let y letrec (sin in)
desugarDec :: SDecl STerm -> Decl NTerm
desugarDec (SLetDec p f [] ty t) = Decl p f (desugar t)
desugarDec (SLetDec p f [x] ty t) = desugarDec $ SLetDec p f [] (FunTy (snd x) ty) (SLam p [x] t)
desugarDec (SLetDec p f (x:xs) ty t) = desugarDec $ SLetDec p f [] (foldr (\x -> FunTy (snd x)) ty (x:xs)) (SLam p (x:xs) t)
-- sletrecdec con 0 argumentos
desugarDec (SLetRecDec p f [x] ty t) = desugarDec $ SLetDec p f [] (FunTy (snd x) ty) (SFix p [(f, FunTy (snd x) ty), x] t)
desugarDec (SLetRecDec p f (x:xs) ty t) = desugarDec $ SLetRecDec p f [x] (foldr (\x -> FunTy (snd x)) ty xs) (SLam p xs t)


-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab' :: NTerm -> Term
elab' (V p v)               = V p (Free v)
elab' (Const p c)           = Const p c
elab' (Lam p v ty t)        = Lam p v ty (close v (elab' t))
elab' (App p h a)           = App p (elab' h) (elab' a)
elab' (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' t))
elab' (IfZ p c t e)         = IfZ p (elab' c) (elab' t) (elab' e)
elab' (UnaryOp i o t)       = UnaryOp i o (elab' t)

elab_decl :: SDecl STerm -> Decl Term
elab_decl = (fmap elab').desugarDec

elab :: STerm -> Term
elab = (\x -> elab'(desugar x))