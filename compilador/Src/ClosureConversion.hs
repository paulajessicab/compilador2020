module ClosureConversion where 

import Lang 
import Subst
import MonadPCF

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Debug.Trace

debug = flip trace

-- | Tipo para declaraciones globales
data IrDecl =
      IrFun { irDecName :: Name, irDeclArity :: Int, irDeclArgNames :: [Name], irDeclBody :: IrTm }
    | IrVal { irDecName :: Name, irDeclDef :: IrTm }
    deriving Show

-- | Tipo de terminos intermedios luego de la conversion
data IrTm   = IrVar Name
            | IrCall IrTm [IrTm]
            | IrConst Const
            | IrBinaryOp BinaryOp IrTm IrTm
            | IrLet Name IrTm IrTm
            | IrIfZ IrTm IrTm IrTm
            | MkClosure Name [IrTm]
            | IrAccess IrTm Int
            deriving Show

closureConvert :: Term -> StateT Int (Writer [IrDecl]) IrTm --abrir los terminos con open
closureConvert (V p (Free n))          = return $ IrVar n
--closureConvert (V p (Bound i))       = do -- Quiero que todas las funciones terminen en top level => no deberia haber variables bindeadas
--                                            s <- get
closureConvert (Const p c)             = return $ IrConst c
closureConvert (Let p n t0 t1)         = do
                                            ct0 <- closureConvert t0
                                            ct1 <- closureConvert t1
                                            return $ IrLet n ct0 ct1
closureConvert (BinaryOp p op t0 t1)   = do 
                                            ct0 <- closureConvert t0
                                            ct1 <- closureConvert t1
                                            return $ IrBinaryOp op ct0 ct1
closureConvert (IfZ p t0 t1 t2)        = do 
                                            ct0 <- closureConvert t0
                                            ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2
                                            return $ IrIfZ ct0 ct1 ct2
closureConvert (App p f x)             = do
                                            clos <- fresh ""
                                            cf <- closureConvert f
                                            cx <- closureConvert x
                                            return $ IrLet clos cf $ IrCall (IrAccess (IrVar clos) 0) [IrVar clos, cx]
                                            --return $ IrCall (IrAccess (IrVar fn) 0) ([(IrVar fn), cx])
closureConvert fun@(Lam p n ty t)          = do
                                            codef <- fresh ""
                                            varName <- fresh n
                                            ct <- closureConvert (open varName t)
                                            cloName <- fresh "clo"
                                            -- No hay que considerar las variables globales, los terminos siempre son cerrados hasta que abrimos de a uno a la vez
                                            -- ver si el indice siempre es uno, ver si hay que sacar los globales
                                            case fv of 
                                              [] -> tell [IrFun codef 2 [cloName, varName] ct]
                                              [x]  -> tell [IrFun codef 2 [cloName, varName] (IrLet x (IrAccess (IrVar cloName) 1) ct)]
                                            return $ MkClosure codef (IrVar <$> fv)
                                              where fv = freeVars fun
closureConvert (Fix p n0 ty0 n1 ty1 t) = undefined


fresh :: Monad m => String -> StateT Int m Name
fresh n = do
            s <- get
            put (s+1)
            return $ "__" ++ n ++ (show s)

ccDecls :: [Decl Term] -> StateT Int (Writer [IrDecl]) ()
ccDecls [] = return ()
ccDecls ((Decl p n a):xs) = do 
                              cx <- closureConvert a
                              tell [IrVal n cx]
                              ccDecls xs
                              return ()

runCC :: MonadPCF m => [Decl Term] -> m [IrDecl]
runCC dt  = return $ execWriter $ evalStateT (ccDecls dt) 0
