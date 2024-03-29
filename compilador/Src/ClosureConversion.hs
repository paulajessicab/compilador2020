module ClosureConversion where 

import Lang 
import Subst
import MonadPCF

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Debug.Trace
import Data.List (isPrefixOf)

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

-- | Realiza la conversion de clausuras y el hoisting de las funciones
closureConvert :: Term -> StateT Int (Writer [IrDecl]) IrTm
closureConvert (V p (Free n))          = return $ IrVar n
-- Caso Bound: Todas las funciones deben terminar siendo top level, es decir, que no van a tener variables bindeadas
closureConvert (V p (Bound i))         = error ">> Error Closure Conversion: Se encontro bound variable."
closureConvert (Const p c)             = return $ IrConst c
closureConvert (Let p n ty t0 t1)      = do
                                            ct0 <- closureConvert t0
                                            ct1 <- closureConvert (open n t1)
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
closureConvert (Lam p n ty t)          = do
                                              codef <- fresh ""
                                              varName <- fresh n
                                              ct <- closureConvert (open varName t)
                                              cloName <- fresh "clo"
                                              -- No hay que considerar las variables globales, los terminos siempre son cerrados hasta que abrimos de a uno a la vez
                                              innerTerm <- closureRefs cloName ct fv
                                              tell [IrFun codef 2 [cloName, varName] innerTerm]
                                              return $ MkClosure codef (IrVar <$> fv) 
                                                where fv = filter (isPrefixOf "__") (freeVars t)
closureConvert (Fix p n0 ty0 n1 ty1 t) = do 
                                            codef <- fresh ""
                                            varName <- fresh n0
                                            n1f <- fresh n1
                                            ct <- closureConvert (openN [varName, n1f] t)
                                            cloName <- fresh "clo"
                                            tell [IrFun codef 2 [cloName, n1f] (IrLet varName (IrVar cloName) ct)] 
                                            return $ MkClosure codef (IrVar <$> fv) 
                                                where fv = filter (isPrefixOf "__") (freeVars t)

-- | Toma el nombre de una clausura, un termino y una lista ordenada de nombres de variables libres
-- | Retorna el IrTm que contiene el termino y las referencias a esas variables dentro de la clausura
closureRefs :: Monad m => Name -> IrTm -> [Name] -> m IrTm
closureRefs cloName t fv = return $ fst $ foldr (fc cloName) (t, length fv) fv
                              where fc cloName n (term, i) = (IrLet n (IrAccess (IrVar cloName) i) term, i-1)

-- | Toma un string y genera un nombre nuevo a partir de una monada de estados
fresh :: Monad m => String -> StateT Int m Name
fresh n = do
            s <- get
            put (s+1)
            return $ "__" ++ n ++ (show s)

-- | Toma una lista de declaraciones y aplica la conversion de clausura a cada una
ccDecls :: [Decl Term] -> StateT Int (Writer [IrDecl]) ()
ccDecls [] = return ()
ccDecls ((Decl p n a):xs) = do 
                              cx <- closureConvert a
                              tell [IrVal n cx]
                              ccDecls xs
                              return ()

-- | Ejecuta la conversion de clausuras a partir del estado 0
runCC :: MonadPCF m => [Decl Term] -> m [IrDecl]
runCC dt  = return $ execWriter $ evalStateT (ccDecls dt) 0
