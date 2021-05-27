module ClosureConversion where 

import Lang 
import Subst
import MonadPCF

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Debug.Trace

debug = flip trace
--import MonadPCF

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
--closureConvert (V p (Bound i))       = do
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
                                            --return $ IrLet fn cf $ IrCall (IrAccess (IrVar fn) 0) ([(IrVar fn), cx])
closureConvert (Lam p n ty t)          = undefined--do
                                         --   fName <- fresh ""
                                         --   varName <- fresh n
                                         --   ct <- closureConvert (open varName t)
                                         --   cloName <- fresh "clo"
                                         --    --`debug` ("CC lambda interno "++show t)
                                         --   tell [IrFun fName 2 [cloName, varName] ct]
                                         --   return $ MkClosure fName []--(map IrVar fv) --ver si siempre va vacio
closureConvert (Fix p n0 ty0 n1 ty1 t) = undefined

fresh :: Monad m => String -> StateT Int m Name
fresh n = do
            s <- get
            put (s+1)
            return $ "__" ++ n ++ (show s)

{-ccDecl :: Decl Term -> [IrDecl]
ccDecl (Decl p n a) = do
                        let res = runWriter $ evalStateT (closureConvert a) 0  
                        --let res2 = runWriter res
                        (snd res) ++ [IrVal n (fst res)] -- agregarle los que se pueden generar adentro
-}
--ccDecl :: Decl Term -> StateT Int (Writer [IrDecl]) ()
--ccDecl (Decl p n a) = do 
--                        cx <- closureConvert x
--                        tell [IrVal n cx]
--                        return ()

ccDecls :: [Decl Term] -> StateT Int (Writer [IrDecl]) ()
ccDecls [] = return ()
ccDecls ((Decl p n a):xs) = do 
                              cx <- closureConvert a
                              tell [IrVal n cx]
                              ccDecls xs
                              return ()

--ccDecl [] = return []
--ccDecl (x:xs) = do 
--                  let (d, w) = runWriter $ closureConvert x
--                  let res = w ++ [IrVal n d]
--                  tell $ concat res (ccDecl xs)
  
--runCC :: MonadPCF m => [Decl Term] -> m [IrDecl]
--runCC dt = return $ concat $ map ccDecl dt

runCC :: MonadPCF m => [Decl Term] -> m [IrDecl]
runCC dt  = return $ execWriter $ evalStateT (ccDecls dt) 0
--runCC dt = return $ concat $ map ccDecl dt
--runCC dt = return $ evalStateT ccDecl dt 0 

--por cada declaracion.. imprimimos primero las del writer y despues el valor final del termino
--corremos la monada en el estado 0
