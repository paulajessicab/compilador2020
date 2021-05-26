{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, bytecompileModule, runBC, bcWrite, bcRead)
 where

import Lang 
import Subst
import MonadPCF

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )
import TypeChecker
import Debug.Trace

debug = flip trace

type Opcode = Int
type Bytecode = [Int]

type Module = [Decl Term]


newtype Bytecode32 = BC { un32 :: [Word32] }

type EnvBVM = [Val]
type StackBVM = [Val]
data Val = I Int | Fun EnvBVM Bytecode | RA EnvBVM Bytecode deriving Show

getValEnv :: MonadPCF m => Val -> m EnvBVM
getValEnv (I n) = failPCF "error"
getValEnv (Fun e _) = return e
getValEnv (RA e _) = return e

getValBC :: MonadPCF m => Val -> m Bytecode
getValBC (I n) = failPCF "error"
getValBC (Fun _ c) = return c
getValBC (RA _ c) = return c

getValInt :: MonadPCF m => Val -> m Int
getValInt (I n) = return n
getValInt _ = failPCF "error"

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs  --putWord32le: Write a Word32 in little endian format
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...
 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.
 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern SUCC     = 6
pattern PRED     = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14
pattern ADD      = 15
pattern SUB      = 16
pattern TAILCALL = 17

{-C(\t) = FUNCTION(C(t); RETURN)
Para serializar el FUNCTION(e) necesito guardar la longitud del
bytecode de e, para poder saltar.

[Bytecode Function, length(e), bytecode de e]
-}



bc :: MonadPCF m => Term -> m Bytecode
bc (Const _ (CNat n)) = return [CONST,n]
{-bc (UnaryOp _ unop e) = do
                          bce <- bc e
                          case unop of
                            Succ -> return $ bce ++ [SUCC]   
                            _    -> return $ bce ++ [PRED]
                            -}
-- Escribo la suma y la resta con notacion polaca inversa
bc (BinaryOp _ op a b) = do 
                            bca <- bc a
                            bcb <- bc b
                            case op of
                              Add -> return $ bca ++ bcb ++ [ADD]
                              _   -> return $ bca ++ bcb ++ [SUB]
bc (V _ (Bound i))    = return [ACCESS, i]
bc (V l (Free n))     = failPCF "No estamos trabajando con variables globales"
bc (Let _ _ e1 e2) = do  
                          bce2 <- bc e2
                          bce1 <- bc e1
                          return $ bce1 ++ [SHIFT] ++ bce2 ++ [DROP]
bc (App _ f e)        = do 
                          bcf <- bc f
                          bce <- bc e
                          return $ bcf ++ bce ++ [CALL]
bc (Lam _ _ _ t)      = do 
                          bct <- tailbc t
                          let bct' = bct ++ [RETURN]
                          return $ [FUNCTION, length bct'] ++ bct'
{-
--Antes de tailcall
bc (Lam _ _ _ t)      = do 
                          bct <- bc t
                          let bct' = bct ++ [RETURN]
                          return $ [FUNCTION, length bct'] ++ bct'
-}
bc (Fix _ _ _ _ _ e)  = do 
                        bce <- bc e
                        let bce' = bce ++ [RETURN]
                        return $ [FUNCTION, length bce'] ++ bce' ++ [FIX] -- ver si va +1 en el largo
bc (IfZ _ c t0 t1)    = do --Primero introduzco el nro de la condición, luego las longitudes del bc de los terminos para poder saltar y, por último, los términos
                          bcc <- bc c
                          bct0 <- bc t0
                          bct1 <- bc t1
                          return $ bcc ++ [IFZ, (length bct0) + 2] ++ bct0 ++ [JUMP, (length bct1)] ++ bct1

tailbc :: MonadPCF m => Term -> m Bytecode
tailbc (App _ f e)      = do 
                          bcf <- bc f
                          bce <- bc e
                          return $ bcf ++ bce ++ [TAILCALL]
tailbc (IfZ _ c t0 t1)  = do
                          bcc <- bc c
                          tbct0 <- tailbc t0
                          tbct1 <- tailbc t1
                          return $ bcc ++ [IFZ, (length tbct0) + 2] ++ tbct0 ++ [JUMP, (length tbct1)] ++ tbct1
tailbc (Let _ _ e1 e2)  = do  
                          bce1 <- bc e1
                          tbce2 <- tailbc e2
                          return $ bce1 ++ [SHIFT] ++ tbce2
tailbc t                = do 
                          bct <- bc t
                          return $ bct ++ [RETURN]
                           


bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule m = do minn <- bcModuleInner m
                         ctp <- bc minn --`debug` ("bytecompileModule " ++ show minn)
                         return $ ctp ++ [PRINT, STOP]

bcModuleInner :: MonadPCF m => Module -> m Term
bcModuleInner [] = failPCF "No code to load"
bcModuleInner [Decl p v e] = return $ Let p v e (close v e)
bcModuleInner ((Decl p v e):xs) = do mxs <- bcModuleInner xs
                                     return $ Let p v e (close v mxs)

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadPCF m => Bytecode -> m ()
runBC c = runBC' c [] []

runBC' :: MonadPCF m => Bytecode -> EnvBVM -> StackBVM -> m ()
runBC' (CONST : n : cs) e s = do runBC' cs e ((I n):s) --`debug` ("code CONST "++show cs)
runBC' (SUCC : cs) e (n:s) = do
                                case n of 
                                  I m -> do runBC' cs e ((I (m+1)):s) --`debug` ("code SUCC "++show cs)
                                  _   -> failPCF "errorrunBC"
runBC' (PRED : cs) e (n:s) = do
                                case n of 
                                  I m -> do runBC' cs e ((I (m-1)):s) --`debug` ("code PRED "++show cs)
                                  _   -> failPCF "errorrunBC"
-- La suma y la resta estan en notacion polaca inversa
-- Tomo los dos ultimos elementos del stack, los saca, los suma/resta
-- y pushea el resultado. Tener en cuenta para la resta que se sacan al reves
runBC' (ADD : cs) e (a:b:s) = do case (a,b) of
                                  (I n, I m) -> do runBC' cs e $ I (m + n):s --`debug` ("code ADD "++show cs)
                                  _ -> failPCF "Error al ejecutar la operacion ADD"
runBC' (SUB : cs) e (a:b:s) = do case (a,b) of
                                  (I n, I m) -> do runBC' cs e $ I (m - n):s --`debug` ("code SUB "++show cs)
                                  _ -> failPCF "Error al ejecutar la operacion SUB"
runBC' (ACCESS : i : cs) e s = do runBC' cs e ((e!!i):s) --`debug` ("code ACCESS "++show cs)
runBC' (CALL : cs) e (v:f:s) = do cf <- getValBC f
                                  ef <- getValEnv f
                                  runBC' cf (v:ef) ((RA e cs):s) --`debug` ("pre CALL "++show cs++ " post CALL " ++ show cf)
runBC' (TAILCALL : cs) e (v:f:s) = do cf <- getValBC f
                                      ef <- getValEnv f
                                      runBC' cf (v:ef) s --`debug` ("pre TAILCALL "++show cs++ " post TAILCALL " ++ show cf)
runBC' (FUNCTION : lenE : cs) e s = do runBC' c e ((Fun e cf):s) --`debug` ("code FUN "++show c)
                                       where c = drop lenE cs
                                             cf = take lenE cs
runBC' (RETURN : _) _ (v:raf:s) = do craf <- getValBC raf
                                     eraf <- getValEnv raf
                                     runBC' craf eraf (v:s) --`debug` ("code RETURN "++show craf)
runBC' (PRINT : cs) e (n:s) = do
                                case n of
                                  I m -> printPCF (show m) 
                                  _   -> failPCF "ErrorrunBC"
                                runBC' cs e (n:s) --`debug` ("code PRINT "++show cs)
runBC' (FIX : cs) e (clos:s) = do
                                  cf <- getValBC clos
                                  let efix = (Fun efix cf) : e
                                  runBC' cs e ((Fun efix cf) : s) --`debug` ("code FIX "++show cs)
runBC' (SHIFT : cs) e (v:s) = do runBC' cs (v:e) s --`debug` ("code SHIFT "++show cs)
runBC' (DROP : cs) (_:e) s = do runBC' cs e s --`debug` ("code DROP "++show cs)
runBC' (STOP : cs) _ _ = return ()
runBC' (JUMP : c : cs) e s = runBC' (drop c cs) e s
runBC' (IFZ : lenT0' : cs) e (n : s) = do        
                                          i <- getValInt n
                                          case i of
                                            0 -> do runBC' cs e s --`debug` ("code IFZ " ++ show cs) --ejecuto t0 y salto t1 con el JUMP del final
                                            _ -> do runBC' (drop lenT0' cs) e s  --`debug` ("code IFZ " ++ show (drop lenT0' cs))  --Salto t0', ejecuto a partir de t1
runBC' _ _ _ = do failPCF "Failed to run code"