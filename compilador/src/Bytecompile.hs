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

type Opcode = Int
type Bytecode = [Int]
type Module = [Decl Term]


newtype Bytecode32 = BC { un32 :: [Word32] }

type EnvBVM = [Val]
type StackBVM = [Val]
data Val = I Int | Fun EnvBVM Bytecode | RA EnvBVM Bytecode

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

{-C(\t) = FUNCTION(C(t); RETURN)
Para serializar el FUNCTION(e) necesito guardar la longitud del
bytecode de e, para poder saltar.

[Bytecode Function, length(e), bytecode de e]
-}



bc :: MonadPCF m => Term -> m Bytecode
bc (Const _ (CNat n)) = return [CONST,n]
bc (UnaryOp _ unop e) = do
                          bce <- bc e
                          case unop of
                            Succ -> return $ bce ++ [SUCC]   
                            _    -> return $ bce ++ [PRED]
bc (V _ (Bound i))    = return [ACCESS, i]
bc (V l (Free n))     = do 
                          mterm <- lookupDecl n
                          case mterm of
                            Just t -> bc $ t
                            Nothing -> failPosPCF l ("No se pudo recuperar la declaracion " ++ n)
bc (App _ (Lam _ _ _ e2) e1) = do  -- Caso app especial: Implemento optimización let bindings internos. Como estoy trabajando en Term (PCF0), tomo el desugar.
                                bce1 <- bc e1
                                bce2 <- bc e2
                                return $ bce1 ++ [SHIFT] ++ bce2 ++ [DROP]
bc (App _ f e)        = do 
                          bcf <- bc f
                          bce <- bc e
                          return $ bcf ++ bce ++ [CALL]
bc (Lam _ _ _ t)      = do --
                          bct <- bc t
                          let bct' = bct ++ [RETURN]
                          return $ [FUNCTION, length bct'] ++ bct'
bc (Fix _ _ _ _ _ e)  = do 
                        bce <- bc e
                        let bce' = bce ++ [RETURN]
                        return $ [FUNCTION, length bce'] ++ bce' ++ [FIX]
bc (IfZ _ c t0 t1)    = do 
                          bcc <- bc c
                          bct0 <- bc t0
                          bct1 <- bc t1
                          return $ [CONST] ++ bcc ++ [IFZ, length bct0, length bct1] ++ bct0 ++ bct1
                          --Primero introduzco el nro de la condición, luego las longitudes del bc de los terminos para poder saltar y, por último, los términos

bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule m = do ctp <- bcModuleInner m
                         return $ ctp ++ [PRINT, STOP]

bcModuleInner :: MonadPCF m => Module -> m Bytecode --type Module = [Decl Term]
bcModuleInner [] = failPCF ("Módulo vacío")
bcModuleInner [(Decl _ v e)]    = do 
                                    bce <- bc e
                                    tv  <- lookupDecl v
                                    case tv of
                                      Nothing -> failPCF "Error"
                                      Just t  -> do
                                                  bcv <- bc t
                                                  return $ bce ++ [SHIFT] ++ bcv ++ [DROP]
bcModuleInner ((Decl _ v e):xs) = do 
                                        bce <- bc e
                                        bcmod <- bcModuleInner xs
                                        return $ bce ++ [SHIFT] ++ bcmod ++ [DROP]


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
runBC' (CONST : n : cs) e s = runBC' cs e ((I n):s)
runBC' (SUCC : cs) e (n:s) = do
                                case n of 
                                  I m -> runBC' cs e ((I (m+1)):s)
                                  _   -> failPCF "error"
runBC' (PRED : cs) e (n:s) = do
                                case n of 
                                  I m -> runBC' cs e ((I (m-1)):s)
                                  _   -> failPCF "error"
runBC' (ACCESS : i : cs) e (n:s) = runBC' cs e ((e!!i):s)
runBC' (CALL : cs) e (v:f:s) = do cf <- getValBC f
                                  ef <- getValEnv f
                                  runBC' cf (v:ef) ((RA e cs):s)
runBC' (FUNCTION : lenE : cs) e s = runBC' c e ((Fun e cf):s)
                                    where c = drop lenE cs
                                          cf = take lenE cs
runBC' (RETURN : cs) _ (v:raf:s) = do craf <- getValBC raf
                                      eraf <- getValEnv raf
                                      runBC' craf eraf (v:s)
runBC' (PRINT : cs) e (n:s) = do
                                case n of
                                  I m -> printPCF (show m) 
                                  _   -> failPCF "Error"
                                runBC' cs e (n:s)
runBC' (FIX : cs) e (clos:s) = do
                                  cf <- getValBC clos
                                  let efix = (Fun efix cf) : e
                                  runBC' cs e ((Fun efix cf) : s)
runBC' (SHIFT : cs) e (v:s) = runBC' cs (v:e) s
runBC' (DROP : cs) (v:e) s = runBC' cs e s
runBC' (STOP : cs) _ _ = return ()
runBC' (JUMP : cs) e (n : s) = do
                                i <- getValInt n 
                                runBC' (drop i cs) e s
runBC' (IFZ : lenT0 : lenT1 : cs) e (n : s) = do 
                                                i <- getValInt n
                                                case i of 
                                                  0 -> runBC' ((take lenT0 cs) ++ (drop (lenT0 + lenT1) cs)) e s --ejecuto t0 y salto t1
                                                  _ -> runBC' (drop lenT0 cs) e s --Salto t0, ejecuto a partir de t1