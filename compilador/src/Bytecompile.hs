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
bc (App _ f e)        = do 
                          bcf <- bc f
                          bce <- bc e
                          return $ bcf ++ bce ++ [CALL]
bc (Lam _ _ _ t)      = do 
                          bct <- bc t
                          let bct' = bct ++ [RETURN]
                          return $ [FUNCTION, length bct'] ++ bct'
bc (Fix _ _ _ _ _ e)  = do 
                        bce <- bc e
                        let bce' = bce ++ [RETURN]
                        return $ [FUNCTION, length bce'] ++ bce' ++ [FIX]
{-bc (IfZ _ c t1 t2)    = do 
                          bcc <- bc c
                          bct1 <- bc t1
                          bct2 <- bc t2
                          return $ [IFZ, length bcc] ++ bcc ++ [length bct1] ++ bct1 ++ [length bct2] ++ bct2
-} -- Creo que la idea es que si es evaluar C(0), si resulta 0 saltar a bct1 y sino a bct2

bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule mod = error "implementame"

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
runBC c = error "implementame"