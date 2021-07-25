
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CIR where

import ClosureConversion (IrDecl, IrTm, IrDecl(IrVal,IrFun), IrTm(IrVar, IrConst, IrBinaryOp, IrIfZ, MkClosure, IrCall, IrLet, IrAccess))
import Lang ( BinaryOp, Name, UnaryOp(Print), Const(CNat) )
import Data.List (intercalate, sortBy)
import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Debug.Trace
import Data.Function (on)
import qualified Data.Map as Map

debug = flip trace


-- | CanonProg types
newtype Reg = Temp String
  deriving Show

data Val = R Reg | C Int | G Name
  deriving Show

type Loc = String

data Inst =
    Assign Reg Expr
  | Store Name Expr
  deriving Show

data Expr =
    BinOp BinaryOp Val Val
  | UnOp UnaryOp Val
  | Phi [(Loc, Val)]
  | Call Val [Val]
  | MkClosure Loc [Val]
  | V Val
  | Access Val Int
  deriving Show

data Terminator =
    Jump Loc
  | CondJump Cond Loc Loc
  | Return Val
  deriving Show

data Cond =
    Eq Val Val
  deriving Show

type BasicBlock = (Loc, [Inst], Terminator)
type Blocks = [BasicBlock]

type CanonFun = (String, [String], [BasicBlock])
type CanonVal = String -- Sólo el nombre, tipo puntero siempre
newtype CanonProg = CanonProg [Either CanonFun CanonVal]

-- | Pretty printing functions
print :: (Blocks, [Inst], Val) -> String
print (bs, is, v) =
  concatMap printBlock bs ++ show is ++ "\n" ++ show v ++ "\n"

printBlock :: BasicBlock -> String
printBlock (loc, is, t) =
  loc ++ ":\n" ++
  concatMap (\i -> "  " ++ show i ++ "\n") is ++
  show t ++ "\n"

instance Show CanonProg where
  show (CanonProg prog) = concatMap pr1 prog where
    pr1 (Left (f, args, blocks)) =
      f ++ "(" ++ intercalate ", " args ++ ") {\n"
        ++ concatMap printBlock blocks ++ "}\n\n"
    pr1 (Right v) =
      "declare " ++ v ++ "\n\n"

runCanon :: [IrDecl] -> CanonProg
runCanon xs = CanonProg ([Left ("pcfmain", [], mainblocks)] ++ a)
              where (a, w) = runWriter $ runCanon' xs             
                    mainblocks = createBlocks $ execCodegen $ do
                                                              entry <- addBlock entryBlockName
                                                              setBlock entry
                                                              mapM_ storeGlobal w
                                                              addTerminator $ Return $ C 0

storeGlobal :: (Name, IrTm) -> Codegen ()
storeGlobal (name, t) = do
                          ct <- canon t
                          addInstruction $ Store name ct

-- Toma la lista de globales, las declaraciones y genera canon
runCanon' :: [IrDecl] -> Writer [(Name, IrTm)] [Either CanonFun CanonVal]
runCanon' [] = return []
runCanon' (x:xs) = do
                    c <- runCanonValOrFun x
                    cs <- runCanon' xs
                    return $ c : cs 

{-
Tal vez tenga que hacer algo como 

let comp = do mapM_ cgInst insts
                cgTerm term
  (t, is) <- runWriterT comp
-}

runCanonValOrFun :: IrDecl -> Writer [(Name, IrTm)] (Either CanonFun CanonVal)
runCanonValOrFun (IrVal name def) = do 
                                 tell [(name, def)]
                                 return (Right name)
runCanonValOrFun (IrFun name ar args body) = return (Left (name, args, blocks))
                                          where blocks = createBlocks $ execCodegen $ do
                                                                                  entry <- addBlock entryBlockName
                                                                                  setBlock entry
                                                                                  retReg <- freshRegName
                                                                                  bc <- canon body
                                                                                  addInstruction $ Assign (Temp retReg) bc
                                                                                  addTerminator $ Return $ R $ Temp retReg




-- | Monada que guarda el estado de la generacion de codigo
-- | En blocks se guarda un diccionario que mapea los nombres de los bloques con su estado
data CodeGenState = CodeGenState { currentBlock :: Name, blocks :: Map.Map Name BlockState, nameGen :: Int}

data BlockState = BlockState { index :: Int, instructions :: [Inst], terminator :: Maybe Terminator}

newtype Codegen a = Codegen { runCodegen :: State CodeGenState a }
    deriving (Functor, Applicative, Monad, MonadState CodeGenState )

-- Necesito un symbol table?


-- | Operadores de la monada de estado
execCodegen :: Codegen a -> CodeGenState
execCodegen m = execState (runCodegen m) emptyState

emptyState :: CodeGenState
emptyState = CodeGenState entryBlockName Map.empty 0

entryBlockName :: Name
entryBlockName = "entry"

createBlocks :: CodeGenState -> [BasicBlock]
createBlocks m = map makeBlock $ (sortBy (compare `on` (index . snd))) $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (name, (BlockState _ i t)) = (name, (reverse i), (makeTerm t))
      where
        makeTerm (Just x) = x
        makeTerm (Nothing) = error $ "El bloque no tiene terminador"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: Name -> Codegen Name
addBlock name = do 
                  bs <- gets blocks
                  nix <- gets nameGen
                  let idx = Map.size bs
                      newBlock = emptyBlock idx
                      newName =  name ++ (show nix)
                  modify $ \s -> s { blocks = Map.insert newName newBlock bs }
                  return newName

setBlock :: Name -> Codegen Name
setBlock name = do
  modify $ \s -> s { currentBlock = name }
  return name

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

getBlock :: Codegen Name
getBlock = gets currentBlock

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

freshRegName :: Codegen Name
freshRegName = do 
                  idx <- gets nameGen
                  modify $ \s -> s { nameGen = idx + 1 }
                  return $ "_r" ++ (show idx)

addInstruction :: Inst -> Codegen ()
addInstruction inst = do 
                        bloqueActual <- current
                        let insts = instructions bloqueActual
                        modifyBlock (bloqueActual {instructions = (inst : insts) }) --reemplaza las instrucciones del bloque actual con las mismas instrucciones pero le agrega la nueva al principio

addTerminator :: Terminator -> Codegen Terminator
addTerminator trm = do
  blk <- current
  modifyBlock (blk { terminator = Just trm })
  return trm

-- | Translations

canon :: IrTm -> Codegen Expr
canon (IrVar n)           = return $ V $ G $ n --ver
canon (IrCall t xs)       = do
                              rt <- freshRegName
                              ct <- canon t
                              addInstruction $ Assign (Temp rt) ct
                              cxs <- mapM canonList xs
                              return $ Call (R (Temp rt)) cxs
                                where canonList x = do
                                            r <- freshRegName
                                            c <- canon x
                                            addInstruction $ Assign (Temp r) c
                                            return $ R $ Temp r
canon (IrConst (CNat i))  = return $ V $ C $ i
canon (IrBinaryOp op a b) = do
                              ra <- freshRegName
                              rb <- freshRegName
                              ca <- canon a
                              cb <- canon b
                              let va = Temp ra
                              let vb = Temp rb
                              addInstruction $ Assign va ca
                              addInstruction $ Assign vb cb
                              return $ BinOp op (R va) (R vb)
canon (IrLet name v t)    = do
                              cv <- canon v
                              addInstruction $ Store name cv --store + assign
                              canon t 
canon (IrIfZ cond t e)    = do
                              rc <- freshRegName
                              rt <- freshRegName
                              re <- freshRegName
                              thenBName <- addBlock "then"
                              elseBName <- addBlock "else"
                              contBName <- addBlock "cont"
                              cc <- canon cond
                              addInstruction $ Assign (Temp rc) cc
                              addTerminator $ CondJump (Eq (C 0) (R (Temp rc))) thenBName elseBName
                              setBlock thenBName
                              ct <- canon t
                              addInstruction $ Assign (Temp rt) ct
                              addTerminator $ Jump contBName
                              setBlock elseBName
                              ce <- canon e
                              addInstruction $ Assign (Temp re) ce
                              addTerminator $ Jump contBName
                              setBlock contBName
                              return $ Phi [(thenBName, (R (Temp rt))), (elseBName, (R (Temp re)))]
canon (ClosureConversion.MkClosure name ts) = do
                                                cts <- mapM canonList ts
                                                return $ CIR.MkClosure name cts
                                                    where canonList x = do
                                                                r <- freshRegName
                                                                c <- canon x
                                                                addInstruction $ Assign (Temp r) c
                                                                return $ R $ Temp r
canon (IrAccess t i)      = do
                              rt <- freshRegName
                              ct <- canon t
                              addInstruction $ Assign (Temp rt) ct
                              return $ Access (R (Temp rt)) i