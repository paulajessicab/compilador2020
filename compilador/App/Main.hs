{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main (main) where


--import Control.Monad
import Control.Monad.Trans
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( stderr, hPutStr )
import Options.Applicative
import Control.Monad.Catch (MonadMask)
import Data.List (nub,  intersperse, isPrefixOf )
import Bytecompile
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Global ( GlEnv(..) )
import Elab ( elab, desugar, desugarDec, elab',desugarDec )
import Eval ( eval )
import TypeChecker ( tc, tcDecl )
import PPrint ( ppTy )
import MonadPCF
import Common ()
import ClosureConversion
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import CEK (evalCEK, valToTerm)
import CIR (runCanon)
import InstSel (codegen)
import LLVM.AST (Module)
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import System.Process (system)
import Data.Maybe(maybeToList)
import Optimizations(constantFolding)

import LLVM.Pretty (ppllvm)

--debug = flip trace

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConversion
          | GenerateLLVM
          | RunLLVM

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

prompt :: String
prompt = "PCF> "

-----------------------
-- Main
-----------------------

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper) ( fullDesc
                                          <> progDesc "Compilador de PCF"
                                          <> header "Compilador de PCF de la materia Compiladores 2020" )

-----------------------
-- Parsers para argumentos
-----------------------

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))

-- | Parser de banderas (modos)
parseMode :: Parser Mode
parseMode =
  flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos")
  <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
  <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM")
  <|> flag' ClosureConversion (long "cc" <> help "Imprimir resultado luego de cc y hoisting")
  <|> flag' GenerateLLVM (long "llvm" <> help "Generar código LLVM")
  <|> flag' RunLLVM (long "runllvm" <> help "Genera y ejecuta código LLVM")
  <|> flag Interactive Interactive ( long "interactive" <> short 'i'
                                                        <> help "Ejecutar en forma interactiva" )

-- | Realiza las operaciones necesarias de acuerdo al modo elegido
go :: (Mode,[FilePath]) -> IO ()
go (Interactive,files) = do
                          runPCF (runInputT defaultSettings (repl files))
                          return ()
go (Typecheck, files) = undefined
go (Bytecompile, files) = do runPCF (bytecompileFiles files)
                             return ()
go (Run,files) = do runPCF (runFiles files)
                    return ()
go (ClosureConversion, files) = do x <- runPCF (closureConvertFiles files)
                                   return  ()
go (GenerateLLVM, files) = do x <- runPCF (genLLVMfromFiles files)
                              return  ()
go (RunLLVM, files) = do x <- runPCF (runLLVMfromFiles files)
                         return  ()

-----------------------
-- Modo interactivo
-----------------------

-- | Función que proporciona el modo interactivo (Read-Eval-Print-Loop)
repl :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
repl args = do
        lift $ catchErrors $ compileFiles args
        s <- lift $ get -- recupera el estado interno de la monada PCF (GlEnv) y lo levanta a InputT
        when (inter s) $ liftIO $ putStrLn -- Si no se modificó la bandera interactiva con el compile intenta el modo interactivo
          (  "Entorno interactivo para PCF0.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop  
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (flip when loop) b

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop
     else
       return (Compile (CompileInteractive x))

-- | Interpreta cada comando disponible
commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Main.Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

-- | Implementa cada comando. Devuelve un booleano indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Main.Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

-- | Mensaje de ayuda del modo interactivo
helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-----------------------
-- Compilacion Eval
-----------------------

-- | Compilacion Eval por linea
compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> do 
                    evalDecl d
                    return ()
      Right t -> do
                    handleTerm t --Todo acomodar elab y elab'
                    return ()

-- | Compilacion Eval para una lista de archivos
-- Toma una lista de nombres de archivos, cambia a modo no interactivo,
-- guarda en el estado el último archivo cargado y los va compilando
compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

compileFile ::  MonadPCF m => String -> m ()
compileFile f = do 
    decls <- fileToDecls f
    case decls of
      Nothing -> printPCF "error"
      Just d  -> mapM_ evalDecl d

-- | Manejo de los terminos (solo para modo interactivo)
handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
          tt <- elab t
          s <- get -- recupero el entorno
          ty <- tc tt (tyEnv s)
          closte <- evalCEK tt
          te <- valToTerm closte 
          printPCF (show te ++ " : " ++ ppTy ty)

-- | Evaluacion de declaraciones
evalDecl ::  MonadPCF m => SDecl STerm -> m ()
evalDecl decl = do
                  nd <- handleDecl decl
                  case nd of
                    Just (Decl p x t) -> do
                      te <- eval t  -- Ver si hay que poner evalCek
                      addDecl (Decl p x te)
                    _ -> return ()

-----------------------
-- Traduccion a Bytecode
-----------------------

-- | Toma una lista de nombres de archivos, los va leyendo
-- | y guardando los archivos con el bytecode correspondiente
bytecompileFiles :: MonadPCF m => [String] -> m ()
bytecompileFiles [] = return ()
bytecompileFiles (x:xs) = do
                            btc <- bytecompileFile x
                            printPCF ("Guardando "++x++"...")
                            liftIO $ catch (bcWrite btc (x ++ ".byte"))
                                  (\e -> do let err = show (e :: IOException)
                                            hPutStr stderr ("No se pudo crear el archivo " ++ x ++ ": " ++ err ++"\n")
                                            return ())
                            bytecompileFiles xs

-- | Toma un nombre de archivo, lo lee y retorna su bytecode
bytecompileFile ::  MonadPCF m => String -> m Bytecode
bytecompileFile f = do
    decls <- fileToDecls f
    case decls of
      Nothing -> do 
                    printPCF "error"
                    return []
      Just d -> do
                  --printPCF "decls: \n"
                  --printPCF $ show d
                  ptm <- sModuleToModule d
                  --printPCF "\nmodule: \n"
                  --printPCF $ show ptm
                  btc <- bytecompileModule ptm
                  --printPCF "\nbytecode: \n"
                  --printPCF $ show btc
                  return btc

-----------------------
-- Run Bytecode
-----------------------

runFiles :: MonadPCF m => [String] -> m ()
runFiles = mapM_ runFile

runFile :: MonadPCF m => String -> m ()
runFile f = do
    printPCF ("Ejecutando "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (bcRead filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return [])
    runBC x

-----------------------
-- Closure Convert
-----------------------

-- | Toma una lista de nombres de archivos, los va leyendo
-- | e "imprime" el resultado de la conversion de clausuras y el hoisting 
closureConvertFiles :: MonadPCF m =>  [String] -> m ()--[[IrDecl]]
closureConvertFiles xs = mapM_ closureConvertFile xs

-- | Toma un nombre de archivo, lo lee y retorna su conversion de clausuras
closureConvertFile :: MonadPCF m => String -> m [IrDecl]
closureConvertFile f = do
    decls <- fileToDecls f
    case decls of
      Nothing -> do 
                    printPCF "error"
                    return []
      Just d -> do
                  --printPCF "SDecls: \n"
                  --printPCF $ show d
                  ptm <- sModuleToModule d
                  --printPCF "\nDecls: \n"
                  --printPCF $ show ptm
                  cc <- runCC ptm
                  --printPCF "\nClosureConversion: \n"
                  --printPCF $ show btc
                  return cc
                  --return []

-----------------------
-- Generate LLVM
-----------------------

genLLVMfromFiles :: MonadPCF m => [String] -> m ()
genLLVMfromFiles xs = mapM_ genLLVMfromFile xs

genLLVMfromFile :: MonadPCF m => String -> m Module
genLLVMfromFile f = do 
                      cc <- closureConvertFile f
                      let opt = constantFolding cc
                      let llvm = codegen (runCanon opt)
                      return llvm --`debug` (L.unpack (ppllvm llvm))
                      
-----------------------
-- Run LLVM
-----------------------
  
runLLVMfromFiles :: MonadPCF m => [String] -> m ()                      
runLLVMfromFiles = mapM_ runLLVMfromFile

runLLVMfromFile :: MonadPCF m => String -> m ()                      
runLLVMfromFile filename = do llvm <- genLLVMfromFile filename
                              let commandline = "clang -Wno-override-module output.ll runtime.c -lgc -o prog"
                              liftIO $ TIO.writeFile "output.ll" (L.toStrict (ppllvm llvm)) 
                              liftIO $ system commandline
                              liftIO $ system "./prog"
                              return ()

-----------------------
-- Pretty Print
-----------------------

-- | Hace el pretty print del termino con SS, terminos con nombres y terminos con indices de Bruijin --TODO usar pretty printer
printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    sterm <- parseIO "<interactive>" tm x
    printPCF "STerm:"
    printPCF (show sterm)
    nterm <- desugar sterm
    printPCF "\nNTerm:"
    printPCF (show nterm)
    let ex = elab' nterm
    t  <- case nterm of 
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printPCF "\nTerm:"
    printPCF (show t)

-----------------------
-- Type Checker
-----------------------

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         --printPCF "STerm:"
         --printPCF (show t)
         tt <- elab t 
         --printPCF "Term:"
         --printPCF (show tt)
         s <- get
         --ty <- tc tt (tyEnv s)
         tc tt (tyEnv s)
         return ()
         --printPCF (ppTy ty)


-----------------------
-- Funciones Auxiliares
-----------------------

-- | Toma un nombre de archivo, un parser y el contenido del archivo, ejecuta el parser y devuelve m [SDecl STerm], o falla
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                          Left e  -> throwError (ParseErr e)
                          Right r -> return r

-- | Toma una lista de declaraciones con SS y devuelve un listado de declaraciones
sModuleToModule :: MonadPCF m => [SDecl STerm] -> m [Decl Term] 
sModuleToModule [] = failPCF "No se introdujo ningún archivo"
sModuleToModule [x] = do dx <- handleDecl x
                         return $ maybeToList dx
sModuleToModule (x:xs) = do dx  <- handleDecl x
                            dxs <- sModuleToModule xs
                            return $ (maybeToList dx) ++ dxs

-- | Manejo de declaraciones
handleDecl ::  MonadPCF m => SDecl STerm -> m (Maybe (Decl Term))
handleDecl decl = do
                  nd <- desugarDec decl
                  case nd of
                    Just (Decl p x t) -> do
                      let tt = elab' t
                      tcDecl (Decl p x tt)
                      return $ Just (Decl p x tt)
                    _ -> return Nothing --Nothing es una declaración de sinonimo de tipos

-- | Toma un archivo y recupera una lista de declaraciones
fileToDecls :: MonadPCF m => String -> m (Maybe [SDecl STerm])
fileToDecls f = do
                  printPCF ("Abriendo "++f++"...")
                  let filename = reverse(dropWhile isSpace (reverse f))
                  x <- liftIO $ catch (readFile filename) 
                            (\e -> do let err = show (e :: IOException) 
                                      hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                                      return "")
                  decls <- catchErrors (parseIO filename program x)
                  return decls