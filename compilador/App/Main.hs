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

import Control.Monad.Trans
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( stderr, hPutStr )
import Options.Applicative
import Control.Monad.Catch (MonadMask)
import Data.List (nub,  intersperse, isPrefixOf)
import Bytecompile
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Global ( GlEnv(..) )
import Elab ( elab, desugar, desugarDec, elab',desugarDec, elabDecl )
import TypeChecker ( tc, tcDecl )
import qualified PPrint ( pp, ppTy, prettifyModule )
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
import Optimizations (optimize)
import LLVM.Pretty (ppllvm)
import System.Exit
import Data.Text (unpack)

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConversion
          | GenerateLLVM
          | PrettyPrint
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

debugFlag :: Bool
debugFlag = False

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
  <|> flag' PrettyPrint (long "prettyprint" <> short 'p' <> help "Imprime el codigo fuente y su optimizacion")
  <|> flag Interactive Interactive ( long "interactive" <> short 'i'
                                                        <> help "Ejecutar en forma interactiva" )

-- | Realiza las operaciones necesarias de acuerdo al modo elegido
go :: (Mode,[FilePath]) -> IO ()
go (Interactive,files)        = do runPCF $ catchErrors $ runInputT defaultSettings (repl files)
                                   return ()
go (Typecheck, files)         = do runPCF $ catchErrors $ typeCheckFiles files
                                   return ()
go (Bytecompile, files)       = do runPCF $ catchErrors $ bytecompileFiles files
                                   return ()
go (Run,files)                = do runPCF $ catchErrors $ runFiles files
                                   return ()
go (ClosureConversion, files) = do runPCF $ catchErrors $ closureConvertFiles True files
                                   return ()
go (GenerateLLVM, files)      = do runPCF $ catchErrors $ genLLVMfromFiles files
                                   return ()
go (PrettyPrint, files)       = do runPCF $ catchErrors $ printFiles files
                                   return ()
go (RunLLVM, files)           = do runPCF $ catchErrors $ runLLVMfromFiles files
                                   return ()

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
       Cmd [":load"]        "<file>"  (Compile . CompileFile) "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Main.Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

-- | Implementa cada comando. Devuelve un booleano indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit         ->  return False
       Noop         ->  return True
       Help         ->  printPCF (helpTxt commands) >> return True
       Browse       ->  do printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                           return True
       Compile c    ->  do
                            case c of
                              CompileInteractive e -> compilePhrase e
                              CompileFile f        -> put (s {lfile=f}) >> compileFile f
                            return True
       Main.Print e -> printPhrase e >> return True
       Type e       -> typeCheckPhrase e >> return True

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
      Left d  -> evalDecl d >> return ()
      Right t -> handleTerm t >> return ()

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
    decls <- readFilePCF f
    mapM_ evalDecl decls

-- | Manejo de los terminos (solo para modo interactivo)
handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
                  tt <- elab t
                  s <- get -- recupero el entorno
                  ty <- tc tt (tyEnv s)
                  closte <- evalCEK tt
                  te <- valToTerm closte 
                  return ()
                  printPCF (PPrint.pp te ++ " : " ++ PPrint.ppTy ty)

-- | Manejo de declaraciones
handleDecl ::  MonadPCF m => SDecl STerm -> m (Maybe (Decl Term))
handleDecl decl = do
                      nd <- desugarDec decl
                      case nd of
                        Just d -> do
                          e <- elabDecl d
                          tcDecl e 
                          return $ Just e
                        _ -> return Nothing --Nothing es una declaración de sinonimo de tipos

-- | Evaluacion de declaraciones
evalDecl ::  MonadPCF m => SDecl STerm -> m ()
evalDecl decl = do
                  nd <- handleDecl decl
                  case nd of
                    Just (Decl p x t) -> do
                      v <- evalCEK t
                      te <- valToTerm v
                      addDecl (Decl p x te)
                    _ -> return ()

-----------------------
-- Traduccion a Bytecode
-----------------------

-- | Toma una lista de nombres de archivos, los va leyendo
-- | y guardando los archivos con el bytecode correspondiente
bytecompileFiles :: MonadPCF m => [String] -> m ()
bytecompileFiles [] = return ()
bytecompileFiles (f:fs) = do
                            btc <- handleFile True debugFlag f >>= bytecompileModule
                            condPrint debugFlag $ "Guardando "++f++"... \n"
                            liftIO $ catch (bcWrite btc (f ++ ".byte"))
                                  (\e -> do let err = show (e :: IOException)
                                            hPutStr stderr ("No se pudo crear el archivo " ++ f ++ ": " ++ err ++"\n")
                                            return ())
                            bytecompileFiles fs
-----------------------
-- Run Bytecode
-----------------------

runFiles :: MonadPCF m => [String] -> m ()
runFiles = mapM_ runFile

runFile :: MonadPCF m => String -> m ()
runFile f = do
    condPrint debugFlag $ "Ejecutando "++f++"... \n"
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (bcRead filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return [])
    liftIO $ runPCF $ catchErrors $ runBC x
    return ()
    
    

-----------------------
-- Closure Convert
-----------------------

-- | Toma una lista de nombres de archivos, los va leyendo
-- | e "imprime" el resultado de la conversion de clausuras y el hoisting 
closureConvertFiles :: MonadPCF m => Bool -> [String] -> m [[IrDecl]]
closureConvertFiles pp xs = mapM (closureConvertFile pp) xs

closureConvertFile :: MonadPCF m => Bool -> String -> m [IrDecl]
closureConvertFile pp file = do
                            cc <- handleFile True debugFlag file >>= runCC
                            condPrint pp "\x1b[36m > Resultado de CC + Hoisting:\n\x1b[0m"
                            condPrint pp (show cc)
                            return cc

-----------------------
-- Generate LLVM
-----------------------

genLLVMfromFiles :: MonadPCF m => [String] -> m ()
genLLVMfromFiles = mapM_ genLLVMfromFile

genLLVMfromFile :: MonadPCF m => String -> m Module
genLLVMfromFile f = do 
                      cc <- closureConvertFile False f
                      let canon = runCanon cc
                      condPrint debugFlag $ "\x1b[36m > CC + Canon: \n\x1b[0m"
                      condPrint debugFlag $ show canon
                      let llvm = codegen canon
                      condPrint debugFlag $ "\x1b[36m > LLVM code: \n\x1b[0m"
                      condPrint debugFlag $ unpack $ L.toStrict $ ppllvm llvm
                      return llvm               
                      
-----------------------
-- Run LLVM
-----------------------
  
runLLVMfromFiles :: MonadPCF m => [String] -> m ()                      
runLLVMfromFiles = mapM_ runLLVMfromFile

runLLVMfromFile :: MonadPCF m => String -> m ()                      
runLLVMfromFile filename = do mllvm <- catchErrors $ genLLVMfromFile filename
                              case mllvm of
                                Nothing -> liftIO $ exitWith (ExitFailure 1)
                                Just llvm -> do
                                                let commandline = "clang -Wno-override-module output.ll runtime.c -lgc -o prog"
                                                liftIO $ TIO.writeFile "output.ll" (L.toStrict (ppllvm llvm)) 
                                                liftIO $ system commandline
                                                liftIO $ system "./prog"
                                                return ()

-----------------------
-- Pretty Print
-----------------------

-- | Hace el pretty print del termino con SS, terminos con nombres y terminos con indices de Bruijin
printPhrase :: MonadPCF m => String -> m ()
printPhrase x =
  do
    sterm <- parseIO "<interactive>" tm x
    printPCF "\x1b[36m STerm:\x1b[0m"
    printPCF (show sterm)
    nterm <- desugar sterm
    printPCF "\n\x1b[36m NTerm:\x1b[0m"
    printPCF (show nterm)
    let ex = elab' nterm
    t  <- case nterm of
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printPCF "\n\x1b[36m Term:\x1b[0m"
    printPCF (PPrint.pp t)

printFiles :: MonadPCF m => [String] -> m ()
printFiles = mapM_ (handleFile True True)

-----------------------
-- Type Checker
-----------------------

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (PPrint.ppTy ty)
         return ()

-- | Typechecking para archivos
typeCheckFiles :: MonadPCF m => [String] -> m ()
typeCheckFiles fs = do e <- catchErrors $ mapM (handleFile False False) fs
                       case e of
                          Nothing -> failPCF ">> Error de tipado..."
                          _       -> printPCF (">> Typecheck realizado correctamente")

-----------------------
-- Funciones Auxiliares
-----------------------

-- | Toma el archivo y deja las declaraciones listas para el tipo de compilacion elegido
handleFile :: MonadPCF m => Bool -> Bool -> String -> m [Decl Term]
handleFile opt pp f = do 
    sdecls <- readFilePCF f
    condPrint debugFlag "\x1b[36m > Declaraciones con Syntactic Sugar:\x1b[0m \n"
    condPrint debugFlag $ show sdecls
    desugared <- mapM desugarDec sdecls
    decls <- mapM elabDecl $ concatMap maybeToList desugared
    mapM tcDecl decls
    condPrint pp "\x1b[36m > Declaraciones:\x1b[0m \n"
    condPrint pp $ PPrint.prettifyModule decls
    condPrint debugFlag $ "\n" ++ show decls
    case opt of  
      True  -> do 
                mapM_ addDecl decls -- Agrego las declaraciones para usarlas en el optimizador
                optdecl <- optimize decls
                condPrint pp "\x1b[36m > Resultado de la optimizacion:\x1b[0m \n"
                condPrint pp $ PPrint.prettifyModule optdecl
                condPrint debugFlag $ "\n" ++ show optdecl
                return optdecl
      _ -> return decls

-- | Toma un archivo y recupera una lista de declaraciones con syntactic sugar
readFilePCF :: MonadPCF m => String -> m [SDecl STerm]
readFilePCF f = do
                  condPrint debugFlag $ "Abriendo "++f++"... \n"
                  let filename = reverse(dropWhile isSpace (reverse f))
                  x <- liftIO $ catch (readFile filename) 
                            (\e -> do let err = show (e :: IOException) 
                                      hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                                      return "")
                  decls <- parseIO filename program x
                  return decls

-- | Toma un nombre de archivo, un parser y el contenido del archivo, ejecuta el parser y devuelve m [SDecl STerm], o falla
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                          Left e  -> throwError (ParseErr e)
                          Right r -> return r

condPrint :: MonadPCF m => Bool -> String -> m ()                  
condPrint True s = printPCF $ s
condPrint False _ = return ()