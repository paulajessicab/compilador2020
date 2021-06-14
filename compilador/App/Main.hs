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
import PPrint ( pp , ppTy )
import MonadPCF
import Common ()
import ClosureConversion
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import CEK (evalCEK, valToTerm)

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConversion

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

-- | Parser de banderas
parseMode :: Parser Mode
parseMode =
  flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos")
  <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
  <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM")
  <|> flag' ClosureConversion (long "cc" <> help "Imprimir resultado luego de cc y hoisting")
  <|> flag Interactive Interactive ( long "interactive" <> short 'i'
                                                        <> help "Ejecutar en forma interactiva" )

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper) ( fullDesc <> progDesc "Compilador de PCF" <> header "Compilador de PCF de la materia Compiladores 2020" )

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

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | Toma una lista de nombres de archivos, cambia a modo no interactivo,
-- | guarda en el estado el último archivo cargado y los va compilando
compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

-- | Toma un nombre de archivo, lo lee, 
compileFile ::  MonadPCF m => String -> m ()
compileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f)) -- Trim
    x <- liftIO $ catch (readFile filename) -- Lectura del archivo, (a IOString)
               (\e -> do let err = show (e :: IOException) -- Cuerpo catch
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x -- Ejecuta el parser de programa (declaraciones)
    mapM_ handleDecl decls

-- | Toma una lista de nombres de archivos, los va leyendo
-- | e "imprime" el resultado de la conversion de clausuras y el hoisting
closureConvertFiles :: MonadPCF m =>  [String] -> m ()--[[IrDecl]]
closureConvertFiles [] = return ()
closureConvertFiles (x:xs) = do
                            cc <- closureConvertFile x
                            closureConvertFiles xs

-- | Toma un nombre de archivo, lo lee y retorna su conversion de clausuras
closureConvertFile :: MonadPCF m => String -> m [IrDecl]
closureConvertFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename) 
               (\e -> do let err = show (e :: IOException) 
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- catchErrors (parseIO filename program x)
    case decls of
      Nothing -> do 
                    printPCF "error"
                    return []
      Just d -> do
                  printPCF "SDecls: \n"
                  printPCF $ show d
                  ptm <- sModuleToModule d
                  printPCF "\nDecls: \n"
                  printPCF $ show ptm
                  btc <- runCC ptm
                  printPCF "\nClosureConversion: \n"
                  printPCF $ show btc
                  return btc
                  return []

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
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
       Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> do 
                    handleDecl d -- Todo arreglar para que haga el typecheck del STerm
                    return ()
      Right t -> do
                    handleTerm t --Todo acomodar elab y elab'
                    return ()

handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
          tt <- elab t
          s <- get -- recupero el entorno
          ty <- tc tt (tyEnv s)
          closte <- evalCEK tt
          te <- valToTerm closte 
          printPCF (show te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    sterm <- parseIO "<interactive>" tm x
    nterm <- desugar sterm
    let ex = elab' nterm
    t  <- case nterm of 
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printPCF "STerm:"
    printPCF (show sterm)
    printPCF "\nNTerm:"
    printPCF (show nterm)
    printPCF "\nTerm:"
    printPCF (show t)

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         printPCF "STerm:"
         printPCF (show t)
         tt <- elab t 
         printPCF "Term:"
         printPCF (show tt)
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)

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
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename) 
               (\e -> do let err = show (e :: IOException) 
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- catchErrors (parseIO filename program x)
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


-- | 
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

-- | Toma un nombre de archivo, un parser y el contenido del archivo, ejecuta el parser y devuelve m a (a es [SDecl STerm]) o falla
--parseIO ::  MonadPCF m => String -> P [SDecl STerm] -> String -> m [SDecl STerm]
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                          Left e  -> throwError (ParseErr e)
                          Right r -> return r


-- TODO: Antes era de la forma MonadPCF m => SDecl STerm -> m (), hacerlo compatible
handleDecl ::  MonadPCF m => SDecl STerm -> m (Decl Term)
handleDecl decl = do
                  nd <- desugarDec decl
                  case nd of
                    Just (Decl p x t) -> do
                      let tt = elab' t
                      tcDecl (Decl p x tt)
                      return (Decl p x tt)
                      --te <- eval tt
                      --addDecl (Decl p x te)
                    _ -> failPCF "error handle dec"

sModuleToModule :: MonadPCF m => [SDecl STerm] -> m [Decl Term]
sModuleToModule [] = failPCF "No se introdujo ningún archivo"
sModuleToModule [x] = do dx <- handleDecl x
                         return [dx]
sModuleToModule (x:xs) = do dx <- handleDecl x
                            dxs <- sModuleToModule xs
                            return $ dx : dxs

{-
test :: MonadPCF m => String -> m ()
test xs = do decls <- parseIO "" program xs
             im <- sModuleToModule decls
             btc <- bytecompileModule im
             runBC btc-}