{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab, desugar, desugarDec, elab',desugarDec )
import Eval ( eval )
import CEK (evalCEK, valToTerm)
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import Common ()

prompt :: String
prompt = "PCF> "

main :: IO ()
main = do args <- getArgs
          runPCF (runInputT defaultSettings (main' args))
          return ()

main' :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
main' args = do
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
    mapM_ handleDecl decls --mapM_ Mapea cada elemento de decls a una acción monadica (handleDecl), evalua de izq a der, ignora los resultados (mapM no).

-- | Toma un nombre de archivo, un parser y el contenido del archivo, ejecuta el parser y devuelve m a (a es [SDecl STerm]) o falla
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

-- | 
handleDecl ::  MonadPCF m => SDecl STerm -> m () --Todo ver
{-handleDecl (STypeAlias p n ty) = do  -- Hacer una funcion que agrupe lo del typechecker? Creo que va en elab porque no hay que chequear el tipo de ty
                                  mty <- lookupTy n
                                  dty <- desugarTy ty
                                  case mty of
                                    Nothing -> do
                                      addTy n dty
                                    Just _  -> failPosPCF p $ n ++" ya está declarado"-}
handleDecl decl                = do
                                  nd <- desugarDec decl
                                  case nd of
                                    Just (Decl p x t) -> do
                                      let tt = elab' t
                                      tcDecl (Decl p x tt)
                                      closte <- evalCEK tt
                                      te <- valToTerm closte 
                                      addDecl (Decl p x te)
                                    _ -> return ()--failPosPCF NoPos $ "Error al eliminar el syntactic sugar" --Cambiar error

       -- let (Decl p x t) = desugarDec d -- Quita el azucar sintáctico
       -- let tt = elab' t                -- Cambia de notación con nombre a índices de Bruijin
       -- tcDecl (Decl p x tt)            -- typechecker. Si el nombre está declarado failPosPCF, si no lo está lo declara en entorno de tipos global (addTy)
       -- te <- eval tt                   -- Evaluador CBV
       -- addDecl (Decl p x te)           -- Agrega la declaración al entorno de declaraciones globales (glb)

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
      Left d  -> handleDecl d -- Todo arreglar para que haga el typecheck del STerm <- no se si este comentario sigue siendo valido jajaja
      Right t -> handleTerm t --Todo acomodar elab y elab'

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
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)