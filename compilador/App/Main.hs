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
import Bytecompile
import Errors
import Lang
import Parse ( P, program, runP )
import Elab ( desugarDec, elab',desugarDec )
import MonadPCF
import TypeChecker ( tcDecl )
import Common ()
import ClosureConversion
--import System.Console.Haskeline ( defaultSettings, runInputT )

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConversion

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

go :: (Mode,[FilePath]) -> IO ()
go (Interactive,files) = do
                          --runPCF (runInputT defaultSettings (repl files))  --repl :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
                          return ()
go (Typecheck, files) = undefined
go (Bytecompile, files) = do runPCF (bytecompileFiles files)
                             return ()
go (Run,files) = do runPCF (runFiles files)
                    return ()
go (ClosureConversion, files) = do x <- runPCF (closureConvertFiles files)
                                   return  ()

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper) ( fullDesc <> progDesc "Compilador de PCF" <> header "Compilador de PCF de la materia Compiladores 2020" )

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
parseIO ::  MonadPCF m => String -> P [SDecl STerm] -> String -> m [SDecl STerm]
parseIO filename p x = case runP p x filename of
                          Left e  -> throwError (ParseErr e)
                          Right r -> return r

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