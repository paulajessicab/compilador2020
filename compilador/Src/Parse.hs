{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )
type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else",
                          "succ", "pred", "ifz", "Nat", "in", "let rec", "type"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyvarP :: P STy
tyvarP = do (SAliasTy <$> tyvar)

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> tyvarP
         <|> parens typeP

typeP :: P STy
typeP = try (do
          x <- tyatom
          reservedOp "->"
          SFunTy x <$> typeP)
      <|> tyatom

const :: P Const
const = CNat <$> num

binding :: P [(Name, STy)]
binding = do vars <- many1 var
             reservedOp ":"
             ty <- typeP
             return $ map (\x -> (x, ty)) vars

binders :: P [(Name, STy)]
binders = do b <- many $ parens binding
             return $ concat b

unaryOpName :: P UnaryOp
unaryOpName =
          (reserved "succ" >> return Succ)
     <|>  (reserved "pred" >> return Pred)

unaryOp:: P STerm
unaryOp = do i <- getPos
             op <- unaryOpName
             arg <- optionMaybe atom
             return $ SUnaryOp i op arg

binOpName :: P BinaryOp
binOpName =
          (reservedOp "+" >> return Add)
     <|>  (reservedOp "-" >> return Sub)     

binOp :: P STerm
binOp = do
          i <- getPos
          SBinaryOp i <$> binOpName

atom :: P STerm
atom =     flip SConst <$> const <*> getPos
       <|> flip SV <$> var <*> getPos
       <|> unaryOp
       <|> binOp
       <|> parens tm
       
lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         bs <- binders
         reservedOp "->"
         SLam i bs <$> tm

app :: P STerm
app = do i <- getPos
         f <- atom
         args <- many atom
         return (foldl (SApp i) f args)

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         SIfZ i c t <$> tm

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         bs <- binders
         reservedOp "->"
         SFix i bs <$> tm

-- | Parser de términos let
termLet :: P STerm
termLet = do
     i <- getPos
     reserved "let"
     v <- var
     bs <- binders
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     t <- tm
     reserved "in"
     SLet i v bs ty t <$> tm

-- | Parser de términos let recursivos
termLetRec :: P STerm
termLetRec = do
     i <- getPos
     reserved "let rec"
     v <- var
     bs <- binders
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     t <- tm
     reservedOp "in"
     SLetRec i v bs ty t <$> tm

-- | Parser de términos
tm :: P STerm
tm =  app <|> binOp <|> lam <|> ifz <|> unaryOp <|> fix <|> termLetRec <|> termLet

-- | Parser de nombres de variables de tipos
tyvar :: P Name
tyvar = Tok.lexeme lexer $ do
          c  <- upper
          ns <- many digit
          cs <- option "" identifier
          return (c:(ns++cs))

-- | Parser de declaración de sinónimos de tipos
declTySyn :: P (SDecl STerm)
declTySyn = do
     i <- getPos
     reserved "type"
     tn <- tyvar
     reservedOp "="
     STypeAlias i tn <$> typeP

-- | Parser de declaraciones let
declLet :: P (SDecl STerm)
declLet = do
     i <- getPos
     reserved "let"
     v <- var
     bs <- binders
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     SLetDec i v bs ty <$> tm

-- | Parser de declaraciones recursivas
declRec :: P (SDecl STerm)
declRec = do
     i <- getPos
     reserved "let rec"
     v <- var
     bs <- binders
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     SLetRecDec i v bs ty <$> tm

-- | Parser de declaraciones
decl :: P (SDecl STerm)
decl = declTySyn <|> declRec <|> declLet

-- | Parser de programas (listas de declaraciones)
program :: P [SDecl STerm]
program = many decl

-- | Parsea una declaración a un término
-- | Útil para las sesiones interactivas
declOrTm :: P (Either (SDecl STerm) STerm)
declOrTm =  try (Right <$> tm) <|> (Left <$> decl)

-- | Corre un parser, chequeando que se pueda consumir toda la entrada
-- | p parser, x cadena a parsear, filename
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- | Para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP tm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
