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
                          "succ", "pred", "ifz", "Nat", "in", "let rec"],
         reservedOpNames = ["->",":","="]
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

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunTy x y))
      <|> tyatom

-- Todo typeAlias
-- TODO desugar types
          
const :: P Const
const = CNat <$> num

binding :: P (Name, Ty)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

binders :: P [(Name, Ty)]
binders = many $ parens $ binding

unaryOp :: P STerm
unaryOp = do
  i <- getPos
  foldr (\(w, r) rest -> try (do 
                                 reserved w
                                 a <- atom
                                 return (r a)) <|> rest) parserZero (mapping i)
  where
   mapping i = [
       ("succ", SUnaryOp i Succ)
     , ("pred", SUnaryOp i Pred)
    ]

atom :: P STerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens tm

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         bs <- binders
         reservedOp "->"
         t <- tm
         return (SLam i bs t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         e <- tm
         return (SIfZ i c t e)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         bs <- binders
         reservedOp "->"
         t <- tm
         return (SFix i bs t)

-- | Parser de términos
tm :: P STerm
tm = app <|> lam <|> ifz <|> unaryOp <|> fix

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
     t <- tm
     reservedOp "in"
     t' <- tm
     return (SLetDec i v bs ty t)

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
     t <- tm
     reservedOp "in"
     t' <- tm
     return (SLetRecDec i v bs ty t)

-- | Parser de declaraciones
decl :: P (SDecl STerm)
decl = declLet <|> declRec


-- | Parser de programas (listas de declaraciones) TODO let rec
program :: P [SDecl STerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (SDecl STerm) STerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> tm)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP tm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
