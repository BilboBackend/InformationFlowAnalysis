module Parser where

import AST as AST 

import Text.Parsec 
import Text.Parsec.String


assignParser :: Parser Stmt
assignParser = Assign <$> (many1 letter) <*> (Term <$> Fact <$> Atom <$> N <$> read <$> (spaces >> char ':' >> char '=' >> spaces >> many1 digit))

aexprParser :: Parser AExpr 
aexprParser =  try plusParser <|> try minusParser <|> try (Term <$> termParser)

plusParser :: Parser AExpr 
plusParser = Plus <$> termParser <*> (spaces >> char '+' >> spaces >> aexprParser)

minusParser :: Parser AExpr 
minusParser = Minus <$> termParser <*> (spaces >> char '-' >> spaces >> aexprParser) 

termParser :: Parser ATerm 
termParser = try multParser <|> Fact <$> factorParser

multParser :: Parser ATerm 
multParser =  Mult <$> factorParser <*> (spaces >> char '*' >> spaces >> termParser)

factorParser :: Parser AFact 
factorParser = try parenParser <|> Atom <$> atomParser 

parenParser :: Parser AFact 
parenParser = Paren <$> (do char '(' >> spaces
                            expr <- aexprParser 
                            spaces >> char ')' 
                            return expr)

atomParser :: Parser AAtom 
atomParser = X <$> (many1 letter) <|> N <$> (read <$> many1 digit)
--
--
-- boolParser :: Parser BExpr 
-- boolParser = (T <$ (string "True")) <|> (Neg <$> (T <$ (string "False")))
--
-- negParser :: Parser BExpr 
-- negParser = Neg <$> (string "not" >> spaces >> bexprParser)
--
-- bexprParser :: Parser BExpr
-- bexprParser = negParser <|> ltParser <|> leqParser <|> eqParser <|> boolParser 
--
-- leqParser :: Parser BExpr
-- leqParser = Leq <$> aexprParser <*> ( spaces >> string "<=" >> spaces >> aexprParser)
--
--
-- ltParser :: Parser BExpr
-- ltParser = Lt <$> aexprParser <*> ( spaces >> char '<' >> spaces >> aexprParser)
--
-- eqParser :: Parser BExpr
-- eqParser = Eq <$> dataParser <*> ( spaces >> char '=' >> spaces >> dataParser)
--
--
--
--   
