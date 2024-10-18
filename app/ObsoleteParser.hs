module Parser where

import AST as AST 

import Text.Parsec 
import Text.Parsec.String


-- try to implement chainl1 for left associativity 
aexprParser :: Parser AExpr 
aexprParser =  try plusMinParser <|> try (Term <$> termParser)

plusMinParser :: Parser AExpr 
plusMinParser = try plusParser <|> try minusParser

plusParser :: Parser AExpr 
plusParser = Plus <$> aexprParser <*> (spaces >> char '+' >> spaces >>  termParser)

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



bexprParser :: Parser BExpr
bexprParser = try orParser <|> (BTerm <$> btermParser)

orParser :: Parser BExpr 
orParser = Or <$> btermParser <*> ( spaces >> string "||" >> spaces >> bexprParser)  

btermParser :: Parser BTerm 
btermParser = try andParser <|> (BFact <$> bfactParser)

andParser :: Parser BTerm 
andParser = And <$> bfactParser <*> ( spaces >> string "&&" >> spaces >> btermParser)

bfactParser :: Parser BFact 
bfactParser = try negParser <|> try bparenParser <|> (BAtom <$> batomParser)

negParser :: Parser BFact 
negParser = Neg <$> (string "not" >> spaces >> bexprParser)

bparenParser :: Parser BFact 
bparenParser = BParen <$> (do 
                            spaces >> char '('
                            expr <- bexprParser
                            spaces >> char ')'
                            return expr)

batomParser :: Parser BAtom 
batomParser = try leqParser <|> try ltParser <|> try eqParser <|> boolParser

boolParser :: Parser BAtom 
boolParser = (T <$ (string "True")) <|> (F <$ (string "False"))

leqParser :: Parser BAtom
leqParser = Leq <$> aexprParser <*> ( spaces >> string "<=" >> spaces >> aexprParser)


ltParser :: Parser BAtom
ltParser = Lt <$> aexprParser <*> ( spaces >> char '<' >> spaces >> aexprParser)

eqParser :: Parser BAtom
eqParser = Eq <$> aexprParser <*> ( spaces >> string "==" >> spaces >> aexprParser)


statementParser :: Parser Stmt 
statementParser = try whileParser <|> try seqParser <|> STerm <$> stermParser  

whileParser :: Parser Stmt
whileParser =  While <$> (string "while" >> spaces >> bexprParser) <*> (spaces >> char '{' >> statementParser <* spaces <* char '}')

seqParser :: Parser Stmt 
seqParser = Seq <$> stermParser <*> (spaces >> char ';' >> spaces >> statementParser)


-- Need to resolve ambiguity for statements 
stermParser :: Parser STerm 
stermParser = try ternaryParser <|> SFact <$> sfactParser 

ternaryParser :: Parser STerm 
ternaryParser = IfThenElse <$> (string "if" >> spaces >> bexprParser)
                  <*> (spaces >> string "then" >> spaces >> sfactParser)
                  <*> (spaces >> string "else" >> spaces >> stermParser)

sfactParser :: Parser SFact 
sfactParser = assignParser 

assignParser :: Parser SFact
assignParser = Assign <$> (many1 letter) <*> (spaces >> string ":=" >> spaces >> aexprParser)

programParser :: Parser Code 
programParser = many1 statementParser

parseInput x = parse statementParser "dummy" x


parseA x = parse aexprParser "dummy" x 
parseB x = parse bexprParser "dummy" x 

