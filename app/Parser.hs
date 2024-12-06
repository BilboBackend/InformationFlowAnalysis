module Parser where 


import AST as AST

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



languageDef = 
  emptyDef { Token.commentStart = "/*"
           , Token.commentEnd = "*/"
           , Token.commentLine = "#"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedNames = [ "if"
                                   , "then"
                                   , "else"
                                   , "while"
                                   , "do"
                                   , "true"
                                   , "false"
                                   , "not"
                                   , "and"
                                   , "or"
                                   ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                                     , "<", ">", "<=",">=","==","!=","and", "or", "not"
                                     ] 
          }


lexer = Token.makeTokenParser languageDef 

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
braces     = Token.braces     lexer -- parses surrounding braces

--   parens p
-- takes care of the parenthesis and
-- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

codeParse :: Parser Stmt 
codeParse = parens stmtParse 
          <|> seqParse  


seqParse = do 
            seq <- (sepBy1 stmtParse semi)
            return $ if length seq == 1 then head seq else Seq seq  

stmtParse :: Parser Stmt 
stmtParse = ifParse <|> whileParse <|> assignParse


whileParse :: Parser Stmt 
whileParse = do
              reserved "while"
              cond <- bexprParse 
              reserved "do"
              code <- braces seqParse 
              return $ While cond code 

ifParse :: Parser Stmt 
ifParse = do 
            reserved "if"
            cond <- bexprParse 
            reserved "then"
            stmt1 <- stmtParse 
            reserved "else"
            stmt2 <- stmtParse 
            return $ If cond stmt1 stmt2 


assignParse :: Parser Stmt 
assignParse = do 
                var <- identifier
                reservedOp ":="
                expr <- aexprParse 
                return $ Assign var expr 


aexprParse :: Parser AExpr
aexprParse = buildExpressionParser aOps aTerm 


bexprParse :: Parser BExpr
bexprParse = buildExpressionParser bOps bTerm 

aOps = [ [Prefix (reservedOp "-"   >> return (Neg              ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinop Mul )) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinop Div )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinop Add )) AssocLeft,
                 Infix  (reservedOp "-"   >> return (ABinop Sub)) AssocLeft]
              ]

bOps = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinop And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinop Or      )) AssocLeft]
             ]

aTerm = parens aexprParse <|> try ( liftM Var identifier  )<|> liftM AConst integer 

bTerm = parens bexprParse <|> (reserved "true" >> return (BConst True)) <|> (reserved "false" >> return (BConst False)) <|> rexprParse 


rexprParse = do
              aexpr1 <- aexprParse
              op <- rOps
              aexpr2 <- aexprParse
              return $ RBinop op aexpr1 aexpr2

rOps = ( reservedOp "<" >> return Lt  )
              <|> ( reservedOp "<=" >> return Leq )
              <|> ( reservedOp "==" >> return Eq  )
              <|> ( reservedOp "!=" >> return Neq )
              <|> ( reservedOp ">" >> return Gt )
              <|> ( reservedOp ">=" >> return Geq )

parseString :: String -> Stmt
parseString str =
   case parse codeParse "" str of
     Left e  -> error $ show e
     Right r -> r
