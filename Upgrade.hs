module Upgrade where 

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Data.Char

-- newtype StateT s a = StateT { runStateT :: [s] -> Maybe (a,[s]) } 
type Parser t a = StateT [t] Maybe a

class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()
  
instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT g
    where
      g s = fmap (const ((), s)) . switch $ f s

data Program    = Program [Func] Stat
                   deriving Show
data Func       = Func Type Expression [Param] Stat
                   deriving Show 
data Stat       = Skip | AR Type Expression AssignRhs | AL AssignLhs AssignRhs | Read AssignLhs | Free Expression | Return Expression | Exit Expression | Print Expression | Println Expression
                  | If Expression Stat Stat | While Expression Stat | Begin Stat | Colon Stat Stat
                   deriving Show
data Type       = BaseType String | ArrayType Type [Char]| PairType PElemType PElemType
                   deriving Show
data PairElem   = PairElem String Expression
                   deriving Show
data PElemType  = PElemType Type
                   deriving Show
data ArgList    = ArgList [Expression]
                   deriving Show
data Param      = Param Type Expression
                   deriving Show
data AssignLhs  = LIdent Expression | AE Expression | PE PairElem 
                   deriving Show
data AssignRhs  = Expr Expression  | ArrayLtrl [Expression] | Newpair Expression Expression | Call Expression ArgList | RPE PairElem
                   deriving Show
data Expression = IntLtrl String  | BooLtrl String | CharLtrl Char  | StringLtrl String | Ident String |
                  ArrayElem Expression [Expression]| PairLtrl | UnaryOp String Expression | 
                  BinaryOp Expression String Expression | ParanExpression Expression
                   deriving Show

main = do
  s <- readFile "wacc_examples/valid/while/fibonacciFullIt.wacc"
  print (runParser program s)

runParser :: Parser t a -> [t] -> Maybe (a, [t])
runParser = runStateT

item :: Parser a a
item = get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                         []    -> empty;

satisfy :: (t -> Bool) -> Parser t t
satisfy = flip check item

check :: (Monad f, Alternative f) => (a -> Bool) -> f a -> f a
check p m = m >>= 
	        \x -> if p x then return x else empty

ltrl :: Eq a => a -> Parser a a
ltrl x = satisfy ((==) x)

whitespace :: Parser Char [Char]
whitespace = some $ satisfy (flip elem " \n\t\r\f")

comment :: Parser Char [Char]
comment = pure (:) <*> ltrl '#' <*> many (notl $ ltrl '\n')

notl :: StateT [b] Maybe a -> StateT [b] Maybe b
notl p = switch p *> item

junk = many (whitespace <|> comment)

{-
disregarder :: Char -> [Char] -> ()
disregarder x xs = ()

disregarder2 :: [Char]-> ()
disregarder2 x = ()
-}
token :: Parser Char a -> Parser Char a
token p = p <* junk

btoken :: Parser Char a -> Parser Char a
btoken p = junk *> p

program :: Parser Char Program
program = btoken $ string "begin" *>
          pure Program <*>
          many func <*>
          stat <*
          string "end"

func :: Parser Char Func
func = token $ pure Func <*> parseType <*> (identifier <* openparan) <*> (question paramList <* closeparan) <*> (string "is" *> stat <* string "end")                  

stat :: Parser Char Stat
stat = token $ (
       statColon                                           <|>
       (string "skip" *> pure Skip)                        <|>
       (pure AR      <*> parseType <*> (identifier <* equals) <*> assignRhs) <|>
       (pure AL      <*> (assignLhs <* equals) <*> assignRhs) <|>
       (pure Read    <*> (string "read"    *> assignLhs))  <|>
       (pure Free    <*> (string "free"    *> expression)) <|>
       (pure Return  <*> (string "return"  *> expression)) <|>
       (pure Exit    <*> (string "exit"    *> expression)) <|>
       (pure Println <*> (string "println" *> expression)) <|>
       (pure Print   <*> (string "print"   *> expression)) <|>
       statIf                                              <|>
       statWhile                                           <|>
       statBegin)

stat2 :: Parser Char Stat
stat2 = token $ (
       (string "skip" *> pure Skip)                        <|>
       (pure AR      <*> parseType <*> (identifier <* equals) <*> assignRhs) <|>
       (pure AL      <*> (assignLhs <* equals) <*> assignRhs) <|>
       (pure Read    <*> (string "read"    *> assignLhs))  <|>
       (pure Free    <*> (string "free"    *> expression)) <|>
       (pure Return  <*> (string "return"  *> expression)) <|>
       (pure Exit    <*> (string "exit"    *> expression)) <|>
       (pure Println <*> (string "println" *> expression)) <|>
       (pure Print   <*> (string "print"   *> expression)) <|>
       statIf                                              <|>
       statWhile                                           <|>
       statBegin)

statWhile :: Parser Char Stat
statWhile = token $ string "while" *> 
            pure While <*> 
            expression <*>
            (string "do"    *> stat <* string "done") 


statIf :: Parser Char Stat
statIf = token $ pure If                       <*
         string "if"                   <*>
         (expression <* string "then") <*>
         (stat <* string "else")       <*>
         (stat <* string "fi")

statBegin :: Parser Char Stat
statBegin = token $
            string "begin" *>
            pure Begin    <*>
            stat          <*
            string "end"

statColon :: Parser Char Stat
statColon = token $
            pure Colon <*>
            stat2       <*
            colon      <*>
            stat


param :: Parser Char Param
param = token $ pure Param <*> parseType <*> identifier

paramList :: Parser Char [Param]
paramList = token $ (pure (:) <*> param <*> many (comma *> param))

assignLhs :: Parser Char AssignLhs
assignLhs = token $ (pure LIdent <*> identifier) <|> (pure AE <*> arrayElem) <|> (pure PE <*> pairElem)

assignRhs :: Parser Char AssignRhs
assignRhs = token $ (pure Expr <*> expression) <|> arrayLtrl <|> newpair <|> (pure RPE <*> pairElem) <|> call

newpair :: Parser Char AssignRhs 
newpair = token $ pure Newpair <*> (openparan *> expression <* comma) <*> (expression <* closeparan) 

call :: Parser Char AssignRhs
call = token $ pure Call <*> (string "call" *> identifier <* openparan) <*> (argList <* closeparan)

question   :: Alternative f => f [t] -> f [t]
question v = v <|> pure empty

string        :: String -> Parser Char [Char]
string ""     = pure ""
string (x:xs) = token $ pure (:) <*> ltrl x <*> string xs

parseType :: Parser Char Type
parseType = token $ arraytype <|> basetype

parseType2 :: Parser Char Type
parseType2 = token $ basetype

basetype :: Parser Char Type
basetype = token $ pure BaseType <*> (string "int" <|> string "bool" <|> string "char" <|> string "string")

arraytype :: Parser Char Type
arraytype = token $ pure ArrayType <*> parseType2 <*> some (openbrace *> closebrace)

expression :: Parser Char Expression
expression = token $ binaryOp <|> intLtrl <|> boolLtrl <|> charLtrl <|> stringLtrl <|> pairLtrl <|> identifier <|> arrayElem <|> unaryOp <|> paranExpression

expression2 :: Parser Char Expression
expression2 = token $ intLtrl <|> boolLtrl <|> charLtrl <|> stringLtrl <|> pairLtrl <|> identifier <|> arrayElem <|> unaryOp <|> paranExpression

paranExpression :: Parser Char Expression
paranExpression = openparan *>
                  pure ParanExpression <*>
                  expression <*
                  closeparan

boolLtrl :: Parser Char Expression
boolLtrl = token $ pure BooLtrl <*> (string "true" <|> string "false")

pairLtrl :: Parser Char Expression
pairLtrl = token $ (string "null" *> pure PairLtrl)

identifier :: Parser Char Expression
identifier = token $ pure Ident <*> (pure (:) <*> (ltrl '_' <|> letter) <*> many (ltrl '_' <|> letter <|> digit))

intLtrl :: Parser Char Expression
intLtrl = token $ pure IntLtrl <*> (pure (++) <*> question intSign <*> some digit)

stringLtrl :: Parser Char Expression
stringLtrl = token $ pure StringLtrl <*> ((ltrl '\"') *> many character <* (ltrl '\"'))

charLtrl :: Parser Char Expression
charLtrl = token $ pure CharLtrl <*> (ltrl '\'' *> character <* ltrl '\'')

digit :: Parser Char Char
digit = satisfy (flip elem "0123456789")

letter :: Parser Char Char
letter = satisfy letterExp

intSign :: Parser Char [Char]
intSign = (string "+" <|> string "-")

lower   :: Char -> Bool 
lower x = (x >= 'a' && x <= 'z')

upper  :: Char -> Bool 
upper x = (x >= 'A' && x <= 'Z')

letterExp :: Char -> Bool 
letterExp x = upper x || lower x

escapeChar  :: Char -> Bool
escapeChar x =  x == '0' ||  x == 'b' || x == 't' || x == 'n' || x == 'f' || x == 'r'|| x == '"' || x == '\\' 

character :: Parser Char Char
character = satisfy (\x -> (x /= '\\' && x /= '"' && x /= '\'')) <|> (ltrl '\\' *> satisfy escapeChar)

arrayElem :: Parser Char Expression
arrayElem = token $ pure ArrayElem <*> identifier <*> some (openbrace *> expression <* closebrace)

unaryOp :: Parser Char Expression
unaryOp = token $ pure UnaryOp <*> (string "!" <|> string "-" <|> string "len" <|> string "ord" <|> string "chr") <*> expression

binaryOp :: Parser Char Expression
binaryOp = token $ pure BinaryOp 
           <*> expression2 <*> (string "*" <|> string "/" <|> string "%" <|> string "+" <|> string "-" <|> string ">" <|> string ">=" <|> string "<" <|> string "<=" <|> string "==" <|> string "!=" <|> string "&&" <|> string "||") 
           <*> expression

pairElem :: Parser Char PairElem
pairElem = token $ pure PairElem <*> 
                  (string "fst" <|> string "snd") <*> 
                   expression

argList :: Parser Char ArgList
argList = token $ pure ArgList <*> (pure (:) <*> expression <*> many (comma *> expression))

arrayLtrl :: Parser Char AssignRhs
arrayLtrl = token $ pure ArrayLtrl <*> (openbrace *> question (pure (:) <*> expression <*> many expression) <* closebrace)

openparan  = token $ ltrl '('
closeparan = token $ ltrl ')'
comma      = token $ ltrl ','
equals     = token $ ltrl '='
openbrace  = token $ ltrl '['
closebrace = token $ ltrl ']'
colon      = token $ ltrl ';'

