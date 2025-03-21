--Daniel Ward

import Data.Char (isDigit, isLetter, digitToInt)

-- Problem 1: Define the Command data type
type Variable = String
type Val = Int

data Expr
  = Const Val                -- A constant value
  | Var Variable             -- A variable
  | Minus Expr Expr          -- Subtraction
  | Times Expr Expr          -- Multiplication
  | Greater Expr Expr        -- Greater than
  deriving (Show)

data Command
  = Assign Variable Expr     -- Assignment
  | Seq Command Command      -- Sequence of commands
  | Cond Expr Command Command -- Conditional
  | While Expr Command       -- While loop
  deriving (Show)

-- Problem 2: Completed the Expr data type above.
--completed above.

-- Problem 3: Implement eval function for expressions
eval :: Expr -> Store -> Val
eval (Const v) _ = v
eval (Var v) store = fetch store v
eval (Minus e1 e2) store = eval e1 store - eval e2 store
eval (Times e1 e2) store = eval e1 store * eval e2 store
eval (Greater e1 e2) store = if eval e1 store > eval e2 store then 1 else 0

-- Problem 4: Interpret an assignment command
--interpret :: Command -> Store -> Store
--interpret (Assign v e) store = update store v (eval e store)

-- Problem 5: Interpret a sequence of commands
--interpret (Seq c1 c2) store = interpret c2 (interpret c1 store)

switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch 1 f1 _ store = f1 store
switch 0 _ f2 store = f2 store
switch _ _ _ _ = error "Invalid value for switch. Only 0 or 1 allowed."

-- Interpret conditional command
--interpret (Cond e c1 c2) store =
  --let val = eval e store
   --in switch val (interpret c1) (interpret c2) store

-- Problem 7: Interpret while loop
--interpret (While e c) store =
  --let val = eval e store
   --in switch val (interpret (Seq c (While e c))) id store

--full interpret
interpret :: Command -> Store -> Store
interpret (Assign v e) store = update store v (eval e store)
interpret (Seq c1 c2) store = interpret c2 (interpret c1 store)
interpret (Cond e c1 c2) store =
  switch (eval e store) (interpret c1) (interpret c2) store
interpret (While e c) store =
  switch (eval e store) (interpret (Seq c (While e c))) id store

-- Problem 8: Define store operations
type Store = Variable -> Val

-- Initial store where all variables default to 0
initial :: Store
initial _ = 0

-- Fetch the value of a variable from the store
fetch :: Store -> Variable -> Val
fetch = id

-- Update the store with a new variable-value pair 
update :: Store -> Variable -> Val -> Store
update store var val v
  | v == var  = val
  | otherwise = store v

-- Problem 9: Test the interpreter
test1 :: Store
test1 = interpret
    (Seq
        (Seq
            (Assign "Y" (Times (Var "X") (Const 3)))-- Y = X * 3
            (Assign "Z" (Minus (Var "Y") (Const 5))) -- Z = Y - 5
        )

        (Seq
            (Cond
                    (Greater (Var "Z") (Const 5)) -- Z > 5
                    (Assign "W" (Const 1)) -- If true, W = 1
                    (Assign "W" (Const 0)) -- If false, W = 0
            )
            (While
                (Greater (Var "Y") (Const 3)) -- While Y > 10
                (Assign "Y" (Minus (Var "Y") (Const 2))) -- Y = Y - 2
            )
        )
    )
    (update (update initial "X" 4) "Count" 0)

test2 :: Store
test2 = interpret
  (Seq
    (While
    (Greater (Const 64) (Var "X")) -- While 64 > X
    (Seq
      (Assign "X" (Times (Var "X") (Const 2))) -- X = X*2
      (Assign "Y" (Minus (Var "Y") (Const 1))) -- Y = Y - 1
    )
    )
    (Seq
      (Assign "Z" (Minus (Var "Z") (Const 1))) -- Z = -1
      (Assign "Z" (Times (Var "Z") (Var "Y"))) -- Z = Y * Z = -6 * -1 = 6
    )
  )
  (update initial "X"  1)

test3 :: Store
test3 = interpret
    (Seq
        (Seq
            (Cond
                (Greater (Var "Z") (Const 5)) -- If Z > 5
                (Assign "W" (Times (Var "X") (Var "Z"))) -- W = X * Z = 64 * 6
                (Assign "W" (Const 0)) -- Otherwise, W = 0
            )
            (Assign "Q" (Minus (Const 0) (Minus (Var "Z") (Var "Y")))) -- Q = 0 - (Z - Y) = 0 - (6 - (-6)) = -12
        )
        (Seq
            (While
                (Greater (Var "Q") (Const (-20))) -- While Q > -20
                (Seq
                    (Assign "Q" (Minus (Var "Q") (Const 2))) -- Q = Q - 2
                    (Assign "V" (Minus (Var "V") (Const (-1)))) -- V = -1 * -1 = 1
                )
            )
            (Cond
                (Greater (Var "W") (Times (Const 30) (Const 30))) -- If W > 30 * 30
                (Assign "Flag" (Const 1)) -- Flag = 1
                (Assign "Flag" (Const 0)) -- Otherwise, Flag = 0
            )
        )
    )
    test2

test4 :: Store
test4 = interpret
    (Seq
        (Assign "Z" (Minus (Var "X") (Var "Y"))) -- Z = X - Y = 15 - 7 = 8
        (Seq
            (Assign "W" (Greater (Var "Z") (Const 5))) -- W = (Z > 5) = 1
            (While
                (Greater (Var "Z") (Const 0)) -- While Z > 0
                (Assign "Z" (Minus (Var "Z") (Const 2))) -- Z = Z - 2
            )
        )
    )
    (update (update initial "X"  15) "Y" 7)

test5 :: Store
test5 = interpret
  (Seq
    (Seq
        (Seq
            (Seq
                (Assign "A" (Const 20)) -- Set A = 20
                (Assign "B" (Const 10)) -- Set B = 10
            )
            (Assign "C" (Minus (Var "A") (Var "B"))) -- C = A - B = 20 - 10 = 10
        )
        (Seq
            (Assign "D" (Times (Var "C") (Const 3))) -- D = C * 3 = 30
            (Cond
                (Greater (Var "D") (Const 20)) -- If D > 20
                (Assign "E" (Const 1)) -- E = 1
                (Assign "E" (Const 0)) -- Otherwise, E = 0
            )
        )
    )
    (Cond
      (Greater (Var "E") (Const 0))
      (Seq
        (While
          (Greater (Var "D") (Var "X")) -- D > X
          (Assign "D" (Minus (Var "D") (Const 1))) -- D = D -1 
        )
        (Assign "X" (Times (Var "D") (Const 2)))
      )
      (Assign "X" (Const (-1)))
    )
  )
  (update (update test4 "A"  20) "B" 10)


-- Parser

-- Token Data Type
data Token = Ident String
           | Number Int
           | Symbol String
           deriving (Show, Eq)


-- Parser Type Definitions
type Parser a = [Token] -> Maybe (a, [Token])
type ParserCommand = Parser Command

-- Abstract Syntax Tree Data Types

-- Parser Combinators
(<|>) :: Parser a -> Parser a -> Parser a
(parser1 <|> parser2) s =
   let parser2IfNothing Nothing = parser2 s
       parser2IfNothing x        = x
   in
     parser2IfNothing (parser1 s)

modify :: Parser a -> (a -> b) -> Parser b
(parser `modify` f) s =
   let modResult Nothing      = Nothing
       modResult (Just (x,y)) = Just (f x,y)
   in
     modResult (parser s)

(<&>) :: Parser a -> Parser b -> Parser (a,b)
(parser1 <&> parser2) s =
   let parser2After Nothing      = Nothing
       parser2After (Just (x,t)) = (parser2 `modify` (\y -> (x,y))) t
   in
     parser2After (parser1 s)

emptyseq :: Parser [a]
emptyseq s = Just ([],s)

optional :: Parser a -> Parser [a]
optional pr = (pr `modify` cons []) <|> emptyseq
               where cons [] x = [x]

number :: Parser Val
number (Number n : tokens) = Just (n, tokens)
number _ = Nothing

--Problem 10
variable :: Parser Variable
variable (Ident v : tokens) = Just (v, tokens)
variable _ = Nothing

literal :: String -> Parser String
literal str (Symbol s : tokens)
    | s == str = Just (s, tokens)
    | otherwise = Nothing
literal _ _ = Nothing

-- Parsers for Expressions
expr :: Parser Expr
expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater
  where optGreater (e1, []) = e1
        optGreater (e1, [(_, e2)]) = Greater e1 e2
        optGreater _ = error "impossible"

-- Parsers for Commands
command :: ParserCommand
command = (unitcom <&> optional (literal ";" <&> command)) `modify` optSeq
  where optSeq :: (Command, [(String, Command)]) -> Command
        optSeq (c1, []) = c1
        optSeq (c1, [(_, c2)]) = Seq c1 c2
        optSeq _ = error "Impossible case in optSeq"

unitcom :: ParserCommand
unitcom = whilecom <|> ifcom <|> assign

aexp :: Parser Expr
aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optSub
  where
        optSub (e1, []) = e1
        optSub (e1, [(_, e2)]) = Minus e1 e2
        optSub _ = error "Impossible case in optSub"

-- Problem 11

bexp :: Parser Expr
bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optMul
  where
        optMul (e1, []) = e1
        optMul (e1, [(_, e2)]) = Times e1 e2
        optMul _ = error "Impossible case in optMul"

cexp :: Parser Expr
cexp = (literal "(" <&> expr <&> literal ")") `modify` unparenth
        <|> (variable `modify` Var)
        <|> (number `modify` Const)
  where
        unparenth :: ((String, Expr), String) -> Expr
        unparenth ((_, e), _) = e


-- Problem 12

--Parser for While Commands
whilecom :: Parser Command
whilecom = (literal "WHILE" <&> expr <&> literal "DO" <&> command <&> literal "END") `modify` mkWhileNode
  where
    mkWhileNode ((((_, cond), _), body), _) = While cond body

--Parser for Cond commands
ifcom :: Parser Command
ifcom = (literal "IF" <&> expr <&> literal "THEN" <&> command <&> literal "ELSE" <&> command <&> literal "ENDIF") `modify` mkIfNode
  where
    mkIfNode ((((((_, cond), _), thenCmd), _), elseCmd), _) = Cond cond thenCmd elseCmd

--Parser for assignments
assign :: Parser Command
assign = (variable <&> literal ":=" <&> expr) `modify` mkAssignNode
  where
    mkAssignNode ((v, _), e) = Assign v e

--main Parser
mainParser :: [Token] -> Command
mainParser = report . command
  where
      report Nothing = error "Parse error"
      report (Just (result, [])) = result
      report (Just (_, unparsed)) = error $ "Syntax error. Unparsed tokens: " ++ show unparsed

--Problem 13 - Tests --Write better tests
testTokens1 :: [Token]
testTokens1 = [Symbol "WHILE", Ident "X", Symbol ">", Ident "Y", Symbol "DO",
  Ident "Y", Symbol ":=", Ident "Y", Symbol "*", Ident "2", Symbol "END"]

testTokens2 :: [Token]
testTokens2 = [Symbol "WHILE", Ident "X", Symbol ">", Number 0, Symbol "DO", Ident "X", Symbol ":=", Ident "X", Symbol "-", Number 1, Symbol "END"]

testTokens3 :: [Token]
testTokens3 = [Symbol "IF", Ident "X", Symbol ">", Number 5, Symbol "THEN", Ident "X", Symbol ":=", Number 1, Symbol "ELSE", Ident "X", Symbol ":=", Number 0, Symbol "ENDIF"]

testTokens4 :: [Token]
testTokens4 = [Symbol "WHILE", Ident "X", Symbol ">", Ident "Y", Symbol "DO",
  Symbol "IF", Ident "X", Symbol ">", Number 5, Symbol "THEN", Ident "X", Symbol ":=", Ident "X", Symbol "-", Number 5,
  Symbol "ELSE", Ident "X", Symbol ":=", Number 0, Symbol "ENDIF", Symbol "END"]

testTokens5 :: [Token]
testTokens5 = [Symbol "IF", Ident "Y", Symbol ">", Number 20, Symbol "THEN",
  Symbol "WHILE", Ident "Y", Symbol ">", Number 20, Symbol "DO",
  Ident "Y", Symbol ":=", Ident "Y", Symbol "-", Number 5, Symbol "END",
  Symbol "ELSE", Ident "X", Symbol ":=", Number 0, Symbol "ENDIF"]



-- Lexer

--problem 14

-- Function to check if a string is a keyword
keyword :: String -> Bool
keyword s = s `elem` ["IF", "THEN", "ELSE", "ENDIF", "WHILE", "DO", "END"]

-- Function to handle keywords or identifiers
keycheck :: String -> Token
keycheck s = if keyword s then Symbol s else Ident s

--Problem 15

-- Function to determine if a character is a letter, digit, prime, or underscore
letdigetc :: Char -> Bool
letdigetc c = isLetter c || isDigit c || c == '\'' || c == '_'

-- Function to determine if a character is a layout character (space, tab, newline)
layout :: Char -> Bool
layout c = c == ' ' || c == '\n' || c == '\t'

-- Function to determine if a character is a symbol (*, -, >, :, =, ;)
symbolchar :: Char -> Bool
symbolchar c = c `elem` ['*', '-', '>', ':', '=', ';']


--Problem 16
-- Lexer functions
lexer :: String -> [Token]
lexer [] = []
lexer (a:x)
  | layout a = lexer x
  | a == '(' = Symbol "(" : lexer x
  | a == ')' = Symbol ")" : lexer x
  | isLetter a = getword [a] x
  | isDigit a = getnum (digitToInt a) x
  | symbolchar a = getsymbol [a] x
  | otherwise = error ("Lexical error : unrecognized token " ++ (a:x))

getword :: String -> String -> [Token]
getword l [] = [keycheck (reverse l)]
getword l (a:x)
  | letdigetc a = getword (a:l) x
  | otherwise = keycheck (reverse l) : lexer (a:x)

getsymbol :: String -> String -> [Token]
getsymbol l [] = [Symbol (reverse l)]
getsymbol l (a:x)
  | symbolchar a = getsymbol (a:l) x
  | otherwise = Symbol (reverse l) : lexer (a:x)

getnum :: Int -> String -> [Token]
getnum n [] = [Number n]
getnum n (a:x)
  | isDigit a = getnum (n * 10 + digitToInt a) x
  | otherwise = Number n : lexer (a:x)

-- Problem 16 Tests -- write better tests

testTokensL1 :: String
testTokensL1 = "IF X > 10 THEN IF X > 20 THEN X := 15 ELSE X := 10 ENDIF ELSE X := 0 ENDIF"

testTokensL2 :: String
testTokensL2 = "WHILE Y > 5 DO Y := Y - 1 END"

testTokensL3 :: String
testTokensL3 = "IF X > 20 THEN WHILE X > 5 DO X := X - 2 END ELSE X := 5 ENDIF"

testTokensL4 :: String
testTokensL4 = "WHILE X > 20 DO IF X > 5 THEN X := X - 5 ELSE X := 0 ENDIF END"

testTokensL5 :: String
testTokensL5 = "WHILE X > 5 DO IF X > Y THEN X := X - 20 ELSE X := Y ENDIF END"

--problem 17
run :: String -> Store -> Store
run program = interpret (mainParser (lexer program))


--Full Program Tests -- write better tests

-- Test 1: WHILE loop with an IF inside that modifies X
testFull1 :: Store
testFull1 = run "WHILE X > 10 DO IF X > Y THEN X := X - 10 ELSE X := Y ENDIF END"
            (update (update initial "X" 20) "Y" 5)
-- Expected result: 10, because X starts at 20, and after the first iteration, it is reduced by 10.

-- Test 2: Basic assignment of X := 5
testFull2 :: Store
testFull2 = run "X := 5" (update initial "X" 0)
-- Expected result: 5, because X is directly assigned the value 5.

-- Test 3: IF statement with condition X > Y
testFull3 :: Store
testFull3 = run "WHILE X > Y DO IF X > 2 * Y THEN X := X - 2 ELSE X := X - 1 ENDIF END"
               (update (update initial "X" 20) "Y" 10)
-- Expected result: 5, because X (15) is greater than Y (10), so X is decremented by 10.

-- Test 4: WHILE loop that decrements X while X > 0
testFull4 :: Store
testFull4 = run "WHILE X > 0 DO X := X - 1 END" (update initial "X" 5)
-- Expected result: 0, because the loop will decrement X from 5 down to 0.

-- Test 5: WHILE loop with nested assignments and conditionals
testFull5 :: Store
testFull5 = run "WHILE X > Y DO IF X > Y THEN X := X - 2 ELSE X := Y ENDIF END"
               (update (update initial "X" 7) "Y" 3)
-- Expected result: 3, because X starts at 7, and it will be decremented by 2 (X := X - 2) until it is 3.


--Test Runs
--Interpreter


--Parser


--lexer


--Full

