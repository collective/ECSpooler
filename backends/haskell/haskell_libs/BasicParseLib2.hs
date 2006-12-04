module BasicParseLib2 where

import Char

-- basic type and data definitions
data Expr = Lit Int | Op Ops Expr Expr
            deriving (Eq, Show)

data Ops = Add | Sub | Mul | Div | Mod
           deriving (Eq, Show)

type Parse a b = [a] -> [(b,[a])]


-- some basic parsers
none :: Parse a b
none inp = []

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]

spot :: (a -> Bool) -> Parse a a 
spot p [] = []
spot p (x:xs)
 | p x       = [(x,xs)]
 | otherwise = []

token :: Eq a => a -> Parse a a 
token a = spot (==a)


-- combining parsers
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp 

infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp = [((y,z),rem2)|(y,rem1) <- p1 inp, (z,rem2) <- p2 rem1]

build  :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [(f x,rem)|(x,rem) <- p inp]

list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

neList :: Parse a b -> Parse a [b]
neList p = (p `build` (:[])) `alt` ((p >*> neList p) `build` (uncurry (:)))

optional :: Parse a b -> Parse a [b]
optional p = (succeed []) `alt` (p `build` (:[]))

nTimes :: Int -> Parse a b -> Parse a [b]
nTimes n p  
    | n==0      = succeed [] 
    | otherwise = ((p >*> nTimes (n - 1) p) `build` (uncurry (:)))

spotWhile :: (a -> Bool) -> Parse a [a]
spotWhile p = return.last.(list (spot p))


-- a parser for expressions
parser:: Parse Char Expr
parser = litParse `alt` opExpParse

opExpParse = (token '(' >*> parser >*> spot isOp >*> parser >*> token ')' )
             `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

litParse = ((optional (token '~')) >*> (neList (spot isDigit)))
            `build` (charlistToExpr . uncurry (++))

isOp :: Char -> Bool
isOp x = elem x "+-*/%"

charToOp :: Char -> Ops
charToOp op = 
    case op of 
        '+' -> Add
        '-' -> Sub
        '*' -> Mul
        '/' -> Div
        '%' -> Mod
        otherwise -> error ("Unexpected char " ++ [op] ++ " in charToOp")

charlistToExpr ('~':chlist) = Lit (negate (read chlist::Int))
charlistToExpr chlist = Lit (read chlist::Int)

topLevel :: Parse a b -> [a] -> b
topLevel p inp = case results of
                  [] -> error "parse unsucessful"
                  _  -> head results
                 where 
                 results = [found | (found, []) <- p inp]
