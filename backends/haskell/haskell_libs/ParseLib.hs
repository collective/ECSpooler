{- additions and changes to make the module work with Haskell 98  -  DR -}

-- import Monad( MonadPlus( .. ) )  -- cf. below

-- zero --> mzero

-- ++ --> mplus

-- imports from Prelude.hs

-- alphanum         = sat isAlphanum --> isAlphaNum

-- apply added to export list 



{---------------------------------------------------------------------

           A HASKELL LIBRARY OF MONADIC PARSER COMBINATORS

                          17th April 1997

               Graham Hutton              Erik Meijer
          University of Nottingham   University of Utrecht

This Haskell 1.3 library is derived from our forthcoming JFP article
"Monadic Parsing in Haskell".  The library also includes a few extra
combinators that were not discussed in the article for reasons of space:

   o force (used to make "many" deliver results lazily);

   o digit, lower, upper, letter, alphanum (useful parsers);

   o ident, nat, int (useful token parsers).

---------------------------------------------------------------------}

module ParseLib where

import Monad( MonadPlus( .. ), guard )

-- import Prelude (isDigit )

-- copy from Prelude.hs

isAscii, isControl, isPrint, isSpace            :: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphaNum  :: Char -> Bool
isAscii c              =  fromEnum c < 128
isControl c            =  c < ' ' ||  c == '\DEL'
isPrint c              =  c >= ' ' &&  c <= '~'
isSpace c              =  c == ' '  ||
			  c == '\t' ||
			  c == '\n' ||
			  c == '\r' ||
			  c == '\f' ||
			  c == '\v' ||
			  c == '\xa0'

isUpper c              =  c >= 'A'    && c <= 'Z'    ||
                          c >= '\xc0' && c <= '\xd6' ||
                          c >= '\xd8' && c <= '\xde'

isLower c              =  c >= 'a'   &&  c <= 'z'    ||
                          c >= '\xdf' && c <= '\xf6' ||
                          c >= '\xf8' && c <= '\xff'

isAlpha c              =  isUpper c  ||  isLower c
isDigit c              =  c >= '0'   &&  c <= '9'
isAlphaNum c           =  isAlpha c  ||  isDigit c

ord         	      :: Char -> Int
ord         	       = fromEnum

chr                   :: Int -> Char
chr                    = toEnum

-- end of copy 

infixr 5 +++

-- Monad of parsers: -------------------------------------------------

newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
   return a      = Parser (\cs -> [(a,cs)])
   p >>= f       = Parser (\cs -> concat [parse (f a) cs' |
                                     (a,cs') <- parse p cs])
{-
instance MonadZero Parser where
   zero          = Parser (\cs -> [])

instance MonadPlus Parser where
   p ++ q        = Parser (\cs -> parse p cs ++ parse q cs)

-}

instance MonadPlus Parser where
   mzero         = Parser (\cs -> [])
   p `mplus` q   = Parser (\cs -> parse p cs ++ parse q cs)

-- Other parsing primitives: -----------------------------------------

-- deconstructor: get the parser function out of the monad

parse           :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

-- item ... parses the first char of a nonempty string

item            :: Parser Char
item             = Parser (\cs -> case cs of
                                     ""     -> []
                                     (c:cs) -> [(c,cs)])

-- sat ... parses the first char of a nonempty string if it
--         satisfies a property

sat             :: (Char -> Bool) -> Parser Char
sat p            = do {c <- item; if p c then return c else mzero}

-- Efficiency improving combinators: ---------------------------------

force           :: Parser a -> Parser a
force p          = Parser (\cs -> let xs = parse p cs in
                              (fst (head xs), snd (head xs)) : tail xs)

-- (+++) ... combines two parsers in such a way that the combination
--           fails if both parsers fail and otherwise the 
--           first result is returned (and all others discarded)

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q          = Parser (\cs -> case parse (p `mplus` q) cs of
                                     []     -> []
                                     (x:xs) -> [x])

-- Recursion combinators: --------------------------------------------

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do {char c; string cs; return (c:cs)}

-- many p ... zero or more succeeding successful applications of parser p
--            returns longest match

many            :: Parser a -> Parser [a]
many p           = force (many1 p +++ return [])

-- many1 p ... one or more succeeding successful applications of parser p
--             returns longest match

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many p; return (a:as)}

-- sepby p sep ... zero or more successful applications of parser p
--                 with a successful application of parser sep (as
--                 separator) between each pair of strings parsed by p;
--                 result is the list of successes of p

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

-- chainl p op a ... parses of p are followed by parses of op that
--                   result in (result building) functions to be applied to 
--                   the result of the parses of p before and after op in a 
--                   left associative manner; a is the default result value;
--                   cf. examples int, addop and mulop  below

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) +++ return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = do {f <- op; b <- p; rest (f a b)}
                               +++ return a

-- Useful parsers: ---------------------------------------------------

char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do {c <- sat isDigit; return (ord c - ord '0')}

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = sat isAlpha

alphanum        :: Parser Char
-- alphanum         = sat isAlphanum
alphanum         = sat isAlphaNum

symb            :: String -> Parser String
symb cs          = token (string cs)

ident           :: [String] -> Parser String
ident css        = do cs <- token identifier
                      guard (not (elem cs css))
                      return cs

identifier      :: Parser String
identifier       = do {c <- lower; cs <- many alphanum; return (c:cs)}

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit `chainl1` return (\m n -> 10*m + n)

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat

-- Lexical combinators: ----------------------------------------------

space           :: Parser String
space            = many (sat isSpace)

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}

apply           :: Parser a -> String -> [(a,String)]
apply p          = parse (do {space; p})

-- Example parser for arithmetic expressions: ------------------------
 
expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
factor = token digit +++ do {symb "("; n <- expr; symb ")"; return n}
 
addop  = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop  = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

----------------------------------------------------------------------
