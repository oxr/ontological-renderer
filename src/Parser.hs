{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser (Parser(..), 
result, 
zero, 
item, 
bind, 
sat, 
char, 
digit, 
lower, 
upper, 
(-|-), (-++-), 
letter, 
alphanum, 
word, 
string, 
many, 
ident, 
(-:-), 
many1, 
nat, 
int, 
pair, 
triple, 
list, 
double ) where
import Data.Char (isDigit, isLower, isUpper, ord)
import GHC.Float (int2Double)
import Data.Ratio((%))
import Data.List.NonEmpty (unfoldr)

newtype Parser a = Parser {run :: String -> [(a,String)]}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser (\s -> [ (f a , s') | (a , s') <- pa s])

instance Applicative Parser where
    pure a = Parser (\x -> [(a,x)])
    (<*>) :: Parser (a->b)-> Parser a -> Parser b 
    f  <*> a = let f' = run f 
                   a' = run a
               in Parser (\s ->  [ (f'' a'' , s'') | (f'', s') <- f' s , (a'' , s'') <- a' s' ])

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    m >>= n = Parser (\s -> [ (b,s'') | (a,s') <- run m s , (b , s'') <- run (n a) s'])

result :: a -> Parser a
result = return

zero :: Parser a
zero = Parser (const [])

item :: Parser Char
item = Parser (\case  
                        [] -> [] 
                        (x :xs ) -> [(x,xs)])

bind      :: Parser a -> (a -> Parser b) -> Parser b
bind = (>>=)

sat  :: (Char -> Bool) -> Parser Char
sat p = do 
         x <- item 
         if p x then return x else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

(-|-) :: Parser a -> Parser a -> Parser a
(Parser p) -|- (Parser q) = Parser (\s -> p s ++ q s)

(-++-) :: Parser [a] -> Parser [a] -> Parser [a]
p -++- q = do 
            xs <- p
            ys <- q
            return (xs ++ ys)

letter :: Parser Char
letter = lower -|- upper

alphanum :: Parser Char
alphanum = letter -|- digit

word :: Parser String
word = neWord -|- return ""
        where neWord = do c <- letter ; cs <- word ; return (c:cs)

string       :: String -> Parser String
string ""     = return ""
string (x:xs) = do _ <- char x ; _ <- string xs ; return (x:xs)

many :: Parser a -> Parser [a]
many p = (do x <- p ; xs <- many p ; return (x:xs)) -|- return []

ident :: Parser String
ident = do { x <- lower ; xs <- many alphanum ; return (x:xs) }

(-:-) :: Parser a -> Parser [a] -> Parser [a]
pa -:- pas = do { a <- pa ; as <- pas ; return (a:as)}

many1 :: Parser a -> Parser [a]
many1 p = p -:- many p

eval :: String -> Int 
eval = foldl (\ n c -> 10*n + Data.Char.ord c - Data.Char.ord '0') 0 

nat :: Parser Int
nat = do { ds <- many1 digit ; return (eval ds)} 

int :: Parser Int
int = do { _ <- char '-' ; n <- nat ; return (-n)} -|- nat


pair :: Parser a -> Parser b -> Parser (a,b)
pair pa pb = do { _ <- char '(' ; a <- pa ; _ <- char ',' ; b <- pb ; _ <- char ')' ; return (a,b) }

triple :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
triple pa pb pc = do { _ <- char '(' ; a <- pa ; _ <- char ',' ; b <- pb ; _ <- char ',' ; c <- pc ; _ <- char ')' ; return (a,b,c) }

             
dropSecond :: Parser a -> Parser b -> Parser a
dropSecond pa pb = do { a<- pa ; _ <- pb; return a}

dropFirst :: Parser a -> Parser b -> Parser b
dropFirst pa pb = do { _ <- pa ; pb }

double :: Parser Double
double = int >>= \i -> return (int2Double i) -|- do 
           _ <- char '.'
           fracd <- many digit
           return (int2Double i + (int2Double(eval fracd) / 10^length fracd))




-- list is like many , except that we have all the other syntax around
-- with the commas, it is not the case that we simple repeat

lift :: (a->b) -> Parser a -> Parser b 
lift f pa = f <$> pa



singleton :: Parser a -> Parser [a]
singleton =  lift return  



neList :: Parser a -> Parser [a]             
neList pa = do
            _   <- char '['
            a   <- pa 
            as  <- many (char ',' `dropFirst` pa)
            _   <- char ']'
            return (a:as)
            

eList ::  Parser [a]
eList = do 
            _ <- char '['
            _ <- many (char ' ')
            _ <- char ']'
            return []

list :: Parser a -> Parser [a]
list pa = eList -|- neList pa

-- parser for lists of an exact length
parsen :: Int -> Parser a -> Parser [a]
parsen n p = do _ <- char '[' ; l <- parseInside n p ; _ <- char ']' ; return l 
              where 
                parseInside :: Int -> Parser a -> Parser [a]
                parseInside n p | n <= 0  = return []
                                | n == 1  = do a<- p ; return [a]
                                | n > 1   = do a <- p ; _ <- char ',' ; as <- parseInside  (n-1) p ; return (a:as)


