{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser where
import Data.Char (isDigit, isLower, isUpper, ord)

newtype Parser a = Parser {run :: String -> [(a,String)]}

instance Functor Parser where
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

(-/-) :: Parser [a] -> Parser [a] -> Parser [a]
p -/- q = do 
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


nat :: Parser Int
nat = do { ds <- many1 digit ; return (eval ds)} 
    where   eval :: String -> Int 
            eval = foldl (\ n c -> 10*n + Data.Char.ord c - Data.Char.ord '0') 0 

int :: Parser Int
int = do { _ <- char '-' ; n <- nat ; return (-n)} -|- nat


pair :: Parser a -> Parser b -> Parser (a,b)
pair pa pb = do { _ <- char '(' ; a <- pa ; _ <- char ',' ; b <- pb ; _ <- char ')' ; return (a,b) }

triple :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
triple pa pb pc = do { _ <- char '(' ; a <- pa ; _ <- char ',' ; b <- pb ; _ <- char ',' ; c <- pc ; _ <- char ')' ; return (a,b,c) }

             

list :: Parser a -> Parser [a]             
list pa = do { _ <- char '['; a <- pa ; c <- item ; as <- tail pa ; _ <- char ']'; return (a:as)}
            where tail :: Parser a -> Parser [a]
                  tail pa = do { _ <- char ',' ; x <- pa ; xs <- tail pa; return (x : xs)}  