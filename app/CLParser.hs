{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module CLParser where
import Data.Char (isDigit, isLower, isUpper)

newtype Parser a = Parser {unParser :: String -> [(a,String)]}

instance Functor Parser where
    fmap f (Parser pa) = Parser (\s -> [ (f a , s') | (a , s') <- pa s])

instance Applicative Parser where
    pure a = Parser (\x -> [(a,x)])
    (<*>) :: Parser (a->b)-> Parser a -> Parser b 
    f  <*> a = let f' = unParser f 
                   a' = unParser a
               in Parser (\s ->  [ (f'' a'' , s'') | (f'', s') <- f' s , (a'' , s'') <- a' s' ])




instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    m >>= n = Parser (\s -> [ (b,s'') | (a,s') <- unParser m s , (b , s'') <- unParser (n a) s'])


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

(-+-) :: Parser a -> Parser a -> Parser a
(Parser p) -+- (Parser q) = Parser (\s -> p s ++ q s)

letter :: Parser Char
letter = lower -+- upper

alphanum :: Parser Char
alphanum = letter -+- digit

word :: Parser String
word = neWord -+- return ""
        where neWord = do c <- letter ; cs <- word ; return (c:cs)


string       :: String -> Parser String
string ""     = return ""
string (x:xs) = do _ <- char x ; _ <- string xs ; return (x:xs)

many :: Parser a -> Parser [a]
many p = (do x <- p ; xs <- many p ; return (x:xs)) -+- zero




