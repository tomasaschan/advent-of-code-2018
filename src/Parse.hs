module Parse
  (Parser, Parse.drop, digits, extract, expect, readRest, peek)
where
  import Control.Applicative() -- Otherwise you can't do the Applicative instance.
  import Control.Monad (liftM, ap)
  import Data.List (isPrefixOf)
  import Text.Printf

  data Parser a = Parser (a, [String]) deriving (Show, Eq)

  instance Functor Parser where
    fmap = liftM
  instance Applicative Parser where
    pure = return
    (<*>) = ap

  instance Monad Parser where
    return x = Parser (x, [])
    Parser (rest, groups) >>= f = Parser (rest', groups ++ groups')
      where
        Parser (rest', groups') = f rest

  extract :: ([String] -> a) -> Parser b -> a
  extract f = f . getParts

  getParts :: Parser a -> [String]
  getParts (Parser (_, parts)) = parts

  drop :: Char -> String -> Parser String
  drop c x
    | x == [] = error (printf "expected char %c but got empty string" c)
    | head x == c = do return $ tail x
    | otherwise = error (printf "expected %c but got %s" c x)

  expect :: String -> String -> Parser String
  expect e s
    | e `isPrefixOf` s = do return $ Prelude.drop (length e) s
    | otherwise = error (printf "expected string %s but got %s" e s)

  readRest :: String -> Parser String
  readRest s = Parser ("", [s])

  peek :: (Char -> String -> Parser String) -> String -> Parser String
  peek f (c:s) = f c (c:s)
  peek _ [] = error ("tried to peek into an empty string")

  digit :: String -> (Maybe String, String)
  digit ('0':rest) = (Just "0", rest)
  digit ('1':rest) = (Just "1", rest)
  digit ('2':rest) = (Just "2", rest)
  digit ('3':rest) = (Just "3", rest)
  digit ('4':rest) = (Just "4", rest)
  digit ('5':rest) = (Just "5", rest)
  digit ('6':rest) = (Just "6", rest)
  digit ('7':rest) = (Just "7", rest)
  digit ('8':rest) = (Just "8", rest)
  digit ('9':rest) = (Just "9", rest)
  digit x = (Nothing, x)

  digits :: String -> Parser String
  digits = _getDigits []

  _getDigits :: [String] -> String -> Parser String
  _getDigits prev x =
    case digit x of
      (Just d, rest) -> do _getDigits (d:prev) rest
      (Nothing, rest) -> do
        Parser (rest, [concat $ reverse prev])

