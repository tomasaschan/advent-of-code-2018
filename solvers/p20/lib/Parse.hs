module Parse (path) where

import           Text.ParserCombinators.Parsec
import           Debug.Trace
import Domain
import Prelude hiding (sequence)

seeNext :: Int -> GenParser Char u ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  let pos = statePos s
  traceShowM (pos,out)

path :: String -> Either ParseError Path
path = parse (string "^" *> path' <* string "$") "path"

path' :: GenParser Char st Path
path' = seeNext 100 >> (try sequence <|> Step <$> step)

sequence :: GenParser Char st Path
sequence =  (<:>) <$> step <*> path'

step :: GenParser Char st Step
step = direction <|> fork

direction :: GenParser Char st Step
direction = Direction <$> (n <|> e <|> s <|> w)
    where n = string "N" >> return N
          e = string "E" >> return E
          s = string "S" >> return S
          w = string "W" >> return W

fork :: GenParser Char st Step
fork = Fork <$> (open <*> fork' <*> close)
    where
        open :: GenParser Char st ([Path] -> [Path] -> [Path])
        open = (\a b c -> a <> b <> c) <$> (try ([Empty] <$ string "(|") <|> [] <$ string "(")

        close :: GenParser Char st [Path]
        close =  [Empty] <$ string "|)" <|> [] <$ string ")"

        fork' :: GenParser Char st [Path]
        fork' = choices

choices :: GenParser Char st [Path]
choices = try ((:) <$> path') <*> many (try $ string "|" *> path')
