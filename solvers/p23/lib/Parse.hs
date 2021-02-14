module Parse
    ( nanobot
    ) where

import           Text.Parsec.Number
import           Text.ParserCombinators.Parsec

import           Domain                  hiding ( position
                                                , range
                                                )

nanobot :: String -> Either ParseError Nanobot
nanobot = parse parseNanobot "nanobot"

parseNanobot :: GenParser Char st Nanobot
parseNanobot = Nanobot <$> position <*> (string ", " *> range)

range :: GenParser Char st Distance
range = string "r=" *> int

position :: GenParser Char st Position
position =
    Position
        <$> (string "pos=<" *> int)
        <*> (string "," *> int)
        <*> (string "," *> int <* string ">")
