{-# LANGUAGE FlexibleContexts #-}

module Computer.Parse where

import Debug.Trace

import           Text.ParserCombinators.Parsec

import Computer (Instruction(..), Reference(..), Register(..))

program :: [String] -> Either ParseError (Register, [Instruction])
program = parse program' "program" . unlines
    where
        program' :: GenParser Char st (Register, [Instruction])
        program' = (,) <$> (initializer <* string "\n") <*> instructions <* (comments *> eof)

initializer :: GenParser Char st Register
initializer = initializer' "0" Zero
          <|> initializer' "1" One
          <|> initializer' "2" Two
          <|> initializer' "3" Three
          <|> initializer' "4" Four
          <|> initializer' "5" Five
    where
        initializer' s r = try (string "#ip " >> string s >> return r)
       

instructions :: GenParser Char st [Instruction]
instructions = many1 (try instruction <* string "\n")

comment :: GenParser Char st String
comment = string "# " *> many (noneOf "\n") <* string "\n"

comments :: GenParser Char st (a -> a)
comments = fmap traceShow . try $ many comment 

instruction :: GenParser Char st Instruction
instruction = comments *> anyInstruction
    where
        anyInstruction =
              instruction' Add "addr" register (Register  <$> register) register
          <|> instruction' Add "addi" register (Immediate <$> value   ) register
          <|> instruction' Mul "mulr" register (Register  <$> register) register
          <|> instruction' Mul "muli" register (Immediate <$> value   ) register
          <|> instruction' Div "divr" register (Register  <$> register) register
          <|> instruction' Div "divi" register (Immediate <$> value   ) register
          <|> instruction' Ban "banr" register (Register  <$> register) register
          <|> instruction' Ban "bani" register (Immediate <$> value   ) register
          <|> instruction' Bor "borr" register (Register  <$> register) register
          <|> instruction' Bor "bori" register (Immediate <$> value   ) register
          <|> try ((string "setr" >> return Set) <*> (string " " *> (Register  <$> register)) <*> (string " " *> number *> string " " *> register))
          <|> try ((string "seti" >> return Set) <*> (string " " *> (Immediate <$> value   )) <*> (string " " *> number *> string " " *> register))
          <|> instruction' Gt  "gtir" (Immediate <$> value)    (Register  <$> register) register
          <|> instruction' Gt  "gtri" (Register  <$> register) (Immediate <$> value)    register
          <|> instruction' Gt  "gtrr" (Register  <$> register) (Register  <$> register) register
          <|> instruction' Eq  "eqir" (Immediate <$> value)    (Register  <$> register) register
          <|> instruction' Eq  "eqri" (Register  <$> register) (Immediate <$> value)    register
          <|> instruction' Eq  "eqrr" (Register  <$> register) (Register  <$> register) register
          <|> Noop <$ string "noop"

        instruction' f name a b c = try $ (string name >> return f) <*> (string " " *> a) <*> (string " " *> b) <*> (string " " *> c)
value :: GenParser Char st Int
value = read <$> many1 digit

register :: GenParser Char st Register
register = (string "0" >> return Zero)
       <|> (string "1" >> return One)
       <|> (string "2" >> return Two)
       <|> (string "3" >> return Three)
       <|> (string "4" >> return Four)
       <|> (string "5" >> return Five)


number :: GenParser Char st Int
number = read <$> many1 digit
