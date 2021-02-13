module CLI where

import           Options.Applicative

data InputSource = StdIn | File FilePath deriving (Show)
data Options = Options {
  solvePart1 :: Bool,
  solvePart2 :: Bool,
  inputFrom :: InputSource
} deriving (Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> (switch $ short 'a' <> help
          "If present, solve the first part of the problem"
        )
    <*> (switch $ short 'b' <> help
          "If present, solve the second part of the problem"
        )
    <*> (fileInput <|> stdInput)
 where
  fileInput :: Parser InputSource
  fileInput = File <$> strOption
    (long "input" <> short 'f' <> metavar "INPUTFILE" <> help
      "Read input from the indicated file"
    )
  stdInput =
    flag' StdIn $ long "stdin" <> short '-' <> help "Read input from STDIN"

options :: ParserInfo Options
options = info (parseOptions <**> helper) $ fullDesc <> progDesc
  "Solve problem 15 of Advent of Code 2018"

getOptions :: IO Options
getOptions = execParser options
