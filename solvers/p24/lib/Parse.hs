{-# LANGUAGE TupleSections #-}
module Parse
    ( armies
    ) where

import           Data.Bifunctor
import           Data.Map.Strict
import           ImmuneSystem
import           Text.Parsec.Number
import           Text.ParserCombinators.Parsec

armies :: String -> Either ParseError Armies
armies input =
    let
        armies' =
            (,)
                <$> (string "Immune System:\n" *> army')
                <*> (string "\n" *> string "Infection:\n" *> army')

        army' = many1 (group' <* string "\n")

        group' =
            let build cnt h (w, i) (d, a) it = Group cnt h i w d a it
            in  build <$> count' <*> hp' <*> types' <*> attack' <*> initiative'
        count' = int <* string " units"
        hp'    = string " each with " *> int <* string " hit points "

        types' =
            let
                open  = string "("
                close = string ")"

                weaknesses' :: GenParser Char st Weaknesses
                weaknesses' =
                    Weaknesses
                        <$> (  string "weak to "
                            *> sepBy1 (many1 letter) (string ", ")
                            )
                immunities' :: GenParser Char st Immunities
                immunities' =
                    Immunities
                        <$> (  string "immune to "
                            *> sepBy1 (many1 letter) (string ", ")
                            )

                onlyWeaknesses' :: GenParser Char st (Weaknesses, Immunities)
                onlyWeaknesses' =
                    (, Immunities []) <$> (open *> weaknesses' <* close)
                onlyImmunities' :: GenParser Char st (Weaknesses, Immunities)
                onlyImmunities' =
                    (Weaknesses [], ) <$> (open *> immunities' <* close)

                weakAndImmune :: GenParser Char st (Weaknesses, Immunities)
                weakAndImmune =
                    (,)
                        <$> (open *> weaknesses' <* string "; ")
                        <*> immunities'
                        <*  close
                immuneAndWeak :: GenParser Char st (Weaknesses, Immunities)
                immuneAndWeak =
                    flip (,)
                        <$> (open *> immunities' <* string "; ")
                        <*> weaknesses'
                        <*  close
            in
                try weakAndImmune
                <|> try immuneAndWeak
                <|> try onlyWeaknesses'
                <|> try onlyImmunities'
                <|> return (Weaknesses [], Immunities [])

        attack' :: GenParser Char st (Int, String)
        attack' =
            (,)
                <$> (  optional (string " ")
                    *> string "with an attack that does "
                    *> int
                    <* string " "
                    )
                <*> (many1 letter <* string " damage")

        initiative' = string " at initiative " *> int

        pack :: ([Group], [Group]) -> Armies
        pack = Armies . fromList . uncurry (<>) . bimap
            (zip [ (ImmuneSystem, i) | i <- [1 ..] ])
            (zip [ (Infection, i) | i <- [1 ..] ])
    in
        fmap pack . parse armies' "armies" $ input
