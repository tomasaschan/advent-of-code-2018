module ImmuneSystem where

import           Data.List                      ( nub
                                                , sortOn
                                                )
import           Data.Map.Strict                ( (!)
                                                , Map
                                                , mapWithKey
                                                , notMember
                                                , toList
                                                , update
                                                )
import           Data.Ord                       ( Down(Down) )
import           Data.Tuple.Extended

data Team = ImmuneSystem | Infection deriving (Show, Eq, Ord)
opponent :: Team -> Team
opponent ImmuneSystem = Infection
opponent Infection    = ImmuneSystem

newtype Weaknesses = Weaknesses [String] deriving (Show, Eq)
newtype Immunities = Immunities [String] deriving (Show, Eq)

data Group = Group
  { count      :: Int
  , hp         :: Int
  , immunities :: Immunities
  , weaknesses :: Weaknesses
  , damage     :: Int
  , attack     :: String
  , initiative :: Int
  }
  deriving (Show, Eq)

effectivePower :: Group -> Int
effectivePower g = count g * damage g

immuneTo :: String -> Group -> Bool
immuneTo a Group { immunities = Immunities i } = a `elem` i
weakTo :: String -> Group -> Bool
weakTo a Group { weaknesses = Weaknesses i } = a `elem` i

damageDealt :: Group -> Group -> Int
damageDealt a d | immuneTo (attack a) d = 0
                | weakTo (attack a) d   = 2 * power
                | otherwise             = power
  where power = effectivePower a

targetSelectionPrio :: Group -> (Int, Int)
targetSelectionPrio a = (effectivePower a, initiative a)

targetPrio :: Group -> Group -> (Int, Int, Int)
targetPrio attacker' candidate =
  ( damageDealt attacker' candidate
  , effectivePower candidate
  , initiative candidate
  )

newtype Armies = Armies (Map (Team, Int) Group)
  deriving (Show,Eq)

data TargetsSelected = TargetsSelected
  { groups      :: Map (Team, Int) Group
  , attackOrder :: [AttackSelection]
  }
  deriving (Show, Eq)

data State = TargetSelection Armies | Attacking TargetsSelected
    deriving (Show,Eq)

data AttackSelection = AttackSelection
  { attacker :: (Team, Int)
  , defender :: (Team, Int)
  , priority :: Int
  }
  deriving (Show, Eq)

selectTargets :: Armies -> TargetsSelected
selectTargets (Armies armies) =
  let
    select
      :: Team
      -> [(Team, Int, Group)]
      -> [(Team, Int, Group)]
      -> [AttackSelection]
      -> [AttackSelection]
    select _ _  [] selected = selected
    select _ [] _  selected = selected
    select team attackers defenders selected =
      let
        attackers' = sortOn (Down . targetSelectionPrio . thd3) attackers
        attacker'  = head attackers'
        defenders' =
          sortOn (Down . targetPrio (thd3 attacker') . thd3) defenders
        defender' = head defenders'
        selection = AttackSelection (team         , snd3 attacker')
                                    (opponent team, snd3 defender')
                                    (initiative $ thd3 attacker')
      in
        if damageDealt (thd3 attacker') (thd3 defender') > 0
          then select team
                      (tail attackers')
                      (tail defenders')
                      (selection : selected)
          else select team (tail attackers') defenders selected

    groupsFromTeam team =
      let regroup ((t, i), g) = (t, i, g)
      in  filter ((== team) . fst3) . fmap regroup . toList $ armies

    immuneSystem = groupsFromTeam ImmuneSystem
    infection    = groupsFromTeam Infection

    order        = sortOn
      (Down . priority)
      (  select ImmuneSystem immuneSystem infection    []
      <> select Infection    infection    immuneSystem []
      )
  in
    TargetsSelected { groups = armies, attackOrder = order }

evaluateAttacks :: TargetsSelected -> Armies
evaluateAttacks state =
  let attackOnce :: [AttackSelection] -> Map (Team, Int) Group -> Armies
      attackOnce [] armies' = Armies armies'
      attackOnce (turn : rest) armies'
        | attacker turn `notMember` armies' || defender turn `notMember` armies'
        = attackOnce rest armies'
      attackOnce (turn : rest) armies' =
        let defend d =
              let dmg = damageDealt (armies' ! attacker turn) d
                  d'  = d { count = count d - dmg `div` hp d }
              in  if count d' > 0 then Just d' else Nothing
            armies'' = update defend (defender turn) armies'
        in  attackOnce rest armies''
  in  attackOnce (attackOrder state) (groups state)

fight :: Armies -> (Int, Team)
fight armies =
  let fight' (TargetSelection (Armies armies')) | over = (survivors, winner)
         where
          over      = (< 2) . length . nub . fmap (fst . fst) . toList $ armies'
          survivors = sum . fmap (count . snd) . toList $ armies'
          winner    = head . fmap (fst . fst) . toList $ armies'
      fight' (TargetSelection arms) = fight' (Attacking $ selectTargets arms)
      fight' (Attacking targets) =
        let afterAttacks = evaluateAttacks targets
            stalemate    = afterAttacks == Armies (groups targets)
        in  if stalemate
              then (-1, Infection)
              else fight' (TargetSelection afterAttacks)
  in  fight' (TargetSelection armies)


findSmallestBoost :: Armies -> Int
findSmallestBoost armies =
  head
    . fmap fst
    . filter ((== ImmuneSystem) . snd)
    . fmap (fight . flip boostBy armies)
    $ [1 ..]

boostBy :: Int -> Armies -> Armies
boostBy boost (Armies armies) =
  let boostOne (Infection   , _) g = g
      boostOne (ImmuneSystem, _) g = g { damage = damage g + boost }
  in  Armies . mapWithKey boostOne $ armies
