module Day21 where

import Control.Monad
import Data.Maybe
import Safe

type HitPoints = Int
type Damage    = Int
type Armor     = Int
type Gold      = Int

data Attacker = Attacker HitPoints Damage Armor

data Item = Item String Gold Damage Armor

data Battle = Battle
  { turn :: Role
  , boss :: Attacker
  , player :: Attacker
  }

data Role
  = Player
  | Boss
  deriving (Eq)

mostExpensiveLoss :: Attacker -> Gold
mostExpensiveLoss = maximum . map fst . filter ((== Boss) . snd) . allOutcomes

cheapestWin :: Attacker -> Gold
cheapestWin = minimum . map fst . filter ((== Player) . snd) . allOutcomes

allOutcomes :: Attacker -> [(Gold, Role)]
allOutcomes b = (outcome <$>) <$> players
  where
    outcome = simulateBattle . Battle Player b
    players = createPlayer 100 <$> itemSets

simulateBattle :: Battle -> Role
simulateBattle b@(Battle _ (Attacker bhp _ _) (Attacker php _ _))
  | php <= 0 = Boss
  | bhp <= 0 = Player
  | otherwise = simulateBattle $ advanceBattle b

advanceBattle :: Battle -> Battle
advanceBattle = \case
  Battle Player (Attacker hp bd a) p@(Attacker _ d _) ->
    Battle Boss (Attacker (hp - d + a) bd a) p
  Battle Boss b@(Attacker _ d _) (Attacker php pd pa) ->
    Battle Player b (Attacker (php - d + pa) pd pa)

createPlayer :: HitPoints -> [Item] -> (Gold, Attacker)
createPlayer h = foldr equipItem (0, (Attacker h 0 0))
  where
    equipItem (Item _ g d a) (cost, (Attacker hp pd pa)) =
      (cost + g, Attacker hp (pd + d) (pa + a))

itemSets :: [[Item]]
itemSets = do
  w <- weapons
  rs <- subsetsOfMaxSize 2 rings
  as <- subsetsOfMaxSize 1 armor
  return $ [w] ++ rs ++ as

subsetsOfMaxSize :: Int -> [a] -> [[a]]
subsetsOfMaxSize n = filter ((<=n) . length) . filterM (const [True, False])

parseAttacker :: String -> Attacker
parseAttacker s = Attacker hp d a
  where [hp, d, a] = catMaybes $ readMay <$> words s

weapons :: [Item]
weapons =
  [ Item "Dagger" 8 4 0
  , Item "Shortsword" 10 5 0
  , Item "Warhammer" 25 6 0
  , Item "Longsword" 40 7 0
  , Item "Greataxe" 74 8 0
  ]

armor :: [Item]
armor =
  [ Item "Leather" 13 0 1
  , Item "Chainmail" 31 0 2
  , Item "Splintmail" 53 0 3
  , Item "Bandedmail" 75 0 4
  , Item "Platemail" 102 0 5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1" 25 1 0
  , Item "Damage +2" 50 2 0
  , Item "Damage +3" 100 3 0
  , Item "Defense +1" 20 0 1
  , Item "Defense +2" 40 0 2
  , Item "Defense +3" 80 0 3
  ]