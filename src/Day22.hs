module Day22 where

type HitPoints = Int
type Mana = Int

data Wizard = Wizard
  { hitPoints :: HitPoints
  , mana :: Mana
  , effects :: [Effect]
  }

data Effect = Effect
  { duration :: Int
  , effectType :: EffectType
  }

data EffectType
  = Shield
  | Poison
  | Recharge