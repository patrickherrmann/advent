{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Control.Lens
import Control.Monad

type Outcome = Either Role GameState

data GameState = GameState
  { _playerHealth :: Int
  , _playerMana :: Int
  , _manaSpent :: Int
  , _bossHealth :: Int
  , _bossDamage :: Int
  , _effects :: [Effect]
  , _toPlay :: Role
  } deriving (Show)

data Effect = Effect
  { _effectType :: EffectType
  , _duration :: Int
  } deriving (Show)

data EffectType
  = Shield
  | Poison
  | Recharge
  deriving (Show, Eq)

data Attack = Attack
  { attackDamage :: Int
  , attackHealing :: Int
  } deriving (Show)

data Spell = Spell
  { name :: String
  , cost :: Int
  , result :: SpellResult
  } deriving (Show)

data SpellResult
  = AttackBoss Attack
  | StartEffect Effect
  deriving (Show)

data Role
  = Player
  | Boss
  deriving (Show, Eq)
  
makeLenses ''GameState
makeLenses ''Effect

playerShield :: GameState -> Int
playerShield gs
  | Shield `elem` activeEffectTypes gs = 7
  | otherwise = 0

applyEffects :: GameState -> Outcome
applyEffects gs = foldM (flip applyEffectType) gs (activeEffectTypes gs)

decrementEffectDurations :: GameState -> GameState
decrementEffectDurations = removeExpiredEffects . decrementDurations
  where
    decrementDurations = effects.traverse.duration -~ 1
    removeExpiredEffects = effects %~ filter notExpired
    notExpired e = e^.duration > 0

activeEffectTypes :: GameState -> [EffectType]
activeEffectTypes gs = gs^..effects.traverse.effectType

applyEffectType :: EffectType -> GameState -> Outcome
applyEffectType = \case
  Shield -> Right
  Recharge -> Right . (playerMana +~ 101)
  Poison -> checkForWinner . (bossHealth -~ 3)

checkForWinner :: GameState -> Outcome
checkForWinner gs
  | gs^.playerHealth <= 0 = Left Boss
  | gs^.bossHealth <= 0 = Left Player
  | otherwise = Right gs

availableSpells :: GameState -> [Spell]
availableSpells gs = filter spellIsAvailable spellBank
  where
    mana = gs^.playerMana
    aets = activeEffectTypes gs
    spellIsAvailable s
      | cost s > mana = False
      | otherwise = case result s of
          AttackBoss _ -> True
          StartEffect (Effect et _) -> et `notElem` aets

opponent :: Role -> Role
opponent = \case
  Player -> Boss
  Boss -> Player

spellBank :: [Spell]
spellBank =
  [ Spell "Magic Missile" 53 (AttackBoss (Attack 4 0))
  , Spell "Drain" 73 (AttackBoss (Attack 2 2))
  , Spell "Shield" 113 (StartEffect (Effect Shield 6))
  , Spell "Poison" 173 (StartEffect (Effect Poison 6))
  , Spell "Recharge" 229 (StartEffect (Effect Recharge 5))
  ]

initialGameState :: GameState
initialGameState = GameState
  { _playerHealth = 50
  , _playerMana = 500
  , _manaSpent = 0
  , _bossHealth = 51
  , _bossDamage = 9
  , _effects = []
  , _toPlay = Player
  }