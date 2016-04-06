{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy

type Outcome = Either Result GameState

type Result = (Role, Int)

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
  , action :: SpellAction
  } deriving (Show)

data SpellAction
  = AttackBoss Attack
  | StartEffect Effect
  deriving (Show)

data Role
  = Player
  | Boss
  deriving (Show, Eq)
  
makeLenses ''GameState
makeLenses ''Effect

cheapestWin :: GameState -> Int
cheapestWin gs = execState (findCheapestWin gs) maxBound

findCheapestWin :: GameState -> State Int ()
findCheapestWin gs = pruneTree gs $
  case applyEffects gs of
    Left r -> recordResult r
    Right gs' -> forM_ (performTurn gs') $ \case
      Left r -> recordResult r
      Right gs'' -> findCheapestWin $ endTurn gs''

pruneTree :: GameState -> State Int () -> State Int ()
pruneTree gs a = do
  m <- get
  if gs^.manaSpent >= m
    then return ()
    else a

recordResult :: Result -> State Int ()
recordResult = \case
  (Boss, _) -> return ()
  (Player, m') -> do
    m <- get
    if m' < m
      then put m'
      else return ()

performTurn :: GameState -> [Outcome]
performTurn gs = case gs^.toPlay of
  Boss -> [bossAttack gs]
  Player -> flip castSpell gs <$> availableSpells gs

castSpell :: Spell -> GameState -> Outcome
castSpell (Spell _ c a) = applySpellAction a . (manaSpent +~ c) . (playerMana -~ c)

applySpellAction :: SpellAction -> GameState -> Outcome
applySpellAction = \case
  AttackBoss (Attack d h) -> checkForWinner . (playerHealth +~ h) . (bossHealth -~ d)
  StartEffect e -> Right . (effects %~ (e:))

bossAttack :: GameState -> Outcome
bossAttack gs = checkForWinner . (playerHealth -~ damage) $ gs
  where
    shield = playerShield gs
    damage = nonNeg $ gs^.bossDamage - shield
    nonNeg x
      | x < 0 = 0
      | otherwise = x

playerShield :: GameState -> Int
playerShield gs
  | Shield `elem` activeEffectTypes gs = 7
  | otherwise = 0

applyEffects :: GameState -> Outcome
applyEffects gs = foldM (flip applyEffectType) gs (activeEffectTypes gs)

advanceEffects :: GameState -> GameState
advanceEffects = removeExpiredEffects . decrementDurations
  where
    decrementDurations = effects.traverse.duration -~ 1
    removeExpiredEffects = effects %~ filter (not . expired)
    expired e = e^.duration < 0

activeEffectTypes :: GameState -> [EffectType]
activeEffectTypes gs = gs^..effects.traverse.effectType

applyEffectType :: EffectType -> GameState -> Outcome
applyEffectType = \case
  Shield -> Right
  Recharge -> Right . (playerMana +~ 101)
  Poison -> checkForWinner . (bossHealth -~ 3)

checkForWinner :: GameState -> Outcome
checkForWinner gs
  | gs^.playerHealth <= 0 = Left (Boss, gs^.manaSpent)
  | gs^.bossHealth <= 0 = Left (Player, gs^.manaSpent)
  | otherwise = Right gs

availableSpells :: GameState -> [Spell]
availableSpells gs = filter spellIsAvailable spellBank
  where
    mana = gs^.playerMana
    aets = activeEffectTypes gs
    spellIsAvailable s
      | cost s > mana = False
      | otherwise = case action s of
          AttackBoss _ -> True
          StartEffect (Effect et _) -> et `notElem` aets

endTurn :: GameState -> GameState
endTurn = (toPlay %~ opponent) . advanceEffects

opponent :: Role -> Role
opponent = \case
  Player -> Boss
  Boss -> Player

spellBank :: [Spell]
spellBank =
  [ Spell "Magic Missile" 53 (AttackBoss (Attack 4 0))
  , Spell "Drain" 73 (AttackBoss (Attack 2 2))
  , Spell "Poison" 173 (StartEffect (Effect Poison 6))
  , Spell "Recharge" 229 (StartEffect (Effect Recharge 5))
  , Spell "Shield" 113 (StartEffect (Effect Shield 6))
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