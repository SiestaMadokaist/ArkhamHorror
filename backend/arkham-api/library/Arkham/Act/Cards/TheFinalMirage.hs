module Arkham.Act.Cards.TheFinalMirage (theFinalMirage) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, hasModifier)
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher

newtype TheFinalMirage = TheFinalMirage ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalMirage :: ActCard TheFinalMirage
theFinalMirage = act (4, A) TheFinalMirage Cards.theFinalMirage Nothing

instance HasModifiersFor TheFinalMirage where
  getModifiersFor (TheFinalMirage a) = modifySelect a (enemyIs Enemies.theNamelessMadness) [AddKeyword Hunter]

instance HasAbilities TheFinalMirage where
  getAbilities (TheFinalMirage a) =
    extend a []

instance RunMessage TheFinalMirage where
  runMessage msg a@(TheFinalMirage attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hasAPlan <- getHasRecord DrKenslerHasAPlan
      eluded <- getHasRecord TheTruthOfTheMirageEludesYou
      collapsed <- hasModifier attrs (ScenarioModifier "collapsed")
      push
        $ if
          | collapsed && hasAPlan -> R1
          | collapsed && eluded -> R2
          | otherwise -> R3
      pure a
    _ -> TheFinalMirage <$> liftRunMessage msg attrs
