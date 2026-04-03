module Arkham.Investigator.Cards.Minsc (minsc) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Capability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype Minsc = Minsc InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

minsc :: InvestigatorCard Minsc
minsc =
  startsWith [Assets.boo]
    $ investigator Minsc Cards.minsc
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 1, combat = 5, agility = 4}

instance HasAbilities Minsc where
  getAbilities (Minsc x) =
    [ playerLimit PerRound
        $ restrictedAbility x 1 (Self <> youExist can.gain.resources)
        $ freeReaction
        $ SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2)
    ]

instance HasChaosTokenValue Minsc where
  getChaosTokenValue iid ElderSign (Minsc attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage Minsc where
  runMessage msg i@(Minsc attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 1
      pure i
    ElderSignEffect (is attrs -> True) -> do
      whenM (can.have.assets.ready attrs.id)
        $ selectEach (assetIs Assets.boo) ready
      pure i
    _ -> Minsc <$> liftRunMessage msg attrs
