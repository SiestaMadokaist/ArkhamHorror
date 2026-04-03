module Arkham.Asset.Assets.Boo (boo) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getCommittableCards)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Boo = Boo AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boo :: AssetCard Boo
boo = allyWith Boo Cards.boo (1, 1) noSlots

instance HasAbilities Boo where
  getAbilities (Boo a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (WouldHaveSkillTestResult #when You AnySkillTest #success) (exhaust a) []
    , mkAbility a 2 $ SilentForcedAbility $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage Boo where
  runMessage msg a@(Boo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      committable <- getCommittableCards iid
      when (notNull committable) do
        chooseOneM iid do
          targets committable \card -> do
            push $ SkillTestCommitCard iid card
            drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      damage <- field AssetDamage (toId attrs)
      horror <- field AssetHorror (toId attrs)
      when (damage > 0) $ assignDamage iid (toSource attrs) damage
      when (horror > 0) $ assignHorror iid (toSource attrs) horror
      returnToHand iid attrs
      pure a
    _ -> Boo <$> liftRunMessage msg attrs
