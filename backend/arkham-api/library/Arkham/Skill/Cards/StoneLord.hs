module Arkham.Skill.Cards.StoneLord (stoneLord) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype StoneLord = StoneLord SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneLord :: SkillCard StoneLord
stoneLord = skill StoneLord Cards.stoneLord

instance HasModifiersFor StoneLord where
  getModifiersFor (StoneLord attrs) = case attrs.placement of
    Unplaced -> modified_ attrs attrs.owner [AnySkillValue (-1)]
    _ -> pure ()

instance RunMessage StoneLord where
  runMessage msg s@(StoneLord attrs) = runQueueT $ case msg of
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      selectEach (assetControlledBy attrs.owner) \aid -> push $ Exhaust (toTarget aid)
      roundModifiers attrs attrs.owner [ControlledAssetsCannotReady]
      pure s
    _ -> StoneLord <$> liftRunMessage msg attrs
