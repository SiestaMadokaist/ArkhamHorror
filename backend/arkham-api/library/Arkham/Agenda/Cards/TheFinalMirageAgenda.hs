module Arkham.Agenda.Cards.TheFinalMirageAgenda (theFinalMirageAgenda) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Window (getBatchId)

newtype TheFinalMirageAgenda = TheFinalMirageAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalMirageAgenda :: AgendaCard TheFinalMirageAgenda
theFinalMirageAgenda = agenda (2, A) TheFinalMirageAgenda Cards.theFinalMirageAgenda (Static 12)

instance HasAbilities TheFinalMirageAgenda where
  getAbilities (TheFinalMirageAgenda a) =
    [ 
        restricted a 1 existInSetAside
        $ forced
        $ WouldPlaceDoomCounter #when #any #any
        , restricted a 2 (Criteria [not_ existInSetAside, exists inOutOfPlaySetAsideZone])
        $ forced
        $ WouldPlaceDoomCounter #when #any #any
    ]
    where
      existInSetAside = SetAsideCardExists $ cardIs Enemies.theNamelessMadness
      inOutOfPlaySetAsideZone = OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theNamelessMadness

instance RunMessage TheFinalMirageAgenda where
  runMessage msg a@(TheFinalMirageAgenda attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ IgnoreBatch batchId
      mleftmost <-
        selectOne
          $ FirstLocation
          $ map
            ((<> LocationWithInvestigator Anyone) . LocationWithLabel)
            ["theGateOfYquaa", "titanicRamp1", "titanicRamp2", "titanicRamp3", "titanicRamp4", "hiddenTunnel"]
      for_ mleftmost (createSetAsideEnemy_ Enemies.theNamelessMadness)
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (getBatchId -> batchId) _ -> do
      push $ IgnoreBatch batchId
      nameless <- select $ (OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theNamelessMadness)
      mleftmost <-
        selectOne
          $ FirstLocation
          $ map
            ((<> LocationWithInvestigator Anyone) . LocationWithLabel)
            ["theGateOfYquaa", "titanicRamp1", "titanicRamp2", "titanicRamp3", "titanicRamp4", "hiddenTunnel"]
      for_ mleftmost $ \leftMost -> do
        forM_ (take 1 nameless) $ \e -> do
          enemyMoveTo attrs e leftMost
      pure a
    _ -> TheFinalMirageAgenda <$> liftRunMessage msg attrs
