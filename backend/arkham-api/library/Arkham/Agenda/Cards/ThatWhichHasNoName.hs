module Arkham.Agenda.Cards.ThatWhichHasNoName (thatWhichHasNoName) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCard, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Window (getBatchId)
import Debug.Trace qualified as Debug

newtype ThatWhichHasNoName = ThatWhichHasNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thatWhichHasNoName :: AgendaCard ThatWhichHasNoName
thatWhichHasNoName = agenda (2, A) ThatWhichHasNoName Cards.thatWhichHasNoName (Static 12)

instance HasAbilities ThatWhichHasNoName where
  getAbilities (ThatWhichHasNoName a) =
    [ restricted a 1 (SetAsideCardExists $ cardIs Enemies.theNamelessMadness)
        $ forced
        $ WouldPlaceDoomCounter #when #any #any
    , restricted a 2 (EnemyCount 15 $ enemyIs Enemies.theNamelessMadness) $ forced $ RoundEnds #when
    ]

instance RunMessage ThatWhichHasNoName where
  runMessage msg a@(ThatWhichHasNoName attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- selectCount $ LocationWithModifier $ ScenarioModifier "collapsed"
      -- should be 15
      -- alreadySpawned <- select $ enemyIs Enemies.theNamelessMadness
      -- Debug.traceM $ "alreadySpawned" <> show alreadySpawned
      let toKeepCount = 15 - (n * 3)
      Debug.traceM $ "toKeep" <> show toKeepCount
      -- createSetAsideEnemy_ Enemies.theNamelessMadness =<< selectJust (OutOfPlay SetAsideZone)
      allNameless <- select $ enemyIs Enemies.theNamelessMadness
      forM_ (take toKeepCount allNameless) (`place` (OutOfPlay RemovedZone))
      forM_ (drop toKeepCount allNameless) (`place` (OutOfPlay SetAsideZone))
      doStep 1 msg
      eachInvestigator (discardAllClues attrs)
      eachInvestigator (`place` Unplaced)
      selectEach (not_ $ locationIs Locations.theGateOfYquaa) removeLocation
      push $ SetLayout ["theGateOfYquaa titanicRamp1 titanicRamp2 titanicRamp3 titanicRamp4 hiddenTunnel"]
      placeRandomLocationGroup "titanicRamp" =<< getSetAsideCardsMatching "Titanic Ramp"
      placeSetAsideLocation_ Locations.hiddenTunnelAWayOut
      doStep 2 msg
      act <- getSetAsideCard Acts.theFinalMirage
      nextAgenda <- getSetAsideCard Cards.theFinalMirageAgenda
      push $ SetCurrentActDeck 1 [act]
      push $ SetCurrentAgendaDeck 1 [nextAgenda]
      toDiscard GameSource attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      pure a
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      connectLocations "theGateOfYquaa" "titanicRamp1"
      connectLocations "titanicRamp1" "titanicRamp2"
      connectLocations "titanicRamp2" "titanicRamp3"
      connectLocations "titanicRamp3" "titanicRamp4"
      connectLocations "titanicRamp4" "hiddenTunnel"
      firstRamp <- selectJust $ LocationWithLabel "titanicRamp1"
      keptInRemoved <- select $ (OutOfPlayEnemy RemovedZone $ enemyIs Enemies.theNamelessMadness)
      eachInvestigator (\iid -> moveTo attrs iid firstRamp)
      forM_ keptInRemoved $ \e -> do
        enemyMoveTo attrs e firstRamp
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ IgnoreBatch batchId
      ls <-
        select
          $ NearestLocationToAny
          $ not_ (LocationWithEnemy $ enemyIs Enemies.theNamelessMadness)
          <> ConnectedTo (LocationWithEnemy $ enemyIs Enemies.theNamelessMadness)
      chooseTargetM iid ls $ createSetAsideEnemy_ Enemies.theNamelessMadness
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      push $ AdvanceAgenda attrs.id
      pure a
    _ -> ThatWhichHasNoName <$> liftRunMessage msg attrs