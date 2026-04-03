module Arkham.Asset.Cards.BaldursGateIII where

import Arkham.Asset.Cards.Import

boo :: CardDef
boo =
  signature "xbg3016"
    $ fast
    $ (asset "xbg3017" ("Boo" <:> "Miniature Giant Space Hamster") 2 Neutral)
      { cdCardTraits = setFromList [Ally, Creature]
      , cdUnique = True
      }
