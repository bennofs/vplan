module Data.VPlan.Combinators where

import Control.Lens hiding (act)
import Data.Foldable (foldMap)
import Data.Group
import Data.Monoid
import Data.Monoid.Action
import Data.Monoid.Coproduct
import Data.VPlan.Plan
import Data.VPlan.Range

-- | A plan with just one entry at the given index
on :: i -> v -> Plan i v
on i v = Plan [Entry v $ When EQ i $ Always True]

-- | Delete a given index. After calling this function, there are no 
-- entries at the given index anymore. Repeated entries see the deleted entry
-- like any other index with no entries.
delete :: (Group i, Ord i) => i -> Plan i v -> Plan i v
delete i p = p & entriesAt i . matcher %~ (And $ Not $ When EQ i $ Always True)

-- | Exclude some indices from a plan. Repeated entries skip over the excluded range, as
-- if those indices didn't exist.
exclude :: i -> i -> Plan i v -> Plan i v
exclude s = act . Transformation . inL . Exclude . return . Range s

-- | Swap the entries at two indices.
swap :: (Group i, Ord i) => i -> i -> Plan i v -> Plan i v
swap a b p = delete a (delete b p) <> foldMap (on b) v1 <> foldMap (on a) v2
  where v1 = p ^.. ix a
        v2 = p ^.. ix b

-- | Move on item to another location.
move :: (Group i, Ord i) => i -> i -> Plan i v -> Plan i v
move f t p = delete f p <> foldMap (on t) (p ^.. ix f)

-- | Repeat a plan after the given time.
every :: (Group i, Ord i) => i -> Plan i v -> Plan i v
every = act . Transformation . inR . Every . Product
