{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-- | Provides a Map data type based on functions and a list of keys.
module Core.FunctionMap where

import qualified Data.List.Utils as U
import qualified Data.List as L
import Data.Key
import Data.Maybe
import Control.Applicative hiding (empty)
import Control.Monad hiding (sequence, join)
import Control.Lens hiding (Indexable)
import Data.Ord (comparing)
import Data.Foldable hiding (toList)
import Data.Traversable
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Monoid
import Prelude hiding (foldr, foldr1, sequence, foldl, foldl1, zip, lookup)

-- | The map data type. It contains a list of keys and a function from keys to values.
newtype Map k a = Map (k -> Maybe a, [k])

-- | An ISO from Map to a pair of (k -> Maybe a, [k])
mapIso :: Iso (k -> Maybe a, [k]) (k -> Maybe b, [k]) (Map k a) (Map k b)
mapIso = iso Map $ \(Map x) -> x

-- | Lens for they keys of a map
keys :: Lens' (Map k a) [k]
keys = from mapIso . _2

-- | Lookup a key in a sorted association list.
lookupSorted :: (Ord a, Eq a) => a -> [(a,b)] -> Maybe b
lookupSorted x ((k,v):xs)
  | x == k = Just v
  | x > k = lookupSorted x xs
  | otherwise = Nothing
lookupSorted _ _ = Nothing

-- | Lens for the mapping function of a map
mappingFunction :: Lens (Map k a) (Map k b) (k -> Maybe a) (k -> Maybe b)
mappingFunction = from mapIso . _1

instance Functor (Map k) where
  fmap f m = m & mappingFunction %~ (fmap f . )

instance (Eq k, Ord k) => Traversable (Map k) where
  sequenceA m = (set mappingFunction ?? m) <$> modifyMapping (m ^. keys) (m ^. mappingFunction)
    where modifyMapping ks f = flip lookupSorted . zip ks <$> sequenceA (mapMaybe f ks)
  
instance (Eq k, Ord k) => Foldable (Map k) where
  foldMap = foldMapDefault

instance FunctorWithIndex k (Map k) where
  imap f m = m & mappingFunction .~ mapNew
    where mapNew i = fmap (f i) (lookup i m)
          
instance (Ord k) => FoldableWithIndex k (Map k) where
  ifoldMap = ifoldMapOf itraversed
  
instance (Ord k) => TraversableWithIndex k (Map k) where
  itraverse f m = sequenceA $ imap f m
  
instance (Monoid a, Ord k) => Monoid (Map k a) where
  mempty = empty
  mappend = unionWith ((<>))
  
type instance Key (Map k) = k
  
instance Lookup (Map k) where
  lookup k m = view mappingFunction m k
  
instance Keyed (Map k) where
  mapWithKey = imap
  
instance (Eq k) => Zip (Map k) where
  zipWith f m n = view mapIso (mappingFunc, (m ^. keys) `L.intersect` (n ^. keys))
    where mappingFunc i = f <$> lookup i m <*> lookup i n
          
instance (Eq k) => Apply (Map k) where
  (<.>) = zap
  
instance (Eq k) => Bind (Map k) where 
  join ms = ms & mappingFunction %~ newMap
    where newMap u i = u i >>= lookup i

instance (Eq k) => ZipWithKey (Map k)

instance Indexable (Map k) where
  index m i = fromMaybe (error "FunctionMap.index: given key is not an element of the map") $ lookup i m
  
instance (Eq k) => Adjustable (Map k) where
  adjust f i m = m & mappingFunction %~ ad
    where ad u i' = if i' == i then fmap f (u i') else u i'

instance (Eq k, Ord k) => FoldableWithKey (Map k) where
  foldrWithKey = ifoldrOf itraversed
  
instance (Eq k, Ord k) => TraversableWithKey (Map k) where
  traverseWithKey = itraverse
  
type instance Index (Map k a) = k
type instance IxValue (Map k a) = a
  
instance (Applicative f, Ord c) => Each f (Map c a) (Map c b) a b where
  each f = itraverse f'
    where f' = indexed f
          
instance (Ord k) => At (Map k a) where
  at k f m = indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
    where mv = lookup k m

instance (Gettable f, Ord k) => Contains f (Map k a) where
  contains = containsLookup (\k m -> lookup k m)

instance (Ord k, Applicative f) => Ixed f (Map k a) where
  ix k f m = case lookup k m of 
    Just v -> indexed f k v <&> \v' -> insert k v' m
    Nothing -> pure m

-- | Alias for 'difference'
-- (\\) :: Map k a -> Map k a -> Map k a
-- (\\) = difference

-- | Is the map empty?
null :: Map k a -> Bool
null = nullOf keys

-- | Return the number of elements in a map
size :: Map k a -> Int
size = lengthOf keys

-- | Check whether some key is a member of the map.
member :: Ord k => k -> Map k a -> Bool
member = view . contains

-- | Check whether some key is not existent in the map.
notMember :: Ord k => k -> Map k a -> Bool
notMember = fmap not . member

-- | Find a value, and if it is not found, return a default value.
findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault d k = fromMaybe d . view (at k)

-- | Construct an empty map.
empty :: Map k a
empty = view mapIso (const Nothing, [])

-- | A map with only one element.
singleton :: (Eq k) => k -> a -> Map k a
singleton k v = view mapIso ((v <$) . guard . (== k), [k]) 

-- | Insert a value into the map at the given key. If they key is already present, its value is replaced.
insert :: Ord k => k -> a -> Map k a -> Map k a
insert k v m = m & keys %~ L.insert k
                 & mappingFunction %~ \f i -> v <$ guard (i == k) <|> f i

-- | Insert a value into a map, using the provided function to combine the values if the key is already present. The provided
-- function is called with the new value as first argument and the old value as second one.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k v  = at k %~ Just . maybe v (f v)

-- | Delete a key and its value from the map. The map is left unmodified if the key is not present.
delete :: Ord k => k -> Map k a -> Map k a
delete x m = m & keys %~ L.delete x 
               & mappingFunction %~ \f i -> guard (i /= x) >> f i
                                            
-- | Update a value at a given key or remove it if the function returns Nothing.
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k = at k %~ (>>= f)

-- | Alters the value or the absence of the element at the given ḱey.
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = flip $ over . at

-- | Union two maps. The resulting map contains the keys of both m and n. The values of m are prefered if the keys are the same.
union :: Ord k => Map k a -> Map k a -> Map k a
union = unionWith const

-- | Union with a combining function for keys that are present in both maps.
unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith c = unionWithKey (const c)

-- | Union with a combining function that also receives the key as input.
unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey c m n = m & mappingFunction %~ newMap & keys %~ L.map L.head . L.group . U.merge (n ^. keys)
  where newMap u i = (c i <$> u i <*> v i) <|> u i <|> v i
        v = n ^. mappingFunction

-- | Difference of two maps. Return elements of map m that don't exist in map n.
difference :: Ord k => Map k a -> Map k a -> Map k a
difference m n = m & mappingFunction %~ newMap
  where newMap u i = if n ^. contains i then Nothing else u i
        
-- | Difference using a combining function when two keys exist in both maps. If it returns Nothing, the key is discared, otherwise, the 
-- the value in the Just is used as new value.
differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith = differenceWithKey . const

-- | Difference using a combining function when two keys exist in both maps, which also receives the key. If it returns Nothing, the
-- element is discared, otherwise the value in the Just is used as the element's value.
differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f m n = m & mappingFunction %~ newMap
  where newMap u i = if n ^. contains i then join $ liftM2 (f i) (u i) (n ^. at i) else u i


-- | Build a map from a list. The precondition that each key should only appear once is not checked (first key is used). This function can't deal with infinite lists. 
fromList :: (Ord k) => [(k,a)] -> Map k a
fromList l = fromAscList l'
  where l' = L.sortBy (comparing fst) l 

-- | Build a map from a sorted list. The precondition that each key should appear only once and that the list is sorted is not checked. This function can deal with ininite list.

fromAscList :: (Ord k) => [(k,a)] -> Map k a
fromAscList l = view mapIso ((`lookupSorted` l), L.map fst l) 

-- | Convert a map to a list. The list will be ordered ascending.
toList :: (Ord k) => Map k a -> [(k,a)]
toList m = m ^@.. itraversed

-- | An iso for the conversation from and to lists.
assoc :: (Ord k, Ord l) => Iso (Map k a) (Map l b) [(k,a)] [(l,b)] 
assoc = iso toList fromAscList

-- | Map a function over the map. This is just an alias for fmap
map :: (a -> b) -> Map k a -> Map k b
map = fmap