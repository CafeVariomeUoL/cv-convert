module JSON.Utils where

import           Data.Aeson            (Value(..), encode)
import           Data.Text             (Text)
import           Data.String.Conv      (toS)
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as S
import qualified Data.Vector           as V
import           Data.Scientific       (isInteger)
import           Data.UUID             as UUID
import           Data.UUID.V4          as UUID
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Catch   (MonadThrow(..))
import           Data.Foldable         (foldlM)

import           Runtime.Error         (SubjectIdNotFound(..))
import           Quickjs.Error         (InternalError(..))
import           DB                    (SubjectID(..))

getSubjectID :: MonadThrow m => Value -> m SubjectID
getSubjectID (Object m) = case HM.lookup "subject_id" m of
  Just x -> return $ SubjectID $ show x
  Nothing ->  throwM $ SubjectIdNotFound
getSubjectID o = throwM $ InternalError $ "Expected " ++ show o ++ " to be an object."


{-|
This function takes a JSON object and generates all paths to the leaf nodes, collecting all the leaf values in a set. E.g. given:

>{
>  "name": "Sam",
>  "age": 25.5,
>  "address": {
>    "street": "Middle",
>    "house_no": 26,
>    "city": "Towcester"
>  }
>}

'createAllPathsWithValues' produces the following map:

>{"name": "str"} -> {"Sam"},
>{"age": "float"} -> {25.5},
>{"address": {"street": "str"}} -> {"Middle"},
>{"address": {"house_no": "int"}} -> {26},
>{"address": {"city": "str"}} -> {"Towcester"}

We return a set of value, in case we encounter the same path at in different objects within the same array:

>[
>  {"name": "Alice", "age": 27}
>  {"name": "John", "age": 25},
>]


Here, 'createAllPathsWithValues' outputs:

>[{"name": "str"}] -> {"Alice", "John"},
>[{"age": "int"}] -> {27, 25},

-}
createAllPathsWithValues :: Value -> HM.HashMap Value (S.HashSet Value)
createAllPathsWithValues (Object o) = 
  HM.foldlWithKey' (\acc k v -> 
      HM.unionWith
        (S.union)
        (HM.foldlWithKey' (\acc' k' v' -> HM.insert (Object $ HM.fromList [(k,k')]) v' acc') HM.empty $ createAllPathsWithValues v)
        acc
    ) HM.empty o
createAllPathsWithValues (Array a) = V.foldl' (\acc v -> 
      HM.unionWith
        (S.union)
        (createAllPathsWithValues v)
        acc
    ) HM.empty a
createAllPathsWithValues o@(String _) = HM.singleton (String "str") (S.singleton o)
createAllPathsWithValues o@(Number n)
  | isInteger n = HM.singleton (String "int") (S.singleton o)
  | otherwise = HM.singleton (String "float") (S.singleton o)
createAllPathsWithValues o@(Bool _) = HM.singleton (String "bool") (S.singleton o)
createAllPathsWithValues Null = HM.singleton (String "null") (S.singleton Null)




{-|
This function takes a JSON object and flattens it into a list of EAV triples. 
The entities are given a uniquely generated UUID, to prevent any clashes with data
already in the EAV database. As a result, running this function repeatedly will generate
different output, since the UUIDs are generated at random.

For example, given:

>{
>  "name": "Sam",
>  "age": 25.5,
>  "address": {
>    "street": "Middle",
>    "house_no": 26,
>    "city": "Towcester"
>  }
>}

'flattenToEAV' returns the UUID of the root object and a list of triples, 
encoding the original object:

>(
>  60bcf653-fc8e-40dd-bb15-c332a99969da,
>  [
>    (60bcf653-fc8e-40dd-bb15-c332a99969da, "name", "Sam"),
>    (60bcf653-fc8e-40dd-bb15-c332a99969da, "age", "25.5"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "parent_uuid", "60bcf653-fc8e-40dd-bb15-c332a99969da"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "parent_attr", "address"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "city", "Towcester"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "street", "Middle"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "house_no", "26")
>  ]
>)

Nesting information is preserved by including the @parent_uuid@ and @parent_attr@ triples:

>(<child object UUID>, "parent_uuid", <parent UUID>),
>(<child object UUID>, "parent_attr", <attribute name>)
-}
flattenToEAV :: MonadIO m => Value -> m (UUID.UUID, [(UUID.UUID, Text, Text)])
flattenToEAV (Object o) = do
  uuid <- liftIO $ UUID.nextRandom
  res <- foldlM (\acc (attr,val) -> do
      (_,r) <- f uuid attr val
      return $ r ++ acc
    ) [] $ HM.toList o
  return $ (uuid, res)
  where
    f :: MonadIO m => UUID.UUID -> Text -> Value -> m (UUID.UUID, [(UUID.UUID, Text, Text)])
    f uuid attr val@(Object _) = do
      (uuid', res) <- flattenToEAV val
      return $ (uuid, (uuid', "parent_uuid", UUID.toText uuid) : (uuid', "parent_attr", attr) : res)
    f uuid attr (Array as) = do
      res <- foldlM (\acc v -> do
          (_,r) <- f uuid attr v 
          return $ r ++ acc
        ) [] as
      return (uuid, res)
    f uuid attr (String val) = pure (uuid, [(uuid, attr, val)])
    f uuid attr val = pure (uuid, [(uuid, attr, toS $ encode val)])
flattenToEAV _ = undefined