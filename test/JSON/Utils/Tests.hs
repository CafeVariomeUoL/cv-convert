module JSON.Utils.Tests where
import           Test.Tasty              (TestTree, testGroup, adjustOption)
import           Test.Tasty.HUnit        (testCase)
import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=))
import qualified Test.QuickCheck         as QC
import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..), encode)
-- import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.Catch     (SomeException, MonadCatch(..))
import           Data.Text               (Text)
import           Data.String.Conv        (toS)
import qualified Data.UUID               as UUID
import qualified Data.HashMap.Strict     as HM
import qualified Data.HashSet            as S
import qualified Data.Vector             as V
import           Data.List               (intercalate, find)
import           Data.Maybe              (fromJust)
import           Data.Hashable           (Hashable)
import           GHC.Generics            (Generic)
 
import           Quickjs.Tests           (genText, genVal)
import           JSON.Utils




unflattenFromEAV :: UUID.UUID -> [(UUID.UUID, Text, Text)] -> Value
unflattenFromEAV rootUUID lst = Object $ rootAttrsFlat --`HM.union` rootAttrsNested

  where
    rootAttrsFlat = foldr (\(k,v) m -> 
        case HM.lookup k m of
          Nothing -> HM.insert k v m
          Just (Array xs) -> HM.insert k (Array $ V.cons v xs) m
          Just x -> HM.insert k (Array $ V.fromList [v, x]) m) 
      rootAttrsNested 
      (map (\(_,k,v) -> (k, String v)) $ filter (\(uuid,k,_) -> rootUUID == uuid && k /= "parent_uuid" && k /= "parent_attr" ) lst)
    
    rootUUIDStr = UUID.toText rootUUID

    filteredChildrenUUIDs = map (\(uuid,_,_) -> uuid) $ filter (\(_,k,v) -> k == "parent_uuid" && rootUUIDStr == v) lst
    findAttrName childUUID = (\(_,_,v) -> v) $ fromJust $ find (\(uuid,k,_) -> k == "parent_attr" && childUUID == uuid) lst
    
    rootAttrsNested = foldr 
      (\childUUID m -> 
        let 
          attrName = findAttrName childUUID 
          unflattenChild = unflattenFromEAV childUUID lst
        in
          case HM.lookup attrName m of
            Nothing -> HM.insert attrName unflattenChild m
            Just (Array xs) -> HM.insert attrName (Array $ V.cons unflattenChild xs) m
            Just x -> HM.insert attrName (Array $ V.fromList [unflattenChild, x]) m) 
      HM.empty 
      filteredChildrenUUIDs



data EAVValue =
    EAVObject (HM.HashMap Text EAVValue)
  | EAVSet (S.HashSet EAVValue)
  | EAVString Text 
  deriving ( Eq, Generic, Hashable)

instance Show EAVValue where
  show (EAVObject o) = 
    "{" ++ (intercalate "," $ map (\(k, v) -> show k ++ ":" ++ show v) $ HM.toList o) ++ "}"
  show (EAVSet s) = 
    "{{" ++ (intercalate "," $ map show $ S.toList s) ++ "}}"
  show (EAVString s) = show s
   

-- instance Eq (EAVValue a) where
--   EAVString a == EAVString b = a == b
--   EAVObject (a :: HM.HashMap Text (EAVValue a)) == EAVObject (b :: HM.HashMap Text (EAVValue a)) = a == b

toEAVValueObject :: Value -> EAVValue
toEAVValueObject (Object o) = EAVObject $ HM.mapMaybe (\v -> 
  case v of
    Array a -> process $ processLst $ V.toList a
    _ -> case toEAVValueObject v of
      EAVObject res | HM.size res == 0 -> Nothing
      x -> Just x) o
  where
    processLst [] = []
    processLst (Array xs:ys) = processLst $ V.toList xs ++ ys
    processLst (x:ys) = case toEAVValueObject x of
      EAVObject res | HM.size res == 0 -> processLst ys
      x' -> x' : processLst ys

    process [] = Nothing
    process [x] = Just x
    process xs = Just $ EAVSet $ S.fromList xs

toEAVValueObject (Array _) = undefined
toEAVValueObject (String s) = EAVString s
toEAVValueObject x = EAVString $ toS $ encode x


equiv :: Value -> Value -> Bool
equiv o1 o2 = toEAVValueObject o1 == toEAVValueObject o2


newtype ToplevelObjectValue = ToplevelObjectValue Value

instance Show ToplevelObjectValue where
  show (ToplevelObjectValue v) = toS $ encode v


genToplevelObjectVal n = do 
  k <- QC.choose (0,n) 
  Object . HM.fromList <$> (zip <$> QC.vectorOf k genText <*> QC.vectorOf k genVal')
  where genVal' = genVal (n `div` 2)

instance QC.Arbitrary ToplevelObjectValue where
  arbitrary = ToplevelObjectValue <$> QC.sized genToplevelObjectVal



flatten_unflatten_equiv :: ToplevelObjectValue -> QC.Property    
flatten_unflatten_equiv (ToplevelObjectValue val) = QC.monadicIO $ do
  (root, eavs) <- flattenToEAV val
  
  QC.assert $ val `equiv` unflattenFromEAV root eavs

tests :: TestTree
tests = 
  -- adjustOption (\_ -> QuickCheckTests 10) $
  -- adjustOption (\_ -> QuickCheckVerbose True) $  
  testGroup "JSON.Utils"
    [ testProperty "flattening to EAV and unflattening gives back same structure modulo flatteining of (lists of) lists to sets" flatten_unflatten_equiv
    ]