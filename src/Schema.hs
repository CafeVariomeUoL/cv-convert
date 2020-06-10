{-# LANGUAGE QuasiQuotes #-}

module Schema(Schema, ValidatorFailure, compileSchema, pValidatorFailure) where
import Data.Aeson(Value, ToJSON, encode)
import qualified Data.HashMap.Strict as HM
import           JSONSchema.Draft4
import JSONSchema.Validator.Draft4
import Data.String.Interpolate ( i )
import Data.Shorten
import Data.List.NonEmpty(toList)
import Data.List.Split(splitOn)
import Data.List(intercalate)
import Data.Char (isSpace)
import qualified Data.Set as Set

compileSchema :: Maybe Schema -> Either SchemaInvalid (Value -> [ValidatorFailure])
compileSchema (Just s) = checkSchema (URISchemaMap HM.empty) (SchemaWithURI s Nothing)
compileSchema Nothing = Right $ \_ -> []

shortenLen :: Int
shortenLen = 30


trunc :: ToJSON a => a -> String
trunc = shorten shortenLen . show . encode


possiblyInline :: String -> [String] -> [String]
possiblyInline s [] = [s]
possiblyInline s [x] = [s ++ " " ++ dropWhile isSpace x]
possiblyInline s xs = s:xs



prettyValidatorFailure :: ValidatorFailure -> [String]
prettyValidatorFailure (FailureMultipleOf (MultipleOfInvalid (MultipleOf _MultipleOf) _Scientific)) = [ [i|The value #{_Scientific} is not a multiple of #{_MultipleOf}|] ]
prettyValidatorFailure (FailureMaximum    (MaximumInvalid (Maximum _ _maximumValue) _Scientific)) = [ [i|The value #{_Scientific} is greater than the biggest allowed: #{_maximumValue}|] ]
prettyValidatorFailure (FailureMinimum    (MinimumInvalid (Minimum _ _minimumValue) _Scientific)) = [ [i|The value #{_Scientific} is smaller than the smallest allowed: #{_minimumValue}|] ]
prettyValidatorFailure (FailureMaxLength  (MaxLengthInvalid (MaxLength _MaxLength) _Text)) = [ [i|The length of "#{_Text}" is greater than allowed: #{_MaxLength}|] ]
prettyValidatorFailure (FailureMinLength  (MinLengthInvalid (MinLength _MinLength) _Text)) = [ [i|The length of "#{_Text}" is smaller than allowed: #{_MinLength}|] ]
prettyValidatorFailure (FailurePattern    PatternNotRegex) = [ "The given pattern is not a valid regular expression." ]
prettyValidatorFailure (FailurePattern    (PatternInvalid (PatternValidator _PatternValidator) _Text)) = [ [i|The string "#{_Text}" does not validate against the pattern: #{_PatternValidator}|] ]
prettyValidatorFailure (FailureMaxItems          (MaxItemsInvalid (MaxItems _MaxItems) _VectorValue)) = [ [i|The array "#{trunc _VectorValue}" is longer than allowed: #{_MaxItems}|] ]
prettyValidatorFailure (FailureMinItems          (MinItemsInvalid (MinItems _MinItems) _VectorValue)) = [ [i|The array "#{trunc _VectorValue}" is shorter than allowed: #{_MinItems}|] ]
prettyValidatorFailure (FailureUniqueItems       (UniqueItemsInvalid _VectorValue)) = [ [i|The array "#{trunc _VectorValue}" contains duplicate elements|] ]
prettyValidatorFailure (FailureItems             (ItemsObjectInvalid _ListIndexValidatorFailure)) = 
    [ "The following object attributes are invalid:" ] ++
        (concat $ map (\(idx, errs) -> possiblyInline (" - " ++ show idx ++ " :") $
            concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
        ) $ toList _ListIndexValidatorFailure)
prettyValidatorFailure (FailureItems             (ItemsArrayInvalid _ListIndexValidatorFailure)) = 
    [ "The following array elements are invalid:" ] ++
        (concat $ map (\(idx, errs) -> possiblyInline ("- " ++ show idx ++ " :") $
            concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
        ) $ toList _ListIndexValidatorFailure)
prettyValidatorFailure (FailureAdditionalItems   (AdditionalItemsBoolInvalid _ListIndexValue)) = 
    [ "This object contains additional attributes which are not specified in the schema:" ] ++ (map (\(idx, _) -> [i| - "#{idx}"|]) $ toList _ListIndexValue)
prettyValidatorFailure (FailureAdditionalItems   (AdditionalItemsObjectInvalid _ListIndexValidatorFailure)) = 
    [ "The following additional object attributes are invalid:" ] ++
        (concat $ map (\(idx, errs) -> possiblyInline ("- \"" ++ show idx ++ "\" : ") $
            concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
        ) $ toList _ListIndexValidatorFailure)
prettyValidatorFailure (FailureMaxProperties     (MaxPropertiesInvalid (MaxProperties _MaxProperties) _MapTextValue)) = [ [i|The number of properties in "#{trunc _MapTextValue}" is greater than allowed: #{_MaxProperties}|] ]
prettyValidatorFailure (FailureMinProperties     (MinPropertiesInvalid (MinProperties _MinProperties) _MapTextValue)) = [ [i|The number of properties in "#{trunc _MapTextValue}" is smaller than allowed: #{_MinProperties}|] ]
prettyValidatorFailure (FailureRequired          (RequiredInvalid (Required _) _SetText _MapTextValue)) =  [ [i|The required properties "#{_SetText}" are not present in: #{trunc _MapTextValue}|] ]
prettyValidatorFailure (FailureDependencies      f@(DependenciesInvalid _)) = splitOn "\n" $ show f
prettyValidatorFailure (FailurePropertiesRelated (PropertiesRelatedInvalid _prInvalidProperties _prInvalidPattern _prInvalidAdditional)) = 
    mkPrInvalidProperties ++ mkPrInvalidPattern ++ (mkPrInvalidAdditional _prInvalidAdditional)
    
    where
        mkPrInvalidProperties :: [String]
        mkPrInvalidProperties
            | HM.size _prInvalidProperties > 0 =
                let res = concat $ map (\(k, errs) -> 
                        if length errs > 0 then
                            possiblyInline ("- " ++ show k ++ " :") $
                            concat (map ((map ("  " ++)) . prettyValidatorFailure) errs)
                        else []) (HM.toList _prInvalidProperties) in
                if length res > 0 then "The following attributes are invalid:":res else []
            | otherwise = []

        mkPrInvalidPattern :: [String]
        mkPrInvalidPattern
            | HM.size _prInvalidPattern > 0 =
                let res = concat $ map (\((regex, k), errs) -> 
                        if length errs > 0 then
                            possiblyInline ("- " ++ show k ++ " (" ++ show regex ++ ") :") $
                            concat (map ((map ("  " ++)) . prettyValidatorFailure) errs)
                        else []) (HM.toList _prInvalidPattern) in
                if length res > 0 then "The following patterns are invalid:":res else []
            | otherwise = []

        mkPrInvalidAdditional Nothing = []
        mkPrInvalidAdditional (Just (APBoolInvalid _MapTextValue))
            | HM.size _MapTextValue > 0 = 
                [ "This object contains additional attributes which are not specified in the schema:" ] ++ (map (\(idx, _) -> [i| - "#{idx}"|]) $ HM.toList _MapTextValue)
            | otherwise = []
        mkPrInvalidAdditional (Just (APObjectInvalid _MapTextListValidatorFailure))
            | HM.size _MapTextListValidatorFailure > 0 = [ "The following additional attributes are invalid:" ] ++
                (concat $ map (\(k, errs) -> possiblyInline ("- " ++ show k ++ " :") $
                    concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
                ) $ HM.toList _MapTextListValidatorFailure)
            | otherwise = []

prettyValidatorFailure (FailureRef   (RefResolution _Text)) = [ [i|could not resolve ref #{_Text}|] ]
prettyValidatorFailure (FailureRef   (RefPointerResolution _JSONPointerError)) = [ [i|could not resolve ref: #{_JSONPointerError}|] ]
prettyValidatorFailure (FailureRef   f@(RefLoop _ _ _)) = splitOn "\n" $ show f
prettyValidatorFailure (FailureRef   (RefInvalid _Text _Value _ListValidatorFailure)) = 
    [ [i|Failed to validate against the schema '#{_Text}'.|] ] ++
    (concat $ map (prettyValidatorFailure) $ toList _ListValidatorFailure)
prettyValidatorFailure (FailureEnum  (EnumInvalid (EnumValidator _ListValue) _Value)) = [ [i|The value #{encode _Value} must be one of #{_ListValue}|] ]
prettyValidatorFailure (FailureType  (TypeValidatorInvalid (TypeValidatorString _SchemaType) _Value)) = [ [i|Expected #{encode _Value} to be of type #{prettySchemaType _SchemaType}|] ]
prettyValidatorFailure (FailureType  (TypeValidatorInvalid (TypeValidatorArray _SetSchemaType) _Value)) = [ [i|Expected #{encode _Value} to be of type #{intercalate "|" $ map prettySchemaType $ Set.toList _SetSchemaType}|] ]
prettyValidatorFailure (FailureAllOf (AllOfInvalid _ListIndexValidatorFailure)) = 
    [ "allOf error:" ] ++
        (concat $ map (\(idx, errs) -> possiblyInline ("- \"" ++ show idx ++ "\" :") $
            concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
        ) $ toList _ListIndexValidatorFailure)
prettyValidatorFailure (FailureAnyOf (AnyOfInvalid _ListIndexValidatorFailure)) = 
    [ "anyOf error:" ] ++
        (concat $ map (\(idx, errs) -> possiblyInline ("- \"" ++ show idx ++ "\" :") $
            concat (map ((map ("  " ++)) . prettyValidatorFailure) $ toList errs)
        ) $ toList _ListIndexValidatorFailure)
prettyValidatorFailure (FailureOneOf f@(TooManySuccesses _ _)) = splitOn "\n" $ show f
prettyValidatorFailure (FailureOneOf f@(NoSuccesses _ _)) = splitOn "\n" $ show f
prettyValidatorFailure (FailureNot   f@(NotValidatorInvalid _ _)) = splitOn "\n" $ show f

prettySchemaType :: SchemaType -> [Char]
prettySchemaType SchemaObject = "Object"
prettySchemaType SchemaArray = "Array"
prettySchemaType SchemaString = "String"
prettySchemaType SchemaNumber = "Number"
prettySchemaType SchemaInteger = "Integer"
prettySchemaType SchemaBoolean = "Boolean"
prettySchemaType SchemaNull = "Null"

pValidatorFailure :: ValidatorFailure -> [Char]
pValidatorFailure = intercalate "\n" . prettyValidatorFailure