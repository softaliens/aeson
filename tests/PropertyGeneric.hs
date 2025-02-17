{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PropertyGeneric ( genericTests ) where

import Prelude.Compat

#if !MIN_VERSION_base(4,16,0)
import Data.Semigroup (Option(..))
#endif
import Encoders
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
#if !MIN_VERSION_base(4,16,0)
import Test.QuickCheck ( (===) )
import Types
#endif
import PropUtils


genericTests :: TestTree
genericTests =
  testGroup "generic" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . gNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . gNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isNullaryTaggedObject . gNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gNullaryToJSONObjectWithSingleField)
          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON gNullaryParseJSONString gNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON gNullaryParseJSON2ElemArray gNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON gNullaryParseJSONTaggedObject gNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON gNullaryParseJSONObjectWithSingleFieldTagged gNullaryToJSONObjectWithSingleFieldTagged)
            , testProperty "ObjectWithSingleField Tagged" (toParseJSON gNullaryParseJSONObjectWithSingleFieldTagged gNullaryToJSONObjectWithSingleFieldTagged)
            ]
        ]
      , testGroup "EitherTextInt" [
          testProperty "UntaggedValue" (isUntaggedValueETI . gEitherTextIntToJSONUntaggedValue)
        , testProperty "roundtrip" (toParseJSON gEitherTextIntParseJSONUntaggedValue gEitherTextIntToJSONUntaggedValue)
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . gSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . gSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON gSomeTypeParseJSON2ElemArray gSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON gSomeTypeParseJSONTaggedObject gSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField gSomeTypeToJSONObjectWithSingleField)
          , testProperty "ObjectWithSingleField Tagged" (toParseJSON gSomeTypeParseJSONObjectWithSingleFieldTagged gSomeTypeToJSONObjectWithSingleFieldTagged)

          , testProperty "2ElemArray unary" (toParseJSON1 gSomeTypeLiftParseJSON2ElemArray gSomeTypeLiftToJSON2ElemArray)
          , testProperty "TaggedObject unary" (toParseJSON1 gSomeTypeLiftParseJSONTaggedObject gSomeTypeLiftToJSONTaggedObject)
          , testProperty "ObjectWithSingleField unary" (toParseJSON1 gSomeTypeLiftParseJSONObjectWithSingleField gSomeTypeLiftToJSONObjectWithSingleField)
          , testProperty "ObjectWithSingleField Tagged unary" (toParseJSON1 gSomeTypeLiftParseJSONObjectWithSingleFieldTagged gSomeTypeLiftToJSONObjectWithSingleFieldTagged)
          ]
        ]
      , testGroup "OneConstructor" [
          testProperty "default" (isEmptyArray . gOneConstructorToJSONDefault)
        , testProperty "Tagged"  (isTaggedObject . gOneConstructorToJSONTagged)
        , testGroup "roundTrip" [
            testProperty "default" (toParseJSON gOneConstructorParseJSONDefault gOneConstructorToJSONDefault)
          , testProperty "Tagged"  (toParseJSON gOneConstructorParseJSONTagged  gOneConstructorToJSONTagged)
          ]
        ]
#if !MIN_VERSION_base(4,16,0)
      , testGroup "OptionField" [
          testProperty "like Maybe" $
          \x -> gOptionFieldToJSON (OptionField (Option x)) === thMaybeFieldToJSON (MaybeField x)
        , testProperty "roundTrip" (toParseJSON gOptionFieldParseJSON gOptionFieldToJSON)
        ]
#endif
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        gNullaryToJSONString `sameAs` gNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        gNullaryToJSON2ElemArray `sameAs` gNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        gNullaryToJSONTaggedObject `sameAs` gNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        gNullaryToJSONObjectWithSingleField `sameAs`
        gNullaryToEncodingObjectWithSingleField
      -- , testProperty "ApproxUnwrap" $
      --   gApproxToJSONUnwrap `sameAs` gApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        gApproxToJSONDefault `sameAs` gApproxToEncodingDefault

      , testProperty "EitherTextInt UntaggedValue" $
        gEitherTextIntToJSONUntaggedValue `sameAs` gEitherTextIntToEncodingUntaggedValue

      , testProperty "SomeType2ElemArray" $
        gSomeTypeToJSON2ElemArray `sameAs` gSomeTypeToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary" $
        gSomeTypeLiftToJSON2ElemArray `sameAs1` gSomeTypeLiftToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary agree" $
        gSomeTypeToEncoding2ElemArray `sameAs1Agree` gSomeTypeLiftToEncoding2ElemArray

      , testProperty "SomeTypeTaggedObject" $
        gSomeTypeToJSONTaggedObject `sameAs` gSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary" $
        gSomeTypeLiftToJSONTaggedObject `sameAs1` gSomeTypeLiftToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary agree" $
        gSomeTypeToEncodingTaggedObject `sameAs1Agree` gSomeTypeLiftToEncodingTaggedObject

      , testProperty "SomeTypeObjectWithSingleField" $
        gSomeTypeToJSONObjectWithSingleField `sameAs` gSomeTypeToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary" $
        gSomeTypeLiftToJSONObjectWithSingleField `sameAs1` gSomeTypeLiftToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary agree" $
        gSomeTypeToEncodingObjectWithSingleField `sameAs1Agree` gSomeTypeLiftToEncodingObjectWithSingleField

      , testProperty "SomeTypeOmitNothingFields" $
        gSomeTypeToJSONOmitNothingFields `sameAs` gSomeTypeToEncodingOmitNothingFields

      , testProperty "OneConstructorDefault" $
        gOneConstructorToJSONDefault `sameAs` gOneConstructorToEncodingDefault
      , testProperty "OneConstructorTagged" $
        gOneConstructorToJSONTagged `sameAs` gOneConstructorToEncodingTagged

#if !MIN_VERSION_base(4,16,0)
      , testProperty "OptionField" $
        gOptionFieldToJSON `sameAs` gOptionFieldToEncoding
#endif
      ]
    ]
