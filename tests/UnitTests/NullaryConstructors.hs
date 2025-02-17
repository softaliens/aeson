{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests.NullaryConstructors
    (
      nullaryConstructors
    ) where

import Prelude.Compat

import Data.Aeson (decode, eitherDecode, fromEncoding, Value)
import Data.Aeson.Types (Parser, IResult (..), JSONPathElement (..), iparse)
import Data.ByteString.Builder (toLazyByteString)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Encoders
import Test.Tasty.HUnit ((@=?), Assertion)
import Types
import qualified Data.ByteString.Lazy.Char8 as L

nullaryConstructors :: [Assertion]
nullaryConstructors =
  [ dec "\"C1\""           @=? thNullaryToJSONString C1
  , dec "\"C1\""           @=? gNullaryToJSONString C1
  , dec "{\"c1\":[]}"      @=? thNullaryToJSONObjectWithSingleField C1
  , dec "{\"c1\":[]}"      @=? gNullaryToJSONObjectWithSingleField C1
  , dec "{\"c1\":{}}"      @=? gNullaryToJSONOWSFNullaryToObject C1
  , dec "{\"c1\":{}}"      @=? thNullaryToJSONOWSFNullaryToObject C1
  , dec "[\"c1\",[]]"      @=? gNullaryToJSON2ElemArray C1
  , dec "[\"c1\",[]]"      @=? thNullaryToJSON2ElemArray C1
  , dec "{\"tag\":\"c1\"}" @=? thNullaryToJSONTaggedObject C1
  , dec "{\"tag\":\"c1\"}" @=? gNullaryToJSONTaggedObject C1

  , decE "\"C1\""           @=? enc (gNullaryToEncodingString C1)
  , decE "\"C1\""           @=? enc (thNullaryToEncodingString C1)
  , decE "[\"c1\",[]]"      @=? enc (gNullaryToEncoding2ElemArray C1)
  , decE "[\"c1\",[]]"      @=? enc (thNullaryToEncoding2ElemArray C1)
  , decE "{\"c1\":[]}"      @=? enc (thNullaryToEncodingObjectWithSingleField C1)
  , decE "{\"c1\":[]}"      @=? enc (gNullaryToEncodingObjectWithSingleField C1)
  , decE "{\"c1\":{}}"      @=? enc (gNullaryToEncodingOWSFNullaryToObject C1)
  , decE "{\"c1\":{}}"      @=? enc (thNullaryToEncodingOWSFNullaryToObject C1)
  , decE "{\"tag\":\"c1\"}" @=? enc (thNullaryToEncodingTaggedObject C1)
  , decE "{\"tag\":\"c1\"}" @=? enc (gNullaryToEncodingTaggedObject C1)

  , ISuccess C1 @=? parse thNullaryParseJSONTaggedObject          (dec "{\"tag\":\"c1\"}")
  , ISuccess C1 @=? parse gNullaryParseJSONTaggedObject           (dec "{\"tag\":\"c1\"}")

  , ISuccess C1 @=? parse thNullaryParseJSONString                (dec "\"C1\"")
  , ISuccess C1 @=? parse gNullaryParseJSONString                 (dec "\"C1\"")
  , ISuccess C1 @=? parse thNullaryParseJSON2ElemArray            (dec  "[\"c1\",[]]")
  , ISuccess C1 @=? parse gNullaryParseJSON2ElemArray             (dec  "[\"c1\",[]]")
    -- both object and empty array are accepted irrespective of the nullaryToObject flag option
  , ISuccess C1 @=? parse thNullaryParseJSONObjectWithSingleField (dec  "{\"c1\":[]}")
  , ISuccess C1 @=? parse gNullaryParseJSONObjectWithSingleField  (dec  "{\"c1\":[]}")
  , thErrObject @=? parse thNullaryParseJSONObjectWithSingleField (dec  "{\"c1\":{}}")
  , gErrObject @=? parse gNullaryParseJSONObjectWithSingleField   (dec  "{\"c1\":{}}")
  , thErrArray @=? parse thNullaryParseJSONOWSFNullaryToObject    (dec  "{\"c1\":[]}")
  , gErrArray @=? parse gNullaryParseJSONOWSFNullaryToObject      (dec  "{\"c1\":[]}")
  , ISuccess C1 @=? parse thNullaryParseJSONOWSFNullaryToObject   (dec  "{\"c1\":{}}")
  , ISuccess C1 @=? parse gNullaryParseJSONOWSFNullaryToObject    (dec  "{\"c1\":{}}")
  , ISuccess C1 @=? parse thNullaryParseJSONOWSFNullaryToObject   (dec  "{\"c1\":{\"extra\":1}}")
  , ISuccess C1 @=? parse gNullaryParseJSONOWSFNullaryToObject    (dec  "{\"c1\":{\"extra\":1}}")
  , thErrTag @=? parse thNullaryParseJSONOWSFNullaryToObject      (dec  "{\"c1\":{},\"_tag\":true}")
  , gErrTag @=? parse gNullaryParseJSONOWSFNullaryToObject        (dec  "{\"c1\":{},\"_tag\":true}")
  , ISuccess C1 @=? parse thNullaryParseJSONObjectWithSingleFieldTagged   (dec  "{\"c1\":[],\"_tag\":true}")
  , ISuccess C1 @=? parse gNullaryParseJSONObjectWithSingleFieldTagged    (dec  "{\"c1\":[],\"_tag\":true}")
  -- Make sure that the old `"contents" : []` is still allowed (and also `"contents" : {}`)
  , ISuccess C1 @=? parse thNullaryParseJSONTaggedObject          (dec "{\"tag\":\"c1\",\"contents\":[]}")
  , ISuccess C1 @=? parse gNullaryParseJSONTaggedObject           (dec "{\"tag\":\"c1\",\"contents\":[]}")
  , ISuccess C1 @=? parse thNullaryParseJSONTaggedObject          (dec "{\"tag\":\"c1\",\"contents\":{}}")
  , ISuccess C1 @=? parse gNullaryParseJSONTaggedObject           (dec "{\"tag\":\"c1\",\"contents\":{}}")
  -- with rejectUnknownFields object must be empty
  , ISuccess C1 @=? parse thNullaryParseJSONOWSFRejectUnknown                 (dec  "{\"c1\":[]}")
  , ISuccess C1 @=? parse gNullaryParseJSONOWSFRejectUnknown                  (dec  "{\"c1\":[]}")
  , thErrObject @=? parse thNullaryParseJSONOWSFRejectUnknown                 (dec  "{\"c1\":{}}")
  , gErrObject @=? parse gNullaryParseJSONOWSFRejectUnknown                   (dec  "{\"c1\":{}}")
  , thErrArray @=? parse thNullaryParseJSONOWSFNullaryToObjectRejectUnknown   (dec  "{\"c1\":[]}")
  , gErrArray @=? parse gNullaryParseJSONOWSFNullaryToObjectRejectUnknown     (dec  "{\"c1\":[]}")
  , ISuccess C1 @=? parse thNullaryParseJSONOWSFNullaryToObjectRejectUnknown  (dec  "{\"c1\":{}}")
  , ISuccess C1 @=? parse gNullaryParseJSONOWSFNullaryToObjectRejectUnknown   (dec  "{\"c1\":{}}")
  , thErrUnknown @=? parse thNullaryParseJSONOWSFNullaryToObjectRejectUnknown (dec  "{\"c1\":{\"extra\":1}}")
  , gErrUnknown @=? parse gNullaryParseJSONOWSFNullaryToObjectRejectUnknown   (dec  "{\"c1\":{\"extra\":1}}")

  , for_ [("kC1", C1), ("kC2", C2), ("kC3", C3)] $ \(jkey, key) -> do
      Right   jkey @=? gNullaryToJSONKey key
      ISuccess key @=? parse gNullaryFromJSONKey jkey
  ]
  where
    enc = eitherDecode . toLazyByteString . fromEncoding
    dec :: L.ByteString -> Value
    dec = fromJust . decode
    decE :: L.ByteString -> Either String Value
    decE = eitherDecode
    parse :: (a -> Parser b) -> a -> IResult b
    parse parsejson v = iparse parsejson v
    thErrObject = IError [] "When parsing the constructor C1 of type Types.Nullary expected Array but got Object."
    gErrObject = IError [Key "c1"] "parsing Types.Nullary(C1) failed, expected Array, but encountered Object"
    thErrArray = IError [] "When parsing the constructor C1 of type Types.Nullary expected Object but got Array."
    gErrArray = IError [Key "c1"] "parsing Types.Nullary(C1) failed, expected Object, but encountered Array"
    thErrUnknown = IError [] "When parsing the constructor C1 of type Types.Nullary expected an empty Object but got Object of size 1."
    gErrUnknown = IError [Key "c1"] "parsing Types.Nullary(C1) failed, expected an empty Object but encountered Object of size 1"
    thErrTag = IError [] "When parsing Types.Nullary expected an Object with a single tag/contents pair but got 2 pairs."
    gErrTag = IError [] "parsing Types.Nullary failed, expected an Object with a single pair, but found 2 pairs"
