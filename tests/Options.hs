{-# LANGUAGE NoImplicitPrelude #-}

module Options (module Options) where

import Prelude.Compat

import Data.Aeson.Types
import Data.Char

optsDefault :: Options
optsDefault = defaultOptions
              { fieldLabelModifier     = map toLower
              , constructorTagModifier = map toLower
              }

opts2ElemArray :: Options
opts2ElemArray = optsDefault
                 { allNullaryToStringTag = False
                 , sumEncoding     = TwoElemArray
                 }

optsUnwrapUnaryRecords :: Options
optsUnwrapUnaryRecords = optsDefault
                         { unwrapUnaryRecords = True
                         }

optsTaggedObject :: Options
optsTaggedObject = optsDefault
                   { allNullaryToStringTag = False }

optsObjectWithSingleField :: Options
optsObjectWithSingleField = optsDefault
                            { allNullaryToStringTag = False
                            , sumEncoding           = ObjectWithSingleField Nothing
                            }

optsObjectWithSingleFieldTagged :: Options
optsObjectWithSingleFieldTagged = optsDefault
                                  { allNullaryToStringTag = False
                                  , sumEncoding           = ObjectWithSingleField (Just "_tag")
                                  }


optsOWSFRejectUnknown :: Options
optsOWSFRejectUnknown = optsDefault
                        { allNullaryToStringTag = False
                        , rejectUnknownFields = True
                        , sumEncoding           = ObjectWithSingleField Nothing
                        }

optsOWSFNullaryToObject :: Options
optsOWSFNullaryToObject = optsDefault
                          { allNullaryToStringTag = False
                          , sumEncoding           = ObjectWithSingleField Nothing
                          , nullaryToObject       = True
                          }

optsOWSFNullaryToObjectRejectUnknown :: Options
optsOWSFNullaryToObjectRejectUnknown = optsDefault
                                       { allNullaryToStringTag = False
                                       , rejectUnknownFields = True
                                       , sumEncoding           = ObjectWithSingleField Nothing
                                       , nullaryToObject       = True
                                       }

optsOmitNothingFields :: Options
optsOmitNothingFields = optsDefault
                        { omitNothingFields = True
                        }

optsUntaggedValue :: Options
optsUntaggedValue = optsDefault
    { sumEncoding = UntaggedValue
    }

optsTagSingleConstructors :: Options
optsTagSingleConstructors = optsDefault
                            { tagSingleConstructors = True
                            , allNullaryToStringTag = False
                            }

optsOptionField :: Options
optsOptionField = optsDefault
                  { fieldLabelModifier = const "field"
                  , omitNothingFields = True
                  }

optsRejectUnknownFields :: Options
optsRejectUnknownFields = optsDefault
                          { rejectUnknownFields = True
                          }

optsRejectUnknownFieldsTagged :: Options
optsRejectUnknownFieldsTagged = optsDefault
                                { rejectUnknownFields = True
                                , tagSingleConstructors = True
                                }
