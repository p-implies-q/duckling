-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.NL.Rules
  ( rules ) where

import Data.String
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Email.Types (EmailData (..))
import qualified Duckling.Email.Types as TEmail
import Duckling.Regex.Types
import Duckling.Types

ruleEmailSpelledOut :: Rule
ruleEmailSpelledOut = Rule
  { name = "email spelled out"
  , pattern =
    [ --regex "([\\w_+-]+(?:(?: dot | punt |\\.)[\\w_+-]+){0,10})(?: at |@| ape(n?)staartje )([a-zA-Z]+(?:(?:\\.| dot | punt )[\\w_-]+){1,10})"
      regex "([\\w_+-]+(?:(?: dot |\\.)[\\w_+-]+){0,10})(?: at |@)([a-zA-Z]+(?:(?:\\.| dot )[\\w_-]+){1,10})"
    ]
  , prod = \xs -> case xs of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) ->
        Just $ Token Email EmailData
          { TEmail.value = Text.concat [replaceDots m1, "@", replaceDots m2] }
        -- where replaceDots = Text.replace " punt " "." . Text.replace " dot " "."
        where replaceDots = Text.replace " dot " "."
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleEmailSpelledOut
  ]
