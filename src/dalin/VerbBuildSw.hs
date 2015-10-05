module VerbBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

verb :: Verb -> Entry
verb = entry 

v1 :: String -> Entry
v1 = verb . vb1

v2 :: String -> Entry
v2 = verb . vb2
