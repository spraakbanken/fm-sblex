module AdjBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw


adjective :: Adjective -> Entry
adjective = entry

av_1_blek :: String -> Entry
av_1_blek = adjective . av1blek
