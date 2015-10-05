module OtherBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

conj :: String -> Entry
conj = entry . (const :: Str -> ConjForm -> Str) . mkStr

prim :: String -> Entry
prim = entry . (const :: Str -> Prim -> Str) . mkStr

prep :: String -> Entry
prep = entry . (const :: Str -> PrepForm -> Str) . mkStr

ab_bort :: String -> Entry
ab_bort = entry . f
 where f s AdCompI = strings [s,s +? "-"]
       f s AdSMSI  = strings [s +? "-"]
       f s _       = strings [s]
