module NounBuildSw where

import General
import Dictionary
import GenRulesSw
import TypesSw
import RulesSw
import Data.Char
import Attr

substantive :: Substantive -> Genus -> Entry
substantive n g = entryI (hyphenate_compounds n) [prValue g]

noun_compound :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_compound n g (apa,apan,apor,aporna,ap,ap2) s = substantive n_f g 
  where n_f p = (mk_substantive_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff ap) (suff ap2)) p
        suff    = apply_suffixes (tk n s)
