----------------------------------------------------------------------
-- |
-- Module      : VerbRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
--
-- Swedish verb functions
-------------------------------------------------------------------------

module VerbRulesSw where

import GenRulesSw
import NounRulesSw
import AdjRulesSw
import General
import TypesSw

mk_verb :: String -> String -> String -> String -> String -> String -> String -> String -> Verb
mk_verb finna finner finne finn fann funne funnit funnen = 
    mk_verb_v (lift finna) (lift finner) (lift finne) (lift finn) (lift fann) (lift funne) 
              (lift funnit) (lift funnen)

mk_verb_v :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Verb
mk_verb_v finna finner finne finn fann funne funnit funnen = mk_verb_vc finna finner finne finn fann funne funnit funnen (map mk_verb_compound finna)

mk_verb_vc :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Verb
mk_verb_vc finna finner finne finn fann funne funnit funnen finn_c = strings . mk_v where 
  mk_v v = case v of
    VF (Pres Act)      -> finner
    VF (Pres SForm)    -> map (mk_vox SForm) finn
    VF (PresPl P1 v)   -> map (mk_vox v) finne
    VF (PresPl P2 v)   -> map (mk_vox v) [s++"n" | s <- finne]
    VF (PresPl P3 v)   -> map (mk_vox v) finna
    VF (Pret Act)      -> fann
    VF (Pret SForm)    -> map (mk_vox SForm) fann
    VF (PretPl P1 v)   -> map (mk_vox v) fann
    VF (PretPl P2 v)   -> map (mk_vox v) [s+?"en" | s <- fann]
    VF (PretPl P3 v)   -> map (mk_vox v) fann
    VF (Imper P22 v)      -> map (mk_vox v) finna
    VF (Imper P23 v)      -> map (mk_vox v) finne
    VF (ImperPl P1 v)     ->  map (mk_vox v) [(tk 1 s) ++ "om" | s <- finna]
    VF (ImperPl P2 v)     ->  map (mk_vox v) [(tk 1 s) ++ "en" | s <- finna]
    VF (ImperPl P3 v)     ->  map (mk_vox v) [(tk 1 s) ++ "e"  | s <- finna]
    VF (PresKonj v)      -> map (mk_vox v) finne
    VF (PresKonjPl P2 v)   -> map (mk_vox v) [s ++ "n" | s <- finne]
    VF (PresKonjPl _ v)    -> map (mk_vox v) finne
    VF (PretKonj v)      -> map (mk_vox v) funne
    VI (Inf v)           -> map (mk_vox v) finna
    VI (Sup v)           -> map (mk_vox v) funnit
    VI (PtPres c)        -> map (mk_case c) $
                             map (\s -> ifEndThen (=='a') s (s++"nde") (s++"ende")) finna
    VI (PtPret a c)      -> map (mk_case c) $ concat $ map (\s -> infer_pret_part s a) funnen
    VComp                -> finn_c ++ [x++"-" | x@(_:_) <- finn_c]
    VSMS                 -> [x++"-" | x@(_:_) <- finn_c]

mk_verb_compound :: String -> String
mk_verb_compound w = case reverse w of
        ('a':lr:c:pre) | elem lr "lr" && c == 'm'       -> (reverse pre) ++ "mm" ++ "e" ++ [lr]
        ('a':lr:c:pre) | elem lr "lr" && is_consonant c && (lr /= c) -> (reverse pre) ++ (c:"e") ++ [lr]
        ('a':'m':'m':v:pre) | is_vowel v  -> (reverse pre) ++ (v:"m")
        ('a':'n':'n':v:pre) | is_vowel v  -> (reverse pre) ++ (v:"n")
        ('a':pre) | any is_vowel pre      -> fv (reverse pre)
        ('s':pre)                         -> []
        _                                 -> fv w

infer_pret_part :: String -> AdjFormPos -> [String]
infer_pret_part bunden = mk_adj_pos [bunden] [bundet] [bundna] [bundna] where
  (bundet,bundna) = case reverse bunden of
    'd':'a':_ -> (fv (bunde) ++ "t",  bunden ++ "e")
    'n':'e':_ -> (bund  ++ "et", fv (ungeminate_m_n bund) ++ "na") -- fun-na, kom-na
    'd':'d':_ -> (fv (bund)  ++ "tt", bunden ++ "a") 
    't':'t':_ -> (bunden,        bunden ++ "a")
    'd':_     -> (fv (bunde) ++ "t",  bunden ++ "a")
    _         -> (bunden,        bunden ++ "a")
  bunde = tk 1 bunden
  bund  = tk 2 bunden

pres_verb_er :: String -> String
pres_verb_er fara
  | a /= "a"                          = fara++"r"
  | a == "a" && elem (dp 1 far) ["r"] = far -- "l" removed, counter example: falla
  | otherwise                         = far ++ "er"
 where a   = dp 1 fara
       far = tk 1 fara

mk_vox :: Vox -> String -> String
mk_vox v s = case v of
               Act   -> fv s
               SForm -> 
                case ((remove_j . ungeminate) s) of 
                  x | not_s x -> fv x ++ "s"
                  x           -> x 
  where not_s [] = False
        not_s s = last s /= 's'

remove_j :: String -> String
remove_j s
 | is_j s = case tk 1 s of
              x | is_d x -> x
              _ -> s
 | otherwise       = s 
 where is_j [] = False
       is_j s = last s == 'j'
       is_d [] = False
       is_d s = last s == 'd'

pres_ind_passive :: String -> String -> [(VerbForm, Str)]
pres_ind_passive leka lek = [(VF (Pres SForm), strings (leks ++ lekes))] where
    lekes = [tk 1 leka ++ "es"] -- for glömmes (*glömes)
    leks  = if (dp 1 lek == "s") then [] else [((remove_j . ungeminate) lek) ++ "s"] 

vb1 :: String -> Verb
vb1 hoppa =  mk_verb hoppa           (hoppa ++ "r")  (tk 1 hoppa ++ "e") hoppa 
                     (hoppa ++ "de") (hoppa ++ "de") (hoppa ++ "t")      (hoppa ++ "d")

vb2 :: String -> Verb
vb2 leka = mk_verb leka  leker leke lek   lekde lekde lekt lekd `excepts` pres_ind_passive leka lek
 where
   stam  = tk 1 leka
   leke  = stam ++ "e"
   lek   = ungeminate stam
   lekd  = let lek = ungeminate_m_n stam in case reverse lek of
             t:v:b | elem t "dt" && not (is_vowel v) -> fv (lek)  -- sände  
             t:_   | is_voiced t -> fv (lek) ++ "d"
             _                   -> fv (lek) ++ "t" 
   lekt  = let (dd,el) = span (=='d') (reverse lekd) in 
             fv (reverse el) ++ replicate (length dd) 't' 
   lekde = lekd ++ "e"
   leker = pres_verb_er leka
