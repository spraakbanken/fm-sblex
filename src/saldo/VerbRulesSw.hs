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

verb_prefixed :: Int -> Suffixes -> Suffixes -> Suffixes -> Suffixes -> 
                        Suffixes -> Suffixes -> Suffixes -> Suffixes -> 
                        String -> Verb
verb_prefixed n finna finner finne finn fann funne funnit funnen s = 
  (mk_verb_v (suff finna) (suff finner) (suff finne) (suff finn) 
             (suff fann)  (suff funne) (suff funnit) (suff funnen))
  where suff = apply_suffixes (tk n s)

ppvar :: Int -> Verb -> String -> Verb
ppvar n v s = variants_or_excepts v [(VF (Pres Ind SForm), strings xs)]
 where ws = unStr (v (VF Imper))
       variants_or_excepts = if all (=="s") (map (dp 1) ws) then excepts else variants
       xs  = [w ++ "es" | w <- ws, not (vow w || single_m w)]
       single_m x = case reverse x of
                      ('m':m:_) -> m /= 'm'
                      _         -> False
       vow [] = False
       vow w@(_:_) = is_vowel (last w) 


mk_verb_v :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Verb
mk_verb_v finna finner finne finn fann funne funnit funnen = mk_verb_vc finna finner finne finn fann funne funnit funnen (map mk_verb_compound finna)

mk_verb_vc :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Verb
mk_verb_vc finna finner finne finn fann funne funnit funnen finn_c = strings . mk_v where 
  mk_v v = case v of
    VF (Pres Ind  Act)   -> finner
    VF (Pres Ind SForm)  -> map (mk_vox SForm) finn -- [s +? "s" | s@(_:_) <- map (remove_j . verb_ungeminate) finn]
    VF (Pres Conj v)     -> map (mk_vox v.verb_ungeminate) finne
    VF (Pret Ind  Act)   -> map verb_ungeminate fann
    VF (Pret Conj Act)   -> map verb_ungeminate funne
    VF (Pret m SForm)    -> map (mk_vox SForm) $ mk_v (VF (Pret m Act)) -- +? "s" --- frös - frös ?
    VF Imper             -> map verb_ungeminate finn
    VI (Inf v)           -> map (mk_vox v.verb_ungeminate) finna
    VI (Sup v)           -> map (mk_vox v.verb_ungeminate) funnit
    VI (PtPres c)        -> map (mk_case c) $
                             map (\s -> ifEndThen (=='a') s (s++"nde") (s++"ende")) finna
    VI (PtPret a c)      -> map (mk_case c) $ concat $ map (\s -> infer_pret_part s a) funnen
    VComp                -> [verb_ungeminate w  | w <- finn_c] ++ [(verb_ungeminate x) ++"-" | x@(_:_) <- finn_c]
    VSMS                 -> [(verb_ungeminate x)++"-" | x@(_:_) <- finn_c]

mk_vox :: Vox -> String -> String
mk_vox v s = case v of
               Act   -> s
               SForm -> 
                case ((remove_j . verb_ungeminate) s) of 
                  x | not_s x -> x ++ "s"
                  x           -> x 
  where not_s [] = False
        not_s s = last s /= 's'

mk_verb_compound :: String -> String
mk_verb_compound w = case reverse w of
        ('a':lr:c:pre) | elem lr "lr" && c == 'm'       -> (reverse pre) ++ "mm" ++ "e" ++ [lr]
        ('a':lr:c:pre) | elem lr "lr" && is_consonant c && (lr /= c) -> (reverse pre) ++ (c:"e") ++ [lr]
        ('a':'m':'m':v:pre) | is_vowel v  -> (reverse pre) ++ (v:"m")
        -- ('a':'n':'n':v:pre) | is_vowel v  -> (reverse pre) ++ (v:"n")
        ('a':pre) | any is_vowel pre      -> (reverse pre)
        ('s':pre)                         -> []
        _                                 -> w

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

infer_pret_part :: String -> AdjFormPos -> [String]
infer_pret_part bunden' = mk_adj_pos [bunden] [bundet] [bundna] [bundna] where
  (bundet,bundna) = case reverse bunden of
    'd':'a':_ -> (bunde ++ "t",  bunden ++ "e")
    'n':'e':_ -> (bund  ++ "et", verb_ungeminate_m_n bund ++ "na") -- fun-na, kom-na
    'd':'d':_ -> (bund  ++ "tt", bunden ++ "a") 
    't':'t':_ -> (bunden, bunden ++ "a")
    'd':_ -> (bunde ++ "t", bunden ++ "a")
    _ -> (bunden, bunden ++ "a")
  bunden = verb_ungeminate bunden'
  bunde = verb_ungeminate $ tk 1 bunden
  bund  = tk 2 bunden

no_konj :: Verb -> Verb
no_konj v = v `missing` conj_forms

vb1 :: String -> Verb
vb1 hoppa =  mk_verb hoppa (hoppa ++ "r") (tk 1 hoppa ++ "e") hoppa 
              (hoppa ++ "de") (hoppa ++ "de") (hoppa ++ "t") (hoppa ++ "d")

vb2sända  = vb2

vb2 :: String -> Verb
vb2 leka = mk_verb leka  leker leke lek   lekde lekde lekt lekd `excepts` pres_ind_passive leka lek
 where
   stam  = tk 1 leka
   leke  = stam ++ "e"
   lek   = verb_ungeminate stam
   lekd  = let lek = verb_ungeminate_m_n stam in case reverse lek of
             t:v:b | elem t "dt" && not (is_vowel v) -> lek   -- sände  
             t:_   | is_voiced t -> lek ++ "d"
             _                   -> lek ++ "t" 
   lekt  = let (dd,el) = span (=='d') (reverse lekd) in 
             reverse el ++ replicate (length dd) 't' 
   lekde = lekd ++ "e"
   leker = pres_verb_er leka

vb3 :: String -> Verb
vb3 bo = mk_verb bo (bo ++ "r") [] bo (bo ++ "dde") (bo ++ "dde") (bo ++ "tt") (bo ++ "dd")

vbvkoka :: String -> Verb
vbvkoka koka = combine (vb1 koka) (vb2 koka)

mk_verb :: String -> String -> String -> String -> String -> String -> String -> String -> Verb
mk_verb finna finner finne finn fann funne funnit funnen = 
    mk_verb_v (lift finna) (lift finner) (lift finne) (lift finn) (lift fann) (lift funne) 
              (lift funnit) (lift funnen)

verb_prefixed_compound :: Int -> Suffixes -> Suffixes -> Suffixes -> Suffixes -> 
                        Suffixes -> Suffixes -> Suffixes -> Suffixes -> Suffixes -> 
                        String -> Verb
verb_prefixed_compound n finna finner finne finn fann funne funnit funnen finn_c s = 
  (mk_verb_vc (suff finna) (suff finner) (suff finne) (suff finn) 
             (suff fann)  (suff funne)  (suff funnit) (suff funnen) (suff finn_c))
  where suff = apply_suffixes (tk n s)


pres_ind_passive :: String -> String -> [(VerbForm, Str)]
pres_ind_passive leka lek = [(VF (Pres Ind SForm), strings (leks ++ lekes))] where
    lekes = [tk 1 leka ++ "es"] -- for glömmes (*glömes)
    leks  = if (dp 1 lek == "s") then [] else [((remove_j . verb_ungeminate) lek) ++ "s"] 

pres_verb_er :: String -> String
pres_verb_er fara
  | a /= "a"                          = fara++"r"
  | a == "a" && elem (dp 1 far) ["r"] = far -- "l" removed, counter example: falla
  | otherwise                         = far ++ "er"
 where a   = dp 1 fara
       far = tk 1 fara

vb4vina :: String -> Verb
vb4vina = no_part_pret . vb4bita

vb4taga :: String -> Verb
vb4taga s v = ((\v -> (tk 2 s) +* ((verb_short_long (dp 2 s) (vb4fara (dp 2 s ++ "ga"))) v)) `except` 
   ([(VF (Pret Conj Act), tk  1 s ++ "oge"),(VF (Pret Conj SForm),  tk 1 s ++ "oges")])) v

vbvbringa :: String -> Verb
vbvbringa bringa = variant (vb1 bringa) $ [(VI (Sup v), mk_vox v bragt) | v <- values] ++
    [(VF (Pret m v), mk_vox v (bragt++"e")) | m <- values, v <- values]
 where bragt = tk 4 bringa ++ "agt"

vb4låta :: String -> Verb
vb4låta s = ppvar 1 (verb_strong_vowel "ä" "å" s) s

vb4bita :: String -> Verb
vb4bita s = ppvar 1 (verb_strong_vowel "e" "i" s) s

verb_strong_vowel :: String -> String -> String -> Verb
verb_strong_vowel a u finna = verb_strong finna fann funnit where
  (f,_,nn) = find_stem_vowel (tk 1 finna)
  fann     = verb_ungeminate $ f ++ a ++ nn
  funnit   = f ++ u ++ nn ++ "it"

no_part_pret :: Verb -> Verb
no_part_pret v = v `missing` part_pret_forms

verb_short_long :: String -> Verb -> Verb
verb_short_long ge giva = 
    giva `variant` 
             ([(VF (Pres Ind Act),  ge ++ "r"), 
              (VF (Pres Ind SForm), ge ++ "s"),
              (VF Imper,ge)] ++ 
              [ (VI (Inf v),mk_vox v ge) | v <- values])

verb_strong :: String -> String -> String -> Verb
verb_strong finna fann funnit = 
    verb_irreg finna (pres_verb_er finna) fann funnit (tk 2 funnit ++ "en")

verb_irreg :: String -> String -> String -> String -> String -> Verb
verb_irreg finna finner fann funnit funnen = 
    mk_verb finna finner finne finn fann (funn ++ "e") funnit funnen
      `excepts` if_a (pres_ind_passive finna finn) []
 where
   if_a a b = if dp 1 finna == "a" then a else b
   finn   = if_a (tk 1 finna) finna
   finne  = if_a (finn ++ "e") finn
   funn   = tk 2 funnit
   funna  = verb_ungeminate funn +? "na"     


vbdhoppas :: String -> Verb
vbdhoppas hoppas = missing (vbdlyckas hoppas) part_forms
 where hopp = tk 2 hoppas

vbdsynas :: String -> Verb
vbdsynas synas = missing (mk_verb synes synas synas synas (syn++"tes")  
                                  (syn++"ades") (syn++"ts") (syn++"ad"))
                          (part_pres_forms ++ active_forms)
 where syn = tk 2 synas
       synes = syn++"es"


vb2göra :: String -> Verb
vb2göra  göra = ppvar 1 (
    mk_verb göra gör (gör ++ "e") gör (gjor ++ "de") (gjor ++ "de") 
            (gjor ++ "t") (gjor ++ "d")) göra
 where g = tk 3 göra
       gör   = tk 1 göra
       gjor = g ++ "jor"


vbdlyckas :: String -> Verb
vbdlyckas lyckas = 
  missing (mk_verb lyckas lyckas lyckas lyckas (lyck++"ades")  
                   (lyck++"ades") (lyck++"ats") (lyck++"ad"))
                   (part_pres_forms ++ active_forms)
 where lyck = tk 2 lyckas

vb4fara :: String -> Verb
vb4fara s = ppvar 1 (verb_strong_vowel "o" "a" s) s

vb4komma :: String -> Verb
vb4komma s = ppvar 1 (verb_strong_vowel "o" "o" s) s

vb4bliva :: String -> Verb
vb4bliva s v = (tk 3 s) +* 
 (verb_short_long (dp 3 s) (vb4bita ((dp 3 s) ++ "va")) `except`
  [(VF (Pret Conj Act), (tk 1 (dp 3 s)) ++ "eve")]) v

no_active :: Verb -> Verb
no_active v = v `missing` active_forms

no_passive :: Verb -> Verb
no_passive v = v `missing` passive_forms

no_part_pres :: Verb -> Verb
no_part_pres v = missing v part_pres_forms

no_vcomp :: Verb -> Verb
no_vcomp v = v `missing` comp_forms
