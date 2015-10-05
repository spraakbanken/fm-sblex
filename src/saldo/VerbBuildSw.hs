module VerbBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

verb :: Verb -> Entry
verb = entry 

verb_full :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ verb_prefixed n finna finner finne finn fann funne funnit funnen w 

verb_full_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_sform_variant n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ ppvar n (verb_prefixed n finna finner finne finn fann funne funnit funnen w) w

verb_full_compound_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_compound_sform_variant n (finna, finne,finner, finn, fann, funne, funnit, funnen,finn_c) w = 
 verb $ ppvar n (verb_prefixed_compound n finna finner finne finn fann funne funnit funnen finn_c w) w 

verb_weak_sform_variant ::  Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_sform_variant n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ ppvar n (verb_prefixed n finna finner [] finn fann [] funnit funnen w) w

verb_weak :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_weak_compound :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_compound n (finna, finner, finn, fann, funnit, funnen,finn_c) w = 
 verb $ verb_prefixed_compound n finna finner [] finn fann [] funnit funnen finn_c w 

verb_weak_compound_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_compound_sform_variant n (finna, finner, finn, fann, funnit, funnen,finn_c) w = 
 verb $ ppvar n (verb_prefixed_compound n finna finner [] finn fann [] funnit funnen finn_c w) w 

verb_weak_no_sform :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_no_sform n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ no_passive $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_weak_no_passive :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_no_passive n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ no_vcomp $ no_part_pres $ no_passive $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_full_no_sform :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_no_sform n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ no_passive $ verb_prefixed n finna finner finne finn fann funne funnit funnen w 

verb_deponens :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes) -> (String -> Entry)
verb_deponens n (finna,finn,fann,funnit,funnen) w = 
 verb $ no_part_pres $ no_active $ verb_prefixed n finna [] [] finn fann [] funnit funnen w 

verb_dwc :: Int -> [String] -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
verb_dwc n finna finn fann funnit funnen w =
 verb $ no_part_pres $ no_active $ verb_prefixed n (map e finna) [] []
                                    (map e finn) (map e fann) []
                                    (map e funnit) (map e funnen) w

v1 :: String -> Entry
v1 = verb . no_konj . vb1

vb_2a_sända :: String -> Entry
vb_2a_sända  = verb . no_konj .vb2sända

vb_2a_göra :: String -> Entry
vb_2a_göra   = verb . no_konj . vb2göra

vb_2s_synas :: String -> Entry
vb_2s_synas    = verb . no_konj . vbdsynas

v3 :: String -> Entry
v3 = verb . no_konj . vb3

vb_va_koka :: String -> Entry
vb_va_koka   = verb . no_konj . vbvkoka 

vb_4m_vina :: String -> Entry
vb_4m_vina  = verb  . vb4vina

vb_4a_ta :: String -> Entry
vb_4a_ta = verb . vb4taga

vb_va_bringa :: String -> Entry
vb_va_bringa = verb . no_konj . vbvbringa

vb_4a_låta :: String -> Entry
vb_4a_låta  = verb . vb4låta

vb_4a_bita :: String -> Entry
vb_4a_bita  = verb . vb4bita

vb_4a_komma :: String -> Entry
vb_4a_komma = verb . vb4komma

vb_1s_hoppas :: String -> Entry
vb_1s_hoppas   = verb . no_konj . vbdhoppas

vb_4a_bliva :: String -> Entry
vb_4a_bliva = verb  . no_passive . vb4bliva

verb_vbm n wfs w = (set_pos "vbm" . verb_weak_no_passive n wfs) w

vbm_inf w = set_pos "vbm" $ verb $ only (vb1 w) [VI (Inf Act)]
