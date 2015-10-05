module NounBuildSw where

import General
import Dictionary
import GenRulesSw
import TypesSw
import RulesSw
import Data.Char
import Attr

substantive :: Substantive -> Genus -> Entry
substantive n g = entryI ((hyphenate_compounds . geminator) n) [prValue g]

noun_f :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_f n g (apa,apan,apor,aporna) s = noun_compound n g (apa,apan,apor,aporna,apa,apa) s 

noun :: Genus -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
noun g apa apan apor aporna s = nounC g apa apan apor aporna apa s 

noun_no_genitive :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_no_genitive g (apa,apan,apor,aporna) s = 
  substantive (missing n_f [(SF n d Gen) | n <- values, d <- values]) g 
   where n_f p = (mk_subst_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff apa)) p
         suff    = apply_suffixes s

nounC :: Genus -> [String] -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
nounC g apa apan apor aporna ap s = substantive n_f g 
  where n_f p = (mk_subst_v apa' apan' apor' aporna' ap') p
        suff    = (s ++)
        apa'    = map suff apa
	apan'   = map suff apan
        apor'   = map suff apor
	aporna' = map suff aporna
	ap'     = map suff ap

noun_compound :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_compound n g (apa,apan,apor,aporna,ap,ap2) s = substantive n_f g 
  where n_f p = (mk_substantive_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff ap) (suff ap2) (suff ap)) p
        suff = apply_suffixes (tk n s)
        
noun_compound_ng :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_compound_ng n g (apa,apan,apor,aporna,ap,ap2) s = substantive (missing n_f [(SF n d Gen) | n <- values, d <- values]) g 
  where n_f p = (mk_substantive_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff ap) (suff ap2) (suff ap)) p
        suff    = apply_suffixes (tk n s)

nna :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
nna g (apa,apas,apan,apans,apor,apors,aporna,apornas) s = 
 replace_attr wp_attr w_attr $ replace_attr h_attr w_attr $ set_pos "nna" $ substantive n_f g 
  where n_f p = (mk_nna (suff apa) (suff apas) (suff apan) (suff apans) (suff apor) (suff apors) 
                        (suff aporna) (suff apornas)) p
        suff          = apply_suffixes s . connect s
        connect s end = concat [[(f,(':':e)),(f,('-':e)),(f,e)]  | (f,e@(_:_)) <- end] ++ (filter (null . snd) end)

nn2 :: String -> Entry
nn2 = substUtrum . RulesSw.nn2

nn4 :: String -> Entry
nn4 = substUtrum . RulesSw.nn4

substUtrum :: Substantive -> Entry
substUtrum s = substantive s Utr

nn3_parti :: String -> Entry
nn3_parti = substNeutrum . nn3parti

nn3_tand :: String -> Entry
nn3_tand = substUtrum . nn3tand

nn_iv_hum :: String -> Entry
nn_iv_hum = substVack . nni_ns

nnac :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
nnac g (apa,apas,apan,apans,apor,apors,aporna,apornas) s = 
 replace_attr wp_attr w_attr $ replace_attr h_attr w_attr $ set_pos "nna" $ substantive n_f g 
  where n_f p = (mk_nna (suff apa) (suff apas) (suff apan) (suff apans) (suff apor) (suff apors) 
                        (suff aporna) (suff apornas)) p
        suff          = apply_suffixes s . connect s
        connect s end = concat [[(f,(':':e)),(f,e)]  | (f,e@(_:_)) <- end] ++ (filter (null . snd) end)

nnm_1ua_öm_låga :: String -> Entry
nnm_1ua_öm_låga s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"ma",w ++ "n"))
                        (cat xs (adj++"ma",tk 1 w ++ "or")) (cat xs (adj++"ma",tk 1 w ++ "orna"))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_nytt_påfund  :: String -> Entry
nnm_6na_nytt_påfund s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 2 adj++"a",(w ++ "et")))
                        (cat xs (tk 2 adj++"a",w)) (cat xs (tk 2 adj++"a",(w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_bevingat_ord :: String -> Entry
nnm_6na_bevingat_ord s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"de",(w ++ "et")))
                        (cat xs (tk 1 adj++"de",w)) (cat xs (tk 1 adj++"de",(w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_runt_tal :: String -> Entry
nnm_6na_runt_tal s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"da",(w ++ "et")))
                        (cat xs (tk 1 adj++"da",w)) (cat xs (tk 1 adj++"da",(w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6ua_allmän_åklagare :: String -> Entry
nnm_6ua_allmän_åklagare s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "n")))
                        (cat xs (adj++"a",w)) (cat xs (adj++"a",(tk 1 w ++ "na")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_öppet_hav  :: String -> Entry
nnm_6na_öppet_hav s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 2 adj++"na",(w ++ "et")))
                        (cat xs (tk 2 adj++"na",w)) (cat xs (tk 2 adj++"na",(w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_öppet_vatten  :: String -> Entry
nnm_6na_öppet_vatten s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 2 adj++"na",(dv w ++ "et")))
                        (cat xs (tk 2 adj++"na",w)) (cat xs (tk 2 adj++"na",(dv w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_kort_varsel :: String -> Entry
nnm_6na_kort_varsel s = noun_m Neutr (cat xs (adj,w)) (cat xs (adj++"a",(dv w ++ "et")))
                        (cat xs (adj++"a",w)) (cat xs (adj++"a",(dv w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_beskt_piller :: String -> Entry
nnm_6na_beskt_piller s =  noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"a",(dv w ++ "et")))
                        (cat xs (tk 1 adj++"a",w)) (cat xs (tk 1 adj++"a",(dv w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_välsmort_munläder :: String -> Entry
nnm_6na_välsmort_munläder s 
    = noun_m Neutr (cat xs (adj++"t",w)) 
                 (cat xs (adj++"da",w ++ "n"))
                 (cat xs (adj++"da",w)) 
                 (cat xs (adj++"da",w ++ "na"))
  where 
    adj = tk 1 adj'
    (w:adj':xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
         
nnm_6na_svart_hål :: String -> Entry
nnm_6na_svart_hål s = noun_m Neutr (cat xs (adj,w)) (cat xs (adj ++"a",(w ++ "et")))
                        (cat xs (adj++"a",w)) (cat xs (adj++"a",(w ++ "en")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])                     



nnm_6ua_oplockad_gås :: String -> Entry
nnm_6ua_oplockad_gås s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"e",w ++ "en"))
                        (cat xs (adj++"e",(vc "ä" w) ++ "s")) 
                        (cat xs (adj++"e",(vc "ä" w) ++ "sen"))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6ua_svensk_mästare :: String -> Entry
nnm_6ua_svensk_mästare s 
    = noun_m Utr (cat xs (adj,w)) 
                 (cat xs (adj++"a",w ++ "n"))
                 (cat xs (adj++"a",w)) 
                 (cat xs (adj++"a",(tk 1 w) ++ "na"))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])


nnm_0ua_frid :: String -> Entry
nnm_0ua_frid s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "en"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0ua_evig_vila :: String -> Entry
nnm_0ua_evig_vila  s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "n"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])


nnm_6na_glykemiskt_index :: String -> Entry
nnm_6na_glykemiskt_index s = noun_m Neutr (cat xs (adj,w)) (cat xs (glykemisk ++"a",(w ++ "et")))
                        (cat xs (glykemisk++"a",w)) (cat xs (glykemisk++"a",(w ++ "en")))
  where glykemisk = tk 1 adj
        (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_6na_dubbelt_budskap :: String -> Entry
nnm_6na_dubbelt_budskap s =  noun_m Neutr (cat xs (adj,w)) (cat xs (dubb ++"la",(w ++ "et")))
                        (cat xs (dubb++"la",w)) (cat xs (dubb++"la",(w ++ "en")))
  where dubb = tk 3 adj
        (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])


nnm_5na_oförrättat_ärende :: String -> Entry
nnm_5na_oförrättat_ärende s = noun_m Neutr (cat xs (adj,w)) (cat xs (oförrätta++"de",(w ++ "n")))
                             (cat xs (oförrätta++"de",w++"n")) (cat xs (oförrätta++"de",(w ++ "na")))
  where oförrätta = tk 1 adj
        (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_5na_krönt_huvud :: String -> Entry
nnm_5na_krönt_huvud s = noun_m Neutr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "et")))
                             (cat xs (adj++"a",w++"en")) (cat xs (adj++"a",(w ++ "ena")))
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_5na_lätt_byte :: String -> Entry
nnm_5na_lätt_byte s = noun_m Neutr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "t")))
                             (cat xs (adj++"a",w++"n")) (cat xs (adj++"a",(w ++ "na")))
  where oförrätta = tk 1 adj
        (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])


nnm_5na_transitivt_hölje :: String -> Entry
nnm_5na_transitivt_hölje s = noun_m Neutr (cat xs (adj,w)) (cat xs (transitiv ++"a",(w ++ "t")))
                             (cat xs (transitiv++"a",w++"n")) (cat xs (transitiv++"a",(w ++ "na")))
  where transitiv = tk 1 adj
        (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
                       
nnm_0ua_grå_starr :: String -> Entry
nnm_0ua_grå_starr s = noun_mv Utr [(cat xs (adj,w))] [(cat xs (adj,(w ++ "en"))), (cat xs (adj++"a",(w ++ "en")))] [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

                       
nnm_0ua_sluten_vård :: String -> Entry
nnm_0ua_sluten_vård s = noun_m Utr (cat xs (adj,w)) (cat xs ((tk 2 adj)++"na",(w ++ "en"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
                       
                       
nnm_0na_aktivt_kol :: String -> Entry
nnm_0na_aktivt_kol s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"a",(w ++ "et"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_blått_blod :: String -> Entry
nnm_0na_blått_blod s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 2 adj++"a",(w ++ "et"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_ont_öga  :: String -> Entry
nnm_0na_ont_öga s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"da",(w ++ "t"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_sunt_förnuft :: String -> Entry
nnm_0na_sunt_förnuft s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"da",(w ++ "et"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_tomt_prat :: String -> Entry
nnm_0na_tomt_prat s = noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"ma",(w ++ "et"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_ekonomiskt_oberoende :: String -> Entry
nnm_0na_ekonomiskt_oberoende s =  noun_m Neutr (cat xs (adj,w)) (cat xs (tk 1 adj++"a",(w ++ "t"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_utarmat_uran :: String -> Entry
nnm_0na_utarmat_uran s = noun_m Neutr (cat xs (adj,w)) (cat xs ((tk 1 adj)++"de",(w ++ "et"))) [] []
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])


nnm_3ua_film :: String -> Entry
nnm_3ua_film s =  noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",w ++ "en"))
                        (cat xs (adj++"a",w++"er")) (cat xs (adj++"a",w ++ "erna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_3ua_förbjuden_frukt :: String -> Entry
nnm_3ua_förbjuden_frukt s =  noun_m Utr (cat xs (adj,w)) (cat xs (dv adj++"a",w ++ "en"))
                        (cat xs (dv adj++"a",w++"er")) (cat xs (dv adj++"a",w ++ "erna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_3ua_tom_fras :: String -> Entry
nnm_3ua_tom_fras s =  noun_m Utr (cat xs (adj,w)) (cat xs (adj++"ma",w ++ "en"))
                        (cat xs (adj++"ma",w++"er")) (cat xs (adj++"ma",w ++ "erna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_3ua_öm_punkt :: String -> Entry
nnm_3ua_öm_punkt s =  noun_m Utr (cat xs (adj,w)) (cat xs (geminate adj++"a",w ++ "en"))
                       (cat xs (geminate adj++"a",w++"er")) (cat xs (geminate adj++"a",w ++ "erna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_3ua_enarmad_bandit :: String -> Entry
nnm_3ua_enarmad_bandit s =  noun_m Utr (cat xs (adj,w)) (cat xs (adj++"e",w ++ "en"))
                        (cat xs (adj++"e",w++"er")) (cat xs (adj++"e",w ++ "erna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
                       
nnm_4ua_lös_förbindelse :: String -> Entry
nnm_4ua_lös_förbindelse s =  
  noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",w ++ "n"))
   (cat xs (adj++"a",w++"r")) (cat xs (adj++"a",w ++ "rna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_1ua_halvkväden_visa :: String -> Entry
nnm_1ua_halvkväden_visa s =  noun_m Utr (cat xs (adj,w++"a")) (cat xs ((tk 2 adj)++"na",w ++ "an"))
                             (cat xs ((tk 2 adj)++"na",w++"or")) (cat xs ((tk 2 adj)++"na",w ++ "orna")) 
  where (w':adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
        w = tk 1 w'

nnm_1ua_grön_våg :: String -> Entry
nnm_1ua_grön_våg s =  noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",w ++ "en"))
                        (cat xs (adj++"a",w++"or")) (cat xs (adj++"a",w ++ "orna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_1ua_svart_låda :: String -> Entry
nnm_1ua_svart_låda s =  noun_m Utr (cat xs (adj,w++"a")) (cat xs (adj++"a",w ++ "an"))
                        (cat xs (adj++"a",w++"or")) (cat xs (adj++"a",w ++ "orna")) 
  where (w':adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
        w = tk 1 w'

nnm_3ua_rolig_historia :: String -> Entry
nnm_3ua_rolig_historia s =  noun_m Utr (cat xs (adj,w++"a")) (cat xs (adj++"a",w ++ "an"))
                        (cat xs (adj++"a",w++"er")) (cat xs (adj++"a",w ++ "erna")) 
  where (w':adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])
        w = tk 1 w'

nnm_2ua_pojke :: String -> Entry
nnm_2ua_pojke s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(dv w ++ "en")))
                        (cat xs (adj++"a",(dv w ++ "ar"))) (cat xs (adj++"a",(dv w ++ "arna"))) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_2ua_nyckel :: String -> Entry
nnm_2ua_nyckel s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "n")))
                        (cat xs (adj++"a",(dv w ++ "ar"))) (cat xs (adj++"a",(dv w ++ "arna"))) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_2ua_kvalificerad_gissning :: String -> Entry
nnm_2ua_kvalificerad_gissning s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"e",(w ++ "en")))
                        (cat xs (adj++"e",(w ++ "ar"))) (cat xs (adj++"e",(w ++ "arna"))) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_2ua_stol :: String -> Entry
nnm_2ua_stol s = noun_m Utr (cat xs (adj,w)) (cat xs (adj++"a",(w ++ "en")))
                        (cat xs (adj++"a",w++"ar")) (cat xs (adj++"a",w ++ "arna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_2ua_beskärd_del :: String -> Entry
nnm_2ua_beskärd_del = nnm_2ua_stol

nnm_2ua_mogen_ålder :: String -> Entry
nnm_2ua_mogen_ålder s = noun_m Utr (cat xs (adj,w)) (cat xs (dv adj++"a",(dv w ++ "n")))
                        (cat xs (dv adj++"a",dv w++"ar")) (cat xs (dv adj++"a",dv w ++ "arna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_2ua_naken_blankning :: String -> Entry
nnm_2ua_naken_blankning s = 
    noun_m Utr (cat xs (adj,w)) (cat xs ((dv adj)++"a",(w ++ "en")))
               (cat xs ((dv adj)++"a",w++"ar")) (cat xs ((dv adj)++"a",w ++ "arna")) 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

nnm_0na_fritt_vivre :: String -> Entry
nnm_0na_fritt_vivre s =  noun_m Neutr (cat xs (adj,w)) (cat xs ((tk  2 adj)++"a",(w ++ "t"))) [] [] 
  where (w:adj:xs) = case reverse $ words s of
                       (x:y:z) -> (x:y:z)
                       _       -> ([]:[]:[[]])

noun_m :: Genus -> String -> String -> String -> String  -> Entry
noun_m g s1 s2 s3 s4 = set_pos "nnm" $ nounC g [s1] [s2] [s3] [s4] [] []

noun_mv :: Genus -> [String] -> [String] -> [String] -> [String]  -> Entry
noun_mv g s1 s2 s3 s4 = set_pos "nnm" $ nounC g s1 s2 s3 s4 [] []

cat :: [String] -> (String,String) -> String
cat xs (a,w) = unwords (reverse xs ++ [a,w])

substNeutrum :: Substantive -> Entry
substNeutrum s = substantive s Neutr

substVack :: Substantive -> Entry
substVack s = substantive s Pend


nn_kol_14 :: String -> Entry
nn_kol_14 = substNeutrum . nnkol14


nnm_5pc_göranden_och_låtanden :: String -> Entry
nnm_5pc_göranden_och_låtanden s =  noun_m GPl [] [] 
                        (unwords [w1,och,w2]) (unwords [w1++"a",och,w2++"a"]) 
  where (w1:och:[w2]) =  case words s of
                           (x:y:[z]) -> (x:y:[z])
                           _       -> [[],[],[]]

nnm_gpc_kreti_och_pleti :: String -> Entry
nnm_gpc_kreti_och_pleti s =  noun_m GPl [] [] (unwords [w1,och,w2])  []
  where (w1:och:[w2]) = case words s of
                       (x:y:[z]) -> (x:y:[z])
                       _       -> [[],[],[]]


nn_0n_oväsen :: String -> Entry
nn_0n_oväsen = substNeutrum . nn0oväsen         

nn_0n_skum :: String -> Entry
nn_0n_skum = substNeutrum . nn0skum         

nn_0n_kaffe :: String -> Entry
nn_0n_kaffe = substNeutrum . nn0kaffe          

nn_0u_skam :: String -> Entry
nn_0u_skam = substUtrum . nn0skam

nn_0v_manna :: String -> Entry
nn_0v_manna = substVack . nn0manna        

nn2_moder :: String -> Entry
nn2_moder = substUtrum . nn2mor

nn2_dotter :: String -> Entry
nn2_dotter = substUtrum . nn2dotter

nn2_öken :: String -> Entry
nn2_öken = substUtrum . nn2öken

nn2_kam :: String -> Entry
nn2_kam = substUtrum . nn2kam

nn3_flanell :: String -> Entry
nn3_flanell = substVack  . nn3flanell

nn3_bok :: String -> Entry
nn3_bok = substUtrum . nn3bok 

nn3_fot :: String -> Entry
nn3_fot = substUtrum . nn3fot

nn3_bockfot :: String -> Entry
nn3_bockfot = substUtrum . nn3bockfot

nn3_vän :: String -> Entry
nn3_vän = substUtrum . nn3vän

nn4_bonde :: String -> Entry
nn4_bonde = substUtrum . nn4bonde

nn5_anmodan :: String -> Entry
nn5_anmodan = substUtrum   . nn5anmodan

nn5_knä :: String -> Entry
nn5_knä = substNeutrum . nn5knä

nn_6u_broder :: String -> Entry
nn_6u_broder = substUtrum . nn6broder

nn_6u_mus :: String -> Entry
nn_6u_mus = substUtrum . nn6mus

nn_6u_vaktman :: String -> Entry
nn_6u_vaktman  = substUtrum . nn6vaktman

nn_6v_borst :: String -> Entry
nn_6v_borst = substVack . nn6borst


nn_vn_garn :: String -> Entry
nn_vn_garn = substNeutrum . nnvgarn

nn_vn_huvud :: String -> Entry
nn_vn_huvud = substNeutrum . nnvhuvud

nn_vn_spektrum :: String -> Entry
nn_vn_spektrum = substNeutrum . nnvspektrum

nn_vu_blinker :: String -> Entry
nn_vu_blinker = substUtrum . nnvblinker

nn_vu_dress :: String -> Entry
nn_vu_dress = substUtrum . nnvdress

nn_vu_hambo :: String -> Entry
nn_vu_hambo = substUtrum . nnvhambo

nn_vu_kaliber :: String -> Entry
nn_vu_kaliber = substUtrum . nnvkaliber

nn_vu_playboy :: String -> Entry
nn_vu_playboy = substUtrum . nnvplayboy

nn6_akademiker :: String -> Entry
nn6_akademiker = substUtrum . nn6akademiker

nn_in_vaj :: String -> Entry
nn_in_vaj = substNeutrum . nni

nn_iu_avvaktan :: String -> Entry
nn_iu_avvaktan = substUtrum . nni

nn_ou_officer :: String -> Entry
nn_ou_officer = substUtrum . nnoofficer

nn_vn_alfa_abc :: String -> Entry
nn_vn_alfa_abc = substNeutrum . nnvabc

nn_vu_trio :: String -> Entry
nn_vu_trio = substUtrum . nnvtrio

nn_vv_borr :: String -> Entry
nn_vv_borr = substVack . nnvborr

nn_vv_test :: String -> Entry
nn_vv_test = substVack . nnvtest

nn_6u_gås :: String -> Entry
nn_6u_gås = substUtrum . nn6gås            

nn_ou_examen :: String -> Entry
nn_ou_examen = substUtrum . nnoexamen

nn_ou_emeritus :: String -> Entry
nn_ou_emeritus = substUtrum . nnoemeritus       

nn_vv_abdomen :: String -> Entry
nn_vv_abdomen = substVack . nnvabdomen

nn_vn_centrum :: String -> Entry
nn_vn_centrum = substNeutrum . nnvcentrum        

nn_vn_nomen :: String -> Entry
nn_vn_nomen = substNeutrum . nnvnomen

nn_vu_jojo :: String -> Entry
nn_vu_jojo = substUtrum . nnvjojo

nn_vu_partner :: String -> Entry
nn_vu_partner = substUtrum . nnvpartner        
