module CommandsSw where

import BuildSw
import Frontend
import TypesSw
import GenRulesSw
import General
import Dictionary
import Attr
import Data.Maybe(catMaybes)
import qualified Data.Map as Map

commands :: [(String, [String], [String] -> Entry)]
commands = diverse_paradigms ++ pm_paradigms  ++ verb_paradigms ++ vbm_paradigms

suffix :: Int -> Entry -> String ->Entry
suffix n e s = map_wordforms ((tk n s)++) e

unDEFINED :: String -> Entry
unDEFINED _ = emptyEntry

nocmp :: (String -> Entry) -> (String -> Entry)
nocmp f = \s -> remove_param "sms" $ remove_param "c" $ remove_param "ci" $ remove_param "cm" $ f s

nocm :: (String -> Entry) -> (String -> Entry)
nocm f = \s -> remove_param "cm" $ f s

nosms :: (String -> Entry) -> (String -> Entry)
nosms f = \s -> remove_param "sms" $ f s

combine_vbm :: (String, [String], [String] -> Entry) -> (String, [String], [String] -> Entry) -> (String, [String], [String] -> Entry)
combine_vbm (s,xs,f) (_,_,f1) = (s,xs,\xs -> combine_tables (f xs) (f1 xs))

vbma_paradigm p xs part =
    combine_vbm (vbm_paradigm p xs)
               (paradigm_h p xs (verb_vbm 0 ([e ""], [], [], [], [],  part)))

vbm_paradigm :: String -> [String] -> (String, [String], [String] -> Entry)
vbm_paradigm p xs = case catMaybes [Map.lookup v_id verb_map | v_id <- vids] of
                      _ | pos /='1' -> paradigm_h p xs vbm_inf
                      (f:_) -> paradigm_h p xs $ expand . clean . first_mw "vbm" (f . (:[]))
                      _     -> paradigm_h p xs vbm_inf
 where verb_map = Map.fromList [(s,f) | (s,_,f) <- verb_paradigms]
       clean = (if is_reflexive p then remove_param "s-form" else id) . remove_param "sms" . remove_param "c" 
               . (if has_pret_part then id else remove_param "pret_part")
       expand = (if is_poss p then expand_poss else (if is_reflexive p then expand_ref else id))
       expand_poss = expand_multiword "sin" [([],[],"min"),([],[],"din"),([],[],"sin"),([],[],"vår"),([],[],"er")] .
                     expand_multiword "sina" [([],[],"mina"),([],[],"dina"),([],[],"sina"),([],[],"våra"),([],[],"era")]
       expand_ref  = expand_multiword "sig" [([],["imper"],"mig"),([],[],"dig"),([],["imper"],"sig"),([],["imper"],"oss"),([],[],"er")]
       vids = case p of
                (v:b:m:u:k:a:rest) ->
                    case span (/= '_') rest of
                      (_,r) -> [(v:b:u:k:a:r),(v:b:u:k:'a':r),(v:b:u:k:'m':r)]
       has_pret_part = case p of
                         (v:b:m:u:k:a:d:rest) -> a == 'a'
       pos = case span (/='_') (reverse p) of
               (_,(_:n:_)) -> n
       is_reflexive p =  case p of
                          (v:b:m:u:k:a:d:rest) -> d == 's'  
       is_poss p = case span (/='_') (reverse p) of
                     (_,(_:s)) -> case span (/='_') s of 
                                    (s',_) -> elem 'z' s'
                                    
diverse_paradigms =
 [
  paradigm_h "ab_2_bra" ["bra"] $ suffix 3 (ab_bra ["bra"] ["bättre"] ["bäst"]),
  paradigm_h "ab_2_nära" ["nära"] $ 
   suffix 4 (ab_bra ["nära"] ["närmare", "närmre"] ["närmast", "närmst"]),
  paradigm_h "ab_2_mycket" ["mycket"] $ suffix 6 (ab_bra ["mycket"] ["mer","mera"] ["mest"]),
  paradigm_h "ab_2_länge" ["länge"] $ suffix 5 (ab_bra ["länge"] ["längre"] ["längst"]),
  paradigm_h "ab_2_illa" ["illa"] $ 
    suffix 4 (ab_bra ["illa"] ["sämre","värre"] ["sämst","värst"]),
  paradigm_h "ab_2_gärna" ["gärna"] $ suffix 5 (ab_bra ["gärna"] ["hellre"] ["helst"]),
  paradigm_h "ab_2_föga" ["föga"]  $ suffix 4 (ab_bra ["föga"] ["mindre"] ["minst"]),
  paradigm_h "ab_ik_vidare" ["vidare"] $
    suffix 1 (ab_comp [] ["e"] [] ["e"]),
  paradigm_h "ab_2_lite" ["lite"] $
     suffix 4 (ab_bra ["lite"] ["mindre"] ["minst"]),
  -- paradigm_h "ie_i_att" ["att"] inf_mark,
  paradigm_h "sn_i_om" ["om"] subj,
  paradigm_h "abh_i_ledes" ["ledes"] $ 
   replace_attr w_attr s_attr . set_pos "abh" . ab_bort,  
  paradigm_h "avh_1_aktig" ["aktig"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_1_blek,
  paradigm_h "avh_1_bent" ["bent"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_1_akut,
  paradigm_h "avh_0_artad" ["artad"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_0_konstlad,
  paradigm_h "avh_0_buren" ["buren"] $ 
   replace_attr w_attr s_attr . set_pos "avh" .
    adj 2 ([e "en"], [e "et"], [e "na"], [e "na"], [], [], []),
  paradigm_h "nnh_4u_bo" ["bo"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . nn4,
  paradigm_h "nnh_2u_siding" ["siding"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_compound 0 Utr ([e ""], [e "en"], [e "ar"], [e "arna"], [(ds, "")],[(ds, "")]),
  paradigm_h "nnh_2u_nyckel" ["jävel"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_compound 0 Utr ([e ""], [e "n"], [(mmr.dvu,"ar"),(mmr.dv, "ar")], [(mmr.dvu,"arna"),(mmr.dv,"arna")],[e ""],[e "",(ds,"")]),      
  paradigm_h "nnh_6u_tonnare" ["tonnare"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" .    
    noun_compound 0 Utr ([e ""], [e "n"], [e ""], [(tk 1, "na")],[(tk 1, "")],[(ds_drop,"")]), 
  paradigm_h "nnh_dn_snåret" ["snåret"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_f 0 Neutr  ([], [e ""], [], []),
  paradigm_h "nnh_du_årsåldern" ["årsåldern"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_f 0 Utr  ([], [e ""], [], []),
  paradigm_h "nn_3n_parti"     ["parti"]       nn3_parti,
  paradigm_h "nn_3u_fiber"     ["fiber"] $
   noun_compound 0 Utr ([e ""], [e "n"], [(dvu,"er")], [(dvu,"erna")],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_3u_tand"      ["tand"]        nn3_tand,
  paradigm_h "nn_3u_film"      ["film"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_3u_bygd"      ["bygd"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e "",e "e"],[e "e", (ds,"")]),   
  paradigm_h "nn_3u_plan"      ["plan"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e ""],[e "e",e "", (ds,"")]),   
  paradigm_h "nn_3u_akademi"   ["akademi"] $
   noun_compound 0 Utr ([e ""], [e "n",e "en"], [e "er"], [e "erna"],[e ""],[e ""]),   
  paradigm_h "nn_dn_rubbet"   ["rubbet"] $
   noun_compound 0 Neutr  ([], [e ""], [], [],[],[]),   
  paradigm_h "nn_dp_tropikerna"   ["tropikerna"] $
   noun_compound 0 GPl ([], [], [], [e ""],[],[]),   
  paradigm_h "nn_du_stampen"   ["stampen"] $
   noun_compound 0 Utr ([], [e ""], [], [],[],[]),   
  paradigm_h "nn_np_ordalag"   ["ordalag"] $
   noun_f 0 GPl ([], [], [e ""], [e "en"]),   
  paradigm_h "nn_rp_benvärmare"   ["benvärmare"] $
   noun_f 0 GPl ([], [], [e ""], [(tk 1, "na")]),   
  paradigm_h "nn_rp_griller"   ["griller"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[],[]),   
  paradigm_h "nn_rp_inälvor"   ["inälvor"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . tk 2,"")],[(ds . tk 2,"")]),   
  paradigm_h "nn_rp_johannesnycklar"   ["johannesnycklar"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 3,"el"),(tk 3,"els")],[(tk 3,"el"),(tk 3,"els")]),   
  paradigm_h "nn_rp_kläder"   ["kläder"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,""),(ds . tk 2,""),(tk 2,"es")],[(tk 2,""),(ds . tk 2,""),(tk 2,"es")]),   
  paradigm_h "nn_rp_kråkfötter"   ["kråkfötter"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . vc "o" . tk 3,"")], [(ds . vc "o" . tk 3,"")]),   
  paradigm_h "nn_rp_paltor"   ["paltor"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"")],[(tk 2,""),(ds . tk 2,"")]),   
  paradigm_h "nn_rp_specerier"   ["specerier"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"")],[(tk 2,"")]),   
  paradigm_h "nn_rp_stadgar"   ["stadgar"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"e")],[(tk 2,"e")]),   
  paradigm_h "nn_rp_svear"   ["svear"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 1,"")],[(tk 1,"")]),   
  paradigm_h "nn_rp_underkläder"   ["underkläder"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . tk 2,""),(tk 2,"es")],[(ds . tk 2,""),(tk 2,"es")]),   
  paradigm_h "pn_o_sån" ["sån"] $ 
    pn_nagon ([e "",(tk 1,"dan")],[e "t",(tk 1,"dant")],[e "a",(tk 1,"dana")]),
  paradigm_h "pn_o_all" ["all"] $ 
    pn_nagon ([e ""],[e "t"],[e "a"]),
  paradigm_h "pn_o_varsin" ["varsin"] $ 
    pn_nagon ([e ""],[(tk 1,"tt")],[e "a"]),
  paradigm_h "pnm_o_var_sin" ["var sin"] $ 
    set_pos "pnm" . pn_nagon ([e ""],[(tk 1,"tt")],[e "a"]),

  paradigm_h "pnm_o_var_och_en" ["var och en"] $ 
    set_pos "pnm" . pn_nagon ([e ""],[(tk 7,"t och ett")],[]),

  paradigm_h "pn_o_vem" ["vem"] $ 
    pn_vem ([e ""],[e ""],[e "s"]),
  paradigm_h "pn_o_varandra" ["varandra"] $ 
    pn_vem ([],[e ""],[e "s"]),
  paradigm_h "pn_o_man" ["man"] $ 
   pn_han (P3,Sg) ([e ""],[(tk 3,"en")],[(tk 3, "ens")]),
  paradigm_h "pn_i_vars" ["vars"] $ pn_inv,
  paradigm_h "pn_o_sig" ["sig"] $
    pn_jag (P3,Sg) ([],[e "",(tk 2,"ej")],[(tk 1,"n")],[(tk 1,"tt")],[(tk 1,"na")]),
  paradigm_h "pn_o_ingen" ["ingen"] $
   pn_nagon ([e ""],[(tk 1,"t")],[(tk 2,"a")]),
  paradigm_h "pn_o_den" ["den"] pn_o_den, 
  paradigm_h "pn_o_någon" ["någon"] $
   pn_nagon ([e "", (tk 3,"n")],[(tk 1,"t"), (tk 3,"t")],[(tk 2,"ra")]),
 paradigm_h "pn_o_ingendera" ["ingendera"] $
    pn_nagon ([e ""],[(tk 5,"tdera")],[(tk 6,"adera")]),
  paradigm_h "pn_o_vi" ["vi"] $
    pn_jag  (P1,Pl) ([e ""],[(tk 2, "oss")],[(tk 2,"vår"),(tk 2,"våran")],[(tk 2,"vårt"),(tk 2,"vårat")],[(tk 2,"våra")]),
  paradigm_h "pn_o_de" ["de"] $
   pn_han (P3,Pl) ([e "",(vc "o","m")],[(id,"m"),(vc "o","m")],[e "ras"]),
  paradigm_h "pn_o_varenda" ["varenda"] $
   pn_nagon ([e "",(tk 1,"e")], [(ins 4 "t", "")],[]),
  paradigm_h "pn_o_vardera" ["vardera"] $
   pn_nagon ([e ""], [(ins 4 "t", "")],[]),
  paradigm_h "pn_o_varannan" ["varannan"] $
   pn_nagon ([e ""],[(ins 4 "t" . tk 1,"t")],[]),
  paradigm_h "pn_o_var" ["var"] $
   pn_nagon ([e ""],[e "t"],[]),
  paradigm_h "pn_o_samma" ["samma"] $
   pn_nagon ([e "",(tk 1,"e")],[e ""],[e ""]),
  paradigm_h "pn_o_ni" ["ni"] $
    pn_jag  (P2,Pl) ([e ""],[(tk 2, "er"),(tk 2,"eder")],[(tk 2,"er"),(tk 2, "eran"),(tk 2,"eder")],[(tk 2,"ert"),(tk 2,"erat"),(tk 2, "edert")],[(tk 2,"era"),(tk 2,"edra")]),
  paradigm_h "pn_o_jag" ["jag"] $
    pn_jag  (P1,Sg) ([e ""],[(tk 3, "mig"),(tk 3,"mej")],[(tk 3,"min")],[(tk 3,"mitt")],[(tk 3,"mina")]),
  paradigm_h "pn_o_högstdensamme" ["högstdensamme"] $
   pn_nagon ([e ""],[(tk 6,"tsamma")],[(tk 6,"samma")]),
  paradigm_h "pn_o_hon" ["hon"] $
   pn_han (P3,Sg) ([e ""],[(vc "e","ne")],[(tk 2,"ennes")]),
  paradigm_h "pn_o_han" ["han"] $
   pn_han (P3,Sg) ([e ""], [(vc "o","om")], [e "s"]),
  paradigm_h "pn_o_hen" ["hen"] $
   pn_han (P3,Sg) ([e ""], [e "om"], [e "s"]),
  paradigm_h "pn_o_endera" ["endera"] $
   pn_nagon ([e ""],[(tk 5,"ttdera")],[]),
  paradigm_h "pn_o_du" ["du"] $
    pn_jag  (P2,Sg) ([e ""],[(tk 1, "ig"),(tk 1,"ej")],[(tk 1,"in")],[(tk 1,"itt")],[(tk 1,"ina")]),
  paradigm_h "pn_o_densamma" ["densamma"] $
    pn_nagon ([e "",(tk 1,"e")],[(tk 6,"tsamma")],[(tk 6,"samma")]),
  paradigm_h "pn_o_denna" ["denna"] $
   pn_nagon ([e "",(tk 1,"e")],[(tk 3,"tta")],[(tk 3,"ssa")]),
  paradigm_h "pn_o_annan" ["annan"] $
   pn_nagon ([e ""],[(tk 1,"t")],[(tk 3,"dra")]),
  paradigm_h "nn_vu_mixer" ["mixer"]  $         
   noun_f 0 Utr  ([e ""], [e "n"], [(id,""), (dv,"ar"),(id,"s")], [(id,"na"), (dv,"arna"),e "sarna"]),   
  paradigm_h "nn_vu_latte" ["latte"]  $         
   noun_compound 0 Utr  ([e ""], [e "n"], [e "", (tk 1,"ar"),e "s"], [e "na", (tk 1,"arna"),e "sarna"],[e ""],[e ""]),   
  paradigm_h "nn_vn_medium" ["medium"]  $     
   noun_compound 0 Neutr  ([e ""], [(tk 2,"et")], [(tk 2,"er"),(tk 2,"a")], [(tk 2,"erna"),(tk 2,"ana")],[(tk 2,"e"),e ""],[(tk 2,"e"),e ""]),
  paradigm_h "nn_ou_bekant" ["bekant"]   $     
    noun Utr [""] ["en"] ["er","a"] ["erna"],
  paradigm_h "nn_on_memorandum" ["memorandum"]  $  
   noun_f 0 Neutr ([e ""], [e "", e "et"], [(tk 2, "a")], [(tk 2,"ana")]),
  paradigm_h "nn_1u_åder" ["åder"]    $       
   noun_compound 0 Utr ([e "",(dv,"a")], [e "n",(dv,"an")], [(dv,"or")], [(dv,"orna")],[e ""],[(dv,"e"), e ""]),
  paradigm_h "nn_1u_gata" ["gata"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1,""),(ungeminate.tk 1,"u")],[(ungeminate.tk 1,""),(ungeminate.tk 1,"u")]),
  paradigm_h "nn_1u_mamma" ["mamma"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1,"a")],[(tk 1,"e"),(tk 1,"a")]),
  paradigm_h "nn_1u_människa" ["människa"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1,"o")],[(ungeminate.tk 1,"o")]),
  paradigm_h "nn_1u_kamera" ["kamera"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [e ""],[e ""]),
  paradigm_h "nn_1u_olja" ["olja"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e")],[(tk 1, "e")]),
  paradigm_h "nn_1u_baksida" ["baksida"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e"),(tk 1,"es")],[(tk 1, "e"),(tk 1,"es")]),
  paradigm_h "nn_1u_sida" ["sida"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1, ""),(tk 1,"o")],[(tk 1, "e"),(tk 1,"es")]),
  paradigm_h "nn_1u_folksaga" ["folksaga"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e"),(tk 1,"o")],[(tk 1, "e"),(tk 1,"o")]),
  paradigm_h "nn_1u_kyrka" ["kyrka"]    $       
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [(ungeminate,""),e "o"],[(ungeminate,""),e "o",(ds, ""),e "e"]),
  paradigm_h "nn_2u_mening" ["mening"]    $       
   noun_compound 0 Utr ([e ""], [e "en"], [e "ar"], [e "arna"], [(ds, "")],[(ds, "")]),
  paradigm_h "nn_3u_salong" ["salong"]    $       
   noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"], [(ds, "")],[(ds, "")]),
  paradigm_h "nn_5n_dike" ["dike"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [(ds, "")],[(ds, "")]),
  paradigm_h "nn_5n_hjärta" ["hjärta"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [(tk 1, "")],[(tk 1, ""),(ds.tk 1,"")]),
  paradigm_h "nn_5n_saldo" ["saldo"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [e ""],[e ""]),
  paradigm_h "nn_6n_departement" ["departement"]    $       
   noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"], [(ds, "")],[(ds, "")]),
  paradigm_h "nn_0u_svenska" ["svenska"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [], [], [(tk 1,"a"),(tk 1,"")],[(tk 1,"a"),(tk 1,"")]),
  paradigm_h "nn_vu_kart" ["kart"]   $       
   noun Utr [""] ["en"] ["ar",""] ["arna","en"],
  paradigm_h "nn_vn_mirakel" ["mirakel"]   $      
   noun_f 0 Neutr ([e ""], [(dv,"et")], [(id,""),(dv,"er")], [(dv,"en"), (dv,"erna")]),
  paradigm_h "nn_0v_trim" ["trim"]  $  
   noun_f 0 Pend  ([e ""], [(geminate,"en"), (geminate,"et")], [], []),
  paradigm_h "nn_0v_blod" ["blod"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[e "",(ds,"")],[e "",(ds,"")]),
  paradigm_h "nn_0v_saffran" ["saffran"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[(ds,"")],[(ds,"")]),
  paradigm_h "nn_0v_gin" ["gin"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[e ""],[e "",(ds,"")]),
  paradigm_h "nn_0v_tö" ["tö"]  $  
   noun_f 0 Pend  ([e ""], [e "et", e "n", e "t"], [], []),
  paradigm_h "nn_0n_bitumen" ["bitumen"]  $
   noun_f 0 Neutr ([e ""], [(id, ""),(vc "i", "et")], [], []),
  paradigm_h "nn_vn_lexikon" ["lexikon"]  $      
   noun_f 0 Neutr ([e ""], [e "et"], [(id,""),(tk 2,"a")], [e "en"]),
  paradigm_h "nn_2u_slarver" ["slarver"]  $    
   noun_f 0 Utr ([e ""], [e "n"], [(dvu,"ar"),(tk 2,"ar")], [(dvu,"arna"), (tk 2,"arna")]),
  paradigm_h "nn_2u_bräken" ["bräken"]  $    
   noun_f 0 Utr ([e ""], [e "", (dv, "en")], [(dv,"ar")], [(dv,"arna")]),
  paradigm_h "nn_2u_himmel" ["himmel"]  $     
   noun_f 0 Utr ([e ""], [(tk 3,"len"), (id,"en"),(id,"n")], [(tk 3, "lar")], [(tk 3, "larna")]),
  paradigm_h "nn_4n_fängelse" ["fängelse"]  $    
   noun Neutr [""] ["t"] ["r"] ["rna"],
  paradigm_h "nn_2u_dag" ["dag"]  $      
   noun_f 0 Utr ([e ""], [(id,"en"),(tk 1,"n")], [(id,"ar"),(tk 1,"r")], [(id,"arna"),(tk 1,"rna")]),
  paradigm_h "nn_vu_ponny" ["ponny"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1, "ier"),(tk 1,"ies"),(id,"er"), e "sar"], [(tk 1,"ierna"), e "erna",(tk 1,"iesarna"), e "sarna"]),
  paradigm_h "nn_vu_kollega" ["kollega"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1,"er"),(tk 1,"or")], [(tk 1,"erna"),(tk 1,"orna")]),
  paradigm_h "nn_vn_kolli" ["kolli"]  $       
   noun Neutr [""] ["t"] ["","n"] ["na"],
  paradigm_h "nn_6u_tum" ["tum"]  $        
   noun Utr [""] ["men"] [""] ["men"],
  paradigm_h "nn_6n_universum" ["universum"]  $    
   noun Neutr [""] ["", "et"] [""] ["en"],
  paradigm_h "nn_0n_gluten" ["gluten"]  $ 
   noun_compound 0 Neutr ([e ""], [e "et",e ""],[],[],[e ""],[e ""]),
  paradigm_h "nn_vu_yard" ["yard"]  $        
   noun Utr [""] ["en"] ["","s"] ["en","sen"],
  paradigm_h "nn_vu_svan" ["svan"]  $         
   noun Utr [""] ["en"] ["ar","or"] ["arna","orna"],
  paradigm_h "nn_vn_tema" ["tema"]  $          
   noun Neutr [""] ["t"] ["n","ta"] ["na","tana"],
  paradigm_h "nn_vn_maximum" ["maximum"]  $      
   noun_f 0 Neutr ([e ""], [e "",e "et",(tk 2, "et")], [(id,""),(tk 2,"a")],[(tk 2,"ana"),(id,"en")]),
  paradigm_h "nn_vn_frö" ["frö"]  $         
   noun Neutr [""] ["et","t"] ["er","n"] ["erna","na","en"],
  paradigm_h "nn_3u_materia" ["materia"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1,"er")], [(tk 1,"erna")]),
  paradigm_h "nn_0n_delirium" ["delirium"]  $
   noun_f 0 Neutr ([e ""],[(tk 2, "et")],[],[]),
  paradigm_h "nn_vv_libretto" ["libretto"]  $
   noun Pend [""] ["n","t"] ["r","n"] ["rna","na"],
  paradigm_h "nn_vu_safari" ["safari"]  $
   noun Utr [""] ["n"] ["er","s"] ["erna","sarna"],
  paradigm_h "nn_vu_bungalow" ["bungalow"]  $
   noun Utr [""] ["en"] ["er","s"] ["erna"],
  paradigm_h "nn_ip_honoratiores" ["honoratiores"]  $
   noun_no_genitive GPl ([],[],[e ""],[]),
  paradigm_h "nn_7u_lady" ["lady"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"ies")],[(tk 1,"ies"),(tk 1,"iesarna")]),
  paradigm_h "nn_6v_kvitten" ["kvitten"]  $
   noun Pend [""] [""] [""] ["a"],
  paradigm_h "nn_vv_franska" ["franska"]  $
   noun_f 0 Pend ([e ""],[e "n",e "t"],[(id,""),(tk 1,"or")], [e "na",(tk 1,"orna")]),
  paradigm_h "nn_4v_folie" ["folie"]  $
   noun Pend [""] ["n","t"] ["r"] ["rna"],
  paradigm_h "nn_3u_donjuan" ["donjuan"]  $
   noun Utr [""] [""] ["er"] ["erna"],
  paradigm_h "nn_2v_finger" ["finger"]  $
   noun_f 0 Pend ([e ""], [(dv,"et"),(id,"n")], [(dv,"ar")],[(dv,"arna")]),
  paradigm_h "nn_2u_biceps" ["biceps"]  $
   noun Utr [""] ["", "en"] ["ar"] ["arna"],
  paradigm_h "nn_1u_ultima" ["ultima"]  $
   noun_f 0 Utr ([e ""], [e "", e "n"], [(tk 1,"or")], [(tk 1,"orna")]),           
  paradigm_h "nn_0n_opium" ["opium"]  $
   noun_compound 0 Neutr ([e ""], [(tk 2,"et"),(id,"et")],[],[],[e ""],[e ""]),
  paradigm_h "nn_vv_skogsrå" ["skogsrå"]  $
   noun Pend [""] ["et","t","n"] ["r","n"] ["rna","na"],
  paradigm_h "nn_vv_prisma" ["prisma"]  $
   noun_f 0 Pend ([e ""], [e "n",e "t"], [(tk 1, "or"),(tk 1,"er")], [(tk 1,"orna"),(tk 1,"erna")]),
  paradigm_h "nn_vv_hult" ["hult"]   $
   noun Pend [""] ["et","en"] ["","ar","er"] ["en","arna","erna"],
  paradigm_h "nn_vu_spaniel" ["spaniel"]   $   
   noun Utr [""] ["n"] ["ar","s"] ["arna"],
  paradigm_h "nn_vu_litteraturkanon" ["litteraturkanon"]  $
   noun Utr [""] ["en",""] ["er",""] ["erna"],
  paradigm_h "nn_vu_igloo" ["igloo"]  $
   noun_compound 0 Utr ([e ""], [e "n"], [e "r",e "er",e "s", e "sar"],[e "rna",e "erna",e "sarna"],[e ""],[e ""]),
  paradigm_h "nn_vn_alfa_io" ["i"] $ nocm $ 
   set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "t", e "et"],[e "ts", e "ets"],[e "n", e ""],[e "ns",e "s"],[e "na",e "en"],[e "nas",e "ens"]),
  
  -- paradigm_h "nn_6n_sms" ["sms"] $
  -- set_pos "nn" . nna Neutr ([e ""],[(ds,"")], [e "et"],[e "ets"],[e ""],[(ds, "ens"],[e "ena"],[e "enas"]),
  paradigm_h "nn_6n_deponens" ["deponens"] $
   noun Neutr [""] [""] [""] ["en"],
  paradigm_h "nn_6n_andeväsen" ["andeväsen"] $
   noun Neutr [""] ["det"] [""] ["a"],
  paradigm_h "nn_5n_altare" ["altare"] $
   noun_compound 1 Neutr ([e "e"], [e "et"], [e "en"], [e "na"],[e ""],[e ""]),
  paradigm_h "nn_3u_geranium" ["geranium"]  $
   noun_f 0 Utr ([e ""],[(tk 2,"en")],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_1u_tavla" ["tavla"] $
   noun_compound 0 Utr ([e ""], [e "n"],[(dv,"or")],[(dv,"orna")],[((\s -> insert_second_last s 'e') . tk 1,"")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")]),
  paradigm_h "nn_1u_stamtavla" ["stamtavla"] $
   noun_compound 0 Utr ([e ""], [e "n"],[(dv,"or")],[(dv,"orna")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")]),
  paradigm_h "nn_0v_dregel" ["dregel"] $
   noun_compound 0 Pend ([e ""],[(id,"n"), (dvu,"et")], [], [],[e ""],[e ""]),
  paradigm_h "nn_vv_paraply" ["paraply"] $
   noun Pend [""] ["et","t","n"] ["er","n"] ["erna","na"],
  paradigm_h "nn_vv_etage" ["etage"]  $
   noun Pend [""] ["t","n"] ["r",""] ["rna","n"],
  paradigm_h "nn_vv_chiffer" ["chiffer"] $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")],[(id,""),(dv,"er")],[(dv,"en"),(dv,"erna")]),
  paradigm_h "nn_vv_bolster" ["bolster"] $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")],[(id,""),(dv,"ar")],[(dv,"arna"), (dv,"en"),(id,"na")]),
  paradigm_h "nn_vu_teve" ["teve"] $
   noun Utr [""] ["n"] ["","ar"] ["na","arna"],
  paradigm_h "nn_vu_rhododendron" ["rhododendron"] $
   noun_f 0 Utr ([e ""], [e "en"], [e "",(tk 2, "er")], [(tk 2, "erna")]),
  paradigm_h "nn_vu_kofot" ["kofot"] $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(umlaut . geminate,"er")],[(id,"arna"),(umlaut . geminate,"erna")]),
  paradigm_h "nn_vu_jourhavande" ["jourhavande"]  $
   noun Utr [""] ["n"] ["","n"] ["na"],
  paradigm_h "nn_vu_jockey" ["jockey"] $
   noun_compound 0 Utr ([e ""],[e "n",e "en"],[e "ar",e "er",e "s",e "sar"],[e "arna",e "erna",e "sarna"],[e ""],[e ""]),
  paradigm_h "nn_vu_grej" ["grej"] $
   noun Utr [""] ["en"] ["er","or"] ["erna","orna"],
  paradigm_h "nn_vu_drive" ["drive"] $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"ar"),(id,"s")],[(tk 1,"arna")]),
  paradigm_h "nn_vu_cello" ["cello"] $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"r"),(tk 1,"i")],[(tk 1,"orna")]),
  paradigm_h "nn_vn_trauma" ["trauma"] $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"er")],[e "na",(tk 1,"erna")]),
  paradigm_h "nn_vn_stall" ["stall"] $
   noun Neutr [""] ["et"] ["","ar"] ["en","arna"],
  paradigm_h "nn_vn_pi" ["pi"] $
   noun Neutr [""] ["et","t"] ["","n"] ["na","en"],
  paradigm_h "nn_vn_paper" ["paper"] $
   noun Neutr [""] ["et"] ["s",""] ["en"],
  paradigm_h "nn_vn_panorama" ["panorama"] $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"or")],[e "na",(tk 1,"orna")]),
  paradigm_h "nn_vn_logi" ["logi"] $
   noun Neutr [""] ["et","t"] ["er","n"] ["erna","na"],
  paradigm_h "nn_ou_medikus" ["medikus"]  $
   noun_f 0 Utr ([e ""], [e "",e "en"],[(tk 3,"ci")], [(tk 3,"cina")]),
  paradigm_h "nn_6v_årder" ["årder"] $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")], [e ""],[(dv,"en"),(id,"na")]),
  paradigm_h "nn_6v_hästskosöm" ["hästskosöm"]  $
   noun_f 0 Pend ([e ""], [(geminate,"et"),(geminate,"en")], [e ""], [(geminate,"en")]),
  paradigm_h "nn_6u_bror" ["bror"]   $
   noun_compound 0 Utr ([(tk 1,"der"), e ""],[(tk 1,"dern"),(id,"n")], [(umlaut . tk 1, "der")], [(umlaut . tk 1, "derna")],[e "",(ds,"")],[e "",(ds,"")]),
  paradigm_h "nn_6n_pansar" ["pansar"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e ""],[e "na",e "en"],[e ""],[e ""]),
  paradigm_h "nn_3n_center" ["center"]  $
   noun_f 0 Neutr ([e ""],[(dv,"et")],[(dv,"er")],[(dv,"erna")]),
  paradigm_h "nn_6n_ankare" ["ankare"]  $
   noun_compound 0 Neutr ([e ""], [e "t"], [e "",e "n"],[(tk 1,"na")],[(tk 1,"")],[(tk 1,"")]),
  paradigm_h "nn_3v_plasma" ["plasma"]  $
   noun_f 0 Pend ([e ""], [e "t", e "n"],[(tk 1,"er")],[(tk 1,"erna")]),
  paradigm_h "nn_3u_papyrus" ["papyrus"]  $
   noun_f 0 Utr ([e ""],[(tk 2,"en"), (id,"en")],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3n_alluvium" ["alluvium"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2, "et")],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3n_alkali" ["alkali"]  $
   noun Neutr [""] ["t"] ["er"] ["erna"],
  paradigm_h "nn_3n_gift" ["gift"] $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[e ""],[e "",(ds,"")]),
  paradigm_h "nn_3n_histamin" ["histamin"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[e ""],[e ""]),
  paradigm_h "nn_3n_portvin" ["portvin"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[(ds,"")],[(ds,"")]),
  paradigm_h "nn_3u_tid" ["tid"]  $
   noun_compound 0 Utr ([e ""],[e "en"],[e "er"],[e "erna"],[e "",(ds,"")],[e "",(ds,"")]),
  paradigm_h "nn_2v_skit" ["skit"]  $
   noun Pend [""] ["en","et"] ["ar"] ["arna"],
  paradigm_h "nn_2u_toddy" ["toddy"]  $
   noun_f 0 Utr ([e ""],[e "n"],[e "ar", (tk 1, "ar")],[e "arna", (tk 1,"arna")]),
  paradigm_h "nn_0u_praxis" ["praxis"]  $
   noun Utr [""] ["en",""] [] [],
  paradigm_h "nn_vv_stimulus" ["stimulus"]  $
   noun_f 0 Pend ([e ""], [e "", e "en", e "et"],[(id ,""),(tk 2,"i")], [e "en",(tk 2, "ina")]),
  paradigm_h "nn_vv_rå_gång" ["rå"]  $
   noun Pend [""] ["n","et","t"] ["r"] ["rna"],
  paradigm_h "nn_vv_ringfinger" ["ringfinger"]  $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")], [(dv,"ar"),(id,"")], [(dv,"arna")]),
  paradigm_h "nn_vv_ordal" ["ordal"]  $
   noun Pend [""] ["et","en"] ["","ier"] ["ierna"],
  paradigm_h "nn_vv_halvankare" ["halvankare"]  $
   noun_f 0 Pend ([e ""], [e "t",e "n"], [e "",e "n"],[(tk 1, "na")]),
  paradigm_h "nn_vu_western" ["western"]  $     
   noun_f 0 Utr ([e ""],[e ""],[e "",e "s"],[e "a"]),
  paradigm_h "nn_vu_torso" ["torso"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"er"),(id,"er"),(id,"r")], [(tk 1,"erna"),(id,"erna"),(id,"rna")]),
  paradigm_h "nn_vu_spång" ["spång"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(vc "ä","er")], [(id,"arna"),(vc "ä","erna")]),
  paradigm_h "nn_vu_scarf" ["scarf"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(tk 1,"ves")],[(id,"arna"),(tk 1,"vesen")]),
  paradigm_h "nn_vu_rubel" ["rubel"]  $
   noun_f 0 Utr ([e ""], [e "n"],[e "",(dvu,"er")],[(dvu,"erna")]),
  paradigm_h "nn_vu_ro" ["ro"]   $
   noun Utr [""] ["n"] ["n","r"] ["na","rna"],
  paradigm_h "nn_vu_promovend" ["promovend"]  $
   noun Utr [""] ["en"] ["er","i"] ["erna","ina"],
  paradigm_h "nn_vu_preses" ["preses"]  $
   noun_f 0 Utr ([e ""], [e ""],[(tk 2, "ides"),(id,"ar")], [e "arna"]),
  paradigm_h "nn_vu_paria" ["paria"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"s"),(tk 1,"or")],[(tk 1,"orna")]),
  paradigm_h "nn_vu_mikron" ["mikron"]  $
   noun Utr [""] ["en"] ["er",""] ["erna","en"],
  paradigm_h "nn_vu_lama" ["lama"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"or"),(id,"er")],[(tk 1,"orna"),(id,"erna")]),
  paradigm_h "nn_vu_glass" ["glass"]  $
   noun Utr [""] ["en"] ["ar","er",""] ["arna","erna"],
  paradigm_h "nn_vu_gladiolus" ["gladiolus"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,""),(tk 2,"er"),(id,"ar")], [(tk 2,"erna"),(id,"arna")]),
  paradigm_h "nn_vu_baby" ["baby"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"ar"),(id,"er"),(tk 1,"ies")], [e "arna",e "erna",(tk 1,"iesarna")]),
  paradigm_h "nn_vu_albino" ["albino"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"er"),(id,"s")],[(tk 1,"erna"),(id,"sarna")]),
  paradigm_h "nn_vn_stånd" ["stånd"]  $
   noun_f 0 Neutr ([e ""], [e "et"],[(id,""),(vc "ä","er")],[(id,"en"),(vc "ä","erna")]),
  paradigm_h "nn_vn_solo" ["solo"]  $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"i")], [e "na"]),
  paradigm_h "nn_vn_serum" ["serum"]  $
   noun_f 0 Neutr ([e ""], [e "",e "et"],[(id,""),(tk 2,"a")], [(tk 2,"ana"),e "en"]),
  paradigm_h "nn_vn_rö" ["rö"]  $
   noun Neutr [""] ["t","et"] ["","n"] ["na"],
  paradigm_h "nn_vn_rekviem" ["rekviem"]  $
   noun_f 0 Neutr ([e ""],[(id,""),(tk 1,"t"),(tk 1,"met")],[(id,""),(tk 1,"r")],[(id,"en"),(tk 1,"rna")]),
  paradigm_h "nn_vn_omen" ["omen"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2,"inet")],[(id,""),(tk 2,"ina")],[(id,"en")]),
  paradigm_h "nn_vn_mineral" ["mineral"]  $
   noun Neutr [""] ["et"] ["er", "ier"] ["erna","ierna"],
  paradigm_h "nn_vn_lim" ["lim"]  $
   noun Neutr [""] ["met"] ["", "mer"] ["men","merna"],
  paradigm_h "nn_vn_kompositum" ["kompositum"]  $
   noun_f 0 Neutr ([e ""],[(id,""),(tk 2,"et")],[(tk 2,"a"), (tk 2,"er")], [(tk 2,"erna")]),
  paradigm_h "nn_vn_ja" ["ja"]  $
   noun Neutr [""] ["et","t"] ["n", ""] ["na"],
  paradigm_h "nn_vn_härad" ["härad"]  $
   noun Neutr [""] ["et"] ["", "er","en"] ["ena","erna"],
  paradigm_h "nn_vn_gag" ["gag"]  $
   (noun Neutr [""] ["et"] ["s", ""] ["sen","en"]),
  paradigm_h "nn_vn_gage" ["gage"]  $
   noun_compound 0 Neutr ([e ""],[e "t"],[e "",e "r"],[e "n",e "rna"],[e ""],[e ""]),
  paradigm_h "nn_vn_apropå" ["apropå"]   $       
   noun Neutr [""] ["t"] ["n","er"] ["na","erna"],
  paradigm_h "nn_vn_alfa_z" ["z"]  $ nocm $
   set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],[e "n", e ""],[e "ns",e "s"],[e "na"],[e "nas"]),
  paradigm_h "nn_vn_ackordion" ["ackordion"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2,"et")],[(id,""),(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_ov_styck" ["styck"]  $
   noun_compound 0 Pend ([e "",e "en",e "na"],[e "en",e "et"],[e "", e "en",e "na"],[e "ena"],[e ""],[e ""]),
  paradigm_h "nn_ov_diktamen" ["diktamen"]  $
   noun_f 0 Pend ([e ""], [e ""],[(tk 2,"ina")], [(tk 2,"ina")]),
  paradigm_h "nn_ou_putto" ["putto"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"i")], [(tk 1,"ina")]),
  paradigm_h "nn_ou_penny" ["penny"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 2,"ce"),(tk 1,"ies")], [(tk 2,"cen"),(tk 1,"iesarna")]),
  paradigm_h "nn_ou_mekanikus" ["mekanikus"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(tk 3,"ci")], [(tk 3,"cina")]),
  paradigm_h "nn_on_gravamen" ["gravamen"]  $
   noun_f 0 Neutr ([e ""], [e "et"],[(tk 2,"ina")], []),
  paradigm_h "nn_7u_slogan" ["slogan"]  $
   noun Utr [""] ["","en"] ["s"] ["sen"],
  paradigm_h "nn_vu_romkom" ["romkom"]  $
   noun_compound 0 Utr ([e ""],[e "en",e "men"],[e "s", e "mar"],[e "sen",e "marna"],[e ""],[e ""]),
  paradigm_h "nn_vu_tortilla" ["tortilla"]  $
   noun_compound 1 Utr ([e "a"],[e "an"],[e "or", e "as"],[e "orna",e "asen",e "asena",e "asarna"],[e "a"],[e "a"]),
  paradigm_h "nn_7u_cashew" ["cashew"]  $
   noun_compound 0 Utr ([e ""],[e "en",e "n"],[e "s"],[e "sen"],[e ""],[e ""]),
  paradigm_h "nn_7n_skinhead" ["skinhead"]  $
   noun Neutr [""] ["et"] ["s"] ["sen"],
  paradigm_h "nn_6v_modus" ["modus"]  $
   noun Pend [""] ["","et"] [""] ["en"],
  paradigm_h "nn_6v_data" ["data"]  $
   noun_compound 0 Pend ([e ""],[e "n",e "t"],[e ""],[e "na"],[e ""],[e ""]),
  paradigm_h "nn_6u_man" ["man"]   $
   noun_f 0 Utr ([e ""], [e "nen"], [(umlaut,""),(id,""),(id,"nar")],[(umlaut,"nen"),(id,"narna")]),
  paradigm_h "nn_6u_iktus" ["iktus"]  $
   noun Utr [""] ["en", ""] [""] ["en"],
  paradigm_h "nn_6n_interregnum" ["interregnum"]  $
   noun_f 0 Neutr ([e ""], [e "et",e ""], [e ""], [e "en"]),
  paradigm_h "nn_5n_ri" ["ri"]  $
   noun Neutr [""] ["et"] ["n"] ["en","na"],
  paradigm_h "nn_3v_gelé" ["gelé"]  $
   noun_compound 0 Pend ([e ""],[e "n",e "t",e "et"],[e "er"],[e "erna"],[e ""],[e ""]),
  paradigm_h "nn_3u_fotnot" ["fotnot"]  $
   noun_f 0 Utr ([e ""], [e "en"], [(id,"er"),(vc "ö","ter")],[(id,"erna"),(vc "ö","terna")]),
  paradigm_h "nn_3u_farao" ["farao"]  $
   noun Utr [""] ["","n"] ["ner"] ["nerna"],
  paradigm_h "nn_3u_eforus" ["eforus"]  $
   noun_f 0 Utr ([e ""], [e ""],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3n_seminarium" ["seminarium"]  $
   noun_f 0 Neutr ([e ""], [e "et",(tk 2, "et")],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3n_futurum" ["futurum"]  $
   noun_f 0 Neutr ([e ""], [e "",e "et"],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3n_dominion" ["dominion"]  $
   noun_f 0 Neutr ([e ""], [e ""],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_3v_aktivum" ["aktivum"]  $
   noun_f 0 Pend ([(tk 2,""), e ""],[(tk 2,"et"),(tk 2,"en"),(id,"et"),(id,"")],[(tk 2,"er")],[(tk 2,"erna")]),
  paradigm_h "nn_2u_stygger" ["stygger"]  $
   noun_f 0 Utr ([e ""],[(id,"n"),(tk 1,"n")],[(tk 2,"ar")],[(tk 2,"arna")]),
  paradigm_h "nn_2u_förmiddag" ["förmiddag"]  $ 
   noun_f 0 Utr ([e ""],[(id,"en"),(tk 1,"n")],[(id,"ar")],[(id,"arna")]),
  paradigm_h "nn_2u_andur" ["andur"]  $
   noun_f 0 Utr ([e ""], [e "en",e "n"],[(dvu,"ar")],[(dvu,"arna")]),
  paradigm_h "nn_1v_antibiotika" ["antibiotika"]  $
   noun_f 0 Pend ([e ""], [e "n",e "t"],[(tk 1,"or")],[(tk 1,"orna")]),
  paradigm_h "nn_0v_status" ["status"]  $
   noun Pend [""] ["","en"] [] [],
  paradigm_h "nn_0v_hysteri" ["hysteri"]  $
   noun Pend [""] ["n","en","et","t"] [] [],
  paradigm_h "nn_0v_facit" ["facit"]  $
   noun_f 0 Pend ([e ""],[e ""],[],[]),
  paradigm_h "nn_0u_makadam" ["makadam"]  $
   noun_f 0 Utr ([e ""], [(geminate,"en"),(id,"en")], [], []),
  paradigm_h "nn_0u_aorta" ["aorta"]  $
   noun Utr [""] ["","n"] [] [],
  paradigm_h "nn_0n_karborundum" ["karborundum"]  $
   noun Neutr [""] ["","et"] [] [],
  paradigm_h "nn_0n_kammarkollegium" ["kammarkollegium"]  $
   noun_f 0 Neutr ([e ""],[e "",(tk 2,"et")],[],[]),
  paradigm_h "nn_0n_gehenna" ["gehenna"]   $
   noun Neutr [""] ["","t"] [] [],
  paradigm_h "nn_2u_bövel" ["bövel"]  $
   noun_f 0 Utr ([e ""],[e "n",e "en"],[(dvu,"ar")],[(dvu,"arna")]),
  paradigm_h "av_0s_ypperst" ["ypperst"]  $ 
   adj 0 ([], [], [], [], [], [e ""], [e "a"]), 
  paradigm_h "av_0_korkad" ["korkad"] av_0_konstlad,
  paradigm_h "av_1_enkel" ["enkel"] av_1_vacker,
  paradigm_h "av_in_lurt" ["lurt"] $ 
   replace_param [("invar","pos indef sg n nom")] . av_i_diverse,
  paradigm_h "av_id_norra" ["norra"] $ 
   replace_param [("invar","pos def sg no_masc nom")] . av_i_diverse,
  paradigm_h "av_im_bemälde" ["bemälde"] $
   replace_param [("invar","pos def sg masc nom")] . av_i_diverse,
  paradigm_h "av_ik_smärre" ["smärre"] 
   $ adj 0 ([], [], [], [], [e ""], [], []), 
  paradigm_h "avm_io0_diverse" ["bevänt med"] $ nocmp $ avm_i,
  -- paradigm_h "avm_ix0_diverse" ["allena saliggörande"] $ nocmp $ avm_i,
  paradigm_h "avm_ia0_diverse" ["före detta"] $ nocmp $ avm_i,
  paradigm_h "avm_ip0_diverse" ["idel öra"] $ nocmp $ avm_i,
  paradigm_h "avm_is20_utom_sig" ["utom sig"] $ nocmp $ avm_i,
  paradigm_h "av_0_höger" ["höger"] 
   (adj_no_masc 2 ([e "er"], [e "er"], [e "ra"], [e "ra"], [], [], [])), 
  paradigm_h "av_0_vareviga" ["vareviga"] 
   $ nocmp (adj 0 ([e ""], [(ins 4 "t", "")], [], [],[], [], [])), 
  paradigm_h "av_0_sankt" ["sankt"] 
   (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 
  paradigm_h "av_0_pytteliten" ["pytteliten"] 
   (adjc 5 ([e "liten"], [e "litet"], [e "lilla"], [e "små"], [], [], [],[e "små"])), 
  paradigm_h "av_0s_innersta" ["innersta"] $ 
   (adj 0 ([], [], [], [], [], [e ""], [])), 
  paradigm_h "av_0d_nästa" ["nästa"] 
   (adj 0 ([], [], [e ""], [e ""], [], [], [])), 
  -- paradigm_h "av_0d_enda" ["enda"] 
  --  (adj 0 ([], [], [e ""], [], [], [], [])), 
  paradigm_h "av_v_ond" ["ond"]  
   (adj 0 ([e ""],[(tk 1,"t")], [e "a"], [e "a"], [(id,"are"), (tk 3, "värre")], [(id,"ast"), (tk 3, "värst")], [(id,"aste"),(tk 3, "värsta")])), 
  paradigm_h "av_v_god" ["god"] 
   (adj 0 ([e ""],[(tk 1,"tt")], [e "a"], [e "a"],[(id,"are"),(tk 3, "bättre")], [(id,"ast"),(tk 3,"bäst")],[(id,"aste"),(tk 3, "bästa")])), 
  paradigm_h "av_2_gammal" ["gammal"] 
   (adj 6 ([e "gammal"], [e "gammalt"], [e "gamla"], [e "gamla"], [e "äldre"], [e "äldst"], [e "äldsta"])), 
   paradigm_h "av_2_bra" ["bra"] 
   (adj 2 ([e "ra"], [e "ra"], [e "ra"], [e "ra"], [e "ättre"], [e "äst"], [e "ästa"])), 
  paradigm_h "av_v_nära" ["nära"]  
   (adj 1 ([e "a"], [e "a"], [e "a"], [e "a"], [e "mare",e "mre"], [e "mast",e "mst"], [e "masta",e "msta"])), 
  paradigm_h "av_v_förnäm" ["förnäm"] 
    (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [e "are"], [e "st",e "ast"], [e "sta",e "asta"])), 
  paradigm_h "av_v_dålig" ["dålig"] 
    (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [(tk 5,"sämre"), e "are"], [(tk 5,"sämst"),e "ast"], [(tk 5,"sämsta"), e "aste"])), 
  paradigm_h "av_2k_bakre" ["bakre"] 
    (adj 2 ([], [], [], [], [e "re"], [e "ersta"], [e "ersta"])), 
  paradigm_h "pn_o_mycken" ["mycken"] $ set_pos "pn" . 
     (adj 5 ([e "ycken"], [e "ycket"], [e "yckna"], [e "yckna"], [e "er",e "era"], [e "est"], [e "esta"])), 
  paradigm_h "pn_o_mången" ["mången"] $ set_pos "pn" . 
    (adj_no_masc 0 ([e ""], [(tk 2, "t"),(tk 1, "t")], [(tk 2,"a")], [(tk 2,"a")], [(tk 6,"fler"),(tk 6,"flera")], [(tk 6, "flest")], [(tk 6,"flesta")])), 
  paradigm_h "av_2_liten" ["liten"]  
    (adjc 5 ([e "liten"], [e "litet"], [e "lilla"], [e "små"], [e "mindre"], [e "minst"], [e "minsta"],[e "små"])), 
  paradigm_h "av_2_få" ["få"] 
    (adj 0 ([e ""], [e ""], [e ""], [e ""],[(umlaut, "rre")], [(umlaut,"rst")], [(umlaut,"rsta")])), 
  paradigm_h "av_1_orange" ["orange"] 
    (adj 1 ([e "e"], [e "e", e "t", e "et"], [e "a", e "e", e "ea"], [e "a", e "e", e "ea"], [e "are", e "eare"], [e "ast",e "east"], [e "aste", e "easte"])), 
  paradigm_h "av_1_gratis" ["gratis"] 
    (adj 0 ([e ""], [e "", e "t"], [e "", e "a"], [e "", e "a"], [e "are"], [e "ast"], [e "aste"])), 
  paradigm_h "av_1_knall" ["knall"] 
    (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"])), 
  paradigm_h "av_1_camp" ["camp"]  
    (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"])), 
 -- paradigm_h "av_1_beige" ["beige"] 
 --   (adj 0 ([e ""], [e "t"], [e "",e "a"], [e "",e "a"],[e "are"], [e "ast"], [e "aste"])), 
  paradigm_h "av_1_ball" ["ball"]  $
    adj 0 ([e ""], [e "",e "t"], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"]), 
  paradigm_h "nnm_vu0_playboy" ["dry martini"] $ nocmp $ set_pos "nnm" . nn_vu_playboy,
  paradigm_h "nnm_vv0_libretto" ["allt i allo"]  $
   nocmp $ last_mw "nnm" ((noun Pend [""] ["n","t"] ["r","n"] ["rna","na"])),
  paradigm_h "nnm_iv0_hum" ["prio noll"] $ 
   nocmp $ first_mw "nnm" nn_iv_hum,
  paradigm_h "nnm_iuc_pest_eller_kolera" ["pest eller kolera"] $ nosms $ last_mw "nnm" $ nn_iu_avvaktan,
  paradigm_h "nnm_vu0_kaffe_latte" ["kaffe latte"]  $
   nocmp $ last_mw "nnm" (noun_compound 0 Utr  ([e ""], [e "n"], [e "", (tk 1,"ar"),e "s", e "r"], [e "na", (tk 1,"arna"),e "sarna", e "rna"],[e ""],[e ""])),
  paradigm_h "nnm_vv0_hult" ["bäst före-datum"]   $
   nocmp $ last_mw "nnm" (noun Pend [""] ["et","en"] ["","ar","er"] ["en","arna","erna"]),
  paradigm_h "nnm_0v0_manna" ["lingua franca"] $
    nocmp $ last_mw "nnm" (noun_f 0 Pend ([e ""],[e "n", e "t"],[],[])),
  paradigm_h "nnm_0v0_blod" ["jet lag"] $
   nocmp $ last_mw "nnm" (noun_f 0 Pend ([e ""],[e "en", e "et"],[],[])),
  paradigm_h "nnm_0n0_äkta_stånd" ["äkta stånd"] $
   nocmp $ last_mw "nnm" (noun_f 0 Neutr ([e ""],[e "et"],[],[])),
  paradigm_h "nnm_du0_stampen" ["gordiska knuten"] $
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([],[e ""],[],[])),
  paradigm_h "nnm_vu0_mikron" ["grand danois"] $
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] ["er",""] ["erna"]),
  paradigm_h "nnm_vu0_trio" ["femme fatale"] $
   nocmp $ last_mw "nnm" (noun Utr [""] ["n"] ["r","s"] ["rna"]),
  paradigm_h "nnm_vu0_bungalow" ["spin off"] $
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] ["er","s"] ["erna","sen"]),
  paradigm_h "nnm_vv0_pain_riche" ["pain riche"] $
   nocmp $ last_mw "nnm" (noun Pend [""] ["n","t"] ["r",""] ["rna","na"]),
  paradigm_h "nnm_vv0_deja_vu" ["déjá vu"]   $
   nocmp $ last_mw "nnm" (noun Pend [""] ["n","t"] ["r","n"] ["rna","na"]),
  paradigm_h "nnm_rp0_griller" ["scampi fritti"] $
   nocmp $ last_mw "nnm" (noun_f 0 GPl ([], [], [e ""], [])),   
  paradigm_h "nnm_vn0_alfa_z" ["ettstrukna c"]  $ 
   nocmp $ last_mw "nnm" (nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],[],[],[],[])),
  paradigm_h "nnm_su0_pojke" ["dödens lammunge"] $
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""], [], [(drop_final_e,"ar")], [])),
  paradigm_h "nnm_su0_tro" ["janssons frestelse"] $
   nocmp $ last_mw "nnm" (noun Utr [""] ["n"] [] []),
  paradigm_h "nnm_7u0_hit" ["negro spiritual"]  $
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] ["s"] ["sen"]),  
  paradigm_h "nnm_7n0_skinhead" ["practical joke"]  $
   nocmp $ last_mw "nnm" (noun Neutr [""] ["t"] ["s"] ["sen"]),  
  paradigm_h "nnm_6u0_yen" ["pol mag"]  $ 
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] [""] ["en"]),  
  paradigm_h "nnm_6n0_blad" ["flygande tefat"]  $
   nocmp $ last_mw "nnm" (noun Neutr [""] ["et"] [""] ["en"]),  
  paradigm_h "nnm_6u0_kikare" ["gröna vågare"]  $
   nocmp $ last_mw "nnm" (noun_f 1 Utr ([e "e"],[e "en"],[e "e"],[e "na"])),
  paradigm_h "nnm_6n1_blad" ["äss i rockärmen"]  $
   nocmp $ first_mw "nnm" (noun Neutr [""] ["et"] [""] ["en"]),  
  paradigm_h "nnm_0n1_dalt" ["grus i maskineriet"]  $
   nocmp $ first_mw "nnm" (noun Neutr [""] ["et"] [] []),
  paradigm_h "nnm_0v1_blod" ["smolk i bägaren"]  $
   nocmp $ first_mw "nnm" (noun Pend [""] ["et","en"] [] []),
  paradigm_h "nnm_5n0_ansikte" ["da capo"]  $
   nocmp $ last_mw "nnm" (noun Neutr [""] ["t"] ["n"] ["na"]),  
  paradigm_h "nnm_ip0_honoratiores" ["lika goda kålsupare"]  $
   nocmp $ last_mw "nnm" (noun_no_genitive GPl ([],[],[e ""], [])),  
  paradigm_h "nnm_3u0_film" ["medicine kandidat"]  $
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] ["er"] ["erna"]),
  paradigm_h "nnm_3u0_filosofie_doktor" ["filosofie doktor"]  $
   nocmp $ last_mw "nnm" (noun Utr [""] ["n"] ["er"] ["erna"]),
  paradigm_h "nnm_3n0_parti" ["12 V-batteri"]  $
   nocmp $ last_mw "nnm" (noun Neutr [""] ["et"] ["er"] ["erna"]),  
 paradigm_h "nnm_3u1_film" ["dans på rosor"]  $
   nocmp $ first_mw "nnm" (noun Utr [""] ["en"] ["er"] ["erna"]),  
  paradigm_h "nnm_2u0_stol" ["vinst- och förlusträkning"]  $ 
   nocmp $ last_mw "nnm" (noun Utr [""] ["en"] ["ar"] ["arna"]),  
  paradigm_h "nnm_2u0_dag" ["bäst före-dag"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""], [(id,"en"),(tk 1,"n")], [(id,"ar"),(tk 1,"r")], [(id,"arna"),(tk 1,"rna")])),
  paradigm_h "nnm_2u0_nyckel" ["golden retriever"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(dvu,"ar")], [(dvu,"arna")])),  
  paradigm_h "nnm_rp1_vägnar" ["fjärilar i magen"]  $ 
   nocmp $ first_mw "nnm" (noun_compound 0 GPl ([],[], [e ""], [e "na"],[],[])),  
  paradigm_h "nnm_1u0_flicka" ["mul- och klövsjuka"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(tk 1,"or")], [(tk 1,"orna")])),  
  paradigm_h "nnm_0u0_hin" ["hin håle"]  $
    nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[], [], [])),  
  paradigm_h "nnm_0u0_frid" ["rhode islandsås"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "en"], [], [])),
  paradigm_h "nnm_0u1_frid" ["hjälp på traven"]  $
   nocmp $ first_mw "nnm" (noun_f 0 Utr ([e ""],[e "en"], [], [])),
  paradigm_h "nnm_np0_ordalag" ["nordiska språk"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 GPl ([],[], [e ""], [e "en"])),  
  paradigm_h "nnm_4u0_linje" ["eau de cologne"]  $
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [e "r"], [e "rna"])),  
  paradigm_h "nnm_2u1_stol" ["ulv i fårakläder"]  $ 
   nocmp $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "en"], [e "ar"], [e "arna"],[],[])),

  paradigm_h "nnm_2u1_nyckel" ["nagel i ögat"]  $ 
   nocmp $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "n"], [(dvu,"ar")], [(dvu,"arna")],[],[])),

  paradigm_h "nnm_2u0_pojke" ["vandrande pinne"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(drop_final_e,"ar")], [(drop_final_e,"arna")])),  
  paradigm_h "nnm_rp0_kalla_kårar" ["kalla kårar"]  $
   nocmp $ last_mw "nnm" (noun_f 0 GPl ([],[], [e ""], [e "na"])),
  paradigm_h "nnm_1u1_flicka" ["fnurra på tråden"]  $ 
   nocmp $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "n"], [(tk 1,"or")], [(tk 1,"orna")],[],[])),  
  paradigm_h "nnm_1us_pudelns_kärna" ["pudelns kärna"]  $ 
   nocmp $ last_mw "nnm" (noun_compound 0 Utr ([e ""],[e "n"], [(tk 1,"or")], [(tk 1,"orna")],[],[])),  
  paradigm_h "nnm_0u0_tro" ["cherry brandy"]  $ 
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [], [])),
  paradigm_h "nnm_0u0_antimateria" ["idé- och lärdomshistoria"]  $
   nocmp $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n", (tk 1,"en")], [], [])),
  paradigm_h "nnm_in0_vaj" ["berått mod"]  $
   nocmp $ last_mw "nnm" (noun_no_genitive Neutr ([e ""],[], [], [])),
  paradigm_h "nnm_iu0_vift" ["gilla gång"]  $
   nocmp $ last_mw "nnm" (noun_no_genitive Utr ([e ""],[], [], [])),
  paradigm_h "nnm_dn0_rubbet" ["rubbet"] $
   nocmp $ last_mw "nnm" (noun_compound 0 Neutr ([],[e ""], [], [],[],[])),
  paradigm_h "nna_0v_pcb"  ["pcb"]  $ 
   nna Pend ([e ""],[e "s"], [e "n", e "en",e "t",e "et"],[e "ns", e "ens",e "ts", e "ets"],[],[],[],[]),
  paradigm_h "nna_vu_dvd"  ["dvd"]  $ 
   nnac Utr ([e ""],[e "s"], [e "n"],[e "ns"],[e "ar",e "er"],[e "ars",e "ers"],[e "arna",e "erna"],[e "arnas",e "ernas"]),
  paradigm_h "nna_0n_hk"  ["hk"]  $ 
   nna Neutr ([e ""],[e "s"], [],[],[],[],[],[]),
  paradigm_h "nna_0u_jo"  ["jo"]  $ 
   nna Utr ([e ""],[e "s"], [],[],[],[],[],[]),
  paradigm_h "nna_vv_dna"  ["dna"]  $ 
   nna Pend ([e ""],[e "s"], 
             [e "n", e "t"],[e "ns",e "ts"],
             [e "",e "er"],[e "s",e "ers"],
             [e "na",e "erna"],[e "nas",e "ernas"]
            ),
  paradigm_h "nna_3u_pda"   ["PDA"]  $ 
   nna Utr ([e ""],[e "s"], [e "n"],[e "ns"], [e "er"], [e "ers"], [e "erna"], [e "ernas"]),
  paradigm_h "nna_6u_lp"   ["lp"]  $ 
   nna Utr ([e ""],[e "s"], [e "n"],[e "ns"],
            [e ""],[e "s"],[e "na"],[e "nas"]),
  paradigm_h "nna_in_ex"   ["ex"]  $ 
   nna Neutr ([e ""],[],[],[],[],[],[],[]),
  -- paradigm_h "nna_6n_ekg"  ["ekg"]  $
  --  nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],
  --          [e "",e "n"],[e "s",e "ns"],[e "na"],[e "nas"]),
  paradigm_h "nna_vv_ekg"  ["ekg"]  $
   nna Pend ([e ""],[e "s"], [e "t",e "n"],[e "ts",e "ns"],
            [e "",e "n"],[e "s",e "ns"],[e "na"],[e "nas"]),
  -- paradigm_h "nna_vv_sms"  ["sms"]  $
  -- nna Neutr ([e ""],[(ds,"")], [e "et",e "en"],[e "ets",e "ens"],
  --          [e "",e "er"],[(ds,""),e "ers"],[e "en",e "erna"],[e "ens",e "ernas"]),
  paradigm_h "nna_6n_sms"  ["sms"]  $
   nna Neutr ([e ""],[(ds,"")], [e "et"],[e "ets"],
            [e ""],[(ds,"")],[e "en"],[e "ens"]),
  paradigm_h "nna_vn_wc"   ["wc"]  $ 
   nna Neutr ([e ""],[e "s"], 
             [e "t"],[e "ts"],
             [e "",e "n"],[e "s",e "ns"],
             [e "en",e "na"],[e "ens",e "nas"]
            ),
  paradigm_h "nna_6v_pm"   ["pm"]  $ 
   nna Pend ([e ""],[e "s"], 
             [e "en", e "et"],[e "ens",e "ets"],
             [e ""],[e "s"],
             [e "na"],[e "nas"]
            ),
  paradigm_h "nna_2u_bh"   ["bh"]  $ 
   nna Utr ([e ""],[e "s"], 
            [e "en",e "n"],[e "ens", e "ns"],
            [e "ar"],[e "ars"],
            [e "arna"],[e "arnas"]
           ),
  paradigm_h "avm_1p0_gul" ["styv i korken"]  $ 
     nocmp $ first_mw "avm" $ av_1_blek_ng,
  paradigm_h "avm_0p0_gul" ["rangen stridig"]  $ 
     nocmp $ last_mw "avm" $ av_1_blek_ng,
  -- paradigm_h "avm_0a0_diverse" ["ute efter"] $ 
  --   nocmp $ last_mw "avm" av_i_diverse,
  paradigm_h "avm_0a0_korkad" ["så kallad"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [(tk 1,"t")], [e "e"], [e "e"], [], [], [])),  
  paradigm_h "avm_0x0_bred" ["naggande god"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [(tk 1,"tt")], [e "a"], [e "a"], [], [], [])),
  paradigm_h "avm_0x0_akut" ["allom bekant"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])),
     
  paradigm_h "avm_0x0_utbrunnen" ["allom given"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [(tk 1, "t")], [(mmn . ungeminate_m_n . dv, "a")], [(mmn . ungeminate_m_n . dv,"a")], [], [], [])),

  paradigm_h "avm_0x0_ny" ["splitter ny"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [e "tt"], [e "a"], [e "a"], [], [], [])),

  paradigm_h "avm_0x1_stum" ["ensam i sitt slag"]  $ 
     nocmp $ first_mw "avm" (adj 0 ([e ""], [e "t"], [e "ma"], [e "ma"], [], [], [])),

  paradigm_h "avm_0x1_utbrunnen" ["gripen ur luften"]  $ 
     nocmp $ first_mw "avm" (adj 0 ([e ""], [(tk 1,"t")], [(mmn . ungeminate_m_n . dv,"a")], [(mmn . ungeminate_m_n . dv,"a")], [], [], [])),

  -- paradigm_h "avm_0x0_gul" ["Gudi behaglig"]  $ 
  --   nocmp $ last_mw "avm" (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [], [], [])),
  paradigm_h "avm_0x0_korkad" ["fly förbannad"]  $ 
     nocmp $ last_mw "avm" (adj 0 ([e ""], [(tk 1, "t")], [e "e"], [e "e"], [], [], [])),
  -- paradigm_h "avm_0x0_utbrunnen" ["inte oäven"]  $ 
  --   nocmp $ last_mw "avm" (adj 0 ([e ""], [(tk 1, "t")], [e "a"], [e "a"], [], [], [])),
  paradigm_h "avm_0p1_gul" ["hal som en ål"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e "t"], [], [e "a"], [], [], [],[])),
  paradigm_h "avm_0p1_gammal" ["gammal som gatan"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e "t"], [], [(tk 3,"la")], [], [], [],[])),
  paradigm_h "avm_0p2_bred" ["mera död än levande"]  $ 
     nocmp $ mw_position 2 "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"tt")], [], [e "a"], [], [], [],[])),
  paradigm_h "avm_0p1_korkad" ["tappad bakom en vagn"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1, "t")], [], [e "e"], [], [], [],[])),
  paradigm_h "avm_0p1_brydd" ["stadd i"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 2,"tt")], [], [e "a"], [], [], [],[])),
  -- paradigm_h "avm_0p0_diverse" ["ute efter"]  $ 
  --   nocmp $ last_mw "avm" av_i_diverse,
  paradigm_h "avm_1p1_bred" ["glad i"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"tt")], [], [e "a"], [e "are"], [e "ast"], [e "aste"],[])),
  -- paradigm_h "avm_1p0_utbrunnen" ["skriven i stjärnorna"]  $ 
  --   nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"t")], [], [], [], [], [],[])),
  paradigm_h "avm_0x2_utbrunnen" ["som fallen från skyarna"]  $ 
     nocmp $ mw_position 2 "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"t")], [], [(mmn . ungeminate_m_n . dv,"na")], [], [], [],[])),
  paradigm_h "avm_1p1_rund" ["rund under fötterna"]  $ 
     nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"t")], [], [e "a"], [e "are"], [e "ast"], [e "aste"],[])),
  paradigm_h "avm_1p1_akut" ["fäst vid"]  $ 
   nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e ""], [], [e "a"], [e "are"], [e "ast"], [e "aste"],[])),
  paradigm_h "avm_1x0_akut" ["politiskt korrekt"]  $ 
   nocmp $ last_mw "avm" (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])),
  -- paradigm_h "avm_1x1_akut" ["märkt av döden"]  $ 
  -- nocmp $ first_mw "avm" (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])),
  -- paradigm_h "avm_1x0_gul" ["gudi behaglig"]  $ 
  -- nocmp $ last_mw "avm" (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [], [], [])),
  paradigm_h "avm_1p1_gul" ["lös i magen"]  $ 
   nocmp $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e ""], [], [e "a"], [e "are"], [e "ast"], [e "aste"],[])),
  paradigm_h "al_o_en" ["en"] $ al_o_en,
  paradigm_h "nn_0n_hindi" ["hindi"]  $
   noun_compound 0 Neutr ([e ""], [],[],[],[e ""],[e ""]),
  paradigm_h "nn_0n_kol-14" ["kol-14"] nn_kol_14,
  paradigm_h "nn_0u_hin" ["hin"]    $
   noun_f 0 Utr ([e ""], [],[],[]),
  paradigm_h "nn_0u_boskap" ["boskap"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[(ds,"")],[(ds,"")]),
  paradigm_h "nn_0u_brödsäd" ["brödsäd"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[(ds,""),e "es"],[(ds,""), e "es"]),
  paradigm_h "nn_0u_månsing" ["månsing"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e ""],[e ""]),
  paradigm_h "nn_0u_kärnkraft" ["kärnkraft"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e "",(ds,"")],[e "",(ds,"")]),
  paradigm_h "nn_6u_mat" ["mat"]    $
   noun_compound_ng 0 Utr ([e ""], [],[e ""],[],[],[]),
  paradigm_h "nn_0u_mjölk" ["mjölk"]    $
    noun_compound 0 Utr ([e ""], [e "en"],[],[],[e ""],[e "",(ds,"")]),
  paradigm_h "nn_0u_säd" ["säd"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e "", e "es"],[e "es",(ds,"")]),
  paradigm_h "nn_2u_sten" ["sten"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[e "ar"],[e "arna"],[e ""],[e "",(ds,"")]),
  paradigm_h "nn_2u_herde" ["herde"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[e "e",e "a"],[e "a",e "e",e "es"]),
  paradigm_h "nn_2u_hjälte" ["hjälte"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[e "e"],[e "e", e "es"]),
  paradigm_h "nn_2u_pojke" ["pojke"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[(ungeminate,"")],[(ds . ungeminate,"")]),
  paradigm_h "nn_2u_ormbunke" ["ormbunke"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[(ungeminate,""),(ds . ungeminate,""),e "e",e "es"],[(ungeminate,""),(ds . ungeminate,""),e "e",e "es"]),
  paradigm_h "nn_2u_ängel" ["ängel"]    $
   noun_compound 0 Utr ([e ""], [e "n"],[(dvu, "ar")],[(dvu,"arna")],[e "",(dv,"a")],[e "",(ds,""),(dv,"a")]),
  paradigm_h "nn_3u_karbid" ["karbid"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[e "er"],[e "erna"],[e ""],[e ""]),
  paradigm_h "nn_ou_deputerad" ["deputerad"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "e"],[e "ena"]),
  paradigm_h "nn_6u_yen" ["yen"]    $ 
   noun_f 0 Utr ([e ""], [e "en"],[e ""],[e "en"]),
  paradigm_h "nn_vu_bagis" ["bagis"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "",e "ar"],[e "en",e "arna"]),
  paradigm_h "nn_vu_order" ["order"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,""),(mmr.dv,"ar")],[e "na", (mmr.dv,"arna")]), 
  paradigm_h "nn_vu_minut" ["minut"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "er",e "rar"],[e "erna",e "rarna"]),
  paradigm_h "av_0_uppsutten" ["uppsutten"] $ 
   (adj 2 ([e "en"], [e "et"], [(ungeminate_m_n,"na")], [(ungeminate_m_n,"na")], [], [], [])), 
  paradigm_h "av_0_uppvikt" ["uppvikt"] $ 
   (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 
  paradigm_h "av_v_trång" ["trång"] $ 
   (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [(vc "ä","re"),(id,"are")], 
           [(vc "ä","st"),(id,"ast")], [(vc "ä","ste"),(id,"aste")])), 
  paradigm_h "pnm_x1_inte_ett_dugg" ["inte småpotatis"]  $ pnm_inv,
  paradigm_h "pnm_x1_vad_än" ["vad än"]  $ pnm_inv,
  paradigm_h "pnm_i_ditt_och_datt" ["ditt och datt"]  $ pnm_inv,
  paradigm_h "pnm_o_vem_som_helst" ["vem som helst"]  $ pnm_gen,
  paradigm_h "pnm_o_en_annan" ["en annan"]  $ pnm_gen2,
  paradigm_h "pnm_o_den_här" ["den här"] pnm_o_den_här,
  paradigm_h "nnm_6na_segel" ["kort varsel"] $ nocmp $ nnm_6na_kort_varsel,
  paradigm_h "nnm_6na_beskt_piller" ["beskt piller"] $ nocmp $ nnm_6na_beskt_piller,

  paradigm_h "nnm_5na_oförrättat_ärende" ["oförrättat ärende"] $ nocmp $ nnm_5na_oförrättat_ärende,
  paradigm_h "nnm_5na_transitivt_hölje" ["transitivt hölje"] $ nocmp $ nnm_5na_transitivt_hölje,
  paradigm_h "nnm_6na_glykemiskt_index" ["glykemiskt index"] $ nocmp $ nnm_6na_glykemiskt_index,

  paradigm_h "nnm_1ua_grön_våg" ["grön våg"]  $ nocmp $ nnm_1ua_grön_våg,
  paradigm_h "nnm_1ua_öm_låga" ["öm låga"] $ nocmp $ nnm_1ua_öm_låga,
  paradigm_h "nnm_6na_nytt_påfund" ["nytt påfund"] $ nocmp $ nnm_6na_nytt_påfund,
  paradigm_h "nnm_6na_öppet_hav" ["öppet hav"] $ nocmp $ nnm_6na_öppet_hav,
  paradigm_h "nnm_6na_öppet_vatten" ["öppet vatten"] $ nocmp $ nnm_6na_öppet_vatten,

  -- paradigm_h "nnm_0u_frid" ["lägre medelklass"]  $ nocmp $ nnm_0u_frid,
  paradigm_h "nnm_0ua_evig_vila" ["evig vila"]  $ nocmp $ nnm_0ua_evig_vila, 
  paradigm_h "nnm_6na_dubbelt_budskap" ["dubbelt budskap"] $ nocmp $ nnm_6na_dubbelt_budskap, 
  paradigm_h "nnm_6u0_vaktman" ["lägre tjänsteman"] $ nocmp $ last_mw "nnm" $ nn_6u_vaktman,
  
  paradigm_h "nnm_6na_svart_hål" ["svart hål"] $ nocmp $ nnm_6na_svart_hål,
  paradigm_h "nnm_6ua_gås"   ["oplockad gås"]  $ nocmp $ nnm_6ua_oplockad_gås,
  paradigm_h "nnm_6ua_kikare" ["svensk mästare"] $ nocmp $ nnm_6ua_svensk_mästare,
  paradigm_h "nnm_6na_välsmort_munläder" ["välsmort munläder"] $ nocmp $ nnm_6na_välsmort_munläder,
  paradigm_h "nnm_0ua_god_ton" ["god ton"]  $ nocmp $ nnm_0ua_frid,
  paradigm_h "nnm_0ua_grå_starr" ["grå starr"]  $ nocmp $ nnm_0ua_grå_starr,
  paradigm_h "nnm_0ua_sluten_vård" ["sluten vård"] $ nocmp $ nnm_0ua_sluten_vård,
  paradigm_h "nnm_0na_aktivt_kol" ["aktivt kol"]  $ nocmp $ nnm_0na_aktivt_kol,

  paradigm_h "nnm_0na_ekonomiskt_oberoende" ["ekonomiskt oberoende"]  $ nocmp $ nnm_0na_ekonomiskt_oberoende,

  paradigm_h "nnm_0na_blått_blod" ["blått blod"] $ nocmp $ nnm_0na_blått_blod,
  paradigm_h "nnm_0na_ont_öga" ["ont öga"] $ nocmp $ nnm_0na_ont_öga,
  paradigm_h "nnm_0na_sunt_förnuft" ["sunt förnuft"] $ nocmp $ nnm_0na_sunt_förnuft, 
  paradigm_h "nnm_0na_tomt_prat" ["tomt prat"] $ nocmp $ nnm_0na_tomt_prat,
  
  paradigm_h "nnm_2ua_beskärd_del" ["beskärd del"] $ nocmp $ nnm_2ua_beskärd_del,
  paradigm_h "nnm_2ua_mogen_ålder" ["mogen ålder"] $ nocmp $ nnm_2ua_mogen_ålder,


  paradigm_h "nnm_3u0_akademi" ["sfärernas harmoni"] $ nocmp $ last_mw "nnm" (noun Utr [""] [] ["er"] []),

  paradigm_h "nnm_3ua_förbjuden_frukt" ["förbjuden frukt"] $ nocmp $ nnm_3ua_förbjuden_frukt,
  paradigm_h "nnm_3ua_tom_fras" ["tom frukt"] $ nocmp $ nnm_3ua_tom_fras,
  
  paradigm_h "nnm_5n1_ansikte" ["helvete på jorden"] $ nocmp $ first_mw "nnm" (noun Neutr [""] ["t"] ["n"] ["na"]),  
  
  paradigm_h "nnm_5na_krönt_huvud" ["krönt huvud"] $ nocmp $ nnm_5na_krönt_huvud,
  paradigm_h "nnm_5na_lätt_byte" ["lätt byte"] $ nocmp $ nnm_5na_lätt_byte,
  
  paradigm_h "nnm_6na_bevingat_ord" ["bevingat ord"] $ nocmp $ nnm_6na_bevingat_ord,
  paradigm_h "nnm_6na_runt_tal" ["runt tal"] $ nocmp $ nnm_6na_runt_tal,
  paradigm_h "nnm_6ua_allmän_åklagare" ["allmän åklagare"] $ nocmp $ nnm_6ua_allmän_åklagare,

  paradigm_h "nnm_0na_utarmat_uran" ["utarmat uran"]  $ nocmp $ nnm_0na_utarmat_uran,
  paradigm_h "nnm_3ua_fransysk_visit" ["fransysk visit"]  $ nocmp $ nnm_3ua_film,
  paradigm_h "nnm_3ua_öm_punkt" ["öm punkt"]  $ nocmp $ nnm_3ua_öm_punkt,
  paradigm_h "nnm_3ua_enarmad_bandit" ["enarmad bandit"]  $ nocmp $ nnm_3ua_enarmad_bandit,
  paradigm_h "nnm_4ua_lös_förbindelse" ["lös förbindelse"]  $ nocmp $ nnm_4ua_lös_förbindelse,
  paradigm_h "nnm_1ua_svart_låda" ["svart låda"]  $ nocmp $ nnm_1ua_svart_låda,
  paradigm_h "nnm_1ua_halvkväden_visa" ["halvkväden visa"] $ nocmp $ nnm_1ua_halvkväden_visa,
  paradigm_h "nnm_3ua_rolig_historia" ["rolig historia"]  $ nocmp $ nnm_3ua_rolig_historia,
  paradigm_h "nnm_2ua_pojke" ["finsk pinne"] $ nocmp $ nnm_2ua_pojke,
  paradigm_h "nnm_2ua_nyckel" ["ond cirkel"] $ nocmp $ nnm_2ua_nyckel,
  paradigm_h "nnm_2u1_pojke" ["droppe i havet"] $ nocmp $ 
   nocmp $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "n"], [(tk 1,"ar")], [(tk 1,"arna")],[],[])),  
  paradigm_h "nnm_2ua_kvalificerad_gissning" ["kvalificerad gissning"] $ nocmp $ nnm_2ua_kvalificerad_gissning,
  paradigm_h "nnm_2ua_naken_blankning" ["naken blankning"] $ nocmp $ nnm_2ua_naken_blankning,
  paradigm_h "nnm_dpc_göranden_och_låtanden" ["göranden och låtanden"]  $ nocmp $ nnm_5pc_göranden_och_låtanden,
  paradigm_h "nnm_0n0_fait_accompli" ["fait accompli"] $
    nocmp $ last_mw "nnm" (noun_f 0 Neutr ([e ""], [],[],[])),
  paradigm_h "nnm_0na_syre" ["fritt vivre"] $ nocmp $ nnm_0na_fritt_vivre,
  paradigm_h "nnm_2ua_stol" ["varm korv"] $ nocmp $ nnm_2ua_stol,
  paradigm_h "nnm_npc_kreti_och_pleti" ["kreti och pleti"] $ nocmp $ nnm_gpc_kreti_och_pleti,
  paradigm_h "ava_i_kungl" ["Kungl."] $ nocmp $ invar "ava",
  paradigm_h "vba_ia_jfr" ["jfr"] $ nocmp $ invar "vba",
  paradigm_h "ppa_i_pga" ["pga"] $ nocmp $ invar "ppa",
  paradigm_h "ppm_i_a_la" ["a la"] $ nocmp $ invar "ppm",
  paradigm_h "ppm_x1_för_skull" ["för vidkommande"] $ nocmp $ invar "ppm",
  paradigm_h "kna_i_o" ["o"] $ nocmp $ invar "kna",
  paradigm_h "snm_i_efter_det_att" ["efter det att"] $ nocmp $ invar "snm",
  -- paradigm_h "ssm_d2_svinhugg_går_igen" ["svinhugg går igen"] $ nocmp $ invar "ssm",
  -- paradigm_h "ssm_i1_märk_väl" ["märk väl"] $ nocmp $ invar "ssm",
  -- paradigm_h "ssm_d2_saken_är_biff" ["saken är biff"] $ nocmp $ invar "ssm",
  paradigm_h "nlm_gi_tusen_sinom_tusen" ["tusen sinom tusen"] $ nocmp $ invar "nlm",
  paradigm_h "mxc_i_dygnetrunt"   ["dygnetrunt"]      (set_pos "mxc" . compound),
  paradigm_h "sxc_i_justitie"      ["justitie"]        compound,
  paradigm_h "ab_1_fort"          ["fort"]            ab_1_fort,
  paradigm_h "ab_i_bort"          ["bort"]            ab_bort,
  paradigm_h "ab_i_aldrig"        ["aldrig"]          ab_i_aldrig,
  paradigm_h "ab_is_främst"       ["främst"]          ab_främst,
  paradigm_h "aba_i_dvs"          ["dvs"]             aba_i_dvs,
  paradigm_h "abm_i_till_exempel" ["till exempel"]    abm_i_till_exempel,
  paradigm_h "abm_u2_bakom_någons_rygg" ["bakom någons rygg"]    abm_i_till_exempel,
  paradigm_h "abm_x1_var_än"      ["hur än"]         abm_i_till_exempel,
  paradigm_h "av_0_lastgammal"    ["lastgammal"]      av_0_lastgammal,
  paradigm_h "av_0_medelstor"     ["medelstor"]       av_0_medelstor,
  paradigm_h "av_1_akut"          ["akut"]            av_1_akut,
  paradigm_h "av_1_blå"           ["blå"]   $
   (adj 0 ([e ""], [e "tt"], [e "a",e ""], [e "a",e ""], [e "are"], [e "ast"], [e "aste"])), 
  paradigm_h "av_1_bred"          ["bred"]            av_1_glad,
  paradigm_h "av_1_brydd"         ["brydd"]           av_1_högljudd,
  paradigm_h "av_1_gul"           ["gul"]             av_1_blek,        
  paradigm_h "av_1_lat"           ["lat"]             av_1_lat,
  paradigm_h "av_1_ny"            ["ny"]              av_1_fri,
  paradigm_h "av_1_rund"          ["rund"]            av_1_hård,
  paradigm_h "av_1_stum"          ["stum"]            av_1_ensam,
  paradigm_h "av_1_utbrunnen"     ["utbrunnen"]       av_1_angelägen,
  paradigm_h "av_i_diverse"       ["diverse"]         av_i_diverse,
  paradigm_h "in_i_aj"            ["aj"]              interj,
  paradigm_h "inm_i_aja_baja"     ["aja baja"]        interjm,
  paradigm_h "kn_i_och"           ["och"]             conj,
  paradigm_h "knm_x_ju_ju" ["både och"] $ set_pos "knm" . conj,
  paradigm_h "nl_g_halvannan"     ["halvannan"] $
   number ([e ""],[(tk 5,"tannat")],[],[],[e ""]),
  -- paradigm_h "nl_g_tu" ["tu"] $
  --  number ([e ""],[e ""],[],[],[e ""]),
  paradigm_h "nl_g_två" ["två"] $
    number ([e ""],[e ""],[],[],[e ""]),
  paradigm_h "nl_g_en" ["en"] $
    number ([e ""],[(tk 1, "tt")],[],[],[e "",(tk 1,"tt")]),
  paradigm_h "nl_g_fyra" ["fyra"] $
    number ([e ""],[e ""],[(tk 3, "järde")],[(tk 3, "järde")],[(tk 1,""), e ""]),
  paradigm_h "nl_g_tvenne" ["tvenne"] $
    number ([e ""],[e ""],[],[],[]),
  paradigm_h "nl_i_i"             ["i"] $
    number_ng ([e ""],[e ""],[],[],[]),
  -- paradigm_h "nl_n_1"             ["1"] nl_n_1, 
  -- paradigm_h "nl_n_elva"          ["elva"] $
  -- number ([e ""],[e ""],[(tk 2, "fte")],[(tk 2, "fte")],[e "",(tk 2, "fte")]),
  -- paradigm_h "nl_n_en"            ["en"] $
  -- number ([e ""],[(tk 1, "tt")],[(tk 2, "första")],[(tk 2, "förste")],[e "", (tk 1, "tt"),(tk 2, "första"),(tk 2, "förste")]),
  -- paradigm_h "nl_n_fem"           ["fem"] $
  -- number ([e ""],[e ""],[e "te"],[e "te"],[e "",e "te"]),
  -- paradigm_h "nl_n_fyra"          ["fyra"] $
  -- number ([e ""],[e ""],[(tk 3, "järde")],[(tk 3,"järde")],[e "",(tk 3,"järde")]),
  --paradigm_h "nl_n_hundra"        ["hundra"] $
  --  number ([e ""],[e ""],[e "de"],[e "de"],[e "",e "de"]),
  -- paradigm_h "nl_n_sex"           ["sex"] $
  --   number ([e ""],[e ""],[(tk 2, "jätte")],[(tk 2,"jätte")],[e "", (tk 2,"jätte")]),
  -- paradigm_h "nl_n_tio"           ["tio"] $
  --  number ([e ""],[e ""],[e "nde"],[e "nde"],[e "", e "nde"]),
  -- paradigm_h "nl_n_tolv"          ["tolv"] $
  --  number ([e ""],[e ""],[(tk 1, "fte")],[(tk 1, "fte")],[e "", (tk 1,"fte")]),
  -- paradigm_h "nl_n_tre"           ["tre"] $
  --  number ([e ""],[e ""],[e "dje"],[e "dje"],[e "", e "dje"]),
  -- paradigm_h "nl_n_två"           ["två"] $
  --  number ([e ""],[e ""],[(tk 3, "andra")],[(tk 3, "andre")],[e "",(tk 3, "andra"),(tk 3,"andre")]),
  -- paradigm_h "nl_n_åtta"          ["åtta"] $
  --  number ([e ""],[e ""],[(tk 1, "onde")],[(tk 1, "onde")],[e "",(tk 1, "onde")]),
  paradigm_h "nn_0u_tro"          ["tro"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(ds,"")],[(ds,"")]),  
  paradigm_h "nn_0u_radar" ["radar"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0u_hemsjuka" ["hemsjuka"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e"),(ds.tk 1,"")],[(tk 1,"e"),(ds.tk 1,"")]),
  paradigm_h "nn_1u_skyltdocka" ["skyltdocka"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"],[e "",e "e",(ds,"")],[e "",e "e",(ds,"")]),
  paradigm_h "nn_0u_saltsyra" ["saltsyra"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e"),e ""],[(tk 1,"e"),e ""]),
  paradigm_h "nn_1u_aminosyra" ["aminosyra"] $
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")],[(tk 1, "e"),e ""],[(tk 1,"e"),e ""]),
  paradigm_h "nn_0u_tjockolja" ["tjockolja"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e")],[(tk 1,"e")]),
  paradigm_h "nn_0n_babbel"       ["babbel"] $
   noun_compound 0 Neutr ([e ""], [(dvu,"et")], [], [],[e ""],[e ""]),  
   paradigm_h "nn_0n_cesium"       ["cesium"] $
   noun_compound 0 Neutr ([e ""], [(tk 2,"et"),e "et", e ""], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0n_dalt"         ["dalt"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e ""],[e "", (ds, "")]),
  paradigm_h "nn_0n_koksalt" ["koksalt"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e "",(ds,"")],[e "",(ds,"")]),
  paradigm_h "nn_0n_latin" ["latin"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e ""],[e ""]),
  paradigm_h "nn_0n_ansvar" ["ansvar"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[(ds,"")],[(ds, "")]),
  paradigm_h "nn_0u_hälsa" ["hälsa"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "o")],[(tk 1, "o"), (tk 1,"e")]),
  paradigm_h "nn_0n_oväsen" ["oväsen"] nn_0n_oväsen,
  paradigm_h "nn_0n_toapapper" ["toapapper"] $
   noun_f 0 Neutr ([e ""],[(dv, "et"),e "et"], [], []),  
  paradigm_h "nn_0n_raseri" ["raseri"] $
   noun_compound 0 Neutr ([e ""],[e "et", e "t"], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0n_skum" ["skum"] nn_0n_skum,
  paradigm_h "nn_0n_syre" ["syre"] nn_0n_kaffe,
  paradigm_h "nn_0u_akribi" ["akribi"] $
 noun_compound 0 Utr ([e ""],[e "n", e "en"], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0u_antimateria"  ["antimateria"] $
   noun_f 0 Utr ([e ""], [e "n",(tk 1, "en")], [], []),  
  paradigm_h "nn_0u_samverkan" ["samverkan"]   $
   noun_compound 0 Utr ([e ""], [e ""], [], [],[(ds, "")],[(ds,"")]),
  paradigm_h "nn_0u_skam" ["skam"] nn_0u_skam,
  paradigm_h "nn_0v_bikarbonat" ["bikarbonat"] $
   noun_compound 0 Pend ([e ""], [e "en", e "et"], [], [],[e ""],[e ""]),
  paradigm_h "nn_0v_manna" ["manna"] nn_0v_manna,
  paradigm_h "nn_1u_ros" ["ros"] $
   noun_compound 0 Utr ([e ""], [e "en"], [e "or"], [e "orna"], [e ""], [(ds,"")]),   
  paradigm_h "nn_2u_afton" ["afton"] $
   noun_compound 0 Utr ([e ""], [e "en"], [(dv, "ar")], [(dv, "arna")],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_2u_bro" ["bro"] nn2,
  paradigm_h "nn_2u_dotter" ["dotter"] nn2_dotter,
  paradigm_h "nn_2u_fordran" ["fordran"] $
   noun_compound 0 Utr ([e ""], [e ""], [(tk 2, "ingar")], [(tk 2, "ingarna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_bokanmälan" ["bokanmälan"] $
   noun_compound 0 Utr ([e ""], [e ""], [(tk 2, "ningar")], [(tk 2, "ningarna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_fröken" ["fröken"] nn2_öken,
  paradigm_h "nn_2u_karl" ["karl"]  $
   noun_f 0 Utr ([e ""], [e "n",e "en"], [e "ar"], [e "arna"]),  
  paradigm_h "nn_2u_mor" ["mor"] nn2_moder,
  paradigm_h "nn_2u_morgon" ["morgon"] $
   noun_f 0 Utr ([e ""], [e "en"], [(dv,"ar"),(tk 3,"nar")], [(dv,"arna"),(tk 3,"narna")]),  
  paradigm_h "nn_2u_mun" ["mun"] nn2_kam,
  paradigm_h "nn_3n_land" ["land"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [(vc "ä", "er")], [(vc "ä", "erna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_3n_stadium" ["stadium"] $
             let f s = if (last s) == 'e' then s++"i" else s++"e" in
             noun_compound 2 Neutr ([e "um"], [e "et"], [e  "er"], [e "erna"],[(f,"")],[(f,"")]),
  paradigm_h "nn_3u_bok" ["bok"] nn3_bok,
  paradigm_h "nn_3u_fot" ["fot"] nn3_fot,
  paradigm_h "nn_3u_bockfot" ["bockfot"] nn3_bockfot,
  paradigm_h "nn_3u_historia" ["historia"] $
   noun_f 0 Utr ([e ""], [(tk 1,"en"),e "n"], [(tk 1, "er")], [(tk 1 ,"erna")]),  
  paradigm_h "nn_3u_kavaljer" ["kavaljer"]  $
   noun_compound 0 Utr ([e ""], [e "en", e "n"], [e "er"], [e "erna"],[e ""],[e "",(ds,"")]),  
  paradigm_h "nn_3u_motor" ["motor"] $
   noun_compound 0 Utr ([e ""], [e "n"], [e "er"], [e "erna"],[e ""],[e ""]),  
  paradigm_h "nn_3u_son" ["son"] $
   (noun_f 0 Utr ([e ""], [e "en"], [(vc "ö","er")], [(vc "ö","erna")])),  
  paradigm_h "nn_3u_stad" ["stad"] $
   (noun_compound 0 Utr ([e ""], [e "en",(tk 1,"n")], [(vc "ä","er")], [(vc "ä","erna")],[(ds,"")],[(ds,"")])),  
  paradigm_h "nn_3u_tång" ["tång"] $
   (noun_f 0 Utr ([e ""], [e "en"], [(vc "ä","er")], [(vc "ä","erna")])),  
  paradigm_h "nn_3u_vän" ["vän"] nn3_vän,
  paradigm_h "nn_3v_flanell" ["flanell"] nn3_flanell,
  paradigm_h "nn_4u_bonde" ["bonde"] nn4_bonde,
  paradigm_h "nn_5n_ansikte" ["ansikte"] $
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"],[(ds_drop, "")],[(ds_drop,"")]),   
  paradigm_h "nn_5n_bo" ["bo"] $
   (noun_f 0 Neutr ([e ""], [e "t", e "et"], [e "n"], [e "en",e "na"])), 
  paradigm_h "nn_5v_libido" ["libido"] $
   noun_compound 0 Pend ([e ""], [e "n", e "t"], [e "n"], [e "na"],[e ""],[e ""]),   
  paradigm_h "nn_5u_anhållan" ["anhållan"] nn5_anmodan,
  paradigm_h "nn_6n_aber" ["aber"] $
   (noun_f 0 Neutr ([e ""], [e "", (dv,"et"), e "et"], [e ""], [(dv, "en"),e "en"])),  
  paradigm_h "nn_6n_blad" ["blad"] $
    noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_6n_system" ["system"] $
    noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e ""],[e ""]),   
  paradigm_h "nn_6n_bord" ["bord"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e "",(ds,"")],[e "", (ds,"")]),   
  paradigm_h "nn_6n_foder" ["foder"] $
   noun_compound 0 Neutr ([e ""], [(dvu, "et")], [e ""], [(dvu,"en"),e "na"],[e ""],[e ""]),   
  paradigm_h "nn_6n_frx" ["f"]  $
   set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "et"],[e "ets"],[e ""],[e "s"],[e "en"],[e "ens"]),
  paradigm_h "nn_6n_papper" ["papper"] $
   (noun_compound 0 Neutr ([e ""], [(id,"et"),(dv,"et")], [(id,"")], [e "en", e "na", (dv,"en")],[(ds,"")],[(ds,"")])),  
  paradigm_h "nn_6n_rum" ["rum"]  $  
   noun_compound 0 Neutr ([e ""], [(geminate,"et")], [e ""], [(geminate,"en")],[(ds,"")],[(ds, "")]),  
  paradigm_h "nn_6n_program" ["program"]  $  
   noun_compound 0 Neutr ([e ""], [(geminate, "et")], [e ""], [(geminate,"en")],[e ""],[e  ""]),  
  paradigm_h "nn_6n_träd" ["träd"] $
   (noun_f 0 Neutr ([e ""], [e "et"], [(id,""),(tk 1, "n")], [(id,"en"),(tk 1, "na")])),  
  paradigm_h "nn_6u_akademiker" ["akademiker"] nn6_akademiker,
  paradigm_h "nn_6u_vapenbroder" ["vapenbroder"] nn_6u_broder,
  paradigm_h "nn_6u_anfader" ["anfader"] $
   (noun_compound 0 Utr ([e ""], [e "n"], [(vc "ä" . tk 2, "er")],[(vc "ä" . tk 2,"erna")],[e "",(ds,"")],[e "",(ds,"")])),
  paradigm_h "nn_6u_far" ["far"] $
   (noun_compound 0 Utr ([e "",(tk 1,"der")], [(tk 1, "dern")], [(vc "ä" . tk 1, "der")],[(vc "ä" . tk 1,"derna")],[e "",(ds,""),(tk 1,"der"),(tk 1,"ders")],[e "",(ds,""),(tk 1,"der"),(tk 1,"ders")])),  
  paradigm_h "nn_6u_kammare" ["kammare"] $
   (noun_compound 0 Utr ([e ""], [e "n"], [(tk 4,"rar"),(id,"")], [(tk 4,"rarna")],[(tk 1,"")],[(tk 1,"")])),  
  paradigm_h "nn_6u_kikare" ["kikare"]  $
   noun_compound 0 Utr ([e ""], [e "n"], [e ""], [(tk 1, "na")],[(tk 1, "")],[(tk 1,"")]),   
  paradigm_h "nn_6u_mus" ["mus"] nn_6u_mus,
  paradigm_h "nn_6u_vaktman" ["vaktman"] nn_6u_vaktman,
  paradigm_h "nn_6v_borst" ["borst"] nn_6v_borst,
  paradigm_h "nn_7u_hit" ["hit"] $ 
   noun_f 0 Utr ([e ""], [e "en"], [e "s"],[e "sen", e "sarna"]),  
  paradigm_h "nn_7u_ranger" ["ranger"] $
   (noun_f 0 Utr ([e ""], [e "n"], [e "s"], [e "sen", e "sarna"])),  
  paradigm_h "nn_in_vaj" ["vaj"] nn_in_vaj,
  paradigm_h "nn_iu_vank" ["vank"] nn_iu_avvaktan,
  paradigm_h "nn_iv_hum" ["hum"] nn_iv_hum,
  paradigm_h "nn_on_öga" ["öga"] 
   (noun_compound 0 Neutr ([e ""], [e "t"], [(tk 1,"on")], [(tk 1,"onen")],[(tk 1,"on")],[(tk 1,"on")])),  
  paradigm_h "nn_ou_officer" ["officer"] nn_ou_officer,
  paradigm_h "nn_vn_alfa_abc" ["a"] $ nocm $  
   replace_attr wp_attr w_attr . replace_attr h_attr w_attr . nn_vn_alfa_abc,
  paradigm_h "nn_vn_garn" ["garn"] nn_vn_garn,
  paradigm_h "nn_vn_huvud" ["huvud"] nn_vn_huvud,
  paradigm_h "nn_vn_kvantum" ["kvantum"] $
   noun_f 0 Neutr  ([e ""], [e "et", e ""], [(tk 2,"a"),e ""], [(tk 2,"ana"),e "en"]), 
  paradigm_h "nn_vn_spektrum" ["spektrum"] nn_vn_spektrum,
  paradigm_h "nn_vu_blinker" ["blinker"] nn_vu_blinker,
  paradigm_h "nn_vu_cyklamen" ["cyklamen"] $
   noun_f 0 Utr  ([e ""], [e ""], [(id,""), (tk 2,"er")], [(id,"a"),(tk 2,"erna")]),   
  paradigm_h "nn_vu_dress" ["dress"] nn_vu_dress,
  paradigm_h "nn_vu_hambo" ["hambo"] nn_vu_hambo,
  paradigm_h "nn_vu_kaliber" ["kaliber"] nn_vu_kaliber,
  paradigm_h "nn_vu_playboy" ["playboy"] nn_vu_playboy,
  paradigm_h "nn_vu_trio" ["trio"] nn_vu_trio,
  paradigm_h "nn_vv_borr" ["borr"] nn_vv_borr,
  paradigm_h "nn_vv_test" ["test"] nn_vv_test,
  paradigm_h "nna_iu_dr" ["dr"] $
   nna Utr ([e ""],[],[],[],[],[],[],[]),
  paradigm_h "nna_iv_nxn" ["log4/log3"] $
   nna Pend ([e ""],[],[],[],[],[],[],[]),
  paradigm_h "nna_vn_ufo" ["UFO"] $
   nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"], [e "n",e "s"],[e "ns",e "s"],[e "na",e "sen"],[e "nas",e "sens"]),  
  paradigm_h "nna_vu_led" ["LED"] $
   nna Utr ([e ""],[e "s"], [e "en"],[e "ens"], [e "",e "er"],[e "s",e "ers"],[e "en",e "erna"],[e "ens",e "ernas"]),  
  paradigm_h "pp_i_i" ["i"] prep,
  paradigm_h "nn_1u_flicka" ["flicka"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [e "e", (ungeminate, "")],[e "e",(ungeminate,""),(ds.ungeminate,"")]),
  paradigm_h "nn_1u_barnstuga" ["barnstuga"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [e "e", e "u"],[e "e",e "u"]),
  paradigm_h "nn_vv_demo" ["demo"] $
   noun_compound 0 Pend 
    ([e ""], [e "n", e "t"], [e "n",e "s",e "sar",e "r", e ""], [e "na",e "sen",e "sarna",e "rna", e "n"],[e ""],[e ""]),
  paradigm_h "nn_2u_botten" ["botten"] $
   noun_compound 0 Utr  ([e ""], [(dvu,"en"),e ""], [(dvu, "ar")], [(dvu,"arna")],[e ""],[e ""]),   
  paradigm_h "nn_2u_nyckel" ["nyckel"] $
    noun_compound 0 Utr ([e ""], [e "n"], [(mmr.dvu,"ar"),(mmr.dv, "ar")], [(mmr.dvu,"arna"),(mmr.dv,"arna")],[e ""],[e "",(ds,"")]),   
  paradigm_h "nn_2u_ålder" ["ålder"] $
    noun_compound 0 Utr ([e ""], [e "n"], [(dvu,"ar"),(dv, "ar")], [(dvu,"arna"),(dv,"arna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_vinge" ["vinge"] $
   noun_compound 0 Utr  ([e ""], [e "n"], [(tk 1,"ar")], [(tk 1, "arna")],[(ungeminate.tk 1, "")],[(ungeminate.tk 1,""),e "s",(ds. ungeminate . tk 1,"")]),   
  paradigm_h "nn_2u_skrake" ["skrake"] $
   noun_compound 0 Utr  ([e "",(tk 1,"")], [e "n"], [(tk 1,"ar")], [(tk 1, "arna")],[(ungeminate.tk 1, "")],[(ungeminate.tk 1,""),e "s",(ds. ungeminate . tk 1,"")]),   
  paradigm_h "nn_2u_stol" ["stol"] $
   noun_compound 0 Utr  ([e ""], [e "en"], [e "ar"], [e "arna"],[e "",(ds,"")],[e "",(ds,"")]),   
  paradigm_h "nn_4u_linje" ["linje"]       nn4,
  paradigm_h "nn_5n_knä" ["knä"] nn5_knä,
  paradigm_h "nn_6n_garage" ["garage"] $ 
   noun_compound 0 Neutr  ([e ""], [e "t"], [e ""], [e "n"],[e ""],[e ""]),   
  paradigm_h "nn_6n_segel" ["segel"] $
   noun_compound 0 Neutr  ([e ""], [(dvu,"et")], [e ""], [(dvu,"en")],[e ""],[e "",(ds,"")]),
  paradigm_h "nn_6n_medel" ["medel"] $
   noun_compound 0 Neutr  ([e ""], [(dvu,"et")], [e ""], [(dvu,"en")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_6u_gås" ["gås"] nn_6u_gås,
  paradigm_h "nn_ou_emeritus" ["emeritus"] nn_ou_emeritus, 
  paradigm_h "nn_ou_examen" ["examen"] nn_ou_examen,
  paradigm_h "nn_vn_centrum" ["centrum"] nn_vn_centrum,
  paradigm_h "nn_vn_nomen" ["nomen"] nn_vn_nomen,
  paradigm_h "nn_vu_jojo" ["jojo"] nn_vu_jojo,
  paradigm_h "nn_vu_partner" ["partner"] nn_vu_partner,
  paradigm_h "nn_vv_abdomen" ["abdomen"] nn_vv_abdomen,
  paradigm_h "av_2_ung" ["ung"] av_2_ung
 ]
 


verb_paradigms :: [(String, [String], [String] -> Entry)]
verb_paradigms = [
  paradigm_h "vb_1s_gillas" ["gillas"]  $
   verb_deponens 2 ([e "as"],[e "as",e "s"],[e "ades",e "des"], [e "ats",e "ts"], []),
  paradigm_h "vb_1a_spara" ["spara"]  $ 
   verb_weak 1 ([e "a"], [e "ar",e ""],[e "a",e ""],[e "ade"],[e "at"],[e "ad"]),
  paradigm_h "vb_1a_vissla" ["vissla"]  $ 
   verb_weak 1 ([e "a"], [e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"]),
  paradigm_h "vb_1a_skapa" ["skapa"]   $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "t"]),
  paradigm_h "vb_1a_hitta" ["hitta"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "e"]),
  paradigm_h "vb_1a_vänta" ["vänta"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "", e "e"]),
  paradigm_h "vb_1a_klaga" ["klaga"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "", e "o"]),
  paradigm_h "vb_1a_beundra" ["beundra"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[]),
  paradigm_h "vb_1m_hisna" ["hisna"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[]),
  paradigm_h "vb_1m_svira" ["svira"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[e ""]),
  paradigm_h "vb_1m_existera" ["existera"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[]),
  paradigm_h "vb_1a_ugnsbaka" ["ugnsbaka"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "t"]),
  paradigm_h "vb_1m_vånna" ["vånna"]  $
   verb_full 1 ([e "a"], [],[e "ar"],[e "a"], [e "ade"], [e "e"], [e "at"], []),    
  paradigm_h "vb_1m_kackla" ["kackla"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[]),
  paradigm_h "vb_1a_unna" ["unna"]  $
   verb_weak 1 ([e "a"], [e "ar"], [e "a"], [e "ade"], [e "at"], [(id,"ad"),(tk 1,"t")]),
  paradigm_h "vb_1a_häda" ["häda"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "d"]),
  paradigm_h "vb_4m_ljuda" ["ljuda"] $
   verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[e "it"], []),
  paradigm_h "vb_4a_fara" ["fara"]  $
   verb_full_sform_variant 1 ([e "a"], [e "e"], [e ""], [e ""], [(vc "o","")], [(vc "o","e")], [e "it"], [e "en"]),
  paradigm_h "vb_2a_leva" ["leva"]  $
   nocmp $ verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [e "t", e "at"], [e "d"]),
  paradigm_h "vb_2a_stödja" ["stödja"]  $
   verb_weak_compound_sform_variant 1 ([(tk 1,"a"),e "a"], [(tk 1, "er"),(id,"er")], [(tk 1,"")], [(tk 1, "de")], [(tk 1 . dsuff "j", "tt")], [(tk 1, "d")],[(tk 1,""),e "e"]),
  paradigm_h "vb_2a_sälja" ["sälja"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "å".tk 1, "de")], [(vc "å".tk 1, "t")], [(vc "å" . tk 1,"d")]),
  paradigm_h "vb_2a_säga" ["säga"]  $
   verb_weak_sform_variant 1 ([(id,"a"),(tk 1, "ja")], [(id, "er"),(tk 1, "jer")], [e "",(tk 1,"j")], [(vc "a".tk 1,"de"),(vc "a".tk 1,"")], [(vc "a","t")] , [(vc "a", "d")]),
  paradigm_h "vb_2a_motsäga" ["motsäga"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a". tk 1, "de")], [(vc "a", "t")], [(vc "a","d")]),
  paradigm_h "vb_2a_mista" ["mista"]  $
   verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "ade",e "e"],[e "",e "at"],[e "ad",(\s -> s +? "t","")]),
  paradigm_h "vb_2a_välja" ["välja"]  $
   let g = vct [("ä","a"),("ö","o")] . tk 1 in 
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(g,"de")], [(g, "t")], [(g,"d")]),
  paradigm_h "vb_2m_hända" ["hända"]  $
   verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "e"],[(tk 1, "t")],[]),
  paradigm_h "vb_2d_må" ["må"] 
   (verb_weak 0 ([],[e ""],[],[e "tte"],[],[])),
  paradigm_h "vb_2m_gitta" ["gitta"]   
   (verb_weak 1 ([e "a"],[e "er"],[e ""], [e "e"], [e "at"], [])),    
  paradigm_h "vb_2a_städja" ["städja"] 
   (verb_weak_sform_variant 2 ([e "ja"], [e "jer",e "er"], [e ""], [(vc "a","de")],[(vc "a".tk 1,"tt")], [(vc "a","d")])),
  paradigm_h "vb_2a_genmäla" ["genmäla"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "de",e "te"],[e "t"],[e "d"])),
  paradigm_h "vb_2m_väga" ["väga"]  
   (verb_weak_no_sform 1 ([e "a"],[e "er"],[e ""],[e "de"],[e "t"],[])),
  paradigm_h "vb_4a_bottenfrysa" ["bottenfrysa"]  
   (verb_full_no_sform 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "te", (vc "ö","")],[(vc "u","e")],[(vc "u","it"),e "t"],[(vc "u","en"),e "t"])),
  paradigm_h "vb_va_frysa" ["frysa"]  
   (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "te", (vc "ö","")],[(vc "u","e")],[(vc "u","it"),e "t"],[(vc "u","en"),e "t"])),
  paradigm_h "vb_2m_höta" ["höta"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "ter",e "er"],[e ""],[e "te"],[e "t"],[])),
  paradigm_h "vb_2m_glädja" ["glädja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [(tk 1, "")], [(vc "a" . tk 1,"de")], [(vc "a" . tk 2, "tt")], [])),
  paradigm_h "vb_2m_böra" ["böra"] 
   (verb_weak_no_sform 1 ([e "a"], [e ""], [e ""], [(vc "o", "de")], [(vc "o", "t")], [])),
  paradigm_h "vb_2a_tämja" ["tämja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [(id, "t"),(tk 1,"t")], [e "d"])),
  paradigm_h "vb_2a_spörja" ["spörja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "o" . tk 1, "de")], [(vc "o" . tk 1, "t")], [(vc "o" . tk 1,"d")])),
  paradigm_h "vb_2s_blygas" ["blygas"]  $  
    (verb_dwc 2 ["as"] ["s","es"] ["des"] ["ts"] []),
  
  paradigm_h "vb_2s_synas" ["synas"] $ 
   verb_deponens 2 ([e "as"],[(ungeminate_m_n . dses,""),e "es"],[(ungeminate_m_n,"tes")],[(ungeminate_m_n,"ts")],[]),

  paradigm_h "vb_3s_brås" ["brås"]  $  
    (verb_dwc 1 ["s"] ["s"] ["ddes"] ["tts"] []),
  paradigm_h "vb_2s_trivas" ["trivas"]  $ 
   verb_deponens 2 ([e "as"],[(ungeminate,"s"),e "as"],[e"des"],[e "ts"],[]),
  paradigm_h "vb_2s_nöjas" ["nöjas"] $ 
   (verb_dwc 2 ["as"] ["s"] ["des"] ["ts"] []),
  paradigm_h "vb_2s_minnas" ["minnas"] $ 
   (verb_dwc 3 ["nas"] ["ns"] ["des"] ["ts"] []),
  paradigm_h "vb_2s_vämjas" ["vämjas"] $ 
   (verb_dwc 3 ["jas"] ["jes","js"] ["jdes","des"] ["jts","ts"] []),
  paradigm_h "vb_2s_töras" ["töras"] $ 
   (verb_dwc 4 ["öras","ordas"] ["örs"] ["ordes"] ["orts"] []),
  paradigm_h "vb_2s_rymmas" ["rymmas"] $ 
   (verb_dwc 3 ["mas"] ["s"] ["des"] ["ts"] []),
  paradigm_h "vb_2s_idas" ["idas"] $ 
   (verb_dwc 3 ["das"] ["ds","des"] ["ddes"] ["dats","tts"] []),
  paradigm_h "vb_2s_hövas" ["hövas"] $ 
   (verb_dwc 2 ["as"] ["es"] ["des"] ["ts"] []),
  paradigm_h "vb_2s_glädjas" ["glädjas"] $ 
   (verb_dwc 5 ["ädjas"] ["äds","ädes"] ["addes"] ["atts"] []),
  paradigm_h "vb_2s_giftas" ["giftas"] $ 
   (verb_dwc 2 ["as"] [] ["es"] ["s"] []),
  paradigm_h "vb_2s_skiljas" ["skiljas"] $ 
   (verb_dwc 3 ["jas"] ["js","s","jes"] ["des"] ["ts"] []),
  paradigm_h "vb_va_vika" ["vika"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "e","")], [(vc "e","e")],[e "it",e "t"], [e "en"])),
  paradigm_h "vb_va_tvinga" ["tvinga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [(id,"ade"),(vc "a","")], [(vc "u","e")],[e "at",(vc "u","it")], [e "ad",(vc "u","en")])),
  paradigm_h "vb_va_löpa" ["löpa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "o","p")], [(vc "u","e")],[(id,"t"),(vc "u","it")], [(vc "u","en"),e "t"])),
  paradigm_h "vb_vm_fnysa" ["fnysa"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "ö","")], [(vc "ö","e")],[e "t"], [])),
  paradigm_h "vb_vm_avvara" ["avvara"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "it",e "at"],[])),
  paradigm_h "vb_va_växa" ["växa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e "a"], [e "te"],[(vc "u", "e")],[(vc "u", "it"),(id,"t")], [(vc "u", "en")])),
  paradigm_h "vb_va_stupa" ["stupa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "ö",""),(id,"ade")], [(vc "ö","e")],[e "at"], [e "ad"])),
  paradigm_h "vb_va_lyda" ["lyda"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"de"),(vc "ö","")], [(vc "ö","e")],[(tk 1,"tt")], [e "d"])),
  paradigm_h "vb_om_skola" ["skola"] 
   (verb_weak_no_sform 1 ([e "a"], [(vc "a" . tk 1, ""),(vc "a","l")], [e "a"], [(vc "u", "le")], [e "at"], [])),
  paradigm_h "vb_vm_snika" ["snika"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e "a"], [(vc "e", ""),(id,"te")], [(vc "e","e")], [e "it"], [])),
  paradigm_h "vb_vm_smälla" ["smälla"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "a",""),(id,"de")], [(vc "u","e")],[e ""], [])),
  paradigm_h "vb_va_tälja" ["tälja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""],[(id,"de"),(vc "a" . tk 1,"de")], [(id,"t"),(vc "a" . tk 1,"t")], [e "d"])),
  paradigm_h "vb_va_två" ["två"] 
   (verb_weak 0 ([e ""], [e "r"], [e ""], [e "dde"], [(id,"tt"),(vc "a","git")],  [e "dd"])),
  paradigm_h "vb_va_smälta" ["smälta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "a", ""),(id,"e")], [(vc "u","e")],[(vc "u","it"),(id,"")], [(vc "u","en")])),
  paradigm_h "vb_va_förmäla" ["förmäla"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "te",e "de"],[e "t"],[e "d"])),
  paradigm_h "vb_va_besvärja" ["besvärja"]  
   (verb_full_sform_variant 1 ([e "a",(tk 1,"a")],[e "e"],[(id,"er"),(tk 1,"")],[e ""], [(vc "o" . tk 1, ""),(id,"de")], [(vc "u" .tk 1,"e")],[(vc "u" . tk 1, "it"),(id,"t")], [e "d", (vc "u" . tk 1, "en")])),
  paradigm_h "vb_om_kunna" ["kunna"]  
   (verb_weak_no_sform 0 ([e ""],[(vc "a" . tk 2, "")],[],[(tk 2, "de")],[e "t"],[])),
  paradigm_h "vb_vm_upphäva" ["upphäva"]       
   (verb_full_sform_variant 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "de",(vc "o","")],[(vc "o","e")],[e "t"],[])),
  paradigm_h "vb_vm_undvara" ["undvara"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at",e "it"],[])),
  paradigm_h "vb_vm_strida" ["strida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(id,"de"), (vc "e", "")], [(vc "e","e")],[(tk 1, "tt"), (id, "it")], [])),
  paradigm_h "vb_vm_sluta" ["sluta"] 
   (verb_full_sform_variant 1 ([e "a"],[e "e"],[e "ar"], [e ""], [(id,"ade"),(vc "ö","")], [(vc "ö","e")],[e "at"], [])),
  paradigm_h "vb_vm_samvara" ["samvara"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "it"],[])),
  paradigm_h "vb_vm_ryka" ["ryka"]  
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö",""),(id,"te")], [(vc "ö","e")],[(id,"t"),(vc "u","it")],[])),
  paradigm_h "vb_vm_nysa" ["nysa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "ö","")], [(vc "ö","e")],[(id,"t"),(id,"it"),(vc "u","it")], [])),
  paradigm_h "vb_vm_kvida" ["kvida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "e", ""),(id,"ade")], [(vc "e","e")],[e "it"], [])),
  paradigm_h "vb_vm_klinga" ["klinga"]  
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [(id,"ade"),(vc "a","")], [(vc "a","e")],[e "at"], [])),
  paradigm_h "vb_va_gälla_kastrera"   ["gälla"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "de",e "ade"],[e "t",e "at"],[e "d",e "ad"])),
  paradigm_h "vb_vm_gala" ["gala"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "o","")], [(vc "o","e")],[(id,"it"),(vc "a","t")], [])),
  paradigm_h "vb_vm_duga" ["duga"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö",""),(id,"ade")], [(vc "ö", "e")],[(id,"t"),(id,"it")], [])),
  paradigm_h "vb_vm_drösa" ["drösa"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "te",e "ade"],[e "at",e "t"],[])),
  paradigm_h "vb_vm_drypa" ["drypa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[(vc "u","it"),(id,"t")], [])),
  paradigm_h "vb_va_utlöpa" ["utlöpa"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "te"], [e "t"], [(vc "u", "en")])),
  paradigm_h "vb_va_träda" ["träda"]  
   (verb_weak_sform_variant 1 ([e "a"], [e "er"],[e ""],[e "ade",e "de"], [(id,"at"),(tk 1,"tt")], [e "d"])),
  paradigm_h "vb_va_strypa" ["strypa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö",""),(id,"te")], [(vc "ö","e")],[e "t"],[e "t"])),
  paradigm_h "vb_va_snusmala" ["snusmala"]  
   (verb_weak_sform_variant 1 ([e "a"],[e ""],[e ""],[e "de"],[e "t"],[e "d",e "en"])),
  paradigm_h "vb_va_skvätta" ["skvätta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a",""),(id,"e")], [(vc "a","e")],[e ""], [e "ad"])),
  paradigm_h "vb_va_simma" ["simma"] 
   (verb_full 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "a" . ungeminate ,""),(id,"ade")], [(vc "a","e")],[(vc "u","it"), (id,"at")],[(vc "u","en"), (id,"ad")])),
  paradigm_h "vb_va_nästa" ["nästa"] 
   (verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade",e "e"],[e "at"],[e "ad"])),
  paradigm_h "vb_va_mala" ["mala"] 
   (verb_weak 1 ([e "a"],[e "er",e ""],[e ""],[e "de"],[e "t"],[e "d",e "en"])),
  paradigm_h "vb_va_kväda" ["kväda"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a",""),(id,"de")], [(vc "a","e")],[e "it"], [e "en"])),
  paradigm_h "vb_va_klyva" ["klyva"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö","")], [(vc "ö","e")],[(vc "u","it"),e "it",e "t"], [(vc "u", "en")])),
  paradigm_h "vb_va_gälda" ["gälda"]  
   (verb_full 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [e "ade"] , [(vc "u","e")],[e "at"], [(vc "u", "en"),(id,"ad")])),
  paradigm_h "vb_va_förse" ["förse"] 
   (verb_weak 0 ([e ""], [e "r"], [e ""],[(vc "å","g"),(id,"dde")], [e "tt"], [e "dd"])),
  paradigm_h "vb_va_förlöpa" ["förlöpa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [e "te"], [(vc "u","e")],[(vc "u","it"),(id,"t")],[(vc "u","en"), (id,"t")])),
  paradigm_h "vb_va_framtvinga" ["framtvinga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [e "ade"],[(vc "u","e")],[(vc "u","it"),(id,"at")], [(vc "u","en"),(id, "ad")])),
  paradigm_h "vb_va_tala" ["tala"]  $ 
   (verb_weak_sform_variant 1 ([e "a"], [e "ar"], [e "a"], [e "ade",e "te"],[e "at",e "t"], [e "ad",e "d"])),
  paradigm_h "vb_va_bestrida" ["bestrida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "e",""),(id,"de")], [(vc "e","e")],[(tk 1,"tt"),(id,"it")], [e "en",e "d"])),
  paradigm_h "vb_va_besluta" ["besluta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "ö",""),(id,"ade")], [(vc "ö","e")],[e "at",e "it"], [e "en",e "ad"])),
  paradigm_h "vb_va_begrava" ["begrava"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "o",""),(id,"de")], [(vc "o","e")],[e "t",e "it"], [e "en",e "d"])),
  paradigm_h "vb_om_vilja" ["vilja"] 
   (verb_weak_no_sform 2 ([e "ja"], [e "l"], [e "ja"], [e "le"],[(vc "e", "at")],[])),
  paradigm_h "vb_om_veta" ["veta"] 
   (verb_full_no_sform 1 ([e "a"],[e "e"],[e ""],[e ""],[(vc "i". tk 1,"sste")],[], [e "at"],[])),
  paradigm_h "vb_om_måste" ["måste"] 
    (verb_weak_no_sform 1 ([e "a"],[e "e"],[e "a"],[e "e"],[e ""],[])),
  paradigm_h "vb_om_heta" ["heta"] 
    (verb_weak_no_sform 1 ([e "a"],[e "er"],[e ""],[e "te"],[e "at"],[])),
  paradigm_h "vb_oa_varda" ["varda"]  
    (verb_weak_no_sform 1 ([e "a"], [e "er"], [e "a"], [e "e"], [(tk 1,"t")],[(vc "o", "en")])),
  paradigm_h "vb_vs_dväljas" ["dväljas"]  $ 
     (verb_deponens 3 ([e "jas"], [e "jes",e "js"],[(vc "a","des"),(id,"jdes")], [(vc "a","ts"),(id,"ts")],[])),
  paradigm_h "vb_4s_vederfaras" ["vederfaras"]  $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "o","s")], [e "its"],[])),
  paradigm_h "vb_4s_tas" ["tas"]  $ 
     (verb_deponens 1 ([e "s"], [e "s"],[(vc "o","gs")], [e "gits"],[])),
  paradigm_h "vb_4s_umgås" ["umgås"] $ 
     (verb_deponens 0 ([e ""], [e ""],[(vc "i".tk 1,"cks")],[(tk 1, "tts")],[])),
  paradigm_h "vb_4s_munhuggas" ["munhuggas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s",e "es"],[(vc "ö","s")], [e "its"],[])),
  paradigm_h "vb_4s_bitas" ["bitas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "e","s")], [e "its"],[])),
  paradigm_h "vb_4s_hållas" ["hållas"] $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "ö","s")], [e "its"],[])),
  paradigm_h "vb_4s_finnas" ["finnas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s",e "es"],[(vc "a","s")],[(vc "u","its")],[])),
  paradigm_h "vb_4s_slåss" ["slåss"]  $ 
     (verb_deponens 2 ([e "ss"], [e "ss"],[(vc "o","gs")],[(vc "a","gits")],[])),
  paradigm_h "vb_4m_svälta_1" ["svälta"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", "")], [(vc "u","e")],[(vc "u", "it")], [])),
  paradigm_h "vb_va_svälta_2" ["svälta"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", ""), e "e"], [(vc "u","e")],[(vc "u", "it")], [e ""])),
  paradigm_h "vb_4m_förslå" ["förslå"]    
   (verb_full 0 ([e ""], [e "ge"],[e "r"], [e ""],[(vc "o", "g")],[(vc "o","e")],[(vc "a","git")], [])),
  paradigm_h "vb_4a_stjäla" ["stjäla"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""],[(tk 3, "al")], [(tk 3,"ule")],[(tk 3,"ulit")],[(tk 3,"ulen")])),
  paradigm_h "vb_4m_vara" ["vara"]  
    (verb_full 4 ([e "vara"], [e "vare"],[e "är"],[], [e "var"], [e "vore"], [e "varit"], [])), 
  paradigm_h "vb_4m_sova" ["sova"] 
    (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""],[e ""],[e "e"],[e "it"],[])),
  paradigm_h "vb_4m_erfara" ["erfara"] 
    (verb_full 1 ([e "a"], [e "e"],[e ""], [e ""],[(vc "o", "")], [(vc "o","e")],[e "it"], [])),
  paradigm_h "vb_4a_bli" ["bli"]  vb_4a_bliva,
  paradigm_h "vb_4a_missförstå" ["missförstå"]     
   (verb_full 0 ([e ""], [],[e "r"], [e ""], [(vc "o", "d")], [(vc "o","de")],[e "dd"], [e "dd"])),
  paradigm_h "vb_4a_äta" ["äta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "å", "")], [(vc "å","e")],[e "it"], [e "en"])),
  paradigm_h "vb_4a_svära" ["svära"] 
   (verb_full_compound_sform_variant 1 ([e "a",e "ja"], [e "je", e "e"],[e "",e "jer"], [e ""], 
               [(vc "o", "")],[(vc "u","e")],[(vc "u","it")],[(vc "u", "en")],[e ""])),
  paradigm_h "vb_4a_emotstå" ["emotstå"] 
   (verb_full 0 ([e ""], [],[e "r"], [e ""],[(vc "o", "d")], [(vc "o","de")],[e "tt"], [e "nden"])),
  paradigm_h "vb_4m_sitta" ["sitta"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", "")],  [(vc "u","e")],[(vc "u", "it")], [])),
  paradigm_h "vb_4a_be" ["be"]  
   (verb_full_compound_sform_variant 0 ([e "",e "dja",e "da"], [e "dje",e "de"],[e "r",e "djer",e "der"], [e "",e "dj",e "d"],[(vc "a", "d")], [(vc "a","de")],[e "tt"], [e "dd"],[])),
  paradigm_h "vb_4m_ryta" ["ryta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö", "")],[(vc "u","e")],[(vc "u", "it")], [])),
  paradigm_h "vb_4m_gråta" ["gråta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ä", "")],[(vc "ä","e")],[e "it"], [])),
  paradigm_h "vb_4m_ligga" ["ligga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "å" . tk 1, "")],[(vc "å","e")],[(vc "e". tk 1, "at")], [])),
  paradigm_h "vb_4m_le" ["le"] 
   (verb_full 0 ([e ""], [],[e "r"], [e ""],[(vc "o", "g")], [(vc "o","ge")],[e "tt"], [])),
  paradigm_h "vb_4m_bekomma" ["bekomma"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [],[(vc "o" . ungeminate, "")], [(vc "o", "e")], [e "it"], [])),
  paradigm_h "vb_4m_småsvära" ["småsvära"] 
   (verb_full_sform_variant 1 ([e "a", e "ja"], [e "e"],[e "",e "jer"], [e ""], [(vc "o", "")],[(vc "u","e")],[(vc "u", "it")], [])),
  paradigm_h "vb_4m_skåpäta" ["skåpäta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""],[(vc "å", "")],[(vc "å","e")],[e "it"], [])),
  paradigm_h "vb_4m_förevara" ["förevara"] 
   (verb_weak_sform_variant 1 ([e "a"], [], [], [e ""], [e "it"], [])),
  paradigm_h "vb_4a_stinga" ["stinga"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [], [],[(vc "u","e")],[(vc "u","it")],[(vc "u","en")])),
  paradigm_h "vb_4a_förgäta" ["förgäta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a","")], [(vc "a","e")],[e "it"], [e "en"])),
  paradigm_h "vb_2m_ha" ["ha"]  $
   verb_weak_compound 0 ([e "",e "va"], [e "r",e "ver"], [e ""], [e "de"], [e "ft"], [],[e ""]),
  paradigm_h "vb_2m_mysa" ["mysa"] $ 
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(ungeminate_m_n, "te")], [(ungeminate_m_n, "t")], []),
  paradigm_h "vb_va_nypa" ["nypa"] $ 
   verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[(id,"t"),(vc "u","it")], [(id,"t"),(vc "u","en")]),
  paradigm_h "vb_0d_lyster" ["lyster"] $ 
   verb_weak 0 ([], [e ""], [], [], [], []),
  paradigm_h "vb_0d_värdes" ["värdes"]    $
   verb_weak 0 ([], [e ""], [], [], [], []),
  paradigm_h "vb_0d_vederböra" ["vederböra"] $
   verb_weak 0 ([e ""], [(tk 1,"")], [], [], [], []),
  paradigm_h "vb_0d_nåde" ["nåde"] $ 
   verb_full 0 ([], [e ""],[],[], [], [], [], []), 
  paradigm_h "vb_0d_lyss" ["lyss"] $ 
   verb_full 0 ([e ""], [],[],[e ""], [], [], [], []), 
  paradigm_h "vb_4d_vederfås" ["vederfås"]  $ 
    verb_dwc 1 [""] [] [] ["tts"] [],
  paradigm_h "vb_id_månde" ["månde"]  $ 
   verb_weak_no_sform 0 ([], [],[],[e ""], [], []), 
  paradigm_h "vb_ik_bevare" ["bevare"] $
   verb_full_no_sform 0 ([],[e ""],[],[], [], [], [], []),
  paradigm_h "vb_2d_torde" ["torde"] $
   verb_full_no_sform 2 ([],[(vc "ö","")],[],[], [], [e "de"], [], []), 
  paradigm_h "vb_2d_rädas" ["rädas"]     $ 
   verb_dwc 2 ["as"] ["as","s"] ["des"] [] [],
  paradigm_h "vb_1a_laga" ["laga"] v1,
  paradigm_h "vb_1s_andas" ["andas"] vb_1s_hoppas,
  paradigm_h "vb_2a_ansöka" ["ansöka"] $
    verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(ungeminate_m_n, "te")], [(ungeminate_m_n, "t")], [(ungeminate_m_n, "t")]),
  paradigm_h "vb_2a_göra" ["göra"] vb_2a_göra,
  paradigm_h "vb_2a_hyra" ["hyra"] $
   verb_weak_sform_variant 1 ([e "a"], [e ""], [e ""], [e "de"], [e "t"], [e "d"]),
  paradigm_h "vb_2a_känna" ["känna"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(tk 1, "de")], [(tk 1,"t")], [(tk 1, "d")]),
  paradigm_h "vb_2a_leda" ["leda"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [(tk 1, "tt")], [e "d"]),
  paradigm_h "vb_2a_lägga" ["lägga"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a".tk 2,  "de"),(vc "a".tk 2,  "")], [(vc "a" . tk 1, "t")], [(vc "a". tk 1,"d")]),
  paradigm_h "vb_2a_sätta" ["sätta"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a", "e")], [(vc "a" , "")], [(vc "a","")]),
  paradigm_h "vb_2a_viga" ["viga"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [e "t"], [e "d"]),
  paradigm_h "vb_4a_falla" ["falla"] $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [e "it"], [e "en"])), 
  paradigm_h "vb_4a_flyga" ["flyga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [(vc "u","it")], [(vc "u","en")])), 
  paradigm_h "vb_4a_ge" ["ge"] $
   verb_full_compound_sform_variant 0 ([e "", (vc "i","va")], [(vc "i","ve")], [e "r",(vc "i","ver")], [e "",(vc "i","v")], [(vc "a","v")], [(vc "å","ve")], [e "tt",(vc "i","vit")], [(vc "i", "ven")],[(vc "i","v")]),
  paradigm_h "vb_4a_hålla" ["hålla"] $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [e "it"], [e "en"])), 
  paradigm_h "vb_4a_komma" ["komma"] $ vb_4a_komma,
  paradigm_h "vb_4a_rida" ["rida"]  $ vb_4a_bita,
  paradigm_h "vb_4a_skjuta" ["skjuta"] $ 
  (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(tk 3,"öt")], [(tk 3,"öte")],[e "it"], [e "en"])),
  paradigm_h "vb_4a_tillåta" ["tillåta"] vb_4a_låta,
  paradigm_h "vb_4a_slå" ["slå"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "o","g")], [(vc "o","ge")], [(vc "a", "git")], [(vc "a","gen")])),
  paradigm_h "vb_4a_se" ["se"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "å","g")], [(vc "å","ge")], [e "tt"], [e "dd"])),
  paradigm_h "vb_4a_gå" ["gå"] $ 
   (verb_full_sform_variant 0 ([e ""], [e "nge"],[e "r"],[e ""], [(vc "i", "ck")], [(vc "i", "nge")], [e "tt"], [e "ngen"])), 
  paradigm_h "vb_4a_dricka" ["dricka"] $ 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "a", "")], [(vc "u", "e")], [(vc "u", "it")], [(vc "u","en")])),
  paradigm_h "vb_4a_bära" ["bära"] $
  (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "a","")], [(vc "u","e")],[(vc "u", "it")], [(vc "u","en")])),
  paradigm_h "vb_4m_innebära" ["innebära"] $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "a","")], [(vc "u","e")],[(vc "u", "it")], [])),
  paradigm_h "vb_4a_ta" ["ta"] $ vb_4a_ta,
  paradigm_h "vb_va_klä" ["klä"]             
  (verb_weak 0 ([e "da",e ""], [e "r",e "der"], [e "",e "d"], [e "dde"], [e "tt"], [e "dd"])),
  paradigm_h "vb_4m_angå" ["angå"] 
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "i","ck")], [(vc "i","nge")], [e "tt"], [])),
  paradigm_h "vb_4m_stå" ["stå"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "o","d")], [(vc "o","de")], [e "tt"], [])),
  paradigm_h "vb_4m_vina" ["vina"] vb_4m_vina,
  paradigm_h "vb_va_bringa" ["bringa"] vb_va_bringa,
  paradigm_h "vb_2a_lyfta" ["lyfta"] $
    verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "e"], [e ""], [e ""]),
  paradigm_h "vb_2a_sända" ["sända"] vb_2a_sända,
  paradigm_h "vb_3a_sy" ["sy"] v3,
  paradigm_h "vb_va_koka" ["koka"] vb_va_koka,
  paradigm_h "vb_va_sprida" ["sprida"] $
   (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""], [(vc "e",""),e "de"], [(vc "e","e")],[(tk 1,"tt"),e "it"], [e "d"]))
 ]

pm_paradigms :: [(String, [String], [String] -> Entry)]
pm_paradigms = [
  paradigm_h "pm_neh_harmagedon" ["Harmagedon"]  $ pm_n "eh",            
  paradigm_h "pm_nls_senatstorget" ["Senatstorget"]  $ pm_n "ls",          
  paradigm_h "pm_uaf_viggen" ["Viggen"] $ pm_u "af",          
  paradigm_h "pm_veh_holocaust" ["Holocaust"] $ pm_v "eh",   
  paradigm_h "pm_fph_kleopatra" ["Kleopatra"] $ pm_f "ph",
  paradigm_h "pm_uls_storgatan" ["Storgatan"] $ pm_u "ls",
  paradigm_h "pm_ulf_centralen" ["Centralen"] $ pm_u "lf",
  paradigm_h "pm_noe_harvard" ["Harvard"] $ pm_n "oe",
  paradigm_h "pm_utz_bambara" ["Bambara"] $ pm_u "tz",
  paradigm_h "pm_vlg_nordsjön" ["Nordsjön"] $ pm_v "lg",
  paradigm_h "pm_uoc_operan" ["Operan"] $ pm_u "oc",
  paradigm_h "pm_ueh_upplysningen" ["Upplysningen"] $ pm_u "eh",
  paradigm_h "pm_naw_titanic" ["Titanic"] $ pm_n "aw",
  paradigm_h "pm_upm_audhumbla" ["Audhumbla"] $ pm_u "pm",
  paradigm_h "pm_uog_polisen" ["Polisen"] $ pm_u "og",
  paradigm_h "pm_uaa_camel" ["Camel"] $ pm_u "aa",
  paradigm_h "pm_poc_hepstars" ["Hepstars"] $ pm_p "oc",
  paradigm_h "pm_nac_saldo" ["SALDO"] $ pm_n "ac",
  paradigm_h "pm_upa_brunte" ["Brunte"] $ pm_u "pa",
  paradigm_h "pm_uop_landsorganisationen" ["Landsorganisationen"]  $ pm_u "op",
  paradigm_h "pm_uoe_kursverksamheten" ["Kursverksamheten"] $ pm_u "oe",
  paradigm_h "pm_uap_vasaorden" ["Vasaorden"] $ pm_u "ap",
  paradigm_h "pm_nwm_aktuellt" ["Aktuellt"] $ pm_n "wm",
  paradigm_h "pm_nop_efta" ["EFTA"] $ pm_n "op",
  paradigm_h "pm_nog_skatteverket" ["Skatteverket"] $ pm_n "og",
  paradigm_h "pm_nog_knesset" ["Knesset"] $ pm_n "og",
  paradigm_h "pm_nog_interpol" ["Interpol"] $ pm_n "og",
  paradigm_h "pm_noc_musikforum" ["Musikforum"] $ pm_n "oc",
  paradigm_h "pm_nla_solsystemet" ["Solsystemet"] $ pm_n "la",
  paradigm_h "pm_hph_af" ["AF"] $ pm_h "ph",
  paradigm_h "pm_fph_barbro" ["Barbro"] $ pm_f "ph",
  paradigm_h "pm_uwa_monalisa" ["Monalisa"] $ pm_u "wa",
  paradigm_h "pm_upc_ttaps-gruppen" ["TTAPS-gruppen"] $ pm_u "pc",
  paradigm_h "pm_uop_atlantpakten" ["Atlantpakten"] $ pm_u "op",
  paradigm_h "pm_uae_keso" ["Keso"] $ pm_u "ae",
  paradigm_h "pm_nwp_charta77" ["Charta77"] $ pm_n "wp",
  paradigm_h "pm_nos_gais" ["Gais"] $ pm_n "os",
  paradigm_h "pm_noa_finnair" ["Finnair"] $ pm_n "oa",
  paradigm_h "pm_nes_vasaloppet" ["Vasaloppet"] $ pm_n "es",
  paradigm_h "pm_nap_nobelpriset" ["Nobelpriset"] $ pm_n "ap",
  paradigm_h "pm_fpm_maria" ["Maria"] $ 
   set_inhs ["f","pm"] . set_pos "pm" . noun_f 1 Utr  ([e "a"], [e "an"], [e "or"], [e "orna"]),   
  paradigm_h "pm_fph_alice" ["Alice"] $ pm_f_alice "ph",
  paradigm_h "pm_fph_karin" ["Karin"] $ pm_f "ph",
  paradigm_h "pm_fph_lisa" ["lisa"] $ 
   set_inhs ["f","ph"] . set_pos "pm" . noun_f 1 Utr  ([e "a"], [e "an"], [e "or"], [e "orna"]),   
  paradigm_h "pm_fpm_idun" ["Idun"] $ pm_f "pm",
  paradigm_h "pm_hph_berg" ["Berg"] $ pm_h "ph",
  paradigm_h "pm_hph_svensson" ["Svensson"] $ 
   set_inhs ["h","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e "en"], [(umlaut, "er")], [(umlaut,"erna")]),   
  paradigm_h "pm_mph_ansgar" ["Ansgar"] $ pm_m "ph",
  paradigm_h "pm_mph_bo" ["Bo"] $ pm_m "ph",
  paradigm_h "pm_mph_lars" ["Lars"] $ 
   set_inhs ["m","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e ""], [e "ar"], [e "arna"]),   
  paradigm_h "pm_mph_sture" ["Sture"] $ 
   set_inhs ["m","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e ""], [(dv,"ar")], [(dv,"arna")]),   
  paradigm_h "pm_mpm_oden" ["Oden"] $ pm_m "pm",
  paradigm_h "pm_nlf_kreml" ["Kreml"] $ pm_n "lf",
  paradigm_h "pm_nlg_delhi" ["Delhi"] $ pm_n "lg",
  paradigm_h "pm_nlg_eurasien" ["Eurasien"] $ pm_n "lg",
  paradigm_h "pm_nlg_göteborg" ["Göteborg"] $ pm_n "lg",
  paradigm_h "pm_nlp_bender" ["Bender"] $ pm_n "lp",
  paradigm_h "pm_nlp_sverige" ["Sverige"] $ pm_n "lp",
  paradigm_h "pm_nog_volvo" ["Volvo"] $ pm_n "og",
  paradigm_h "pm_nop_centern" ["Centern"] $ pm_n "op",
  paradigm_h "pm_plg_alperna" ["Alperna"] $ pm_p "lg",
  paradigm_h "pm_uag_saab" ["Saab"] $ pm_u "ag",
  paradigm_h "pm_ula_månen" ["Månen"] $ pm_u "la",
  paradigm_h "pm_ulg_fyris" ["Fyris"] $ pm_u "lg",
  paradigm_h "pm_uwb_hemsöborna" ["Hemsöborna"] $ pm_u "wb",
  paradigm_h "pm_uwc_faust" ["Faust"] $ pm_u "wc",
  paradigm_h "pm_uwn_aftonbladet" ["Aftonbladet"] $ pm_u "wn",
  paradigm_h "pm_vlf_globen" ["Globen"] $ pm_v "lf",
  paradigm_h "pma_nlg_gbg" ["Gbg"] $ pma_n "lg",
  paradigm_h "pma_nog_fn" ["FN"] $ pma_n "og",
  paradigm_h "pma_nop_cuf" ["CUF"] $ pma_n "op",
  paradigm_h "pma_woc_od" ["OD"] $ pma_w "oc",
  paradigm_h "pma_nwb_blm" ["FoF"] $ pma_n "wb",
  paradigm_h "pma_nom_svt" ["BBC"] $ pma_n "om",
  paradigm_h "pma_nos_gsk" ["GSK"] $ pma_n "os",
  paradigm_h "pma_nog_ab" ["AB"] $ pma_n "og",
  paradigm_h "pma_noa_sas" ["SAS"] $ pma_n "oa",
  paradigm_h "pma_nam_thx" ["THX"] $ pma_n "am",
  paradigm_h "pma_naf_jas" ["JAS"] $ pma_n "af",
  paradigm_h "pma_naa_lep" ["LEP"] $ pma_n "aa",
  paradigm_h "pma_mph_jr" ["Jr"] $ pma_m "ph",
  paradigm_h "pma_hph_nn" ["N.N."] $ pma_h "ph",
  paradigm_h "pma_noe_gu" ["GU"] $ pma_n "oe",
  paradigm_h "pma_nlp_eu" ["EU"] $ pma_n "lp",
  paradigm_h "pma_uwn_dn" ["DN"] $ pma_u "wn",
  paradigm_h "pma_ntm_cp" ["cp"] $ pma_n "tm",
  paradigm_h "pma_mpm_st" ["St"] $ pma_m "pm",
  paradigm_h "pmm_p0lg_klippiga_bergen" ["Klippiga bergen"] $ pmm_p "lg",
  paradigm_h "pmm_u0af_jas_gripen" ["JAS Gripen"] $ pmm_u "af",
  paradigm_h "pmm_u0pm_fågel_fenix" ["fågel Fenix"] $ pmm_u "pm",
  paradigm_h "pmm_n0eh_andra_världskriget" ["Andra världskriget"] $ pmm_n "eh",
  paradigm_h "pmm_u0wc_fröken_julie" ["Fröken Julie"] $ pmm_u "wc",
  paradigm_h "pmm_u0la_stora_björnen" ["Stora björnen"] $ pmm_u "la",
  paradigm_h "pmm_n0op_grön_ungdom" ["Grön ungdom"] $ pmm_n "op",
  paradigm_h "pmm_u0tb_betula_alba" ["betula alba"] $ pmm_u "tb",
  paradigm_h "pmm_m0ph_birger_jarl" ["Birger Jarl"] $ pmm_m "ph",
  paradigm_h "pmm_u0oe_svenska_institutionen" ["Svenska institutionen"] $ pmm_u "oe",
  paradigm_h "pmm_u0aa_koh_i_noor" ["Koh i noor"] $ pmm_u "aa",
  paradigm_h "pmm_m0ph_per_olov" ["Per Olov"] $ pmm_m "ph",
  paradigm_h "pmm_m0ph_karl_den_tolfte" ["Karl den tolfte"] $ pmm_m "ph",
  paradigm_h "pmm_m0ph_el_greco" ["el greco"] $ pmm_m "ph",
  paradigm_h "pmm_f0pm_jungfru_maria" ["jungfru Maria"] $ pmm_f "pm",
  paradigm_h "pmm_n0lf_vita_huset" ["Vita huset"] $ pmm_n "lf",
  paradigm_h "pmm_m0ph_karl_xii" ["Karl xii"] $ pmam_m "ph",
  paradigm_h "pmm_h0ph_jonsson_lind" ["Jonsson Lind"] $ pmm_h "ph",
  paradigm_h "pmm_u0tm_parkinsons_sjukdom" ["Parkinsons sjukdom"] $ pmm_u "tm",
  paradigm_h "pmm_u0op_nysvenska_rörelsen" ["Nysvenska rörelsen"] $ pmm_u "op",
  paradigm_h "pmm_u0ls_lilla_nygatan" ["Lilla Nygatan"] $ pmm_u "ls",
  paradigm_h "pmm_u0en_big_bang" ["Big Bang"] $ pmm_u "en",
  paradigm_h "pmm_u0aw_cutty_sark" ["Cutty Sark"] $ pmm_u "aw",
  paradigm_h "pmm_n0oc_ebba_grön" ["Ebba Grön"] $ pmm_n "oc",
  paradigm_h "pmm_m0pm_john_blund" ["John Blund"] $ pmm_m "pm",
  paradigm_h "pmm_m0ph_adam_av_bremen" ["Adam av Bremen"] $ pmm_m "ph",
  paradigm_h "pmm_v0lf_notre_dame" ["Notre Dame"] $ pmm_v "lf",
  paradigm_h "pmm_u0wn_dagens_nyheter" ["Dagens nyheter"] $ pmm_u "wn",
  paradigm_h "pmm_u0es_davis_cup" ["Davis cup"] $ pmm_u "es",
  paradigm_h "pmm_u0er_marie_bebådelse" ["Marie bebådelse"] $ pmm_u "er",
  paradigm_h "pmm_u0eh_franska_revolutionen" ["Franska Revolutionen"] $ pmm_u "eh",
  paradigm_h "pmm_u0ag_rolls_royce" ["Rolls Royce"] $ pmm_u "ag",
  paradigm_h "pmm_p0ph_bröderna_grimm" ["Bröderna Grimm"] $ pmm_p "ph",
  paradigm_h "pmm_m0ph_plinius_d_y" ["Plinius d y"] $ pmam_m "pa",
  paradigm_h "pmm_m0pa_pelle_svanslös" ["Pelle Svanslös"] $ pmm_m "pa",
  paradigm_h "pmm_f0ph_eva_ek" ["Eva Ek"] $ pmm_f "ph",
  paradigm_h "pmm_uatm_multipel_skleros" ["multipel skleros"] $ pmm_u "tm",
  paradigm_h "pmm_uatm_cerebral_pares" ["cerebral pares"] $ pmm_u "tm",
  paradigm_h "pmm_pcpm_hugin_och_munin" ["Hugin och Munin"] $ pmm_p "pm",
  paradigm_h "pmm_f1pm_jungfrun_från_orleans" ["Jungfrun från Orleans"] $ pmm_f "pm",
  paradigm_h "pmm_nu0wn_svenska_dagbladet" ["Svenska Dagbladet"] $ pmm_n "wn",
  paradigm_h "pmm_nu0wn_svenskt_associationslexikon" ["Svenskt associationslexikon"] $ pmm_n "wn",
  paradigm_h "pmm_h0ph_de_saussure" ["de Saussure"] $ nocmp $ pmm_h "ph",
  paradigm_h "pmm_m0ph_bo_ek" ["Bo Ek"] $ nocmp $ pmm_m "ph",
  paradigm_h "pmm_n0lg_new_delhi" ["New Delhi"] $ nocmp $ pmm_n "lg",
  paradigm_h "pmm_n0lg_svarta_havet" ["Svarta havet"] $ pmm_n "lg",
  paradigm_h "pmm_n0lp_sri_lanka" ["Sri Lanka"] $ pmm_n "lp",
  paradigm_h "pmm_n0oe_göteborgs_universitet" ["Göteborgs universitet"] $ pmm_n "oe",
  paradigm_h "pmm_n0og_nordiska_rådet" ["Nordiska rådet"] $ pmm_n "og",
  paradigm_h "pmm_u0lg_torne_älv" ["Torne älv"] $ pmm_u "lg",
  paradigm_h "pmm_u0wb_det_går_an" ["Quo vadis?"] $ pmm_u "wb",
  paradigm_h "pmm_n0wm_ring_p1" ["Ring P1"] $ pmam_n "wm",
  paradigm_h "pmm_u0ec_alla_hjärtans_dag" ["Alla hjärtans dag"] $ pmm_u "ec",
  paradigm_h "pmm_u0og_svenska_akademien" ["Svenska Akademien"] $ pmm_u "og"
  ]

vbm_paradigms = [
  vbma_paradigm "vbm_1ap1_ansöka" ["åka på"] [(tk 1 . fl,"t")],
  vbm_paradigm "vbm_1mp1_ansöka" ["åka in"],  
  vbm_paradigm "vbm_1mp1_vina" ["skita ner"],
  vbm_paradigm "vbm_1sa1_laga" ["skatta sig lycklig"],    
  vbm_paradigm "vbm_1sd1_andas" ["vattnas i munnen"],   
  vbm_paradigm "vbm_1vzd1_laga" ["rotera i sin grav"],          
  vbm_paradigm "vbm_2mps1_lägga" ["lägga bakom sig"],
  vbm_paradigm "vbm_2mq1_göra" ["göra varken till eller från"],    
  vbm_paradigm "vbm_2msd1_hyra" ["uppföra sig som folk"],
  vbm_paradigm "vbm_2msd1_skilja" ["skilja sig från mängden"], 
  vbm_paradigm "vbm_2msvzd1_hända" ["vända sig i sin grav"],
  vbm_paradigm "vbm_2mt1_ha" ["ha en gås oplockad"],
  vbm_paradigm "vbm_2mvud1_hända" ["vända till sin fördel"],
  vbm_paradigm "vbm_3msd1_sy" ["bete sig som folk"],
  vbm_paradigm "vbm_4md1_ryta" ["hugga i sten"],    
  vbm_paradigm "vbm_4mf1_hålla" ["hålla god min i elakt spel"],
  vbm_paradigm "vbm_4mpud1_ta" ["ta ut sin rätt"],    
  vbm_paradigm "vbm_4mt1_komma" ["komma på grön kvist"],
  vbm_paradigm "vbm_4mts1_ta" ["dra något gammalt över sig"],     
  vbm_paradigm "vbm_4mzd1_ljuda" ["sluta sina dagar"],         
  vbm_paradigm "vbm_oml1_veta" ["veta besked"],    
  vbm_paradigm "vbm_omq1_veta" ["veta varken ut eller in"],    
  vbm_paradigm "vbm_omzd1_veta" ["veta sin plats"],


  vbm_paradigm "vbm_1mdrd1_laga" ["skudda stoftet av sina fötter"],  
  vbm_paradigm "vbm_1mds1_laga" ["släpa fötterna efter sig"],  
  vbm_paradigm "vbm_1mnd1_laga" ["lätta sitt samvete"],  
  vbm_paradigm "vbm_1mprd1_slå" ["slå upp sina bopålar"],  
  vbm_paradigm "vbm_1mrd1_laga" ["förena sina krafter"],  
  vbm_paradigm "vbm_1msd1_laga" ["kasta sig i stoftet"],  
  vbm_paradigm "vbm_1msl1_laga" ["fatta sig kort"],  
  vbm_paradigm "vbm_1mud1_laga" ["förneka sin natur"],  
  vbm_paradigm "vbm_1mut1_laga" ["prisa sin lyckliga stjärna"],  
  vbm_paradigm "vbm_1mvsl1_laga" ["tala för sig själv"],  
  vbm_paradigm "vbm_2mds1_ha" ["ha tid på sig"],  
  vbm_paradigm "vbm_2mds1_känna" ["skämmas ögonen ur sig"],  
  vbm_paradigm "vbm_2mds1_lägga" ["lägga band på sig"],    
  vbm_paradigm "vbm_2mf1_ha" ["ha ett finger med i spelet"],  
  vbm_paradigm "vbm_2mlrt1_ha" ["ha sett sina bästa dagar"],  
  vbm_paradigm "vbm_2mp1_lyfta" ["svälta ihjäl"],  
  vbm_paradigm "vbm_2mprd1_lägga" ["lägga ut sina krokar"],  
  vbm_paradigm "vbm_2mq1_leda" ["smida medan järnet är varmt"],  
  vbm_paradigm "vbm_2mrd1_ansöka" ["sköta sina kort"],   
  vbm_paradigm "vbm_2mrd1_göra" ["göra sina behov"],  
  vbm_paradigm "vbm_2mrd1_viga" ["väga sina ord"],  
  vbm_paradigm "vbm_2mrt1_lägga" ["lägga sina ord väl"],    
  vbm_paradigm "vbm_2ms1_stödja" ["stödja sig"],  
  vbm_paradigm "vbm_2msl1_säga" ["säga sig själv"],  
  vbm_paradigm "vbm_2msvrt1_sätta" ["sätta sig på sina höga hästar"],  
  vbm_paradigm "vbm_2mt1_skilja" ["skilja agnarna från vetet"],  
  vbm_paradigm "vbm_2mud1_göra" ["göra sin kur"],  
  vbm_paradigm "vbm_2mud1_sätta" ["sätta sin lit"],    
  vbm_paradigm "vbm_2mud1_viga" ["höja sin röst"],  
  vbm_paradigm "vbm_2mut1_göra" ["göra sin röst hörd"],  
  vbm_paradigm "vbm_2mvs1_ansöka" ["sträcka på sig"],   
  vbm_paradigm "vbm_3mrd1_sy" ["två sina händer"],  
  vbm_paradigm "vbm_3mt1_sy" ["må som en prins"],  
  vbm_paradigm "vbm_4md1_bära" ["skära i hjärtat"],      
  vbm_paradigm "vbm_4md1_ljuda" ["insupa med modersmjölken"],  
  vbm_paradigm "vbm_4md1_sova" ["sova på saken"],  
  vbm_paradigm "vbm_4md1_tillåta" ["låta masken falla"],   
  vbm_paradigm "vbm_4mds1_angå" ["få klart för sig"],  
  vbm_paradigm "vbm_4mdsd1_tillåta" ["låta tillfället gå sig ur händerna"],  
  vbm_paradigm "vbm_4mi1_gå" ["gå in genom det ena örat och ut genom det andra"],        
  vbm_paradigm "vbm_4ml1_flyga" ["bjuda armen"],  
  vbm_paradigm "vbm_4mnq1_ta" ["ta sitt förnuft till fånga"],  
  vbm_paradigm "vbm_4mq1_sitta" ["sitta som på glödande kol"],     
  vbm_paradigm "vbm_4mq1_slå" ["slå blå dunster i ögonen"],  
  vbm_paradigm "vbm_4mq1_tillåta" ["låta nåd gå före rätt"],  
  vbm_paradigm "vbm_4mrd1_vina" ["vrida sina händer"],     
  vbm_paradigm "vbm_4mrq1_slå" ["slå sina kloka huvuden ihop"],  
  vbm_paradigm "vbm_4ms1_se" ["förse sig"],  
  vbm_paradigm "vbm_4msd1_sitta" ["finna sig till rätta"],    
  vbm_paradigm "vbm_4msl1_sitta" ["finna sig tillrätta"],  
  vbm_paradigm "vbm_4msl1_tillåta" ["låta sig göra"],
  vbm_paradigm "vbm_4msp1_ljuda" ["sluta sig samman"],    
  vbm_paradigm "vbm_4msp1_vina" ["bita sig fast"],  
  vbm_paradigm "vbm_4mst1_ta" ["ta sig vatten över huvudet"],  
  vbm_paradigm "vbm_4mt1_vina" ["riva upp gamla sår"],      
  vbm_paradigm "vbm_4mud1_ta" ["ta sin hand"],  
  vbm_paradigm "vbm_4mut1_angå" ["få sin vilja fram"],  
  vbm_paradigm "vbm_4mut1_gå" ["gå sin egen väg"],       
  vbm_paradigm "vbm_4mvnd1_vina" ["lida mot sitt slut"],  
  vbm_paradigm "vbm_4mvs1_vina" ["gripa omkring sig"],  
  vbm_paradigm "vbm_4mvsl1_sova" ["sova av sig ruset"],    
  vbm_paradigm "vbm_vml1_sprida" ["sprida ljus"],  
  vbm_paradigm "vbm_vmud1_upphäva" ["upphäva sin röst"],  
  vbm_paradigm "vbm_vmvud1_stupa" ["stupa på sin post"],
  
  vbm_paradigm "vbm_2mps1_lyfta" ["gifta om sig"],    
  vbm_paradigm "vbm_2mq1_viga" ["nämna vid sitt rätta namn"],        
  vbm_paradigm "vbm_2mt1_hyra" ["köra huvudet i väggen"],                
  vbm_paradigm "vbm_2tud1_hyra" ["nära en orm vid sin barm"],          
  vbm_paradigm "vbm_4mq1_hålla" ["hålla tungan rätt i munnen"],       
  vbm_paradigm "vbm_4msp1_hålla" ["hålla sig framme"],              
  vbm_paradigm "vbm_4msp1_äta" ["äta sig in"],
  vbm_paradigm "vbm_4mt1_ligga" ["ligga nära till hands"],       
  vbm_paradigm "vbm_iii_märk_väl" ["märk väl"],             
  vbm_paradigm "vbm_iri_langen_går" ["langen går"],      
  vbm_paradigm "vbm_vmd1_bringa" ["bringa till stånd"],      
  vbm_paradigm "vbm_vmd1_växa" ["växa över huvudet"],    
  vbm_paradigm "vbm_2ad1_känna" ["bränna på bål"],
  vbm_paradigm "vbm_4ml1_skjuta" ["skjuta prick"],
  vbm_paradigm "vbm_2mnd1_ansöka" ["möta sitt Waterloo"],
  vbm_paradigm "vbm_2mud1_ansöka" ["möta sin skapare"],
  vbm_paradigm "vbm_4msl1_ge" ["ge sig tid"],
  vbm_paradigm "vbm_2ml1_ta" ["ta skruv"],    
  vbm_paradigm "vbm_2mpl1_lägga" ["lägga ut kursen"],
  vbm_paradigm "vbm_2mq1_lägga" ["lägga det långa benet före"],            
  vbm_paradigm "vbm_2mq1_sätta" ["sätta det långa benet före"], 
  vbm_paradigm "vbm_2mq1_ta" ["ta det långa benet före"],    
  vbm_paradigm "vbm_2mt1_sätta" ["sätta långa benet före"],            
  vbm_paradigm "vbm_2mt1_ta" ["ta långa benet före"],    
  vbm_paradigm "vbm_3md1_sy" ["ro i land"],               
  vbm_paradigm "vbm_4md1_bli" ["bli svaret skyldig"],   
  vbm_paradigm "vbm_4mps1_flyga" ["supa ned sig"],
  vbm_paradigm "vbm_4mq1_vina" ["bita i det sura äpplet"],     
  vbm_paradigm "vbm_vmp1_frysa" ["frysa på"], 
  vbm_paradigm "vbm_vmp1_klä" ["bre ut"],
  vbm_paradigm "vbm_vsp1_talas" ["talas vid"],
  vbm_paradigm "vbm_vmt1_växa" ["växa så det knakar"],  
  vbm_paradigm "vbm_vmsp1_ryckas" ["ryckas bort"],
  vbm_paradigm "vbm_vms1_tvinga" ["tilltvinga sig"],
  vbm_paradigm "vbm_vms1_sprida" ["sprida sig"], 
  vbm_paradigm "vbm_vms1_klä" ["utbre sig"],
  vbm_paradigm "vbm_vms1_förlöpa" ["belöpa sig"],
  vbm_paradigm "vbm_vms1_besvärja" ["sammansvärja sig"],
  vbm_paradigm "vbm_vms1_besluta" ["besluta sig"],                          
  vbm_paradigm "vbm_vmps1_växa" ["växa till sig"],
  vbm_paradigm "vbm_vmps1_vika" ["vika ut sig"],
  vbm_paradigm "vbm_vmps1_sprida" ["sprida ut sig"], 
  vbm_paradigm "vbm_vmps1_smälla" ["smälla i sig"],
  vbm_paradigm "vbm_vmps1_klä" ["klä ut sig"],
  vbm_paradigm "vbm_vmp1_upphäva" ["häva upp"],
  vbm_paradigm "vbm_vmp1_smälta" ["smälta in"],
  vbm_paradigm "vbm_vmp1_ryka" ["ryka ihop"],       
  vbm_paradigm "vbm_vml1_mala" ["mala tomning"],
  vbm_paradigm "vbm_vml1_klä" ["klä skott"],
  vbm_paradigm "vbm_vmd1_smälta" ["smälta i munnen"], 
  vbm_paradigm "vbm_val1_klä" ["trå dansen"],
  vbm_paradigm "vbm_vad1_bringa" ["bringa till världen"],
  vbm_paradigm "vbm_oms1_vilja" ["vilja sig"],
  vbm_paradigm "vbm_omp1_vilja" ["vilja till"],
  vbm_paradigm "vbm_omp1_veta" ["veta av"],
  vbm_paradigm "vbm_4nq1_ta" ["ta sitt tillfället i akt"],
  vbm_paradigm "vbm_4mzt1_ta" ["ta sitt eget liv"],       
  vbm_paradigm "vbm_4mzt1_angå" ["få sina fiskar varma"],
  vbm_paradigm "vbm_4mzq1_ta" ["ta sin mats ur skolan"],
  vbm_paradigm "vbm_4mzd1_ta" ["ta sitt liv"],      
  vbm_paradigm "vbm_4mzd1_stå" ["stå sitt kast"],
  vbm_paradigm "vbm_4mvrd1_gå" ["gå i någons ledband"],    
  vbm_paradigm "vbm_4mt1_tillåta" ["låta udda vara jämnt"],          
  vbm_paradigm "vbm_4mt1_ta" ["ta med en nypa salt"], 
  vbm_paradigm "vbm_4mt1_svära" ["svära som en borstbindare"],         
  vbm_paradigm "vbm_4mt1_stå" ["stå på egna ben"], 
  vbm_paradigm "vbm_4mt1_slå" ["slå huvudet på spiken"],         
  vbm_paradigm "vbm_4mt1_skjuta" ["gjuta olja på elden"],
  vbm_paradigm "vbm_4mt1_sitta" ["sitta som på nålar"],       
  vbm_paradigm "vbm_4mt1_rida" ["driva till sin spets"], 
  vbm_paradigm "vbm_4mt1_hålla" ["hålla en låg profil"],
  vbm_paradigm "vbm_4mt1_gå" ["gå sin gilla gång"],     
  vbm_paradigm "vbm_4mt1_flyga" ["sjunga på sista versen"],    
  vbm_paradigm "vbm_4mt1_angå" ["få tummen ur röven"],      
  vbm_paradigm "vbm_4msp1_viga" ["ta sig genom"],   
  vbm_paradigm "vbm_4msp1_ta" ["ta sig ut"],
  vbm_paradigm "vbm_4msp1_stå" ["förstå sig på"],
  vbm_paradigm "vbm_4msp1_slå" ["slå sig på"],
  vbm_paradigm "vbm_4msp1_se" ["se sig för"],               
  vbm_paradigm "vbm_4msp1_komma" ["komma sig upp"],
  vbm_paradigm "vbm_4msp1_ge" ["ge sig ut"],
  vbm_paradigm "vbm_4msp1_flyga" ["bryta sig in"],
  vbm_paradigm "vbm_4msp1_finnas" ["finnas till"],
  vbm_paradigm "vbm_4msp1_bära" ["bära sig åt"],
  vbm_paradigm "vbm_4msl1_ta" ["ta sig ton"],           
  vbm_paradigm "vbm_4msl1_stå" ["stå sig slätt"],    
  vbm_paradigm "vbm_4msd1_ta" ["ta sig i akt"],      
  vbm_paradigm "vbm_4msd1_slå" ["slå sig till ro"],
  vbm_paradigm "vbm_4msd1_skjuta" ["skjuta sig i foten"],
  vbm_paradigm "vbm_4msd1_hålla" ["hålla sig i skinnet"],            
  vbm_paradigm "vbm_4msd1_ge" ["ge sig till tåls"],
  vbm_paradigm "vbm_4ms1_vina" ["slita sig"],
  vbm_paradigm "vbm_4ms1_utfalla" ["undfalla sig"],
  vbm_paradigm "vbm_4ms1_tillåta" ["nedlåta sig"],
  vbm_paradigm "vbm_4ms1_ta" ["åta sig"],
  vbm_paradigm "vbm_4ms1_stå" ["understå sig"],
  vbm_paradigm "vbm_4ms1_sova" ["försova sig"],
  vbm_paradigm "vbm_4ms1_slå" ["slå sig"],
  vbm_paradigm "vbm_4ms1_skåpäta" ["föräta sig"],
  vbm_paradigm "vbm_4ms1_skjuta" ["utgjuta sig"],
  vbm_paradigm "vbm_4ms1_sitta" ["utspinna sig"],
  vbm_paradigm "vbm_4ms1_ryta" ["snyta sig"],
  vbm_paradigm "vbm_4ms1_ljuda" ["ansluta sig"],
  vbm_paradigm "vbm_4ms1_komma" ["komma sig"],
  vbm_paradigm "vbm_4ms1_hålla" ["hålla sig"],
  vbm_paradigm "vbm_4ms1_gråta" ["utlåta sig"],
  vbm_paradigm "vbm_4ms1_ge" ["hänge sig"],
  vbm_paradigm "vbm_4ms1_förhålla" ["uppehålla sig"],
  vbm_paradigm "vbm_4ms1_falla" ["falla sig"],
  vbm_paradigm "vbm_4ms1_dra" ["dra sig"],     
  vbm_paradigm "vbm_4ms1_bära" ["skära sig"],
  vbm_paradigm "vbm_4ms1_be" ["utbe sig"],
  vbm_paradigm "vbm_4ms1_angå" ["förgå sig"],
  vbm_paradigm "vbm_4mq1_ta" ["ta en ände med förskräckelse"],  
  vbm_paradigm "vbm_4mq1_flyga" ["ljuga som en häst travar"], 
  vbm_paradigm "vbm_4mq1_bära" ["bära ut med fötterna före"],    
  vbm_paradigm "vbm_4mpzd1_slå" ["slå ned sina bopålar"],
  vbm_paradigm "vbm_4mpzd1_komma" ["komma till sin rätt"],     
  vbm_paradigm "vbm_4mpzd1_falla" ["falla ur sin roll"], 
  vbm_paradigm "vbm_4mps1_vina" ["bita ifrån sig"],        
  vbm_paradigm "vbm_4mps1_ta" ["ta till sig"],  
  vbm_paradigm "vbm_4mps1_stå" ["stå på sig"],             
  vbm_paradigm "vbm_4mps1_slå" ["slå ifrån sig"],
  vbm_paradigm "vbm_4mps1_skåpäta" ["äta upp sig"],
  vbm_paradigm "vbm_4mps1_skjuta" ["skjuta in sig"],
  vbm_paradigm "vbm_4mps1_se" ["se framför sig"],      
  vbm_paradigm "vbm_4mps1_ryta" ["hugga för sig"],   
  vbm_paradigm "vbm_4mps1_komma" ["komma ihop sig"],
  vbm_paradigm "vbm_4mps1_hålla" ["hålla i sig"],
  vbm_paradigm "vbm_4mps1_gå" ["gå för sig"],  
  vbm_paradigm "vbm_4mps1_ge" ["ge med sig"],
  vbm_paradigm "vbm_4mps1_angå" ["få för sig"],
  vbm_paradigm "vbm_4mpl1_ta" ["ta till storsläggan"],
  vbm_paradigm "vbm_4mpl1_ge" ["ge upp andan"],
  vbm_paradigm "vbm_4mpl1_angå" ["få ihop det"],  
  vbm_paradigm "vbm_4mpd1_ta" ["ta i med hårdhandskarna"],
  vbm_paradigm "vbm_4mpd1_slå" ["slå av på takten"],
  vbm_paradigm "vbm_4mpd1_gå" ["gå in i väggen"],
  vbm_paradigm "vbm_4mp1_vina" ["lida pin"],
  vbm_paradigm "vbm_4mp1_vara" ["vara till"],
  vbm_paradigm "vbm_4mp1_ta" ["ta överbalansen"],
  vbm_paradigm "vbm_4mp1_stå" ["stå ut"],
  vbm_paradigm "vbm_4mp1_sova" ["sova ut"],  
  vbm_paradigm "vbm_4mp1_slå" ["slå över"],
  vbm_paradigm "vbm_4mp1_skjuta" ["skjuta över"],
  vbm_paradigm "vbm_4mp1_sitta" ["sticka upp"],
  vbm_paradigm "vbm_4mp1_se" ["se ut"],
  vbm_paradigm "vbm_4mp1_ryta" ["stryka med"],
  vbm_paradigm "vbm_4mp1_ligga" ["ligga efter"],
  vbm_paradigm "vbm_4mp1_le" ["dö ut"], 
  vbm_paradigm "vbm_4mp1_komma" ["komma vid"],
  vbm_paradigm "vbm_4mp1_hålla" ["hålla upp"],
  vbm_paradigm "vbm_4mp1_gå" ["gå till väga"],
  vbm_paradigm "vbm_4mp1_ge" ["ge igen"],  
  vbm_paradigm "vbm_4mp1_flyga" ["bryta av"],
  vbm_paradigm "vbm_4mp1_fara" ["fara ut"],
  vbm_paradigm "vbm_4mp1_falla" ["falla samman"],       
  vbm_paradigm "vbm_4mp1_bära" ["skära för"],
  vbm_paradigm "vbm_4mp1_bli" ["bli kvar"],
  vbm_paradigm "vbm_4mp1_angå" ["få till"],  
  vbm_paradigm "vbm_4mlp1_slå" ["slå dövörat till"],
  vbm_paradigm "vbm_4mlp1_angå" ["få det ihop"],  
  vbm_paradigm "vbm_4ml1_vina" ["slita hund"],               
  vbm_paradigm "vbm_4ml1_tillåta" ["låta bli"],     
  vbm_paradigm "vbm_4ml1_ta" ["ta betäckning"],
  vbm_paradigm "vbm_4ml1_stå" ["stå rycken"],
  vbm_paradigm "vbm_4ml1_sova" ["sova sked"],          
  vbm_paradigm "vbm_4ml1_slå" ["slå vad"],
  vbm_paradigm "vbm_4ml1_sitta" ["vinna gehör"],
  vbm_paradigm "vbm_4ml1_se" ["se dubbelt"],           
  vbm_paradigm "vbm_4ml1_ryta" ["stryka flagg"],
  vbm_paradigm "vbm_4ml1_rida" ["knipa käft"],
  vbm_paradigm "vbm_4ml1_ljuda" ["ljuta döden"],
  vbm_paradigm "vbm_4ml1_ligga" ["ligga ogill"],
  vbm_paradigm "vbm_4ml1_komma" ["komma underfund"],
  vbm_paradigm "vbm_4ml1_hålla" ["hålla stången"],
  vbm_paradigm "vbm_4ml1_gå" ["gå miste"],
  vbm_paradigm "vbm_4ml1_ge" ["ge vika"],
  vbm_paradigm "vbm_4ml1_fara" ["fara illa"],    
  vbm_paradigm "vbm_4ml1_bära" ["bära frukt"], 
  vbm_paradigm "vbm_4ml1_angå" ["få bukt"],
  vbm_paradigm "vbm_4mi1_vina" ["rida ranka"],
  vbm_paradigm "vbm_4mi1_ge" ["ge bagarbarn bröd"],
  vbm_paradigm "vbm_4mi1_bära" ["bära syn för sagen"],
  -- vbm_paradigm "vbm_4mi1_angå" ["få bära hundhuvudet"],
  vbm_paradigm "vbm_2md1_leda" ["leda i bevis"], 
  vbm_paradigm "vbm_4md1_äta" ["äta ur huset"],

  vbm_paradigm "vbm_4mds1_ta" ["dra benen efter sig"],
  vbm_paradigm "vbm_4mds1_dra" ["dra öronen åt sig"],     
  vbm_paradigm "vbm_4md1_vina" ["skita på sig"],
  vbm_paradigm "vbm_4md1_ta" ["ta till orda"],
  vbm_paradigm "vbm_4md1_stå" ["stå till svars"],
  vbm_paradigm "vbm_4md1_slå" ["slå en drill"],
  vbm_paradigm "vbm_4md1_skjuta" ["skjuta i höjden"],
  vbm_paradigm "vbm_4md1_sitta" ["sitta på understol"],
  vbm_paradigm "vbm_4md1_se" ["se mellan fingrarna"],                   
  vbm_paradigm "vbm_4md1_ligga" ["ligga på lur"],
  vbm_paradigm "vbm_4md1_le" ["le i mjugg"],
  vbm_paradigm "vbm_4md1_komma" ["komma till kritan"],
  vbm_paradigm "vbm_4md1_hålla" ["hålla låg profil"],
  vbm_paradigm "vbm_4md1_gå" ["gå till väga"],
  vbm_paradigm "vbm_4md1_ge" ["ge till spillo"],
  vbm_paradigm "vbm_4md1_flyga" ["krypa till korset"],
  vbm_paradigm "vbm_4md1_fara" ["fara med lögn"],     
  vbm_paradigm "vbm_4md1_falla" ["falla i talet"],   
  vbm_paradigm "vbm_4md1_dra" ["dra åt tumskruvarna"], 
  vbm_paradigm "vbm_4md1_be" ["be om ursäkt"],       
  vbm_paradigm "vbm_4md1_angå" ["få på moppe"],
  vbm_paradigm "vbm_4ma1_komma" ["komma undan"],     
  -- vbm_paradigm "vbm_4l1_slå" ["slå mynt"],
  vbm_paradigm "vbm_4at1_ta" ["ta på bar gärning"],
  vbm_paradigm "vbm_4apl1_ta" ["ta ner skylten"],
  vbm_paradigm "vbm_4apd1_ta" ["ta ner på jorden"],
  vbm_paradigm "vbm_4al1_ta" ["ta sönder"],
  vbm_paradigm "vbm_4al1_rida" ["rida barbacka"], 
  vbm_paradigm "vbm_4al1_komma" ["komma ifråga"],
  vbm_paradigm "vbm_4al1_ge" ["ge tillkänna"],
  -- vbm_paradigm "vbm_4al1_flyga" ["suga musten"],
  vbm_paradigm "vbm_4al1_dricka" ["vinna segerpalmen"],
  vbm_paradigm "vbm_4ad1_ta" ["ta till vara"],
  vbm_paradigm "vbm_4ad1_se" ["se i vitögat"],
  vbm_paradigm "vbm_4ad1_komma" ["komma till skott"],
  vbm_paradigm "vbm_4ad1_hålla" ["hålla i schack"],
  vbm_paradigm "vbm_4ad1_gå" ["förbigå med tystnad"],          
  vbm_paradigm "vbm_4ad1_ge" ["ge till känna"],
  -- vbm_paradigm "vbm_4ad1_flyga" ["suga all must"],
  vbm_paradigm "vbm_4ad1_falla" ["falla till föga"],
  vbm_paradigm "vbm_4ad1_bära" ["bära i gullstol"],
  vbm_paradigm "vbm_3mzd1_sy" ["så sin vildhavre"],  
  vbm_paradigm "vbm_3msp1_sy" ["bry sig om"],
  vbm_paradigm "vbm_3ms1_sy" ["ty sig"],
  vbm_paradigm "vbm_3mp1_sy" ["rå om"],
  vbm_paradigm "vbm_3ml1_sy" ["fly fältet"],
  -- vbm_paradigm "vbm_2ud1_ansöka" ["möta sin skapare"],  
  vbm_paradigm "vbm_2st1_synas" ["mötas på halva vägen"],            
  vbm_paradigm "vbm_2sp1_synas" ["hjälpas åt"],
  vbm_paradigm "vbm_2sp1_minnas" ["kännas vid"],
  vbm_paradigm "vbm_2sp1_blygas" ["följas åt"],
  vbm_paradigm "vbm_2mznt1_ha" ["ha sitt på det torra"], 
  vbm_paradigm "vbm_2mzn1_ha" ["ha sitt"],         
  vbm_paradigm "vbm_2mzd1_välja" ["svälja sin stolthet"],
  vbm_paradigm "vbm_2mzd1_sätta" ["sätta sin lit"],
  vbm_paradigm "vbm_2mzd1_känna" ["känna sina pappenheimare"],        
  vbm_paradigm "vbm_2mzd1_göra" ["göra sina lärospån"],
  vbm_paradigm "vbm_2mt1_viga" ["hänga på ett hår"],        
  vbm_paradigm "vbm_2mt1_lägga" ["lägga benen på ryggen"],            
  vbm_paradigm "vbm_2mt1_hända" ["vända kappan efter vinden"],    
  vbm_paradigm "vbm_2mt1_göra" ["göra slag i saken"],
  vbm_paradigm "vbm_2mt1_ansöka" ["röka som en skorsten"],        
  vbm_paradigm "vbm_2msp1_viga" ["ställa sig in"],
  vbm_paradigm "vbm_2msp1_sätta" ["sätta sig över"],
  vbm_paradigm "vbm_2msp1_lägga" ["lägga sig vinn"],
  vbm_paradigm "vbm_2msp1_hyra" ["höra sig för"],
  vbm_paradigm "vbm_2msp1_göra" ["göra sig till"],
  vbm_paradigm "vbm_2msp1_ansöka" ["tänka sig för"],
  vbm_paradigm "vbm_2msl1_viga" ["överleva sig själv"],   
  vbm_paradigm "vbm_2msl1_lägga" ["lägga sig platt"],     
  vbm_paradigm "vbm_2msl1_göra" ["göra sig kvitt"],
  vbm_paradigm "vbm_2msd1_viga" ["ställa sig i bakhåll"],            
  vbm_paradigm "vbm_2msd1_sätta" ["sätta sig på tvären"],       
  vbm_paradigm "vbm_2msd1_lägga" ["lägga sig i bakhåll"],   
  vbm_paradigm "vbm_2ms1_välja" ["vänja sig"],
  vbm_paradigm "vbm_2ms1_viga" ["hänga sig"],
  vbm_paradigm "vbm_2ms1_sätta" ["sätta sig"],
  vbm_paradigm "vbm_2ms1_sälja" ["sälja sig"],
  vbm_paradigm "vbm_2ms1_skilja" ["skilja sig"],
  vbm_paradigm "vbm_2ms1_säga" ["säga sig"],
  vbm_paradigm "vbm_2ms1_lyfta" ["utfästa sig"],
  vbm_paradigm "vbm_2ms1_lägga" ["vinnlägga sig"],
  vbm_paradigm "vbm_2ms1_leda" ["reda sig"],
  vbm_paradigm "vbm_2ms1_känna" ["missminna sig"],
  vbm_paradigm "vbm_2ms1_hyra" ["uppföra sig"],
  vbm_paradigm "vbm_2ms1_hända" ["vända sig"],
  vbm_paradigm "vbm_2ms1_göra" ["tillgodogöra sig"],
  vbm_paradigm "vbm_2ms1_glädja" ["glädja sig"],
  vbm_paradigm "vbm_2ms1_ansöka" ["stöta sig"],
  vbm_paradigm "vbm_2mrf1_hyra" ["begära någons huvud på ett fat"],
  vbm_paradigm "vbm_2mq1_ha" ["ha rent mjöl i påsen"],
  vbm_paradigm "vbm_2mps1_viga" ["hänga upp sig"],
  vbm_paradigm "vbm_2mps1_sätta" ["sätta i sig"],
  vbm_paradigm "vbm_2mps1_säga" ["säga upp sig"],  
  vbm_paradigm "vbm_2mps1_känna" ["skämma ut sig"],
  vbm_paradigm "vbm_2mps1_hyra" ["köra ihop sig"],
  vbm_paradigm "vbm_2mps1_ha" ["ha för sig"],
  vbm_paradigm "vbm_2mps1_göra" ["göra på sig"],
  vbm_paradigm "vbm_2mps1_ansöka" ["smäcka i sig"],
  vbm_paradigm "vbm_2mpnd1_viga" ["leva om sitt liv"],        
  vbm_paradigm "vbm_2mpnd1_lägga" ["lägga in sitt veto"],  
  vbm_paradigm "vbm_2mpl1_ha" ["ha ihop det"],
  vbm_paradigm "vbm_2mp1_säga" ["säga emot"],
  vbm_paradigm "vbm_2mp1_viga" ["hänga över"],
  vbm_paradigm "vbm_2mp1_sätta" ["sätta efter"],
  vbm_paradigm "vbm_2mp1_lägga" ["lägga ut"],
  vbm_paradigm "vbm_2mp1_leda" ["träda tillbaka"],
  vbm_paradigm "vbm_2mp1_känna" ["känna till"],
  vbm_paradigm "vbm_2mp1_hyra" ["höra till"],
  vbm_paradigm "vbm_2mp1_hända" ["vända om"],              
  vbm_paradigm "vbm_2mp1_ha" ["ha kvar"],      
  vbm_paradigm "vbm_2mp1_göra" ["göra åt"],
  vbm_paradigm "vbm_2mp1_ansöka" ["blåsa upp"],
  vbm_paradigm "vbm_2mnd1_lägga" ["lägga sitt veto"],  
  vbm_paradigm "vbm_2mlvs1_ansöka" ["leka rommen av sig"],
  vbm_paradigm "vbm_2mlp1_ha" ["ha det ihop"],  
  vbm_paradigm "vbm_2mld1_viga" ["bygga bo i huvudet"],
  vbm_paradigm "vbm_2ml1_välja" ["svälja stoltheten"],
  vbm_paradigm "vbm_2ml1_viga" ["bygga broar"],
  vbm_paradigm "vbm_2ml1_sätta" ["sätta fingret"],    
  vbm_paradigm "vbm_2ml1_lyfta" ["fästa blicken"],    
  vbm_paradigm "vbm_2ml1_lägga" ["inlägga förtjänst"],
  vbm_paradigm "vbm_2ml1_leda" ["skräda orden"],
  vbm_paradigm "vbm_2ml1_känna" ["bekänna färg"],   
  vbm_paradigm "vbm_2ml1_hyra" ["styra kosan"],        
  vbm_paradigm "vbm_2ml1_hända" ["vända ryggen"],            
  vbm_paradigm "vbm_2ml1_ha" ["ha reda"],
  vbm_paradigm "vbm_2ml1_göra" ["göra susen"],
  vbm_paradigm "vbm_2ml1_ansöka" ["väcka uppmärksamhet"],
  vbm_paradigm "vbm_2mi2_leda" ["inte skräda orden"],
  vbm_paradigm "vbm_2mi1_lägga" ["lägga näsan i blöt"],
  vbm_paradigm "vbm_2mi1_ansöka" ["åka på en propp"],
  vbm_paradigm "vbm_2mds1_göra" ["göra rätt för sig"],          
  vbm_paradigm "vbm_2md1_viga" ["ställa till svars"],

  -- vbm_paradigm "vbm_2md1_hyra" ["röra på påkarna"],
  
  vbm_paradigm "vbm_2mq1_hyra" ["röra upp himmel och jord"],
  vbm_paradigm "vbm_vmp1_drypa" ["drypa av"],

  vbm_paradigm "vbm_2md1_sätta" ["sätta ner foten"],             
  vbm_paradigm "vbm_2md1_lägga" ["lägga på hullet"],
  vbm_paradigm "vbm_2md1_känna" ["bränna under fötterna"],  
  vbm_paradigm "vbm_2md1_hyra" ["föra bakom ljuset"],     
  vbm_paradigm "vbm_2md1_hända" ["vända ryggen till"],   
  vbm_paradigm "vbm_2md1_ha" ["ha på tråden"],
  vbm_paradigm "vbm_2md1_göra" ["göra en pudel"],             
  vbm_paradigm "vbm_2md1_ansöka" ["tänka på refrängen"],
  vbm_paradigm "vbm_2at1_viga" ["ställa på sin spets"],          
  vbm_paradigm "vbm_2at1_sätta" ["sätta på det hala"],        
  vbm_paradigm "vbm_2at1_ansöka" ["möta på halva vägen"],           
  vbm_paradigm "vbm_2apd1_ansöka" ["trycka ner i skoskaften"],
  vbm_paradigm "vbm_2al1_viga" ["ställa tillrätta"],          
  vbm_paradigm "vbm_2al1_sätta" ["sätta p"],
  vbm_paradigm "vbm_2al1_lägga" ["lägga emellan"],
  vbm_paradigm "vbm_2al1_göra" ["göra förnär"], 
  vbm_paradigm "vbm_2ai1_ansöka" ["släppa gäcken lös"],
  vbm_paradigm "vbm_2ad1_viga" ["höja till skyarna"],     
  vbm_paradigm "vbm_2ad1_sätta" ["sätta på spel"],
  vbm_paradigm "vbm_2ad1_lägga" ["lägga i dagen"],
  vbm_paradigm "vbm_2ad1_göra" ["göra för när"], 
  vbm_paradigm "vbm_2ad1_ansöka" ["märka för livet"],        
  vbm_paradigm "vbm_1szt1_andas" ["utandas sin sista suck"],
  vbm_paradigm "vbm_1sp2_andas" ["inte låssas om"],
  vbm_paradigm "vbm_1sp1_andas" ["turas om"],
  vbm_paradigm "vbm_1mzd1_laga" ["omgjorda sina länder"],
  vbm_paradigm "vbm_1mvzd1_laga" ["peta i sin mat"],
  vbm_paradigm "vbm_1mvud1_laga" ["dansa efter någons pipa"],
  vbm_paradigm "vbm_1mt1_laga" ["ana ugglor i mossen"],
  vbm_paradigm "vbm_1mst1_laga" ["visa sig på styva linan"],
  vbm_paradigm "vbm_1msp1_laga" ["snigla sig fram"],
  vbm_paradigm "vbm_1ms1_laga" ["överila sig"],
  vbm_paradigm "vbm_1mq1_laga"  ["trampa på en öm tå"],
  vbm_paradigm "vbm_1mpzd1_laga" ["spela ut sin roll"], 
  vbm_paradigm "vbm_1mpt1_laga" ["surfa in på en räkmacka"],
  vbm_paradigm "vbm_1mps1_laga" ["tuffa till sig"],
  vbm_paradigm "vbm_1mpl1_laga" ["blanda bort korten"],
  vbm_paradigm "vbm_1ml1_laga" ["vädra morgonluft"],
  vbm_paradigm "vbm_1md1_laga" ["råka illa ut"],
  -- vbm_paradigm "vbm_1md1_ansöka" ["klippa med ögonen"],
  vbm_paradigm "vbm_1ic_laga" ["handla och vandla"],
  -- vbm_paradigm "vbm_1ap1_ansöka" ["rycka bort"],
  vbm_paradigm "vbm_1al1_laga" ["utöva makt"],    
  vbm_paradigm "vbm_1ad1_laga" ["jämna med marken"],
  vbm_paradigm "vbm_2mrd1_leda" ["skräda sina ord"],
  vbm_paradigm "vbm_2msp1_känna" ["känna sig för"],
  vbm_paradigm "vbm_4mf1_ta" ["ta det säkra före det osäkra"],
  vbm_paradigm "vbm_4mps1_ligga" ["ligga till sig"],
  vbm_paradigm "vbm_1mp1_laga" ["lämna över"], -- [(fl, "d")],
  vbma_paradigm "vbm_vap1_växa" ["växa ur"] [(vc "u" . tk 1 . fl,"en")],
  vbma_paradigm "vbm_vap1_vika" ["vika ut"] [(tk 1 . fl,"en")],  
  vbma_paradigm "vbm_vap1_tala" ["spela upp"] [(tk 1 . fl,"t")], 
  vbma_paradigm "vbm_vap1_strypa" ["rycka fram"] [(tk 1 . fl,"t")], 
  vbma_paradigm "vbm_vap1_sprida" ["sprida ut"] [(tk 1 . fl, "d")],
  vbma_paradigm "vbm_vap1_smälta" ["smälta samman"] [(vc "u" . tk 1 . fl,"en")],
  vbma_paradigm "vbm_vap1_smälla" ["smälla av"] [(vc "u" . tk 1 . fl,"en")],
  vbma_paradigm "vbm_vap1_nypa" ["nypa av"] [(tk 1 . fl,"t")],
  vbma_paradigm "vbm_vap1_lyfta" ["smälta ihop"] [(tk 1 . fl,"")],
  vbma_paradigm "vbm_vap1_koka" ["koka in"] [(tk 1 . fl,"t")],
  vbma_paradigm "vbm_vap1_klä" ["klä ut"] [(fl,"dd")], 
  vbma_paradigm "vbm_vap1_frysa" ["frysa ut"] [(vc "u" . tk 1 . fl, "en")],  
  vbma_paradigm "vbm_vap1_fnysa" ["dyka upp"] [(tk 1 . fl,"t")],
  vbma_paradigm "vbm_4ap1_tillåta" ["låta upp"] [(tk 1 . fl,"en")],  
  vbma_paradigm "vbm_4ap1_äta" ["äta upp"] [(tk 1 . fl,"en")],
  vbma_paradigm "vbm_4ap1_ta" ["dra in"] [(fl,"gen")],
  vbma_paradigm  "vbm_4ap1_slå" ["slå ner"] [(vc "a" . fl,"gen")],   
  vbma_paradigm "vbm_4ap1_skjuta" ["skjuta in"] [(tk 1 . fl, "en")],  
  vbma_paradigm "vbm_4ap1_sitta" ["sticka ner"] [(vc "u" . tk 1 . fl, "en")],
  vbma_paradigm "vbm_4ap1_se" ["se till"] [(fl,"dd")],
  vbma_paradigm "vbm_4ap1_rida" ["slita ut"] [(tk 1 . fl,"en")],   
  vbma_paradigm  "vbm_4ap1_missförstå" ["stå bi"] [(fl,"dd")],
  vbma_paradigm "vbm_4ap1_komma" ["komma ihåg"] [(tk 1 . fl,"en")], 
  vbma_paradigm "vbm_4ap1_hålla" ["hålla av"] [(tk 1.fl,"en")],
  vbma_paradigm "vbm_4ap1_gå" ["gå bort"] [(fl,"ngen")],
  vbma_paradigm "vbm_4ap1_ge" ["ge upp"] [(vc "i" . fl,"ven")],
  vbma_paradigm "vbm_4ap1_flyga" ["bryta ut"] [(vc "u" . tk 1 . fl,"en")],   
  vbma_paradigm "vbm_4ap1_falla" ["falla bort"] [(tk 1 . fl, "en")],
  vbma_paradigm "vbm_4ap1_dricka" ["sitta av"] [(vc "u" . tk 1 . fl, "en")],
  vbma_paradigm "vbm_4ap1_bära" ["skära ner"] [(vc "u" . tk 1. fl,"en")],
  vbma_paradigm "vbm_4ap1_bli" ["bli varse"] [(fl,"ven")],
  vbma_paradigm "vbm_3ap1_sy" ["sy in"] [(fl,"dd")],
  vbma_paradigm "vbm_2ap1_välja" ["smörja in"] [(vc "o" . tk 2 . fl, "d")],
  vbma_paradigm "vbm_2ap1_viga" ["döma ut"] [(tk 1.fl,"d")],
  vbma_paradigm "vbm_2ap1_sätta" ["sätta av"] [(vc "a" . tk 1 . fl,"")],
  vbma_paradigm "vbm_2ap1_sända" ["tända på"] [(tk 1 . fl,"")],
  vbma_paradigm "vbm_2ap1_skilja" ["skilja åt"] [(tk 1 . fl, "d")],
  vbma_paradigm "vbm_2ap1_säga" ["säga upp"] [(vc "a" . tk 1 . fl,"d")],
  vbma_paradigm "vbm_2ap1_lyfta" ["gifta bort"] [(tk 1.fl,"")],
  vbma_paradigm "vbm_2ap1_lägga" ["lägga undan"] [(vc "a" . tk 2 . fl,"d")],
  vbma_paradigm "vbm_2ap1_leda" ["reda ut"] [(tk 1 . fl,"d")],
  vbma_paradigm "vbm_2ap1_känna" ["skämma ut"] [(ungeminate_m_n . tk 1 .fl,"d")],
  vbma_paradigm "vbm_2ap1_hyra" ["hyra ut"] [(tk 1 . fl, "d")],
  vbma_paradigm "vbm_2ap1_göra" ["göra ned"] [(tk 3 . fl, "jord")],
  vbma_paradigm "vbm_2ap1_ansöka" ["knäppa upp"] [(tk 1 . fl, "t")],
  -- vbma_paradigm "vbm_1ap1_viga" ["slänga bort"] [(tk 1 . fl, "d")],
  vbma_paradigm "vbm_1ap1_skapa" ["tjäna av"] [(fl, "d")],
  vbma_paradigm "vbm_1ap1_laga" ["pigga upp"] [(fl, "d")]
  ]
