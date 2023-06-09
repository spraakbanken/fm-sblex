module AdjBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

adj :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes) -> (String -> Entry)
adj n (liten,litet,lilla,sma,mindre,minst,minsta) s = adjective $ 
    adjective_prefixed n liten litet lilla sma mindre minst minsta s        

adjective :: Adjective -> Entry
adjective = entry


adjc :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes,Suffixes) -> (String -> Entry)
adjc n (liten,litet,lilla,sma,mindre,minst,minsta,småc) s = adjective $ 
    excepts (adjective_prefixed n liten litet lilla sma mindre minst minsta s) 
    [(AdjComp,strings $ concat [[x,x+?"-"] | x <- apply_suffixes (tk n s) småc]),
     (AdjSMS,strings $ [x+?"-" | x <- apply_suffixes (tk n s) småc])
                                                                               ]
adj_no_genitive_c :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes,Suffixes) -> (String -> Entry)
adj_no_genitive_c n (liten,litet,lilla,sma,mindre,minst,minsta,småc) s = adjective $ no_genitive $
    excepts (adjective_prefixed n liten litet lilla sma mindre minst minsta s) 
    [(AdjComp,strings $ concat [[x,x+?"-"] | x <- apply_suffixes (tk n s) småc]),
     (AdjSMS,strings $ [x+?"-" | x <- apply_suffixes (tk n s) småc])
                                                                               ]
av_i_diverse :: String -> Entry
av_i_diverse s = entry $ f
  where f AdjInvForm = mkStr s  
        f AdjInvComp = mkStr s

avm_i :: String -> Entry
avm_i = entry . (const :: Str -> AdjMInv -> Str) . mkStr

av_1_blek :: String -> Entry
av_1_blek = adjective . av1blek

av_1_akut :: String -> Entry
av_1_akut = adjective . av1akut

av_1_vacker :: String -> Entry
av_1_vacker = adjective . av1vacker

adj_no_masc :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes, Suffixes) -> (String -> Entry)
adj_no_masc n (liten,litet,lilla,sma,mindre,minst,minsta) s = adjective $ 
    no_masc $ adjective_prefixed n liten litet lilla sma mindre minst minsta s        

av_0_konstlad :: String -> Entry
av_0_konstlad = adjective . av0konstlad

av_1_blek_ng :: String -> Entry
av_1_blek_ng = adjective . no_genitive . av1blek


av_0_medelstor :: String -> Entry
av_0_medelstor  = adjective . av0medelstor

av_0_lastgammal :: String -> Entry
av_0_lastgammal = adjective . av0lastgammal 

av_1_glad :: String -> Entry
av_1_glad = adjective . av1glad

av_1_högljudd :: String -> Entry
av_1_högljudd = adjective . av1högljudd

av_1_lat :: String -> Entry
av_1_lat = adjective . av1lat

av_2_ung :: String -> Entry
av_2_ung = adjective . av2ung


av_1_fri :: String -> Entry
av_1_fri = adjective . av1fri

av_1_hård :: String -> Entry
av_1_hård = adjective . av1hård

av_1_ensam :: String -> Entry
av_1_ensam = adjective . av1ensam

av_1_angelägen :: String -> Entry
av_1_angelägen = adjective . av1angelägen
