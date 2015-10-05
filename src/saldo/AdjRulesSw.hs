----------------------------------------------------------------------
-- |
-- Module      : AdjRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
-- Swedish adjective functions
-------------------------------------------------------------------------
module AdjRulesSw where

import TypesSw
import General
import NounRulesSw
import GenRulesSw
import Data.List(nub)

adjective_prefixed :: Int -> Suffixes -> Suffixes -> Suffixes -> Suffixes -> 
                             Suffixes -> Suffixes -> Suffixes -> String -> Adjective
adjective_prefixed n liten litet lilla sma mindre minst minsta s = 
     mk_adjective_v (f liten) (f litet) (f lilla) (f sma) (f mindre) (f minst) (f minsta)
  where f = apply_suffixes (tk n s)

mk_adj_pos :: [String] -> [String] -> [String] -> [String] -> AdjFormPos -> [String]
mk_adj_pos liten litet lilla sma a = case a of
  Strong gn  -> case gn of
    ASgUtr   -> liten
    ASgNeutr -> litet
    APl      -> sma
  Weak sn -> case sn of
    AxSg g -> giveValues [lilla, lille] g
    AxPl -> sma
 where
   lille = let x = map (\s -> if (last s == 'a') then tk 1 s else s) lilla in 
           -- avoid bra -> bre, få -> fe
             if (and (map (any is_vowel) x)) then nub (map (+?"e") x) else lilla

mk_adjective_v :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Adjective
mk_adjective_v liten litet lilla sma mindre minst minsta AdjComp = strings $ concat [[x,x +? "-"] | x@(_:_) <- liten]
mk_adjective_v liten litet lilla sma mindre minst minsta AdjSMS  = strings $ [x +? "-" | x@(_:_) <- liten]
mk_adjective_v liten litet lilla sma mindre minst minsta (AF a c) = 
  strings $ map (mk_case c) $ case a of
    Pos p   -> mk_adj_pos liten litet lilla sma p
    Comp    -> mindre 
    Super s -> giveValues [minst, minsta,minste] s
 where minste = let x = map  (\s -> if (last s == 'a') then tk 1 s else s) minsta in 
                 if 
                     (and (map (any is_vowel) x)) then nub (map (+?"e") x) else []

no_genitive :: Adjective -> Adjective
no_genitive f = missing f [(AF v Gen) | v <- values]

av0lastgammal :: String -> Adjective
av0lastgammal lastgammal = no_compare $ 
       adj_almost_reg lastgammal (lastgammal ++ "t") (lastgamla) 
 where lastgamla =  (ungeminate (tk 2 lastgammal)) ++ "la"

av1glad :: String -> Adjective
av1glad glad = adj_almost_reg glad (tk 1 glad  ++  "tt") (glad ++ "a")

av1högljudd :: String -> Adjective
av1högljudd högljudd = adj_almost_reg högljudd (höglju ++  "tt") (högljudd  ++ "a")
 where höglju = case reverse högljudd of
                 (c:c1:_) | c == c1 -> tk 2 högljudd
                 _                  -> tk 1 högljudd

av0medelstor :: String -> Adjective
av0medelstor medelstor = no_compare $ 
  adj_almost_reg medelstor (medelstor ++ "t") (medelstor++"a")

adj_almost_reg :: String -> String -> String -> Adjective
adj_almost_reg ljummen ljummet ljumma = mk_adjective ljummen ljummet ljumma ljumma (ljumma ++ "re") (ljumma ++ "st") (ljumma ++ "ste")


no_compare :: Adjective -> Adjective
no_compare a = a `only` positive_forms

mk_adjective :: String -> String -> String -> String -> String -> String -> String -> Adjective
mk_adjective liten litet lilla sma mindre minst minsta = 
    mk_adjective_v [liten] [litet] [lilla] [sma] [mindre] [minst] [minsta]

av1blek :: String -> Adjective
av1blek blek = adj_almost_reg blek (blek ++ "t") (blek ++ "a")

av1akut :: String -> Adjective
av1akut akut = adj_almost_reg akut (aku ++ "t") (akut ++ "a")
 where aku = tk 1 akut

av1vacker :: String -> Adjective
av1vacker vacker = adj_almost_reg vacker vackert (mmn (drop_last_vowel vacker) ++ "a")
  where vackert = ifEndThen (== 'n') vacker (tk 1 vacker ++"t") (vacker++"t")

no_neutrum :: Adjective -> Adjective
no_neutrum f = missing f [(AF (Pos (Strong ASgNeutr))  c) | c <- values]

no_masc :: Adjective -> Adjective
no_masc f = missing f $ [AF (Pos (Weak (AxSg Masc))) v | v <- values] ++ [AF (Super (SupWeak Masc)) v | v <- values]


av0konstlad :: String -> Adjective
av0konstlad konstlad = adj_almost_reg konstlad (konstla ++ "t") (konstlad ++ "e") `only` positive_forms
 where konstla =tk 1 konstlad

av1lat :: String -> Adjective
av1lat lat = no_neutrum (av1akut lat)

av2ung :: String -> Adjective
av2ung ung = adj_irreg3 ung (yng++"re") (yng++"st")
  where yng = umlaut ung

av1fri :: String -> Adjective
av1fri fri = adj_almost_reg fri (fri ++ "tt") (fri ++ "a")

av1hård :: String -> Adjective
av1hård hård = adj_almost_reg hård (tk 1 hård ++ "t")  (hård ++ "a")


av1ensam :: String -> Adjective
av1ensam ensam = mk_adjective ensam (ensam++"t") (ensam++ma) (ensam++ma) (ensam++ma ++ "re") (ensam++ ma ++ "st") (ensam++ma++"ste")
 where ma = [last ensam] ++ "a"

av1angelägen :: String -> Adjective
av1angelägen angelägen = adj_almost_reg angelägen (angeläg ++ "et") ((ungeminate angeläg) +? "na")
 where angeläg = tk 2 angelägen

adj_irreg3 :: String -> String -> String -> Adjective
adj_irreg3 ung yngre yngst = adj_irreg ung (ung ++ "t") yngre yngst

adj_irreg :: String -> String -> String -> String -> Adjective
adj_irreg god gott battre bast = mk_adjective god gott (god ++ "a") (god ++ "a") battre bast (bast ++ "a")
