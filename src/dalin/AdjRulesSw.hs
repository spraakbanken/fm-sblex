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

import NounRulesSw
import TypesSw
import General
import GenRulesSw
import Data.List(nub)

mk_adjective :: String -> String -> String -> String -> String -> String -> String -> Adjective
mk_adjective liten litet lilla sma mindre minst minsta = 
    mk_adjective_v [liten] [litet] [lilla] [sma] [mindre] [minst] [minsta]

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

adj_almost_reg :: String -> String -> String -> Adjective
adj_almost_reg ljummen ljummet ljumma = mk_adjective ljummen ljummet ljumma ljumma (fv (ljumma) ++ "re") (fv (ljumma) ++ "st") (fv (ljumma) ++ "ste")

av1blek :: String -> Adjective
av1blek blek = adj_almost_reg blek (blek ++ "t") (blek ++ "a")

