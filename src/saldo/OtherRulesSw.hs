----------------------------------------------------------------------
-- |
-- Module      : OtherRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
--
-------------------------------------------------------------------------
module OtherRulesSw where

import Data.Char
import GenRulesSw
import General
import TypesSw
import AdjRulesSw
import NounRulesSw

mk_pm :: String -> (PNForm -> Str)
mk_pm s p = let nomfs = mk_case_v Nom [s]
                genfs = mk_case_v Gen $ [s +? "s"]
                combhyph = [c ++ "-" | c <- nomfs]
             in case p of
                 (PNForm Nom) -> strings $ nomfs
                 (PNForm Gen) -> strings $ genfs
                 (PNComp)     -> strings $ nomfs ++ combhyph
                 (PNSMS)      -> strings $ combhyph

mk_pm_alice :: String -> (PNForm -> Str)
mk_pm_alice s (PNForm Gen) = strings $ [s,s +? "s"]
mk_pm_alice s p = mk_pm s p

mk_pmm0 :: [String] -> (PNMForm -> Str)
mk_pmm0 xs (PNMForm c) = strings [unwords ((init xs) ++ [r]) | r <- mk_case_v c [last xs]]

--mk_pma :: String -> (PNAForm -> Str)
--mk_pma s (PNAForm c) = strings $ mk_ccase c s

mk_pma :: String -> (PNAForm -> Str)
mk_pma s p = let nomfs = mk_case_v Nom [s]
                 genfs = mk_case_v Gen $ [s +? "s"]
                 combhyph = [c ++ "-" | c <- nomfs ++ genfs]
              in case p of
                 (PNAForm Nom) -> strings $ nomfs
                 (PNAForm Gen) -> strings $ genfs
                 (PNAComp)     -> strings $ combhyph
                 (PNASMS)      -> strings $ combhyph

mk_pmam0 :: [String] -> (PNMForm -> Str)
mk_pmam0 xs (PNMForm c) =  strings [unwords ((init xs) ++ [g]) | g <- (mk_ccase c (last xs))]

mk_ccase c s = case c of 
    Nom -> [s]
    Gen | elem (last s) "sxzSXZ" -> [s]
    _ -> [s ++ "-s",s ++ ":s",s++"s"]

ab_i_no_comp :: String -> AdverbInv
ab_i_no_comp s AdverbInvForm = mkStr s
ab_i_no_comp s _             = nonExist

mk_number :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> Number
mk_number (en,ett,första,förste,en_c) s (NumC) =
 let xs = apply_suffixes s en_c in 
     strings $ xs ++ [x ++ "-" | x <- xs]
mk_number (en,ett,första,förste,en_c) s (NumF c o) = 
  strings $ map (mk_case c) $ case o of
   Numeral         -> apply_suffixes s en
   NumeralN        -> apply_suffixes s ett
   Ordinal NoMasc  -> apply_suffixes s första
   Ordinal Masc    -> apply_suffixes s förste


mk_adverb :: [String] -> [String] -> [String] -> Adverb
mk_adverb bra battre bast af = strings $
   case af of
    (AdverbForm Posit)  -> bra
    (AdverbForm Compar) -> battre
    (AdverbForm Superl) -> bast
    AdComp -> concat [[x,x +? "-"] | x <- bra]
    AdSMS -> [x +? "-" | x <- bra]


mk_pron_jag :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> PronPN
mk_pron_jag (jag,mig,min,mitt,mina) s c = strings $ case c of
  PNom -> capsuff jag
  PAcc -> capsuff mig
  PGen (ASgUtr)   -> capsuff min
  PGen (ASgNeutr) -> capsuff mitt
  PGen APl -> capsuff mina
-- map (++ "a") min 
 where suff = apply_suffixes s
       capsuff = cap . suff
       cap ws = 
         case suff jag of
           ((x:_):_) | isUpper x -> [(toUpper c:cs) | (c:cs) <- ws] 
           _ -> ws
           
mk_pron_nagon :: (Suffixes,Suffixes,Suffixes) -> String -> PronAdj
mk_pron_nagon (nagon,nagot,nagra) s (AP gn c) = strings $ map (mk_case c) $
    case gn of
     ASgUtr   -> suff nagon
     ASgNeutr -> suff nagot
     APl      -> suff nagra
 where suff = apply_suffixes s

mk_pron_han :: (Suffixes, Suffixes, Suffixes) -> String -> PronPN
mk_pron_han (han, honom, hans) s c = strings $ case c of
  PNom   -> suff han
  PAcc   -> suff honom
  PGen _ -> suff hans
 where suff = apply_suffixes s

abm :: String -> AdverbMInv
abm till_exempel _ = mkStr $ till_exempel


mk_adverb_c :: [String] -> [String] -> [String] -> [String] -> Adverb
mk_adverb_c bra battre bast comp af = strings $
   case af of
    (AdverbForm Posit)  -> bra
    (AdverbForm Compar) -> battre
    (AdverbForm Superl) -> bast
    AdComp              -> concat [[x,x +? "-"] | x <- comp]
    AdSMS               -> [x +? "-" | x <- comp]

mk_pn_o_den :: String -> PronAdj
mk_pn_o_den den (AP gn c) = strings $ 
    case (gn,c) of
     (ASgUtr,Nom)   -> [den]
     (ASgUtr,Gen)   -> [den++"s", de++"ss"] 
     (ASgNeutr,Nom) -> [de++"t"]
     (ASgNeutr,Gen) -> [de++"ss"]
     _               -> []
 where de = tk 1 den


pnm_o_den_här_rule :: String -> PronAdj
pnm_o_den_här_rule den_här (AP gn c) = 
    case (gn,c) of
      (ASgUtr,Nom)   -> mkStr $ unwords [de++"n",här]
      (ASgNeutr,Nom) -> mkStr $ unwords [de++"t",här]
      (APl,Nom)      -> mkStr $ unwords [de,här]
      _ -> nonExist
 where (de,här) = case words den_här of
                    [den,här] -> (tk 1 den, här)
                    _         -> ([],[])

mk_number_ng :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> Number
mk_number_ng suffs s = missing (mk_number suffs s) [NumF Gen o | o <- values]

nln1 :: String -> Number
nln1 x1 (NumC) = mkStr [] 
nln1 x1 (NumF c o) = 
  mkStr $ case (o,c) of
   (Ordinal NoMasc,Nom)  -> x1 ++ ":a" 
   (Ordinal NoMasc,Gen)  -> x1 ++ ":as"
   (Ordinal Masc,Nom)    -> x1 ++ ":es" 
   (Ordinal Masc,Gen)    -> x1 ++ ":es" 
   (_,Nom)               -> x1
   (_,Gen)               -> x1 ++ ":s"

no_comp :: Adverb -> Adverb
no_comp f = missing f [AdComp, AdSMS]

abfrämst :: String -> Adverb
abfrämst främst = no_comp $ mk_adverb [] [] [främst]

abfort fort = mk_adverb [fort] [fort +? "are"] [fort +? "ast"]
