----------------------------------------------------------------------
-- |
-- Module      : NounRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
--
-- Swedish noun functions
-------------------------------------------------------------------------
module NounRulesSw where

import GenRulesSw
import General
import TypesSw
import Data.Char(isDigit)

type NComp  = String
type NSg    = String
type NSgD   = String 
type NPl    = String
type NPlD   = String


hyphenate_compounds :: Substantive -> Substantive
hyphenate_compounds f = compvariants [w +? "-" | w <- unStr (f InitComposite)]
                                     [w +? "-" | w <- unStr (f MedComposite)] f

compvariants :: [NComp] -> [NComp] -> Substantive -> Substantive
compvariants compi compm f = variants f [
                              (InitComposite,strings compi),
                              (MedComposite,strings compm),
                              (SMS,strings [ x | x@(_:_) <- compi,last x=='-'])
                             ] 


mk_substantive_v :: [NSg] -> [NSgD] -> [NPl] -> [NPlD] -> [NComp] -> [NComp] -> Substantive
mk_substantive_v apa apan apor aporna ap_comp ap_comp2 a = 
        case a of
          SF n s c -> strings $ mk_case_v c $ 
                       case (n,s) of
                         (Sg,Indef) -> apa
                         (Sg,Def)   -> apan
                         (Pl,Indef) -> apor
                         (Pl,Def)   -> aporna
          InitComposite    -> strings ap_comp
          MedComposite     -> strings ap_comp2
          SMS              -> nonExist

mk_case_v :: Casus -> [String] -> [String]
mk_case_v c xs = concat $ map f (filter (not.null) xs)
  where f s = case c of
               Nom                                          -> [s]
               Gen | not (null s) && elem (last s) "sxzSXZ" -> [s]
               Gen | not (null s) && (elem (last s) "!?" || isDigit (last s)) -> [s ++ ":s",s++"-s"]
               Gen                                          -> [s ++ "s"]

mk_case :: Casus -> String -> String
mk_case c [] = []
mk_case c s = case c of
  Nom                          -> s
  Gen | elem (last s) "sxzSXZ" -> s
  Gen | elem (last s) "?!" || isDigit (last s) -> s ++ ":s"
  Gen                          ->  s ++ "s"
