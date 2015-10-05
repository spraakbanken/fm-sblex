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
import Data.List

type NSg    = String
type NSgD   = String 
type NPl    = String
type NPlD   = String
type NComp  = String
type NDeriv = String 

mk_substantive_v :: [NSg] -> [NSgD] -> [NPl] -> [NPlD] -> [NComp] -> [NComp] -> [NDeriv]-> Substantive
mk_substantive_v apa apan apor aporna ap_comp ap_comp2 ap_deriv a = 
 case a of
  SF n s c -> strings $ mk_case_v c $ 
   case (n,s) of
    (Sg,Indef) -> apa
    (Sg,Def) -> apan
    (Pl,Indef) -> apor
    (Pl,Def) -> aporna
  InitComposite -> strings ap_comp
  MedComposite -> strings ap_comp2
  SMS -> nonExist
  Deriv -> strings ap_deriv

mk_case :: Casus -> String -> String
mk_case c [] = []
mk_case c s = case c of
  Nom                          -> s
  Gen | elem (last s) "sxzSXZ" -> s
  Gen | elem (last s) "?!" || isDigit (last s) -> s ++ ":s"
  Gen                          ->  s ++ "s"

mk_case_v :: Casus -> [String] -> [String]
mk_case_v c xs = concat $ map f (filter (not.null) xs)
  where f s = case c of
         Nom -> [s]
         Gen | not (null s) && elem (last s) "sxzSXZ" -> [s]
         Gen | not (null s) && (elem (last s) "!?" || isDigit (last s)) -> [s ++ ":s",s++"-s"]
         Gen -> [s ++ "s"]

hyphenate_compounds :: Substantive -> Substantive
hyphenate_compounds f = compvariants [w +? "-" | w <- unStr (f InitComposite)]
                                     [w +? "-" | w <- unStr (f MedComposite)] f
                                     
compvariants :: [NComp] -> [NComp] -> Substantive -> Substantive
compvariants compi compm f = 
  variants f [
   (InitComposite,strings compi),
   (MedComposite,strings compm),
   (SMS,strings [ x | x@(_:_) <- compi,last x=='-'])
   ] 
                             

compvariant :: [NComp] -> Substantive -> Substantive
compvariant compi f = variants f [(InitComposite,strings compi), (MedComposite,strings (concat [compound_s w | w <- compi]))]

geminator :: Substantive -> Substantive
geminator f 
 | and [isSuffixOf "mm" w | w <- unStr (f (SF Sg Indef Nom))] = f
 | otherwise = compexcept' [ungeminate w  | w <- unStr (f InitComposite)] [ungeminate w | w <- unStr (f MedComposite)] f

compexcept' ::  [NComp] -> [NComp] -> Substantive -> Substantive
compexcept' compi compm f = excepts f [(InitComposite,strings compi), (MedComposite,strings compm)]

mk_nna :: [String] -> [String] -> [String] -> [String] ->
          [String] -> [String] -> [String] -> [String] -> Substantive
mk_nna apa apas apan apans apor apors aporna apornas a = strings $
        case a of
          SF n s c -> 
                       case (n,s,c) of
                         (Sg,Indef,Nom) -> apa
                         (Sg,Indef,Gen) -> apas
                         (Sg,Def,Nom)   -> apan
                         (Sg,Def,Gen)   -> apans
                         (Pl,Indef,Nom) -> apor
                         (Pl,Indef,Gen) -> apors
                         (Pl,Def,Nom)   -> aporna
                         (Pl,Def,Gen)   -> apornas
          InitComposite   -> map (++ "-") apa
          MedComposite    -> map (++ "-") apa
          SMS             -> map (++ "-") apa
          Deriv           -> map (++ "-") apa

mk_substantive :: NSg -> NSgD -> NPl -> NPlD -> NComp -> NDeriv -> Substantive
mk_substantive apa apan apor aporna ap_comp ap_deriv = 
 variants (mk_substantive_v (lift apa) (lift apan) (lift apor) 
  (lift aporna) (lift ap_comp) (lift ap_comp) (lift ap_deriv)) [(MedComposite,strings (compound_s ap_comp))]

mk_subst_v :: [NSg] -> [NSgD] -> [NPl] -> [NPlD] -> [NComp] -> Substantive
mk_subst_v bil bilen bilar bilarna bilc = 
    variants (mk_substantive_v bil bilen bilar bilarna bilc bilc bilc) [(MedComposite,strings (concat (map compound_s bilc)))]

mk_subst :: NSg -> NSgD -> NPl -> NPlD -> Substantive
mk_subst bil bilen bilar bilarna = 
    mk_substantive bil bilen bilar bilarna bil bil

nn2 :: String -> Substantive
nn2 pojke = mk_substantive pojke pojken (pojk ++ "ar") (pojk ++ "arna") pojk pojk
 where pojk   = drop_final_e pojke
       pojken = pojke ++ if_vowel (last pojke) "n" "en" 

nn4 :: String -> Substantive
nn4 linje = mk_subst linje (linje ++ "n") (linje ++ "r") (linje++"rna") 

nn3parti :: String -> Substantive
nn3parti parti = sgdefvariants [parti++"t"] (nn3vin parti) 

sgdefvariants :: [NSgD] -> Substantive -> Substantive
sgdefvariants nom_s finite_f = 
    variants finite_f [(SF Sg Def Nom, strings nom_s), (SF Sg Def Gen, strings (map (mk_case Gen) nom_s))]

nn3vin :: String -> Substantive
nn3vin vin = mk_subst vin (vin ++ "et") (vin ++ "er") (vin ++ "erna") 

nn3tand :: String -> Substantive
nn3tand tand = mk_subst tand (tand++"en") (tänd ++ "er") (tänd ++ "erna") 
 where tänd = umlaut tand

nni_ns :: String -> Substantive
nni_ns w = compexcept' [w] [w] (missing (nngfebruari w) [SF Sg Indef Gen])

nngfebruari :: String -> Substantive
nngfebruari februari = mk_subst februari [] [] []


nnkol14 :: String -> Substantive 
nnkol14 kol14 n = strings $ case n of
 (SF Sg Indef Nom) -> [kol14]
 (SF Sg Indef Gen) -> [kol14++"-s",kol14++":s"]
 InitComposite -> [kol14++"-"]
 MedComposite -> [kol14++"-"]
 _ -> []

nn0oväsen :: String -> Substantive
nn0oväsen oväsen = sgdefexcepts  [oväsen++"det",oväsen++"et"] (nn0 oväsen)

sgdefexcepts :: [NSgD] -> Substantive -> Substantive
sgdefexcepts nom_s finite_f  = 
    excepts finite_f [(SF Sg Def Nom, strings nom_s), (SF Sg Def Gen, strings (map (mk_case Gen) nom_s))]

sgindefvariants :: [NSg] -> Substantive -> Substantive
sgindefvariants nom_s finite_f = 
    variants finite_f [(SF Sg Indef Nom, strings nom_s), (SF Sg Indef Gen, strings (map (mk_case Gen) nom_s))]

nn0skum :: String -> Substantive
nn0skum skum = sgdefexcepts  [skumm++"et"] (nn0 skum)
 where skumm = geminate skum

nn0kaffe :: String -> Substantive          
nn0kaffe kaffe = sgdefexcepts  [kaffe++"t"] (nn0 kaffe)

nn0skam :: String -> Substantive
nn0skam skam = sgdefexcepts [skamm++"en"] (nn0 skam) 
 where skamm = geminate skam


nn0manna :: String -> Substantive
nn0manna manna = sgdefexcepts [manna++"n", manna++"t"] (nn0 manna) 


nn2mor :: String -> Substantive
nn2mor mor_or_moder = compvariants [moder++"s",mor,mor++"s"] [mor,mor++"s"] $ sgindefvariants [moder] $
 mk_substantive mor (moder ++"n") (moedr ++"ar") (moedr ++"arna") moder moder
    where (mor,moder)   = 
              if (dp 3 mor_or_moder) == "der" then (tk 3 mor_or_moder ++ "r",
                                                    mor_or_moder)
               else
                   (mor_or_moder, tk 1 mor_or_moder ++ "der")
          moedr = umlaut $ drop_last_vowel moder

nn0 :: String -> Substantive
nn0 mjölk = mk_subst mjölk (mjölk++"en") [] [] 

nn2dotter :: String -> Substantive
nn2dotter dotter = mk_substantive dotter (dotter ++"n") (döttr ++"ar") (döttr ++"arna") dotter []
 where döttr = umlaut $ drop_last_vowel dotter

nnvkaliber :: String -> Substantive
nnvkaliber kaliber = plvariants [kalibr++"er"] [kalibr++"erna"] $ 
 mk_subst kaliber (kaliber++"n") (kalibr++"ar") (kalibr++"arna")
 where kalibr = drop_last_vowel kaliber

nnvplayboy :: String -> Substantive
nnvplayboy playboy = plvariants [playboy++"s"] [playboy++"sen",playboy++"sarna"] $
 mk_subst playboy (playboy++"en") (playboy++"ar") (playboy++"arna")
                                    

plvariants :: [NPl] -> [NPlD] -> Substantive -> Substantive
plvariants pl plet f = pldefvariants plet (plindefvariants pl f) 

pldefvariants :: [NPlD] -> Substantive -> Substantive
pldefvariants nom_s finite_f = 
    variants finite_f [(SF Pl Def Nom, strings nom_s), (SF Pl Def Gen, strings (map (mk_case Gen) nom_s))]

plindefvariants :: [NPl] -> Substantive -> Substantive
plindefvariants nom_s finite_f = 
    variants finite_f [(SF Pl Indef Nom, strings nom_s), (SF Pl Indef Gen, strings (map (mk_case Gen) nom_s))]

nnvspektrum :: String -> Substantive
nnvspektrum spektrum = plvariants [spektrum,spektr++"a"] 
                                  [spektrum++"en", spektr++"ana"] $
    sgdefvariants [spektr++"et", spektrum++"et"] $
       mk_subst spektrum spektrum (spektr++"er") (spektr++"ena") 
 where spektr = tk 2 spektrum

nnvblinker :: String -> Substantive
nnvblinker blinker = plvariants [blinker++"s"] [blinker++"sen",blinker++"sarna"] $
                     mk_subst blinker (blinker++"n") 
                             (blinkr++"ar") (blinkr++"arna") 
 where blinkr = mmr (drop_last_vowel blinker)

nnvdress :: String -> Substantive
nnvdress dress = plvariants [dress++"er"] [dress++"erna"] $ 
                 mk_subst dress (dress++"en") (dress++"ar") (dress++"arna")

nnvhambo :: String -> Substantive
nnvhambo hambo = plvariants [hambo++"r"] [hambo++"rna"] $
          mk_subst hambo (hambo++"n") (hambo++"er") (hambo++"erna")


nnvgarn :: String -> Substantive
nnvgarn garn = combine (nn3vin garn) (nn6 garn)

nnvhuvud :: String -> Substantive
nnvhuvud huvud =  plindefvariants [huvud++"en"]
                   (mk_subst huvud (huvud++"et") huvud (huvud++"ena"))


nn6borst :: String -> Substantive
nn6borst borst = sgdefvariants  [borst++"en"] (nn6 borst)


nn3bok :: String -> Substantive
nn3bok bok = mk_subst bok (bok ++"en") (böcker) (böcker++"na") 
 where böcker = umlaut (tk 1 bok) ++ "cker"

nn3fot :: String -> Substantive
nn3fot fot = mk_subst fot (fot ++"en") (fötter) (fötter++"na") 
 where fötter = umlaut (geminate fot) ++ "er"

nn3bockfot :: String -> Substantive
nn3bockfot fot = compexcept' [fot,fot+?"s"] [fot,fot+?"s"] (mk_subst fot (fot ++"en") (fötter) (fötter++"na")) 
 where fötter = umlaut (geminate fot) ++ "er"

nn3vän :: String -> Substantive
nn3vän vän = mk_subst vän (vänn ++"en") (vänn ++"er") (vänn ++"erna") 
    where vänn = vän ++ [last vän]

nn6 :: String -> Substantive
nn6 lik = mk_subst lik (lik ++ "et") lik (lik ++ "en")

nn6broder :: String -> Substantive
nn6broder broder = compvariants [broder++"s"] [] $ mk_subst broder (broder ++ "n") (bröd++"er") (bröd ++ "erna")
 where bröd = umlaut brod
       brod = tk 2 broder
       
nn6mus :: String -> Substantive
nn6mus mus = mk_subst mus (mus ++"en") möss (möss ++"en") 
    where möss = vc "ö" $ geminate mus


nn6vaktman :: String -> Substantive
nn6vaktman man = mk_subst man (mann ++"en") män (männ ++"en") 
    where män   = umlaut man
          männ  = geminate män
          mann  = geminate man


nn5knä :: String -> Substantive
nn5knä knä = sgdefvariants [knä++"et"] (nn5 knä) 

nn5anmodan :: String -> Substantive
nn5anmodan anmodan = mk_subst anmodan anmodan anmodanden (anmodanden++"a")
 where anmodanden = anmodan++"den"

nn4bonde :: String -> Substantive
nn4bonde bonde = compvariant [bond] $ mk_substantive bonde (bonde ++ "n") (bönd ++ "er") (bönd++"erna") bonde bond
   where bond = tk 1 bonde
         bönd = umlaut bond
         
nn5 :: String -> Substantive
nn5 rike = mk_subst rike (rike++et) (rike++en) (rike++ena) 
  where et  = if_vowel (last rike) "t" "et"
        en  = if_vowel (last rike) "n" "en"
        ena = if_vowel (last rike) "na" "ena"

nn3flanell :: String -> Substantive
nn3flanell flanell = sgdefvariants  [flanell++"et"] (nn3 flanell)

nn2kam :: String -> Substantive
nn2kam kam = mk_subst kam (kamm ++"en") (kamm ++"ar") (kamm ++"arna")
    where kamm = geminate kam

nn2öken :: String -> Substantive
nn2öken öken = mk_subst öken (ökn ++"en") (ökn ++"ar") (ökn ++"arna") 
 where ökn = drop_last_vowel öken

nn3 :: String -> Substantive
nn3 sak = mk_subst sak (sak ++ "en") (sak ++ "er") (sak++"erna") 

nni :: String -> Substantive
nni w = missing (nngfebruari w) [SF Sg Indef Gen]

nnoofficer :: String -> Substantive
nnoofficer officer = sgdefvariants [officer++"n"] (mk_subst officer (officer++"en") (officer++"are") (officer++"arna")) 

nnoemeritus :: String -> Substantive       
nnoemeritus emeritus = mk_subst emeritus emeritus emeriti  (emeriti++"na") 
 where emeriti = tk 2 emeritus ++ "i"

nnoexamen :: String -> Substantive
nnoexamen examen =  mk_subst examen examen (exam++"ina") (exam++"ina")
  where exam = tk 2 examen

nn6gås :: String -> Substantive            
nn6gås gås = mk_subst gås (gås++"en") gäss (gäss++"en") 
 where gäss = (vc "ä" gås) ++"s"

nnvtrio :: String -> Substantive
nnvtrio trio = plvariants [trio++"s"] [trio++"sen"] $
    mk_subst trio (trio++"n") (trio++"r") (trio++"rna")

nnvborr :: String -> Substantive
nnvborr borr = combine (nn2 borr) (nn6 borr)

nnvtest :: String -> Substantive
nnvtest test = combine (nn3 test) (nn6 test)


nn6akademiker :: String -> Substantive
nn6akademiker akademiker = 
 mk_subst akademiker (akademiker ++ "n") akademiker (akademiker ++ "na") 

nnvabc :: String -> Substantive
nnvabc = nn5abc


nn5abc :: String -> Substantive
nn5abc abc nf = strings $ case nf of
    (SF Sg  Indef Nom) -> [abc]
    (SF Sg Indef Gen) -> [abc ++ ":s",abc++"-s"]
    (SF Sg Def   Nom) -> [abc ++ "-et", abc++"-t",abc ++ ":et", abc++":t"]
    (SF Sg Def   Gen) -> [abc ++ "-ets", abc++"-ts",abc ++ ":ets", abc++":ts"]
    (SF Pl Indef Nom) -> [abc ++ ":n",abc++"-n"]
    (SF Pl Indef Gen) -> [abc ++ ":ns",abc++"-ns"]
    (SF Pl Def   Nom) -> [abc++"-na",abc++":na"]
    (SF Pl Def   Gen) -> [abc++"-nas",abc++":nas"]
    InitComposite         -> [abc++"-"]
    MedComposite         -> [abc++"-"]
    SMS               -> [abc++"-"]
    Deriv             -> [abc]

nnvabdomen :: String -> Substantive
nnvabdomen abdomen = plvariants  [abdom++"ina"] [abdom++"ina"] (mk_substantive abdomen abdomen abdomen abdomen abdomen abdomen)
 where abdom = tk 2 abdomen

nnvcentrum :: String -> Substantive        
nnvcentrum centrum = plvariants [centr++"a", centr++"er"]  
                                [centr++"ana", centr++"erna"] $
                                 sgdefvariants [centr++"et",centrum] um_f 
  where um_f  =  mk_subst centrum (centrum++"et") centrum (centrum++"en") 
        centr = tk 2 centrum

nnvnomen :: String -> Substantive
nnvnomen nomen = plvariants [nom++"ina"] [nom++"ina"] (mk_substantive nomen (nomen++"et") nomen (nomen++"en") nomen nomen) 
 where nom = tk 2 nomen

nnvjojo :: String -> Substantive
nnvjojo jojo = combine (nn2 jojo) (nn4 jojo)

nnvpartner :: String -> Substantive        
nnvpartner partner = plvariants  [partner] [partner ++"na",partner ++"sarna"]  $
    mk_subst partner (partner++"n") (partner++"s") (partner++"sen")
