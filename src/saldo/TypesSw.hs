module TypesSw where

import General
import Dictionary
import Attr


data Genus = Utr | Neutr | GPl | GDPl | Pend | MascGen | FemGen | Human | PNeutr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Genus where 
    values = enum
    prValue g  = case g of
     Utr -> "u"
     Neutr -> "n"
     GPl -> "p"
     GDPl -> "d"
     Pend -> "v"
     MascGen -> "m"
     Human -> "h"
     FemGen  -> "f"
     PNeutr  -> "w"

data Numerus = Sg | Pl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Numerus where 
    values = enum
    prValue Sg = "sg"
    prValue Pl = "pl"

data Species = Indef | Def
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Species where 
    values = enum
    prValue Indef = "indef"
    prValue Def   = "def"

data Casus = Nom | Gen
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Casus where 
    values = enum
    prValue Nom = "nom"
    prValue Gen = "gen"

type Substantive = SubstForm -> Str

data SubstForm = SF Numerus Species Casus | 
                 InitComposite |
                 MedComposite  |
                 SMS           | 
		 Deriv
                 deriving (Eq, Ord, Show, Read)

instance Param SubstForm where 
 values = [SF a b c | a <- values, b <- values, c <- values] ++ [InitComposite,MedComposite,SMS] -- Deriv removed
 prValue (SF a b c) = unwords [prValue a, prValue b, prValue c]
 prValue InitComposite = "ci"
 prValue MedComposite  = "cm"
 prValue SMS = "sms"
 prValue Deriv = "deriv"

instance Dict SubstForm where 
 category _  = "nn"
 defaultAttr _ = h_attr
 attrException _ = 
    [(InitComposite,init_attr),
     (MedComposite,medial_attr),
     (Deriv,d_attr)]

type Verb = VerbForm -> Str

data VFin = 
   Pres Modus Vox
 | Pret Modus Vox
 | Imper --- no passive
  deriving (Eq, Ord, Show, Read)

data VInf =
   Inf Vox
 | Sup Vox
 | PtPres Casus
 | PtPret AdjFormPos Casus
  deriving (Eq, Ord, Show, Read)

data VerbForm = 
   VF VFin
 | VI VInf
 | VComp
 | VSMS
  deriving (Eq, Ord, Show, Read)

instance Param VFin where
 values = map (uncurry Pres) mvs ++ map (uncurry Pret) mvs ++ [Imper] 
   where mvs = [(m,v) | m <- values, v<- values]
 prValue (Pres m v) = unwords ["pres",prValue m, prValue v]
 prValue (Pret m v) = unwords ["pret",prValue m, prValue v]
 prValue (Imper) = "imper"

instance Param VInf where
  values = map Inf values ++ map Sup values ++ map PtPres values ++ 
           [PtPret a c | a <- values, c <- values]
  prValue (Inf v) = unwords ["inf", prValue v]
  prValue (Sup v) = unwords ["sup", prValue v]
  prValue (PtPres c)     = unwords ["pres_part",prValue c]
  prValue (PtPret adj c) = unwords ["pret_part",prValue adj, prValue c]

instance Param VerbForm where
 values = map VF values ++ map VI values ++ [VComp,VSMS]
 value0 = VI (Inf Act) -- to show the infinitive as dictionary form
 prValue (VF f) = prValue f
 prValue (VI f) = prValue f
 prValue (VComp) = "c"
 prValue (VSMS)  = "sms"


instance Dict VerbForm where 
 category _ = "vb"
 dictword f = case unStr (f (VI (Inf Act))) of
  (x:_) -> x
  _ -> getDictWord f
 defaultAttr _ = h_attr
 attrException _ = [(VComp, c_attr),(VSMS,w_attr)]

data Modus = Ind | Conj
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Modus where 
 values = enum
 prValue Ind = "ind"
 prValue Conj = "konj"

data Vox = Act | SForm
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Vox where 
 values = enum
 prValue Act = "aktiv"
 prValue SForm = "s-form"


data Sex = NoMasc | Masc
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Sex     where 
    values = enum
    prValue NoMasc = "no_masc"
    prValue Masc   = "masc"

type Adjective = AdjForm -> Str

data GenNum = ASgUtr   |
              ASgNeutr |
              APl
  deriving (Eq, Ord, Show, Read,Enum,Bounded)

instance Param GenNum where
  values = enum
  prValue (ASgUtr)   = "sg" ++ " " ++ prValue Utr
  prValue (ASgNeutr) = "sg" ++ " " ++ prValue Neutr
  prValue APl        = "pl"

data SexNum = AxSg Sex | AxPl
  deriving (Eq, Ord, Show, Read)

instance Param SexNum where
  values = map AxSg values ++ [AxPl] -- needed for pronouns
  prValue (AxSg sex) = "sg" ++ " " ++ prValue sex
  prValue (AxPl)      = "pl"

data AdjFormPos =
    Strong GenNum
  | Weak   SexNum
  deriving (Eq, Ord, Show, Read)

instance Param AdjFormPos where 
  values = map Strong values ++ map Weak values
  prValue (Strong gn) = "indef " ++ prValue gn
  prValue (Weak   sn) = "def " ++ prValue sn

data AdjFormSuper = SupStrong | SupWeak Sex
  deriving (Eq, Ord, Show, Read)

instance Param AdjFormSuper where 
    values = SupStrong : [SupWeak s | s <- values]
    prValue (SupStrong) = "indef"
    prValue (SupWeak s) = unwords ["def", prValue s]

data AdjFormGrad =
    Pos    AdjFormPos
  | Comp  
  | Super  AdjFormSuper
  deriving (Eq, Ord, Show, Read)

instance Param AdjFormGrad where 
  values = map Pos values ++ [Comp] ++ map Super values
  prValue (Pos adj) = "pos " ++ prValue adj
  prValue (Comp)    = "komp"
  prValue (Super adj) = "super " ++ prValue adj

data AdjForm = AF AdjFormGrad Casus | AdjComp | AdjSMS
  deriving (Eq, Ord, Show, Read)

instance Param AdjForm where 
  values = [AF a c | a <- values, c <- values] ++ [AdjComp,AdjSMS]
  prValue (AF a c) = unwords [prValue a, prValue c]
  prValue (AdjComp) = "c"
  prValue (AdjSMS)  = "sms"

instance Dict AdjForm where 
 category _ = "av"
 defaultAttr _ = w_attr
 attrException _ = [(AdjComp,c_attr),(AdjSMS,h_attr)]

type PN = PNForm -> Str

data PNForm = PNForm Casus | PNComp | PNSMS
 deriving (Eq, Ord, Show, Read)

instance Param PNForm where 
 values = [PNForm c | c <- values] ++ [PNComp, PNSMS]
 prValue (PNForm c) = prValue c
 prValue (PNComp) = "c"
 prValue (PNSMS) = "sms"

instance Dict PNForm where 
 category _  = "pm"
 defaultAttr _ = w_attr
 attrException _ = [(PNComp, c_attr),(PNSMS,w_attr)]

data PNMForm = PNMForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PNMForm where 
 values = [PNMForm c | c <- values]
 prValue (PNMForm c) = prValue c

instance Dict PNMForm where 
 category _  = "pmm"
 defaultAttr _ = w_attr

data PNAForm = PNAForm Casus | PNAComp | PNASMS
 deriving (Eq, Ord, Show, Read)

instance Param PNAForm where 
 values = [PNAForm c | c <- values] ++ [PNAComp, PNASMS]
 prValue (PNAForm c) = prValue c
 prValue (PNAComp) = "c"
 prValue (PNASMS) = "sms"

instance Dict PNAForm where 
 category _ = "pma"
 defaultAttr _ = w_attr
 attrException _ = [(PNAComp, c_attr),(PNASMS,w_attr)]

conj_forms :: [VerbForm]
conj_forms = [VF (Pres Conj v) | v <- values] ++ [VF (Pret Conj v) | v <- values]

part_pret_forms :: [VerbForm]
part_pret_forms = [VI (PtPret a c) | a <- values, c <- values]

active_forms :: [VerbForm]
active_forms =
  map VF (concat [[Pres m Act, Pret m Act] | m <- values]) ++ 
  map VI [Inf Act, Sup Act] ++ part_pret_forms ++ conj_forms

part_pres_forms :: [VerbForm]
part_pres_forms = [VI (PtPres c) | c <- values] 

passive_forms :: [VerbForm]
passive_forms = 
  map VF (concat [[Pres m SForm, Pret m SForm] | m <- values]) ++ 
  map VI [Inf SForm, Sup SForm] -- ++ part_pret_forms ++ conj_forms


part_forms :: [VerbForm]
part_forms = part_pres_forms ++ part_pret_forms 


type Preposition = PrepForm -> Str

data PrepForm = PrepForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PrepForm where 
 values = enum
 prValue _ = "invar"

instance Dict PrepForm where 
 category _  = "pp"
 defaultAttr _ = w_attr

-- Conjunction

type Conjunction = ConjForm -> Str

data ConjForm = ConjForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ConjForm where 
 values = enum
 prValue _ = "invar"

instance Dict ConjForm where 
 category _  = "kn"
 defaultAttr _ = w_attr

-- Subjunction

type Subjunction = SubForm -> Str

data SubForm = SubForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param SubForm where 
 values = enum
 prValue _ = "invar"

instance Dict SubForm where 
 category _  = "sn"
 defaultAttr _ = w_attr

type AdverbInv = AdverbInvForm -> Str

data AdverbInvForm = AdverbInvForm | AdCompI | AdSMSI
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbInvForm where 
 values = enum
 prValue AdverbInvForm = "invar"
 prValue AdCompI = "c"
 prValue AdSMSI = "sms"

instance Dict AdverbInvForm where 
 category _ = "ab"
 defaultAttr _ = h_attr
 attrException _ = [(AdCompI,c_attr),(AdSMSI,w_attr)]

type Adverb = AdverbForm -> Str

data AdverbForm = AdverbForm Grade | AdComp | AdSMS
  deriving (Eq, Ord, Show, Read)

data Grade = Posit | Compar | Superl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Grade where 
    values = enum
    prValue Posit = "pos"
    prValue Compar = "komp"
    prValue Superl = "super"

instance Param AdverbForm where 
 values = [AdverbForm g | g <- values] ++ [AdComp,AdSMS]
 prValue (AdverbForm g) = prValue g
 prValue (AdComp) = "c"
 prValue (AdSMS) = "sms"

instance Dict AdverbForm where 
 category _ = "ab"
 defaultAttr _ = h_attr
 attrException _ = [(AdComp,c_attr),(AdSMS,w_attr)]

type Number = NumForm -> Str

data NumOrd = Ordinal Sex | Numeral | NumeralN
  deriving (Eq, Ord, Show, Read)

instance Param NumOrd where
 values = (Numeral:NumeralN:[Ordinal s | s <- values])
 prValue (Ordinal m) = unwords ["ord", prValue m]
 prValue Numeral     = unwords ["num", prValue Utr]
 prValue NumeralN    = unwords ["num", prValue Neutr]

data NumForm = NumF Casus NumOrd | NumC
 deriving (Eq, Ord, Show, Read)

instance Param NumForm where 
 values = [NumF c o | c <- values, o <- values] ++ [NumC]
 prValue (NumF c o) = unwords [prValue c, prValue o]
 prValue (NumC) = "c"

instance Dict NumForm where 
 category _  = "nl"
 defaultAttr _ = w_attr

data Compound = CMP | CSMS 
 deriving (Eq,Ord,Show,Read)

instance Param Compound where
 values = [CMP,CSMS]
 prValue CMP  = "c"
 prValue CSMS = "sms"

instance Dict Compound where 
 category     _  = "sxc"
 defaultAttr _ = c_attr
 attrException _ = [(CSMS,h_attr)]

type Interjection = InterjForm -> Str

data InterjForm = InterjForm 
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InterjForm where 
 values = enum
 prValue _ = "invar"

instance Dict InterjForm where 
 category _  = "in" 
 defaultAttr _ = w_attr

comp_forms :: [VerbForm]
comp_forms = [VComp,VSMS]


data Person = P1 | P2 | P3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Person  where 
    values = enum
    prValue P1 = "p1"
    prValue P2 = "p2"
    prValue P3 = "p3"

type PronPN  = PronCasus   -> Str
type PronAdj = AdjPronForm -> Str

data PronCasus = PNom | PAcc | PGen GenNum
  deriving (Eq, Ord, Show, Read)

instance Param PronCasus where
 values = PNom : PAcc : map PGen values
 prValue (PNom) = "nom"
 prValue (PAcc) = "ack"
 prValue (PGen gn) = unwords ["poss", prValue gn]

instance Dict PronCasus where 
 category _  = "pn"
 defaultAttr _ = w_attr

data AdjPronForm = AP GenNum Casus
 deriving (Eq, Ord, Show, Read)

instance Param AdjPronForm where
 values = [AP g c | g <- values, c <- values]
 prValue (AP gn c) = unwords [prValue gn, prValue c]

instance Dict AdjPronForm where 
 category _  = "pn"
 defaultAttr _ = w_attr

type AdjInv = AdjInvForm -> Str

data AdjInvForm = AdjInvForm | AdjInvComp
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjInvForm where 
 values = enum
 prValue AdjInvForm = "invar"
 prValue AdjInvComp = "c"

instance Dict AdjInvForm where 
 category _ = "av"
 defaultAttr _ = h_attr

data AdjMInv = AdjMInv
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjMInv where
 values = enum
 prValue (AdjMInv) = "invar"

instance Dict AdjMInv where 
 category _ = "avm"
 defaultAttr _ = w_attr


data PronMInvForm = PronMInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PronMInvForm where 
 values = enum
 prValue _ = "invar"

instance Dict PronMInvForm where 
 category _ = "pnm"
 defaultAttr _ = w_attr


type AdverbMInv = AdverbMInvForm -> Str

data AdverbMInvForm = AdverbMInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbMInvForm where 
 values = enum
 prValue _ = "invar"

instance Dict AdverbMInvForm where 
 category _  = "abm"
 defaultAttr _ = w_attr

type InfMark = InfMarkForm -> Str

data InfMarkForm = InfMarkForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InfMarkForm where 
 values = enum
 prValue _ = "invar"

instance Dict InfMarkForm where 
 category _  = "ie"
 defaultAttr _ = w_attr


type PronInv = PronInvForm -> Str

data PronInvForm = PronInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PronInvForm where 
 values = enum
 prValue _ = "invar"

instance Dict PronInvForm where 
 category _  = "pn"
 defaultAttr _ = w_attr


data PronMCForm = PronMCForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PronMCForm where 
 values = [PronMCForm c | c <- values]
 prValue (PronMCForm c) = prValue c

instance Dict PronMCForm where 
 category _ = "pnm"
 defaultAttr _ = w_attr

type Article = ArticleForm -> Str

data ArticleForm = ArticleForm GenNum Species
 deriving(Eq,Ord,Show,Read)

instance Param ArticleForm where 
   values = [ArticleForm g s | g <- values, s <- values]
   prValue (ArticleForm g s) = unwords [prValue g, prValue s]

instance Dict ArticleForm where 
 category _  = "al"
 defaultAttr _ = w_attr

positive_forms :: [AdjForm]
positive_forms = [AF (Pos a) c | a <- values, c <- values]


-- Abbreviations
data ABAForm = ABAForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ABAForm where 
 values = enum
 prValue _ = "invar"

instance Dict ABAForm where 
 category _  = "aba"
 defaultAttr _ = w_attr



{-

-- parameter types for Swedish morphology

-- enumerated parameter types

-- real parameter types: mostly hierarchical

-- substantives (= common nouns)

--data SubstFormA = SFA Casus
--  deriving (Eq, Ord, Show, Read)

--instance Param SubstFormA where
-- values = [SFA c | c <- values]
-- prValue (SFA c) = prValue c

data SubstM = SFM SubstForm
  deriving (Eq, Ord, Show, Read)

instance Param SubstM where
 values = [SFM t | t <- values]
 prValue (SFM t) = prValue t


-- Compound forms

-- adjectives

data AdjM = AdjM AdjForm
  deriving (Eq, Ord, Show, Read)

instance Param AdjM where
 values = [AdjM t | t <- values]
 prValue (AdjM t) = prValue t

-- verbs
data VerbFormM = VM VerbForm
  deriving (Eq, Ord, Show, Read)

instance Param VerbFormM where
  values = map VM values 
  value0 = VM value0 
  prValue (VM vf) = prValue vf

type VerbM = VerbFormM -> Str

-- adverbs

-- invariant adverbs


-- invariant adjectives

data AdjCompInvForm = AdjCompInvForm | AdjCompCompInvForm 
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjCompInvForm where 
			     values = enum
			     prValue AdjCompInvForm = "komp"
                             prValue AdjCompCompInvForm = "c"
-- invariant interrogative adverbs

type InterrogInv = InterrogInvForm -> Str

data InterrogInvForm = InterrogInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InterrogInvForm where 
			     values = enum
			     prValue _ = "invar"


-------------------------------
-- closed classes -------------
-------------------------------

-- pronouns


-- articles

-- auxiliary verbs

data AuxVerbForm = AuxInf | AuxPres | AuxPret | AuxSup 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AuxVerbForm where 
    values = enum
    prValue (AuxInf)  = "inf"
    prValue (AuxPres) = "pres"
    prValue (AuxPret) = "pret"
    prValue (AuxSup)  = "sup"

type AuxVerb = AuxVerbForm -> Str

-- Prepositions

-- Particles

type Particle = PartForm -> Str

data PartForm = PartForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PartForm where 
			values = enum
			prValue _ = "invar"
		   
-- Infinitive mark

-- Proper Noun

-- Dictionary instances
-- Part of Speech Declarations.
-- (Dict instances)


instance Dict SubstM  where 
 category     _  = "nnm"
 defaultAttr _ = w_attr
 --attrException _ = [(SFM Composite,1), (SFM Deriv,2)]

-- instance Dict SubstFormA      where category _ = "nna"
instance Dict AdjM            
    where category _ = "avm"
          defaultAttr _ = w_attr
instance Dict VerbFormM  
    where category _ = "vbm"
          defaultAttr _ = w_attr
instance Dict AdjCompInvForm  
    where category _ = "av"
          defaultAttr _ = h_attr
instance Dict AuxVerbForm     
    where category _  = "vb"
          defaultAttr _ = w_attr
instance Dict PartForm        
    where category _  = "pp"
          defaultAttr _ = w_attr


instance Dict InterrogInvForm 
    where category _  = "pn" 
          defaultAttr _ = w_attr

-}
