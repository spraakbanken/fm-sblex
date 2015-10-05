module TypesSw where

import General
import Dictionary
import Attr

type Substantive = SubstForm -> Str

data SubstForm = SF Numerus Species Casus | 
                 InitComposite |
                 MedComposite  |
                 SMS           
                 deriving (Eq, Ord, Show, Read)

instance Param SubstForm 
  where values = [SF a b c | a <- values, b <- values, c <- values] ++ [InitComposite,MedComposite,SMS]
	prValue (SF a b c) = unwords [prValue a, prValue b, prValue c]
        prValue InitComposite = "ci"
        prValue MedComposite  = "cm"
        prValue SMS           = "sms"

instance Dict SubstForm       where 
 category     _  = "nn"
 defaultAttr _ = h_attr
 attrException _ = [(InitComposite,init_attr),
                    (MedComposite,medial_attr)]

data Genus = Fem | Masc | Neutr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Genus where 
    values = enum
    prValue g  = case g of
                     Fem     -> "f"
                     Masc    -> "m"
                     Neutr   -> "n"

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

instance Param Casus   where 
    values = enum
    prValue Nom = "nom"
    prValue Gen = "gen"



type Adjective = AdjForm -> Str

data GenNum = ASgUtr   |
              ASgNeutr |
              APl
  deriving (Eq, Ord, Show, Read,Enum,Bounded)

instance Param GenNum where
  values = enum
  prValue (ASgUtr)   = "sg" ++ " " ++ prValue Fem
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
  values = [AF a c | a <- values, c <- values]
  prValue (AF a c) = unwords [prValue a, prValue c]
  prValue (AdjComp) = "c"
  prValue (AdjSMS)  = "sms"

instance Dict AdjForm         
    where category _ = "av"
          defaultAttr _ = w_attr
          attrException _ = [(AdjComp,c_attr),(AdjSMS,h_attr)]

data Sex = NoMasc | Sex_Masc
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Sex     where 
    values = enum
    prValue NoMasc   = "no_masc"
    prValue Sex_Masc = "masc"

type Verb = VerbForm -> Str

data VFin = 
   Pres Vox
 | PresPl Person Vox
 | Pret Vox
 | PretPl Person Vox
 | PresKonj Vox
 | PresKonjPl Person Vox
 | PretKonj Vox
 | Imper Person2 Vox
 | ImperPl Person Vox
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
  values = [Pres v | v <- values] ++
           [PresPl p v | p <- values, v <- values] ++
           [Pret v | v <- values] ++
           [PretPl p v | p <- values, v <- values] ++
           [PresKonj v | v <- values] ++
           [PresKonjPl p v | p <- values, v <- values] ++
           [PretKonj v | v <- values] ++
           [Imper p2 v | p2 <- values, v <- values] ++
           [ImperPl p v | p <- values, v <- values]
  prValue (Pres v)         = unwords ["pres", "sg","ind", prValue v]
  prValue (PresPl p v)     = unwords ["pres","pl","ind",prValue p, prValue v]
  prValue (Pret v)         = unwords ["pret", "sg","ind",prValue v]
  prValue (PretPl p v)     = unwords ["pret","pl","ind",prValue p, prValue v]
  prValue (Imper p2 v)       = unwords ["imper","sg",prValue p2,prValue v]
  prValue (ImperPl p v)      = unwords ["imper","pl",prValue p, prValue v]
  prValue (PresKonj v)     = unwords ["pres","sg","konj",prValue v]
  prValue (PresKonjPl p v) = unwords ["pres","pl", "konj",prValue p, prValue v]
  prValue (PretKonj v)     = unwords ["pret","konj",prValue v]

instance Param VInf where
  values = map Inf values ++ map Sup values ++ map PtPres values ++ 
           [PtPret a c | a <- values, c <- values]
  prValue (Inf v) = unwords ["inf", prValue v]
  prValue (Sup v) = unwords ["sup", prValue v]
  prValue (PtPres c)     = unwords ["pres_part",prValue c]
  prValue (PtPret adj c) = unwords ["pret_part",prValue adj, prValue c]

instance Param VerbForm where
  values = map VF values ++ map VI values -- ++ [VComp,VSMS]
  value0 = VI (Inf Act) -- to show the infinitive as dictionary form
  prValue (VF f) = prValue f
  prValue (VI f) = prValue f
  prValue (VComp) = "c"
  prValue (VSMS)  = "sms"

instance Dict VerbForm        where 
    category _ = "vb"
    dictword f = case unStr (f (VI (Inf Act))) of
                   (x:_) -> x
                   _     -> getDictWord f
    defaultAttr _ = h_attr
    attrException _ = [(VComp, c_attr),(VSMS,w_attr)]

data Vox = Act | SForm
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Vox     where 
    values = enum
    prValue Act   = "aktiv"
    prValue SForm = "s-form"

data Person = P1 | P2 | P3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Person  where 
    values = enum
    prValue P1 = "p1"
    prValue P2 = "p2"
    prValue P3 = "p3"


data Person2 = P22 | P23
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Person2  where 
    values = enum
    prValue P22 = "p2"
    prValue P23 = "p3"


type Conjunction = ConjForm -> Str

data ConjForm = ConjForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ConjForm where 
  values = enum
  prValue _ = "invar"

instance Dict ConjForm        
    where category _  = "kn"
          defaultAttr _ = w_attr

data Prim = Prim
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Prim where
    values              = enum
    prValue (Prim) = "-"

instance Dict Prim         
    where category _ = "-"
          defaultAttr _ = w_attr

type Preposition = PrepForm -> Str

data PrepForm = PrepForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PrepForm where 
  values = enum
  prValue _ = "invar"

instance Dict PrepForm        
    where category _  = "pp"
          defaultAttr _ = w_attr


type AdverbInv = AdverbInvForm -> Str

data AdverbInvForm = AdverbInvForm | AdCompI | AdSMSI
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbInvForm where 
  values = [AdverbInvForm]
  prValue AdverbInvForm = "invar"
  prValue AdCompI = "c"
  prValue AdSMSI = "sms"

instance Dict AdverbInvForm   
    where category _ = "ab"
          defaultAttr _ = h_attr
          attrException _ = [(AdCompI,c_attr),(AdSMSI,w_attr)]
