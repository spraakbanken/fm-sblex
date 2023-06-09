{-
    Functional Morphology: Latin type system
    Copyright (C) 2004  Author: Markus Forsberg

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}


module TypesLat where

import General
import Invariant

{- Latin noun -}

{- Latin noun inflectional parameters -}

data Case   = Nominative | 
	      Vocative   |
	      Accusative | 
	      Genitive   |
	      Dative     | 
	      Ablative     
 deriving (Show,Eq,Enum,Ord,Bounded)

instance Param Case    where values = enum

data Number = Singular | 
	      Plural
 deriving (Show,Eq,Enum,Ord,Bounded)

instance Param Number  where values = enum

{- Latin noun inherent parameter -}

data Gender = Masculine | 
              Feminine  | 
	      Neuter
 deriving (Show,Eq,Enum,Ord,Bounded)

instance Param Gender where values = enum

data NounForm = NounForm Number Case
 deriving (Show,Eq,Ord)

instance Param NounForm where
    values = [NounForm n c | n <- values , c <- values]
    prValue (NounForm n c) = unwords $ [prValue n, prValue c]
			
type Noun = NounForm -> Str

{- Latin adjectives -}

data Grade = Positive    | 
	     Comparative | 
	     Superlative
 deriving (Show,Eq,Enum,Ord,Bounded)

instance Param Grade   where values = enum

data AdjectiveForm = AdjectiveForm Grade Gender Number Case
 deriving (Show,Eq)

instance Param AdjectiveForm where
    values = [AdjectiveForm gr g n c | 
	      gr <- values, 
	      g  <- values, 
	      n <- values, 
	      c <- values]
    prValue (AdjectiveForm gr g n c) = 
	unwords $ [prValue gr, prValue g, prValue n, prValue c]

type Adjective = AdjectiveForm -> Str

{- Adverbs -}

data AdverbForm = AdverbForm Grade
 deriving (Show,Eq)

instance Param AdverbForm where
    values  = [AdverbForm g | g <- values]
    prValue (AdverbForm g) = prValue g

type Adverb = AdverbForm -> Str

{- Particles  -}

data ParticleForm = ParticleForm Invariant
 deriving (Show,Eq)

instance Param ParticleForm where
    values     = [ParticleForm p | p <- values]
    prValue _  = "Invariant"

type Particle    = ParticleForm  -> Str

{- Preposition -}

data PrepForm = PrepForm Invariant
 deriving (Show,Eq)


instance Param PrepForm where
    values     = [PrepForm p | p <- values]
    prValue _  = "Invariant"

type Preposition = PrepForm -> Str

{- Verb -}

data Person = First  |
	      Second |
	      Third 
 deriving (Show,Eq,Enum,Ord,Bounded)

data PersonI = SecondI |
	       ThirdI 
 deriving (Show,Eq,Enum,Ord,Bounded)	  

data Tense = Present       |
	     Imperfect     | 
	     Future        |
	     Perfect       |
	     FuturePerfect |
	     PluPerfect
 deriving (Show,Eq,Enum,Ord,Bounded)	  

data TenseI = PresentI |
	      PerfectI |
	      FutureI 
 deriving (Show,Eq,Enum,Ord,Bounded)	  

data TenseS = PresentS   |
              ImperfectS |
	      PerfectS   |
	      PluPerfectS
  deriving (Show,Eq,Enum,Ord,Bounded)

data Voice = Active |
	     Passive
 deriving (Show,Eq,Enum,Ord,Bounded)	  

data VerbForm = 
    Indicative Person Number Tense Voice   |
    Infinitive TenseI Voice                |
    ParticiplesFuture  Voice               |
    ParticiplesPresent                     |
    ParticiplesPerfect                     |
    Subjunctive Person Number TenseS Voice |
    ImperativePresent Number Voice         |
    ImperativeFutureActive  Number PersonI |
    ImperativeFuturePassiveSing PersonI    |
    ImperativeFuturePassivePl              |
    GerundGenitive                         |
    GerundDative                           |
    GerundAcc                              |
    GerundAbl                              |
    SupineAcc                              |
    SupineAblative 
 deriving (Show,Eq,Ord)	  

{- Instance of Param -}

instance Param Person  where values = enum
instance Param PersonI where values = enum
instance Param Tense   where values = enum
instance Param TenseI  where values = enum
instance Param TenseS  where values = enum
instance Param Voice   where values = enum

instance Param VerbForm where
    values = 
     [Indicative p n t v | 
      v <- values,
      t <- values, 
      n <- values,
      p <- values
     ] ++
     [Infinitive t v | t <- values, v <- values] ++
     [ParticiplesFuture v | v <- values] ++ 
     [ParticiplesPresent, 
      ParticiplesPerfect] ++
     [Subjunctive p n t v | 
      v <- values,
      t <- values,
      n <- values, 
      p <- values
     ] ++
     [ImperativePresent n v | 
      n <- values, 
      v <- values] ++   
     [ImperativeFutureActive  n p | n <- values, p <- values] ++
     [ImperativeFuturePassiveSing p | p <- values] ++
     [
      ImperativeFuturePassivePl,
      GerundGenitive, GerundDative, GerundAcc,    
      GerundAbl, SupineAcc, SupineAblative
     ]


type Verb = VerbForm -> Str
