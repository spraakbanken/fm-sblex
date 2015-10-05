{-
    Functional Morphology: Latin command definitions
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

module CommandsLat where

import BuildLat
import Frontend

commands = 
 [
  paradigm_h "p_fun" ["x","y"]  $ p_fun,
  paradigm_h "d1rosa"   ["rosa"]   $ d1rosa,
  paradigm_h "d1poeta"  ["poeta"]  $ d1poeta,
  paradigm_h "d2servus" ["servus"] $ d2servus,
  paradigm_h "d2pinus"  ["pinus"]  $ d2pinus,
  paradigm_h "d2virus"  ["virus"]  $ d2virus,
  paradigm_h "d2bellum" ["bellum"] $ d2bellum,
  paradigm_h "d2puer"   ["puer"]   $ d2puer,
  paradigm_h "d2liber"  ["liber"]  $ d2liber,
  paradigm_h "prep"     ["ad"]     $ prep,
  paradigm_h "v1amare"  ["amare"]  $ v1amare,
  paradigm_h "v2habere" ["habere"] $ v2habere
  ]
