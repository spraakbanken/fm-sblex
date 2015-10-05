module CommandsSw where

import BuildSw
import Frontend
import TypesSw
import GenRulesSw
import General(tk,(+?))
import Dictionary
import Attr
import Data.Maybe(catMaybes)
import qualified Data.Map as Map

commands :: [(String, [String], [String] -> Entry)]
commands = [  

 paradigm_h "nn_0m_tobak" ["tobak"] $
   noun_compound 0 Masc ([e ""], [e "en"], [], [],[(ungeminate, ""),(ds.ungeminate,"")],[(ungeminate,""),(ds.ungeminate,"")]),

 paradigm_h "nn_0n_kaffe" ["kaffe"] $
   noun_compound 0 Neutr ([e ""], [(drop_final_e,"et")], [], [],[(ungeminate, ""),(ds.ungeminate,"")],[(ungeminate,""),(ds.ungeminate,"")]),

 paradigm_h "nn_1f_dufva" ["dufva"] $
   noun_compound 1 Fem ([e "a"], [e "an"], [e "or"], [e "orna"],[(ungeminate, ""),(ds.ungeminate,"")],[(ungeminate,""),(ds.ungeminate,"")]),

 paradigm_h "nn_2m_ulf" ["ulf"] $
  noun_compound 0 Masc ([e ""], [(dpe.fr,"en")], [(dpe.fr,"ar")], [(dpe.fr,"arne"), (dpe.fr,"arna")],[(dpeu, ""),(ds.dpeu,"")],[(dpeu,""),(ds.dpeu,"")]),
 
 paradigm_h "nn_2f_vår" ["vår"] $
  noun_compound 0 Fem ([e ""], [e "en"], [e "ar"], [e "arne", e "arna"],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_2n_lof" ["lof"] $
  noun_compound 0 Neutr ([e ""], [(dpe.fr,"et")], [(dpe.fr,"ar")], [(dpe.fr,"arna")],[(dpeu, ""),(ds.dpeu,"")],[(dpeu,""),(ds.dpeu,"")]),

 paradigm_h "nn_3f_linie" ["linie"] $ 
  noun_compound 0 Fem ([e ""], [(dpe,"en")], [(dpe,"er")], [(dpe,"erna")],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_3m_vers"  ["vers"] $ 
  noun_compound 0 Masc ([e ""], [e "en"], [e "er"], [e "erne", e "erna"],[(dpeu, ""),(ds.dpeu,"")],[(dpeu,""),(ds.dpeu,"")]),

 paradigm_h "nn_3n_vin"  ["vin"] $ 
  noun_compound 0 Neutr ([e ""], [e "et"], [e "er"], [e "erna"],[(dpeu, ""),(ds.dpeu,"")],[(dpeu,""),(ds.dpeu,"")]),

 paradigm_h "nn_4m_bakdantare"  ["bakdantare"] $ 
  noun_compound 0 Masc ([e ""], [(dpe,"en")], [(dpe,"en")], [e "ne", e "na"],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_4n_äple" ["äple"] $   
 noun_compound 0 Neutr ([e ""], [(dpe,"et")], [(dpe,"en")], [e "na"],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_5f_mil" ["mil"] $ 
  noun_compound 0 Fem ([e ""], [e "en"], [e ""], [e "arne"],[e "",(ds,"")],[e "",(ds,"")]),  

 paradigm_h "nn_5m_väfvare" ["väfvare"] $  
  noun_compound 0 Masc ([e ""], [(dpe,"en")], [e ""], [(dpe,"ne"),(dpe,"na")],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_5n_qval"  ["qval"] $ 
  noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e "",(ds,"")],[e "",(ds,"")]),

  paradigm_h "av_2_blek" ["blek"] av_1_blek,

  paradigm_h "vb_1a_kalla" ["kalla"] v1,

  paradigm_h "vb_2a_ärfva" ["ärfva"] v2,

  paradigm_h "ab_i_öfver" ["öfver"] ab_bort,

  paradigm_h "pp_i_af" ["af"] prep,

  paradigm_h "kn_i_och" ["och"] conj,
  
  paradigm_h "gf" ["GF"] prim,
  paradigm_h "nn" ["GF"] $ set_pos "nn" . prim,
  paradigm_h "vb" ["GF"] $ set_pos "vb" . prim,
  paradigm_h "av" ["GF"] $ set_pos "av" . prim,
  paradigm_h "ab" ["GF"] $ set_pos "ab" . prim,
  paradigm_h "in" ["GF"] $ set_pos "in" . prim,
  paradigm_h "kn" ["GF"] $ set_pos "kn" . prim,
  paradigm_h "nl" ["GF"] $ set_pos "nl" . prim,
  paradigm_h "pn" ["GF"] $ set_pos "pn" . prim,
  paradigm_h "pp" ["GF"] $ set_pos "pp" . prim,
  paradigm_h "abm" ["GF"] $ set_pos "abm" . prim
 ]
