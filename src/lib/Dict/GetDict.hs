-- automatically generated by BNF Converter
module Dict.GetDict where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Dict.Lex
import Dict.Par
import Dict.Skel
import Dict.Print
import Dict.Abs
import Dict.ErrM

getDict :: String -> Dict.ErrM.Err Dictionary
getDict s = let ts = myLexer s in pDictionary ts 

getEntry :: String -> Dict.ErrM.Err Entry
getEntry s = let ts = myLexer s in 
              case pListEntry ts of
                Ok [x]   -> Ok x
                Ok  []   -> Bad []  
                Ok _     -> error "internal error in Dict.GetDict: multiple entries. this is a bug."
                Bad s    -> Bad s
