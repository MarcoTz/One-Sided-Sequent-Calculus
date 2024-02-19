module Examples where 

import Typed.Program
import Untyped.Syntax

import List
import Pair
import Stream
import LPair
import Bool



tys :: [Decl]
tys = [boolDecl, listDecl, pairDecl, streamDecl,lpairDecl]
terms :: [Term]
terms = [exList,exCase]
cmds :: [Command]
cmds = [exCmd]
