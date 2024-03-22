module cex06

data TwoCons : + {
  MkOne,
  MkTwo
}

badPat :: TwoCons :-;
badPat := case {MkOne => Done};
