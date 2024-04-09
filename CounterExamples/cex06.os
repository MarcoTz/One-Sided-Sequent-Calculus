module cex06

data TwoCons{
  MkOne,
  MkTwo
}

-- case does not include all xtors
badPat :: TwoCons;
badPat := case {MkOne => Done};
