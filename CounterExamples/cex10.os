module cex10

--cannot use forall in data declaration
data X:+{
  MkX(Forall X.X)
}
