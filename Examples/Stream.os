module Stream

data Stream(a:-):-{
  Head(a),
  Tail(Stream(a))
}
