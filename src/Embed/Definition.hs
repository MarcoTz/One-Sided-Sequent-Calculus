module Embed.Definition where 


class Embed a b where 
  embed :: a -> b 
