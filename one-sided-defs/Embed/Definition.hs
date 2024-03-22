module Embed.Definition (
  Embed,
  embed
)where 

class Embed a b where 
  embed :: a -> b 
