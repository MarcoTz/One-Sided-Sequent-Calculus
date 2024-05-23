module Maybe

import Fun 

data Maybe(a:+) { 
  Nothing,
  Just(a)
  }

maybe :: forall a b. b -> (a -> b) -> Maybe(a) -> b
maybe := \b. \f. \a. mu c. < a | CBV | case { Nothing => <b|CBV|c>, Just(a) => <f [a]|CBV|c> }>
