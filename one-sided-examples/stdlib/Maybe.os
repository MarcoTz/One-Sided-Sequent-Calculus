module Maybe

import Fun;
import Bool;

data Maybe(a:+) { 
  Nothing,
  Just(a)
  }

maybe :: forall a b. b -> (a -> b) -> Maybe(a) -> b;
maybe := \b. \f. \a. mu c. < a | CBV | case { Nothing => <b|CBV|c>, Just(a) => <f a|CBV|c> }>;

isJust :: forall a. Maybe(a) -> Bool;
isJust := \a. mu c. < a | CBV | case { Nothing => <False|CBV|c>, Just(a) => <True|CBV|c> }>;

isNothing :: forall a. Maybe(a) -> Bool;
isNothing := \a. mu c. <a | CBV | case { Nothing => <True|CBV|c>, Just(a) => <False|CBV|c>}>;

fromJust :: forall a. Maybe(a) -> a;
fromJust := \a. mu c. <a|CBV|case {Nothing => error "expected Just", Just(x) => <x|CBV|c>}>;

fromMaybe :: forall a. a -> Maybe(a) -> a;
fromMaybe := \a. \ma. mu c. <ma|CBV|case {Nothing => <a|CBV|c>, Just(a1)=><a1|CBV|c>}>;
