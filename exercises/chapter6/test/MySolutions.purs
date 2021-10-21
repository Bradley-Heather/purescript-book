module Test.MySolutions where

import Prelude

import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)

newtype Point = Point 
   { x :: Number 
   , y :: Number 
   }

instance showPoint :: Show Point where 
    show (Point {x , y}) = "(" <> show x <> ", " <> show y <> ")"

--------------------------
newtype Complex = Complex 
    { real :: Number 
    , imaginary :: Number 
    } 

instance showComplex :: Show Complex where 
    show (Complex {real , imaginary}) = show real <> (if imaginary >= 0.0 then "+" else "") <> show imaginary <> "i"
    
derive instance eqComplex :: Eq Complex

instance semiringComplex :: Semiring Complex where 
  zero = Complex {real: 0.0, imaginary: 0.0}
  add (Complex c1) (Complex c2) = Complex $ c1 + c2
  mul
    (Complex { real: r1, imaginary: i1 })
    (Complex { real: r2, imaginary: i2 })
       = Complex
          { real:      r1 * r2 - i1 * i2
          , imaginary: r1 * i2 + r2 * i1
          }
  one  = Complex {real: 1.0, imaginary: 0.0}

derive newtype instance ringComplex :: Ring Complex

----------------------------
data Shape 
   = Circle Point Number 
   | Rectangle Point Number Number
   | Line Point Point 
   | Text Point String 

derive instance genericShape :: Generic Shape _ 

instance showShape :: Show Shape where 
   show = genericShow

----------------------------
data NonEmpty a = NonEmpty a (Array a) 

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where 
     eq (NonEmpty a as) (NonEmpty a' as') = a == a' && as == as' 

instance showNonEmpty :: Show a => Show (NonEmpty a) where 
     show (NonEmpty a as) = show a <> " " <> show as

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where 
     append (NonEmpty a as) (NonEmpty a' as') = NonEmpty a (as <> [a'] <> as')

instance functorNonEmpty :: Functor NonEmpty where 
     map f (NonEmpty a as) = NonEmpty (f a) (map f as)

