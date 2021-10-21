module Test.MySolutions where

import Prelude
import Undefined

import Data.Generic.Rep  (class Generic)
import Data.Foldable 
import Data.Show.Generic (genericShow)
import Data.Array        (nub, nubEq)

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

-----------------------------
data Extended a = Infinite | Finite a 

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where 
    compare Infinite Infinite      = EQ
    compare Infinite (Finite _)    = GT 
    compare (Finite _) Infinite    = LT 
    compare (Finite a) (Finite a') = compare a a'

------------------------------
instance foldableNonEmpty :: Foldable NonEmpty where
    foldr f n (NonEmpty a as) = foldr f n ([a] <> as)
    foldl f n (NonEmpty a as) = foldl f n ([a] <> as) 
    foldMap f (NonEmpty a as) = foldMap f ([a] <> as)

------------------------------
data OneMore f a = OneMore a (f a) 

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where 
    foldr f n (OneMore a fa) = f a (foldr f n fa)
    foldl f n (OneMore a fa) = foldl f (f n a) fa 
    foldMap f (OneMore a fa) = f a <> foldMap f fa   

-------------------------------

instance eqShape :: Eq Shape where 
   eq (Circle p r) (Circle p' r')            = p == p' && r == r'
   eq (Rectangle p l b) (Rectangle p' l' b') = p == p' && l == l' && b == b' 
   eq (Line p1 p2) (Line p1' p2')            = p1 == p1' && p2 == p2' 
   eq (Text p t) (Text p' t')                = p == p' && t == t'
   eq _ _                                    = false 

instance eqPoint :: Eq Point where
   eq (Point {x : a, y : b}) (Point {x : a', y : b'}) = a == a' && b == b'

derive instance ordPoint :: Ord Point 

derive instance ordShape :: Ord Shape 

dedupShapes :: Array Shape -> Array Shape 
dedupShapes = nubEq 

dedupShapesFast :: Array Shape -> Array Shape 
dedupShapesFast = nub 