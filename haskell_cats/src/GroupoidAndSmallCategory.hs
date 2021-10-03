module GroupoidAndSmallCategory where
{-
   Maga cube

            Monoid             Group
              +----------------+
             / |             / |
            /  |            /  |
 Semigroup +--+------------+ inverse semigroup
           |   |           |   |
           |   +-----------|---+ Inverse Semigroup
           |  / Unital     |  /
           | /  Magma      | /           assoc  identity
           +---------------+                | /
        Magma              Quasigroup       + -- inverse

    Some combinations are not that useful.
-}


-------------------------------------------------------------------------------
-- groupoid is partial group
-------------------------------------------------------------------------------
{-
   | A group needs to have:
        totality, associativity, identity, invertibility.

   | A groupoid needs to have:
                  associativity, identity, invertibility.

   - Category of Groupoid, `Grpd` forms category that all morphism are
    invertable.
-}

class Groupoid a where
  gappend :: a -> a -> a

class Monoid a => Group a where
  ginv :: a -> a


-------------------------------------------------------------------------------
-- small category is partial monoid
-------------------------------------------------------------------------------
{-
   pullback:

     - pullback is a concept in category `Set`

   internal category:

   small set:
     a category is small if it has a small set of objects and a small set of
     morphisms.
     So a small category is an internal category of `Set`

   | A small category needs to have:
               associativity, identity

   | A monoid needs to have:
     totality, associativity, identity.

-}
