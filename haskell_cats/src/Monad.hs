module Monad where

-------------------------------------------------------------------------------
-- Monoid as catagory means elements as morphisms
-------------------------------------------------------------------------------
-- Usually if you think about monoid you think about something like (C, *, e),
-- a set with associative binary operations and an identity. * has type like
-- *: C x C -> C.

-- How does this form a category?
--
-- If you think about category of set, objects and morphisms are very clear,
-- a set is an object, and a function from set to set a morphism in the
-- category.
--
-- But in monoid we are working with one set, how do we view it as a category?


-- The trick is to think a monoid as a category with only one object and bunch
-- of morphisms, each morphism corresponds to an element that maps from the
-- monoid into itself.

-- e.g (Z5, +) is a monoid, can treat each element x as (x+), which
-- add x to all element in the set. Each elements corresponds to one of this
-- morphism.

-- If we think about the homset Hom((Z5, +)), it's a set with cardinarlity 5,
-- and there is a unique one to one mapping from Hom((Z5, +)) to (Z5, +).


-------------------------------------------------------------------------------
-- Another way to define monoid
-------------------------------------------------------------------------------

