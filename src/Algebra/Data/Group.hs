{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      : Algebra.Data.Group
-- Description : A representation of "groups", an algebraic structure with one set and one associative unital invertible binary operation defined over it.
-- Copyright   : (c) greatBigDot, 2018
-- License     : BSD-3-Clause
-- Maintainer  : greatBigDot@gmail.com
-- Stability   : experimental
--
-- This module defines a mathematical "group" as a Haskell typeclass. (See
-- module "Algebra" for an overview of algebraic structures. See typeclass
-- 'Group' for discussion about implementation.) Groups are defined by a single
-- set \(G\) and an associative unital invertible binary operation \(*\) defined
-- over it. Alternatively, one can view a group as consisting of said set, an
-- associative binary operation \(*\), a nullary operation (i.e., an element of
-- the set) \(1\), and a unary operation \(\left(\textemdash\right)^{-1}\) over
-- it, such that \(1\) is an identity of \(*\) and
-- \(\left(\textemdash\right)^{-1}\) inverts \(*\). These perspectives are
-- equivalent; to prove equivalence, however, you must first show that the first
-- definition implies the uniqueness of the identity and the inversion operator.
-- (Proving this is left as an exercise to the reader.) The latter view, typical
-- in universal algebra, will be much more convenient to use in Haskell; as
-- mentioned in the package overview, the structures' axioms cannot be verified,
-- and so treating the identity and inversion operators as part of a group's
-- definition allows us to verify more of the mathematical details.
--
-- To speak more formally, a __group__ is a 4-tuple
-- \(\mathscr{G} = (G : __\mathscr{V}__,* : G^2 \to G, 1 : G, \(\left(\textemdash\right)^{-1}\) : G \to G)\)
-- satisfying the following axioms (all variables are implcitly universally
-- quantified over \(G\)):
--
-- * /Associativity:/ \(\quad\) \((x*y)*z = x*(y*z)\)
-- * /Identity:/ \(\quad\) \(x*1 = 1*x = x\)
-- * /Inverses:/ \(\quad\) \(x*(x^{-1}) = (x^{-1})*x = 1\)
--
-- Groups are useful for modelling /symmetries/. That is, you have some
-- object---say, a geometrical figure---that you can transform in various ways
-- that leave the object unchanged. For example, if you have a square, you can
-- rotate it \(90^{\circ}\), \(180^{\circ}\), \(270^{\circ}\), or \(0^{\circ}\)
-- and it looks the same as it did before. You can view these
-- "symmetries"---that is, transformations that preserve the salient features of
-- the object---as forming a group, with composition as the group operation.
-- Indeed, any two symmetric transforms applied one after another yield another
-- symmetry; applying one symmetry, and then (applying a second, then a third)
-- is exactly equivalent to (applying one symmetry, and then a second), and then
-- applying a third; there is an identity symmetry that leaves other symmetries
-- unchanged upon composition (the identity function (or if you prefer, rotation
-- by \(0^{\circ}\))); and every symmetry has a corresponding symmetry that
-- undoes it. The awesome thing is that these observations don't merely apply
-- the rotations of a square---if you have /any/ set of ways to transform /any/
-- mathematical object, such that you include a "do-nothing" transformation and
-- every transformation has a corresponding transformation done "in reverse"---
-- well, then you have a group! (Assoicativity if guaranteed because function
-- composition is of course always associative!) Interestingly, the converse of
-- this idea holds; every group is a subgroup of some symmetric group. As for
-- applications, group theory is central to modern particle physics due to the
-- various symmetric properties the universe exhibits at that scale (sorry for
-- being so vague, but it's too complicated to go into further detail here).
--
-- Groups probably have the highest product of simplicity and interesting-ness,
-- for some values of those terms. They provide a natural foudnation for
-- defining more "structured" structures, such as rings.
--
module Algebra.Data.Group ( Group((*), id, inv)
                          , (^), order) where

-- I think this works
import qualified Prelude as P ((*),id,(^))
import Prelude hiding ((*),id,(^))

-- |
-- A @Group@ is a type @G@ combined with an instance declaration for the 'Group'
-- typeclass, whose minimally complete (and in fact only possible) definition is
-- of @(((*) :: G -> G -> G), ((id) :: G), (inv :: G -> G))@ that satisfies the
-- following unchecked properties:
--
-- prop> (x * y) * z == x * (y * z)
-- prop> (x * id == x) && (id * x == x)
-- prop> (x * inv x == x) && (inv x * x == x)
--
-- Note that the same type can be a group in multiple ways. This is a bit
-- annoying in Haskell, as it necessitates the use of @type@ wrappers to
-- implement a class in different ways in the same scope.
class (Eq g) => Group g where
  (*) :: g -> g -> g
  id  :: g
  inv :: g -> g

-- |
-- Exponentiation, or repeated application of the group function "(*)". Note
-- that this is /not/ a binary operation over a 'Group'; the first argument is a
-- member of the group, but the second is just an integer, specifying how many
-- times to repeat the operation. Raising to the zeroth power yields identity;
-- raising to a negative power is the corresponding positive power of the
-- inverse.
(^) :: (Group grp) => grp -> Integer -> grp
g ^ n
  | n == 0 = id
  | n <  0 = (inv g) ^ (-n)
  | n >  0 = g * (g ^ (n-1))

-- | The 'order' of an element of a group is the smallest positive integer such that
-- raising the group element to that power (see '(^)') yields the identity,
-- 'id'---or positive infinity if no such element exists. The latter case
-- implies that this function may be (and in fact often is) non-terminating. In
-- finite groups (groups with finitely many elements), however, it is a useful
-- construct; the order of any element in a fintie group is finite.
order :: (Group grp) => grp -> Integer
order g = toInteger . length . takeWhile (/= id) . map (g^) $ [1..]

-- | For any two groups \(G\) and \(H\), there is a group \(G \times H\) defined
-- in the obvious way.
instance (Group g, Group h) => Group (g,h) where
  (g1, h1) * (g2, h2) = (g1*g2, h1*h2)
  id  = (id, id)
  inv = \(g,h) -> (inv g, inv h)

-- | The integers form a group under addition, with \(0\) as the identity. (The
-- notation is quite unfortunate, especially since the integers don't forma
-- group under multiplication!)
instance Group Integer where
  (*) = (+)
  id  = 0
  inv = negate

