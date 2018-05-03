{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Algebra.Data.AbelianGroup
-- Description : A representation of abelian (commutative) groups, an algebraic structure with one set and one associative abelian unital invertible binary operation defined over it.
-- Copyright   : (c) greatBigDot, 2018
-- License     : BSD-3-Clause
-- Maintainer  : greatBigDot@gmail.com
-- Stability   : experimental
--
-- This module defines the algebraic structure known as an "abelian group" as a
-- Haskell typeclass. Abelian groups are simply groups (see 'Group' in
-- "Algebraic.Data.Group") with the extra condition that the binary operation
-- must be abelian (that is, commutative).
--
-- Formally, an abelian group is a 4-tuple
-- \(\mathscr{G} = (G : \mathscr{V}, + : G^2 \to G, 0 : G, - : G \to G)\)
-- satisfying the following axioms:
--
-- * \((G, +, 0, -)\) forms a group
-- * \(x + y = y + x\)
--
-- You may have noticed that the notation here differs from the one presented
-- for groups---here, the plus sign is used as the operation (and zero as the
-- identity), while for more general groups, the multiplication symbol was used
-- (and one as the identity). This is intentional. The class of abelian groups
-- has rather different behavior, arising in different situations, than groups.
-- At first this fact may be a surprise, but the commutativity axiom is actually
-- quite strong, especially in the context of groups. Remember, a natural way to
-- think of groups is that they model symmetries---that is, invertible
-- composable mappings from an object to itself. In general, for /functions/ to
-- be commutative with respect to composition is a very special property;
-- reversing the order of two transformations will typically yield different
-- results. E.g., rotating a polygon, then flipping it over; or squaring a
-- number, then adding 5. Flipping the order in which these are applied
-- "typically" changes the outcome. In abelian groups, however, /every/ pair of
-- transformations must yield the same outcome as applying them in other
-- directions. This suggests that viewing them as "symmetries" might not be the
-- best way to think of abelian groups. You can see more discussion of this idea
-- over at MathOverflow, a Q&A site for professional mathematicians:
-- <https://mathoverflow.net/questions/2551/>
--
-- Abelian groups are important in algebraic geometry. Algebraic geometry is,
-- roughly speaking, the study of systems of multivariate polynomial equations.
-- An important idea is that of an "algebraic variety", which is essentially the
-- intersection of the roots of a set of multivariate polynomials. For example,
-- a circle with radius \(r\) and center \((h,k)\) is an algebraic variety,
-- being the zero of the 2-variable polynomial \((x-h)^2+(y-k)^2-r^2\). In this
-- framework, one can formulate and pursue various interesting ideas---e.g., a
-- generalization of the fundamental theorem of algebra to higher dimensions, a
-- result known as B&#x00E9;zout's Theorem. Anyways, as it turns out, a certain
-- subset of algebraic varietites, known as "abelian varieties", form an abelian
-- group. That is, the points comprising the variety are the group elements, and
-- using the tools of algebraic geometry one can define a natural group
-- operation obeying the group laws that is always commutative. As it turns out,
-- this idea is central to modern cryptography.
--
-- Another branch of mathematics where abelian groups shine is category theory.
-- Categories, among many other things, abstracts abstract algebra. For example,
-- there is a category of groups, which consists of all groups and all the
-- homomorphisms between them (that is, functions preserving the group
-- operations). Category theory is a very useful and interesting branch of math;
-- it simultaneously generalizes many disprate ideas and introduces many new
-- interesting questions of its own. Also, in a very pleasing and fun way, it
-- abstracts itself---there is a category of categories. (Though, that last idea
-- must be treated with utmost care to avoid set-theoretic Russel's
-- paradox--esque issues; to bypass most of them, you can just consider the
-- category of "small" categories.) The category of abelian groups exhibits
-- various special properties that are useful in the study of categories in
-- general. As such, a subset of categories known as "abelian categories" have
-- been defined, with the category of abelian groups being the motivating
-- example.
--
module Algebra.Data.AbelianGroup ( AbelianGroup((+), zero, neg) ) where

import qualified Prelude as P
import Prelude hiding ( Num((+), (*), (-))
                      , Fractional((/))
                      , (^), id )
import Algebra.Data.Group ( Group((*), id, inv) )

-- | An 'AbelianGroup' is a type that is an instance of 'Group' that also
-- satisfies the following unchecked property:
--
-- prop> x + y == y + x
--
-- As the signature of abelian group is the same as that of a group, and since
-- the axioms are currently unchecked, the minimally complete definition is
-- the empty set.
class Group g => AbelianGroup g where
  (+)  :: g -> g -> g
  zero :: g
  neg  :: g -> g

  (+)  = (*)
  zero = id
  neg  = inv
