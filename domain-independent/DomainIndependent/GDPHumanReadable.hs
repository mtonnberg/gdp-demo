{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module DomainIndependent.GDPHumanReadable
  ( Named,
    SuchThat,
    SuchThatIt,
    extractProof,
    withProof,
    withoutProof,
  )
where

import GDP (Proof, conjure, exorcise, (...), type (:::), type (?), type (~~))

-- | ### alias for GDP (~~) 
type Named a n = a ~~ n

-- | ### alias for GDP (:::) 
type SuchThat a p = a ::: p

-- | ### alias for GDP (?) 
type SuchThatIt a p = a ? p

-- | ### alias for GDP conjure 
extractProof :: (a ::: p) -> Proof p
extractProof = conjure

-- | ### alias for GDP (...) 
withProof :: a -> Proof p -> a ::: p
withProof = (...)

-- | ### alias for GDP exorcise 
withoutProof :: (a ::: p) -> a
withoutProof = exorcise
