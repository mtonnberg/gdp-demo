{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DomainIndependent.ProveInIsolation (ProvableInIsolation, proveFromHttpApiData) where

import GDP (Proof, (...), type (:::), type (~~))
import Servant
  ( FromHttpApiData,
    parseUrlPiece,
  )

class ProvableInIsolation a p where
  proveFromHttpApiData :: a -> Proof p

instance (FromHttpApiData (a ~~ n), FromHttpApiData a, ProvableInIsolation (a ~~ n) p) => FromHttpApiData (a ~~ n ::: p) where
  parseUrlPiece t =
    do
      (j :: a ~~ n) <- parseUrlPiece t
      let p = proveFromHttpApiData j
      return (j ... p)
