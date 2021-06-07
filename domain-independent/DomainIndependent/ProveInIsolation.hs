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

module DomainIndependent.ProveInIsolation (ProvableInIsolation, proveInIsolation) where

import GDP (Proof, (...), type (:::), type (~~))
import Data.Text (Text)
import Servant
  ( FromHttpApiData,
    parseUrlPiece,
  )

class ProvableInIsolation a p where
  proveInIsolation :: a -> Either Text (Proof p)

instance (FromHttpApiData (a ~~ n), FromHttpApiData a, ProvableInIsolation (a ~~ n) p) => FromHttpApiData (a ~~ n ::: p) where
  parseUrlPiece t =
    do
      (j :: a ~~ n) <- parseUrlPiece t
      (j ...) <$> proveInIsolation j
