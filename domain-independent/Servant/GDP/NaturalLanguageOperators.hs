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

module Servant.GDP.NaturalLanguageOperators (Named, SuchThatIt, SuchThat) where

import GDP (type (:::), type (?), type (~~))

-- | # Alias for GDP (~~)
type Named a b = (~~) a b

-- | # Alias for GDP (?)
type SuchThatIt a b = (?) a b

-- | # Alias for GDP (:::)
type SuchThat a b = (:::) a b
