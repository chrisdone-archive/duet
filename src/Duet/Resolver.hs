-- | Resolve type-class instances.

module Duet.Resolver where

import Duet.Types

resolveBindGroup
  :: Monad m
  => BindGroup Name (TypeSignature Name l)
  -> m (BindGroup Name (TypeSignature Name l))
resolveBindGroup (BindGroup explicit implicit) =
 do explicits <- mapM (error "TODO: explicit bind groups not supported") explicit
    implicits <- mapM (mapM resolveImplicit) implicit
    pure (BindGroup explicits implicits)

resolveImplicit
  :: Monad m
  => ImplicitlyTypedBinding Name (TypeSignature Name l)
  -> m (ImplicitlyTypedBinding Name (TypeSignature Name l))
resolveImplicit (ImplicitlyTypedBinding l name alt) = undefined
