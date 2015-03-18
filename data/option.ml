open Control.Monad
open Control.Transformers

include OptionT (IdentityMonad)
