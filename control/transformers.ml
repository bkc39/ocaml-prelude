open Data.Either

open Pointed
open Monad

module type MONAD_TRANSFORMER =
  functor (M : MONAD_DEF_) -> sig
    include Monad.S
    val lift : 'a M.t -> 'a t
  end

module OptionT =
  functor (M : MONAD_DEF_) -> struct
      module Def = struct
          type 'a t = 'a option M.t

          let return x = M.return (Some x)

          let bind m f = M.bind m (function
                                    | Some x -> f x
                                    | None   -> M.return None)
        end
      include MakeMonad_ (Def)
      let lift x = M.bind x return
    end

module ErrorT (T : sig type t end) =
  functor (M : MONAD_DEF_) -> struct
      module Def = struct
          type 'a t = (T.t, 'a) either M.t

          let return x = M.return (Right x)

          let bind m f = M.bind m (function
                                    | Left _ as e -> M.return e
                                    | Right v     -> f v)
        end

      include MakeMonad_ (Def)
      let lift x = M.bind x return
    end
