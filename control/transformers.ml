open Data.Either
open Data.Function
open Data.Monoid

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

module ReaderT (T : sig type t end) =
  functor (M : MONAD_DEF_) -> struct
      module Def = struct
          type 'a t = T.t -> 'a M.t
          let return x t = M.return x
          let bind m f t = M.bind (m t) (fun x -> f x t)
        end

      include MakeMonad_ (Def)
      let lift = const
    end

module WriterT (Mon : MONOID_DEF) =
  functor (M : MONAD_DEF_) -> struct
      module Def = struct
          type 'a t = (Mon.t * 'a) M.t
          let return x = M.return (Mon.mid, x)
          let bind m f =
            M.bind m (fun (x, a) ->
                      M.bind (f a) (fun (y, b) -> M.return (Mon.mop x y, b)))
        end
      let lift m = M.bind m (fun x -> M.return (Mon.mid, x))
    end
