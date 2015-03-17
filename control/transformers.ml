open Data.Either
open Data.Function
open Data.Monoid

open Pointed
open Monad

module type MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> sig
    include Monad.S
    val lift : 'a M.t -> 'a t
  end

module OptionT =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = 'a option M.t

          let return x = M.return (Some x)

          let bind m f = M.bind m (function
                                    | Some x -> f x
                                    | None   -> M.return None)
        end
      include MakeMonadDefault (Def)
      let lift x = M.bind x return

      open FunctorInfix
      let maybe x f m     = (function None -> x | Some v -> f v)      <$> m
      let is_some m       = (function None -> false | Some _ -> true) <$> m
      let is_none m       = (function None -> true | Some _ -> false) <$> m
      let from_option x m = maybe x (fun x -> x) m
    end

module ErrorT (T : sig type t end) =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = (T.t, 'a) either M.t

          let return x = M.return (Right x)

          let bind m f = M.bind m (function
                                    | Left _ as e -> M.return e
                                    | Right v     -> f v)
        end

      include MakeMonadDefault (Def)
      let lift x = M.bind x return
    end

module ReaderT (T : sig type t end) =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = T.t -> 'a M.t
          let return x t = M.return x
          let bind m f t = M.bind (m t) (fun x -> f x t)
        end

      include MakeMonadDefault (Def)
      let lift = const
    end

module WriterT (Mon : MONOID_DEF) =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = (Mon.t * 'a) M.t
          let return x = M.return (Mon.mid, x)
          let bind m f =
            M.bind m (fun (x, a) ->
                      M.bind (f a) (fun (y, b) -> M.return (Mon.mop x y, b)))
        end
      include MakeMonadDefault (Def)
      let lift m = M.bind m (fun x -> M.return (Mon.mid, x))
    end

module StateT (T : sig type t end) =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = T.t -> ('a * T.t) M.t
          let return x s = M.return (x, s)
          let bind m f s = M.bind (m s) (uncurry f)
        end
      include MakeMonadDefault (Def)
      let lift m s = M.bind m (fun x -> M.return (x, s))
    end

module ContT (T : sig type t end) =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = ('a -> T.t M.t) -> (T.t M.t)
          let return x k = k x
          let bind m f k = m (fun v -> f v k)
        end
      include MakeMonadDefault (Def)
      let lift = bind
    end
