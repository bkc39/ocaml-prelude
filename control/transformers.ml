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

module OptionT : MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = 'a option M.t

          let return x = M.return (Some x)

          let bind m f = M.bind m (function
                                    | Some x -> f x
                                    | None   -> M.return None)
        end
      type 'a t = 'a Def.t
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

      type 'a t = 'a Def.t
      include MakeMonadDefault (Def)
      let lift x = M.bind x return

      open MonadInfix
      let throw_error e = return (Left e)
      let catch_error m handler = m >>= function
                                  | Left e       -> handler e
                                  | Right _ as e -> return e
    end

module ReaderT (T : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = T.t -> 'a M.t
          let return x t = M.return x
          let bind m f t = M.bind (m t) (fun x -> f x t)
        end

      type 'a t = 'a Def.t
      include MakeMonadDefault (Def)
      let lift = const

      open FunctorInfix
      let with_reader f m t = m (f t)
      let ask t             = M.return t
      let local f m         = with_reader f m
      let asks f            = f <$> ask
    end

module WriterT (Mon : MONOID_DEF) : MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = (Mon.t * 'a) M.t
          let return x = M.return (Mon.mid, x)
          let bind m f =
            M.bind m (fun (x, a) ->
                      M.bind (f a) (fun (y, b) -> M.return (Mon.mop x y, b)))
        end
      type 'a t = 'a Def.t
      include MakeMonadDefault (Def)
      let lift m = M.bind m (fun x -> M.return (Mon.mid, x))

      open FunctorInfix
      let exec_writer m = snd <$> m
      let tell w        = M.return (w, ())
      let listen m      = (fun (a,w) -> ((a,w), w))    <$> m
      let listens f m   = (fun (a,w) -> ((a, f w), w)) <$> m
      let pass m        = (fun ((a,f), w) -> (a, f w)) <$> m
      let censor f m    = (fun (a, w) -> (a, f w))     <$> m
    end

module StateT (T : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = T.t -> ('a * T.t) M.t
          let return x s = M.return (x, s)
          let bind m f s = M.bind (m s) (uncurry f)
        end
      type 'a t = 'a Def.t
      include MakeMonadDefault (Def)
      let lift m s = M.bind m (fun x -> M.return (x, s))

      open FunctorInfix
      open ApplicativeInfix
      let eval_state m s = fst <$> m s
      let exec_state m s = snd <$> m s
      let get t          = M.return (t,t)
      let put s          = M.return ((), s)
      let modify f t     = M.return ((), f t)
      let gets f         = f <$> get
      let with_state f m = modify f *> m
    end

module ContT (T : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD_DEFAULT) -> struct
      module Def = struct
          type 'a t = ('a -> T.t M.t) -> (T.t M.t)
          let return x k = k x
          let bind m f k = m (fun v -> f v k)
        end
      type 'a t = 'a Def.t
      include MakeMonadDefault (Def)
      let lift = M.bind

      let eval_cont m  = m M.return
      let map_cont f m k = f (m k)
      let with_cont f m k = m (f k)
      let call_cc f k = f (fun x _ -> k x) k
      let reset m = lift (eval_cont m)
      let shift f m = eval_cont (f m)

      open MonadInfix
      let lift_local ask local f m k =
        ask >>= fun v -> local f (m (fun x -> local (const v) (k x)))
    end
