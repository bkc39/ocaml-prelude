open Data.Function
open Functor
open Pointed
open Applicative

module type MONAD_DEF = sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

module type MONAD_DEF_JOIN = sig
    type 'a t
    val join : 'a t t -> 'a t
  end

module type S = sig
    include Applicative.S
    include MONAD_DEF      with type 'a t := 'a t
    include MONAD_DEF_JOIN with type 'a t := 'a t

    module MonadInfix :
    sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( =<| ) : ('a -> 'b t) -> 'a t -> 'b t
      val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
      val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
    end

    val forever  : 'a t -> 'b t
    val whenever : bool -> unit t -> unit t
    val unless   : bool -> unit t -> unit t
  end

module MakeMonad
         (P : POINTED_DEF)
         (M : MONAD_DEF with type 'a t := 'a P.t)
       : (S with type 'a t := 'a P.t) = struct

    include M

    module MonadInfix = struct
        let ( >>= )       = bind
        let ( =<| ) f x   = flip bind f x
        let ( >=> ) f g x = f x >>= g
        let ( <=< ) f g x = g x >>= f
      end

    open MonadInfix

    module FunctorInstance = struct
        type 'a t = 'a P.t
        let map f x = x >>= (fun v -> P.return (f v))
      end

    module ApplicativeInstance = struct
        type 'a t = 'a P.t
        let ap f x = f >>= (fun f' -> bind x (fun x' -> P.return (f' x')))
      end

    include MakeApplicative (FunctorInstance) (P) (ApplicativeInstance)

    let join m        = m >>= (fun x -> x)
    let rec forever m = m >>= (fun _ -> forever m)
    let whenever b m  = if b then m >>= return else return ()
    let unless b m    = whenever (not b) m
  end

module MakeMonadJoin
         (P : POINTED_DEF)
         (F : FUNCTOR_DEF    with type 'a t := 'a P.t)
         (M : MONAD_DEF_JOIN with type 'a t := 'a P.t)
       : (S with type 'a t := 'a P.t) = struct

    module Bind = struct
        type 'a t = 'a P.t
        let bind m f = M.join (F.map f m)
      end

    include MakeMonad (P) (Bind)
  end

module type MONAD_DEF_ = sig
    include POINTED_DEF
    include MONAD_DEF with type 'a t := 'a t
  end


module MakeMonad_ (M : MONAD_DEF_)
       : (S with type 'a t := 'a M.t) = MakeMonad (M) (M)

module MakeMonadJoin_
         (M : sig include FUNCTOR_DEF
                  include POINTED_DEF    with type 'a t := 'a t
                  include MONAD_DEF_JOIN with type 'a t := 'a t
              end)
       : (S with type 'a t := 'a M.t) = MakeMonadJoin (M) (M) (M)
