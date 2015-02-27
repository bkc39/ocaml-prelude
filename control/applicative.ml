open Data.Function
open Functor
open Pointed

module type APPLICATIVE_DEF = sig
    type 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end

module type S = sig
    include Pointed.S
    include APPLICATIVE_DEF with type 'a t := 'a t

    val lift2 : ('a -> 'b -> 'c) ->
                'a t -> 'b t -> 'c t

    val lift3 : ('a -> 'b -> 'c -> 'd) ->
                'a t -> 'b t -> 'c t -> 'd t
    val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) ->
                'a t -> 'b t -> 'c t -> 'd t -> 'e t

    val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
                'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t

    module ApplicativeInfix :
    sig
      val ( <*> )  : ('a -> 'b) t -> 'a t -> 'b t
      val ( *> )   : 'a t -> 'b t -> 'b t
      val ( <* )   : 'a t -> 'b t -> 'a t
      val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
    end
  end

module MakeApplicative
         (F : FUNCTOR_DEF)
         (P : POINTED_DEF with type 'a t := 'a F.t)
         (A : APPLICATIVE_DEF with type 'a t := 'a F.t)
       : (S with type 'a t := 'a F.t) = struct
    include MakePointed (F) (P)
    include A

    module ApplicativeInfix = struct
        open FunctorInfix
        let ( <*> )      = ap
        let ( *> ) x y   = const <$> y <*> x
        let ( <* ) x y   = const <$> x <*> y
        let ( <**> ) x y = flip ap x y
      end

    open FunctorInfix
    open ApplicativeInfix

    let lift2 f x y       = f <$> x         <*> y
    let lift3 f x y z     = lift2 f x y     <*> z
    let lift4 f x y z w   = lift3 f x y z   <*> w
    let lift5 f x y z w u = lift4 f x y z w <*> u
  end
