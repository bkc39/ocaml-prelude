open Data.Function

module type FUNCTOR_DEF = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

(* module type FUNCTOR_INFIX = sig *)
(*   type 'a t *)
(*   val ( |> )  : 'a t -> ('a -> 'b) -> 'b t *)
(*   val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t *)
(*   val ( <$ )  : 'a -> 'b t -> 'a t *)
(*   val ( $> )  : 'a -> 'b -> 'c t -> 'b t *)
(* end *)

module type S = sig
    type 'a t
    val map  : ('a -> 'b) -> 'a t -> 'b t
    val void : 'a -> 'b t -> unit t
    module FunctorInfix :
    sig
      val ( |> )  : 'a t -> ('a -> 'b) -> 'b t
      val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
      val ( <$ )  : 'a -> 'b t -> 'a t
      val ( $> )  : 'a -> 'b -> 'c t -> 'b t
    end
  end

module MakeFunctor (F : FUNCTOR_DEF) : (S with type 'a t := 'a F.t) = struct
    include F

    let void _ = map (const ())

    module FunctorInfix = struct
        let ( |> ) x f = map f x
        let ( <$> )    = map
        let ( <$ ) x   = map (const x)
        let ( $> ) _ x = map (const x)
      end
  end
