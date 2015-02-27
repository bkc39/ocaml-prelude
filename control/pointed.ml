open Functor

module type POINTED_DEF = sig
    type 'a t
    val return : 'a -> 'a t
  end

module type S = sig
    include Functor.S
    include POINTED_DEF with type 'a t := 'a t
  end

module MakePointed
         (F : FUNCTOR_DEF)
         (P : POINTED_DEF with type 'a t := 'a F.t)
       : (S with type 'a t := 'a F.t) = struct
    include MakeFunctor (F)
    include P
  end
