module type MONOID_DEF = sig
    type t
    val mid : t
    val mop : t -> t -> t
  end

module type S = sig
    include MONOID_DEF
    module MonoidInfix :
    sig
      val ( <> ) : t -> t -> t
    end
  end

module MakeMonoid (M : MONOID_DEF)
       : (S with type t := M.t) = struct
    include M
    module MonoidInfix = struct
        let ( <> ) = mop
      end
  end
