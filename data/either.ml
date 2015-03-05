open Function

type ('a,'b) either = Left of 'a | Right of 'b

let cases f g = function
  | Left l  -> f l
  | Right r -> g r

let is_left = function Left _ -> true | Right _ -> false

let is_right = function Right _ -> true | Left _ -> false
