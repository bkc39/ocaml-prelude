let id x             = x
let const x _        = x
let flip f x y       = f y x
let rec until p f a  = if p a then a else until p f (f a)
let rec undefined () = failwith "Base.undefined"
let curry f x y      = f (x,y)
let uncurry f (x, y) = f x y
