type t = {x: float; y: float; z:float}

let create x y z = {x=x;y=y;z=z;}

let negate v1 = {x= -.v1.x;y= -.v1.y;z= -.v1.z;}

let (+=) v1 v2 = {x=v1.x+.v2.x;y=v1.y+.v2.y;z=v1.z+.v2.z;}

let (-=) v1 v2 = {x=v1.x-.v2.x;y=v1.y-.v2.y;z=v1.z-.v2.z;}

let prod v1 value = {x=v1.x*.value;y=v1.y*.value;z=v1.z*.value;}

let prod_vecs v1 v2 = {x=v1.x*.v2.x;y=v1.y*.v2.y;z=v1.z*.v2.z;}

let (/!) v1 t = 
    let new_t = 1. /. t in 
    prod v1 new_t;  
;;
let dot_prod v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z) 

let cross v1 v2 =
  {
    x = v1.y *. v2.z -. v1.z *. v2.y;
    y = v1.z *. v2.x -. v1.x *. v2.z;
    z = v1.x *. v2.y -. v1.y *. v2.x;
  }
;;

let length_squared v1 = dot_prod v1 v1 

let length v1 = sqrt(length_squared v1)

let unit_vec v1 = (/!) v1  (length v1)
 
let lerp v1 v2 t = 
    (prod v1 (1. -. t)) += (prod v2 t)


