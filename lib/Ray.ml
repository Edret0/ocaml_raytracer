[@@@warning "-27"]
module Point3 = Vec3 
module Color = Vec3 
type t = {origin:Point3.t;direction:Point3.t}

let create origin direction = {origin; direction;}

let at (t:float) ({origin; direction;} : t) = 
    Point3.(+=) origin (Point3.prod direction t)

let ray_color ray = 
    let unit_direction = ray.direction in
    let a = 0.5 *. (unit_direction.y +. 1.0) in 
    Vec3.lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) a


