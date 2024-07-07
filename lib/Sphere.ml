open Vec3
open Ray 

let sphere_hit (center: Vec3.t) (radius: float) (ray: Ray.t) : bool = 
    let original_center = center -= ray.origin in 
    let a = dot_prod ray.direction ray.direction in 
    let b = 2.0 *. (dot_prod ray.direction original_center) in 
    let c = (dot_prod original_center original_center) -. (radius *. radius) in 
    let discriminat = (b *. b) -. (4. *. a *. c) in
    discriminat >= 0.;
;;

let sphere_ray_color (ray: Ray.t) : Vec3.t = 
    let v1 = Vec3.create 0. 0. (-1.) in
    let result = 
        match sphere_hit v1 0.5 ray with 
        | true -> Vec3.create 1. 0. 0. 
        | false -> 
                let unit_dir = unit_vec ray.direction in 
                let a = 0.5 *. (unit_dir.y +. 1.0) in 
                lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) a;
    in
    result;

