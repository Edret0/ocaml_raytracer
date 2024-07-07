open Core 
open Sys_unix
module Point3 = Vec3
open Vec3 
let create_PPM = 
    let file_name = "image.ppm" in 
    let file_exists_result = match file_exists file_name with 
    | `No -> false; 
    | `Yes -> true; 
    | `Unknown -> false; 
    in

    if file_exists_result then (
        remove file_name;
    );
    let max_color = 255 in 
    let image_width = 400 in 
    let image_height = 225 
    and horizontal = Vec3.create 4.0 0.0 0.0
    and vertical = Vec3.create 0.0 2.0 0.0
    and origin = Vec3.create 0.0 0.0 0.0 in 
    let lower_left_corner = 
        origin -= (horizontal /! 2.0) -= (vertical /! 2.0) -= (create 0. 0. 1.) in


    let outer_loop = Sequence.range 0 image_height in
    let inner_loop = Sequence.range 0 image_width in
    let cart_prod = Sequence.cartesian_product outer_loop inner_loop in 
    let open_channel = Out_channel.create file_name in
    fprintf open_channel "P3\n"; 
    fprintf open_channel "%i %i\n%i\n" image_width image_height max_color;
    Sequence.iter cart_prod ~f:(fun (j, i) ->
        let u = float_of_int i /. float_of_int (image_width) in
        let v = float_of_int j /. float_of_int (image_height) in
        let r = Ray.create origin 
            (lower_left_corner += (prod horizontal u) += (prod vertical v)) in
        let color = Sphere.sphere_ray_color r in 

        let rbyte = int_of_float(color.x *. 255.999) in 
        let gbyte = int_of_float(color.y *. 255.999) in 
        let bbyte = int_of_float(color.z *. 255.999) in 

        Printf.fprintf open_channel "%i %i %i\n" rbyte gbyte bbyte;
        );  
    Out_channel.close open_channel;
    Printf.printf "\rDone\n";

;;
