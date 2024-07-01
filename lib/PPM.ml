[@@@ warning "-27"]
[@@@ warning "-26"]
module Point3 = Vec3.Vec3 
let aspect_ratio = 16.0 /. 9.0
let create_PPM (rows: int) (cols: int) (max_color: int) : unit = 
    let file_name = "image.ppm" in 
    if Sys.file_exists file_name then
        Printf.printf "File exists\n Removing old file\n";
        Sys.remove file_name;
    (*image*)
    let cols = cols in
    let rows = int_of_float(float_of_int(cols) /. aspect_ratio) in
    let rows = 
        if rows < 1 then 1 
        else rows in 

    let focal_length = 1.0 in 
    let viewport_height = 2.0 in 
    let viewport_width = 
        viewport_height *. (float_of_int(rows) /. float_of_int(cols)) in
    let camera_center = Point3.create 0.0 0.0 0.0 in
 
    let viewport_u = Point3.create viewport_width 0.0 0.0 in
    let viewport_v = Point3.create 0.0 (-1.0 *. viewport_height) 0.0 in
    let float_cols = float_of_int(cols) in 
    let float_rows = float_of_int(rows) in 
    let pixel_delta_u = Point3.scalar_division viewport_u float_cols in
    let pixel_delta_v = Point3.scalar_division viewport_v float_rows in 
    let viewport_u_half = Point3.scalar_division viewport_u 2.0 in 
    let viewport_v_half = Point3.scalar_division viewport_v 2.0 in 
    let viewport_upper_left = 
        Point3.sub_vectors camera_center {x=0.0;y=0.0;z=focal_length} in
    let viewport_upper_left = 
        Point3.sub_vectors viewport_upper_left viewport_u_half in 
    let viewport_upper_left = 
        Point3.sub_vectors viewport_upper_left viewport_v_half in
    let add_pixel_delta = Point3.add_vectors pixel_delta_u pixel_delta_v in 
    let viewport_upper_left_half = 
        Point3.add_vector_scalar viewport_upper_left 0.5 in
    let pixel_00_loc = 
        Point3.cross_product viewport_upper_left_half add_pixel_delta in

    (*creating the ppm*)
    let open_channel = open_out file_name in
    Printf.fprintf open_channel "P3\n"; 
    Printf.fprintf open_channel "%i %i\n%i\n" cols rows max_color;
    for j = 0 to rows-1 do
        let scanlines_remaining = rows - j in
        Printf.printf "\rScanlines remaining: %d\n" scanlines_remaining;
        for i = 0 to cols-1 do
            
            let normalized_red = float_of_int(i) /. float_of_int(rows-1) in
            let normalized_green = float_of_int(j) /. float_of_int(cols - 1) in
            let normalized_blue = 0.0 in 
            let normalized_color_vector = 
                Point3.create normalized_red normalized_green normalized_blue in 

            let scaled_red = 
                int_of_float(normalized_color_vector.x *. 255.999) in
            let scaled_green = 
                int_of_float(normalized_color_vector.y *. 255.999) in
            let scaled_blue = 
                int_of_float(normalized_color_vector.z *. 255.999) in
            Printf.fprintf open_channel "%i %i %i\n" scaled_red scaled_green scaled_blue;
        done;
        
    done;
    close_out open_channel;
    Printf.printf "\rDone\n";
    
;;
