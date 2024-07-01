module Vec3 = 
    struct
        type t = {
            x: float;
            y: float;
            z: float;
        }
        let create x_val y_val z_val = { x = x_val; y = y_val; z = z_val;}
        let negate_vector vec3 = {x= -.vec3.x; y= -.vec3.y; z= -.vec3.z}
        let add_vectors (v1: t) (v2: t) : t = {
            x = v1.x +. v2.x;
            y = v1.y +. v2.y;
            z = v1.z +. v2.z;
        }
        ;;
        
        let add_vector_scalar (v1: t) (value: float) : t =
            {
                x = v1.x +. value;
                y = v1.y +. value;
                z = v1.z +. value;
            }
        ;;
        let sub_vectors (v1: t) (v2: t) : t = 
            {
                x = v1.x -. v2.x;
                y = v1.y -. v2.y;
                z = v1.z -. v2.z;
            }
        ;;
        let sub_vector_scalar (v1: t) (value: float) : t = 
            {
                x = v1.x -. value;
                y = v1.y -. value;
                z = v1.z -. value;
            }
        ;;
        let dot_product (v1: t) (v2: t) : float =
            (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
        ;;
        let cross_product (v1: t) (v2: t) : t = 
            {
                x = (v1.y *. v2.z) -. (v1.z *. v2.y);
                y = (v1.z *. v2.x) -. (v1.x *. v2.z);
                z = (v1.x *. v2.y) -. (v1.y *. v2.x);
            }
        ;;
        let vector_multi_scalar (v1: t) (value: float): t =
            {
                x = v1.x *. value;
                y = v1.y *. value;
                z = v1.z *. value;
            }
        ;;
        let magnitude (v1 : t) : float = 
            sqrt((Float.pow v1.x 2.0) +. (Float.pow v1.y 2.0) +. (Float.pow v1.z 2.0)) 
        ;;
        let normalize (v1: t) : t = 
            let mag = magnitude v1 in
            {
                x = v1.x /. mag;
                y = v1.y /. mag;
                z = v1.z /. mag;
            }
        ;;
        let scalar_division (v1 : t) (value: float) : t =
            {
                x = v1.x /. value;
                y = v1.y /. value;
                z = v1.z /. value;
            }
        ;;
        let to_string (v1: t) : unit = 
            Format.printf "%.2f %.2f %.2f" v1.x v1.y v1.z
    end
