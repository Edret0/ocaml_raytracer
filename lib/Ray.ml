[@@@ warning "-27"]
module Point3 = Vec3.Vec3

module Ray =
    struct
        type t = 
            {
                origin: Point3.t;
                direction: Point3.t
            }
        ;;
        let at (value: float) (ray: t) : Point3.t = 
            let newDir = Point3.vector_multi_scalar ray.direction value in
            Point3.add_vectors ray.origin newDir
        ;;
        let ray_color (ray: t) : Point3.t = 
            Point3.create 0.0 0.0 0.0
    end
