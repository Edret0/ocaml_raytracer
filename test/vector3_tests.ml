open OUnit2
open Rays 
open Rays.Vec3

let v1 = Vec3.create 1. 2. 3.
let v2 = Vec3.create 2. 4. 5.

let r = Ray.create v1 (Vec3.create 1. 0. 0.)


let tests = "test suite" >::: [
    "Vec3 creating "  >:: (fun _ ->
        assert_equal v1.x 1.;
        assert_equal v1.y 2.;
        assert_equal v1.z 3.
      );

    "Vec3 negation "  >:: (fun _ ->
        assert_equal (Vec3.negate v1) (Vec3.create (-1.) (-2.) (-3.))
      );

    "Vec3 + "  >:: (fun _ ->
        assert_equal (v1 += v2) (Vec3.create 3. 6. 8.)
      );

    "Vec3 - "  >:: (fun _ ->
        assert_equal (v1 -= v2) (Vec3.create (-1.) (-2.) (-2.))
      );

    "Vec3 * "  >:: (fun _ ->
        assert_equal (Vec3.prod v1 2.) (Vec3.create 2. 4. 6.)
      );

    "Vec3 / "  >:: (fun _ ->
        assert_equal (v1 /! 2.) (Vec3.create 0.5 1. 1.5)
      );

    "Vec3 dot" >:: (fun _ ->
        assert_equal (Vec3.dot_prod v1 v2) 25.
      );

    "Vec3 length_square" >:: (fun _ ->
        assert_equal (Vec3.length_squared v1) 14.
      );

    "Vec3 length" >:: (fun _ ->
        assert_equal (Vec3.length v1) (sqrt 14.)
      );

    "Vec3 cross" >:: (fun _ ->
        assert_equal (Vec3.cross v1 v2) (Vec3.create (-2.) 1. 0.)
      );

    "Vec3 normalize" >:: (fun _ ->
        assert_equal (Vec3.unit_vec v1)
          (Vec3.create (1. /. (sqrt 14.)) (2. /. (sqrt 14.)) (3. /. (sqrt 14.)))
      );

    "Vec3 leap" >:: (fun _ ->
        assert_equal (Vec3.lerp v1 v2 0.5) (Vec3.create 1.5 3. 4.)
      );

    "Ray at" >:: (fun _ ->
        assert_equal (r |> Ray.at 2.) (Vec3.create 3. 2. 3.)
      );

]

let _ = run_test_tt_main tests
