(*[@@@warning "-33"]*)
(*[@@@warning "-32"]*)
(**)
open OUnit2
open Rays.Vec3

let create_test _ = 
    let v1 = create 123.4 24.5 247.234 in
    assert_equal {x = 123.4; y = 24.5; z = 247.234} v1
;;

let negate_test _ = 
    let v1 = create 123.4 234.2 73.35 in
    let negateV1 = negate_vector v1 in 
    assert_equal {x = -123.4; y = -234.2; z = -73.35} negateV1;
;;

let add_vectors_test _ = 
    let v1 = create 123.4 234.2 73.35 in 
    let v2 = create 1.0 1.0 1.0 in
    let addedVectors = add_vectors v1 v2 in
    assert_equal {x=124.4; y=235.2; z=74.35;} addedVectors
;;

let add_vectors_scalar_test _ = 
    let v1 = create 123.4 234.2 73.35 in
    let scalar_val = 1.0 in 
    let new_v1 = add_vector_scalar v1 scalar_val in 
    assert_equal {x=124.4; y=235.2; z=74.35;} new_v1;
;;
let sub_vectors_test _ = 
    let v1 = create 12.0 31.0 23.0 in
    let v2 = create 23.0 10.0 12.0 in
    let newV1 = sub_vectors v1 v2 in
    assert_equal {x= -11.0; y = 21.0; z = 11.0;} newV1;
;;
let sub_vector_scalar_test _ =
    let v1 = create 200.0 345.0 (-12.0) in
    let scalarValue = 10.0 in
    let newV1 = sub_vector_scalar v1 scalarValue in
    assert_equal {x = 190.0; y = 335.0; z = -22.0;} newV1;
    let scalarValue = -1.0 *. scalarValue in 
    let newV1 = sub_vector_scalar v1 scalarValue in 
    assert_equal {x = 210.0; y = 355.0; z = -2.0} newV1;
;;

let dot_prod_test _ =
    let v1 = create 100.0 20.0 10.0 in
    let v2 = create 1.0 1.0 1.0 in 
    let dot_result = dot_product v1 v2 in
    assert_equal 130.0 dot_result;
;;
let cross_prod_test _ = 
    let v1 = create 20.0 10.0 50.0 in
    let v2 = create 2.0 100.0 0.0 in
    let result = cross_product v1 v2 in
    assert_equal {x = -5000.0; y = 100.0; z = 1980.0} result;
;;
let vector_prod_scalar_test _ = 
    let v1 = create 10.0 100.0 402.0 in 
    let scalar_val = 2.0 in 
    let res = vector_multi_scalar v1 scalar_val in 
    assert_equal {x = 20.0; y = 200.0; z = 804.0;} res;
    let scalar_val = -2.0 in 
    let res = vector_multi_scalar v1 scalar_val in 
    assert_equal {x = -20.0; y = -200.0; z = -804.0;} res;
;;
let mag_test _ = 
    let v1 = create 10.0 100.0 20.0 in 
    let newV1 = magnitude v1 in 
    assert_equal (sqrt(10500.0)) newV1;
;;
let norm_test _ = 
    let v1 = create 10.0 100.0 20.0 in 
    let res = normalize v1 in 
    let mag_result = sqrt(10500.0) in 
    assert_equal {
        x = v1.x /. mag_result; 
        y = v1.y /. mag_result;
        z = v1.z /. mag_result;
    } res;
;;
let test_suite = 
    "Vec3 Tests" >::: [
        "create_test" >:: create_test;
        "negate_test" >:: negate_test;
        "add_vectors_test" >:: add_vectors_test;
        "add_vector_scalar_test" >:: add_vectors_scalar_test;
        "sub_vectors_test" >:: sub_vectors_test;
        "sub_vector_scalar_test" >:: sub_vector_scalar_test;
        "dot_prod_test" >:: dot_prod_test;
        "cross_prod_test" >:: cross_prod_test;
        "vector_prod_scalar_test" >:: vector_prod_scalar_test;
        "mag_test" >:: mag_test;
        "norm_test" >:: norm_test;
    ]
;;
let () = run_test_tt_main test_suite

