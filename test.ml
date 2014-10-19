open OUnit

(*let print_sat l =
  let open BDD in
  let first = ref true in
  List.iter (fun (b,v) ->
      if !first then first := false
      else print_string ", ";
      if not b then print_string "~";
      print_int v;
    ) l;
  print_endline ""*)

let _ =
  run_test_tt_main begin "1" >::: [
      "true_canonical" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let t1 = BDD.dtrue man in
          let t2 = BDD.dtrue man in
          assert_equal ~cmp:BDD.equal t1 t2
        );
      "false_canonical" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let t1 = BDD.dfalse man in
          let t2 = BDD.dfalse man in
          assert_equal ~cmp:BDD.equal t1 t2
        );
      "true != false" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let t1 = BDD.dfalse man in
          let t2 = BDD.dtrue man in
          let r = BDD.equal t1 t2 in
          assert_equal r false
        );
      "and false" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let v0 = BDD.ithvar man 0 in
          let t1 = BDD.dfalse man in
          let r = BDD.is_false (BDD.dand v0 t1) in
          assert_equal r true
        );
      "double_negate" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let t1 = BDD.dfalse man in
          let t2 = BDD.dnot (BDD.dnot t1) in
          assert_equal ~cmp:BDD.equal t1 t2
        );
      "and_canonical" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let v0 = BDD.ithvar man 0 in
          let v1 = BDD.ithvar man 1 in
          let t1 = BDD.dand v0 v1 in
          let t2 = BDD.dand v1 v0 in
          assert_equal ~cmp:BDD.equal t1 t2
        );
      "eq_exists" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let v0 = BDD.ithvar man 0 in
          let v1 = BDD.ithvar man 1 in
          let v2 = BDD.ithvar man 2 in
          let t1 = BDD.dand 
              (BDD.eq v0 v1)
              (BDD.eq v1 v2) in
          let t1 = BDD.exists (BDD.support v1) t1 in
          let t2 = BDD.eq v0 v2 in
          assert_equal ~cmp:BDD.equal t1 t2
        );
      "and_project" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let v0 = BDD.ithvar man 0 in
          let v1 = BDD.ithvar man 1 in
          let t1 = BDD.dand v0 v1 in
          let t2 = BDD.exists (BDD.support v1) t1 in
          assert_equal ~cmp:BDD.equal v0 t2
        );
      "tautology" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let imply a b = BDD.dor (BDD.dnot a) b in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = imply p q in
          let hypoth = BDD.dand pq p in
          let modus_ponens = imply hypoth q in
          let taut = BDD.forall (BDD.support modus_ponens) modus_ponens in
          assert_equal (BDD.is_true taut) true
        );
      "support string" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.xor p q in
          let supp = BDD.support pq in
          let s = BDD.string_of_support supp in
          assert_equal s "0,1"
        );
      "bddstring1" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let s = BDD.to_string p in
          assert_equal s "(0, F, T,2)"
        );
      "bddstring2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let s = BDD.to_string (BDD.dnot p) in
          assert_equal s "~(0, F, T,2)"
        );
      "bddstringb1" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let s = BDD.to_stringb p in
          assert_equal s "(0, F, T,2)"
        );
      "sat2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dand p q in
          match BDD.sat pq with
          | None -> assert_failure "unreachable"
          | Some s ->
            assert_equal s [(true, 0); (true, 1)]
        );
      "allsat2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dand p q in
          let r = BDD.allsat pq in
          assert_equal r [[(true, 0); (true, 1)]]
        );
      "itersat2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dand p q in
          BDD.itersat (fun s ->
              assert_equal s [(true, 0); (true, 1)]
            ) pq
        );
      "prime2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dor p q in
          match BDD.prime pq with
          | None -> assert_failure "unreachable"
          | Some s ->
            let r = s = [(true, 0)] || s = [(true, 1)] in
            assert_equal r true
        );
      "allprime2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dor p q in
          let p = BDD.allprime pq in
          let r1 = [[(true, 1)];[(true, 0)]] in
          let r2 = [[(true, 0)];[(true, 1)]] in
          let r = p = r1 || p = r2 in
          assert_equal r true
        );
      "iterprime2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dor p q in
          BDD.iterprime (fun s ->
              let r1 = [(true, 0)] in
              let r2 = [(true, 1)] in
              let r = s = r1 || s = r2 in
              assert_equal r true
            ) pq
        );
      "cofactor1" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dand p q in
          let (c0,c1) = BDD.cofactor 0 pq in
          assert_equal (BDD.is_false c0) true;
          assert_equal ~cmp:BDD.equal c1 q
        );
      "cofactor2" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let pq = BDD.dand p q in
          let (c0,c1) = BDD.cofactor 1 pq in
          assert_equal (BDD.is_false c0) true;
          assert_equal ~cmp:BDD.equal c1 p
        );
      "cofactor3" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let r = BDD.ithvar man 2 in
          let pq = BDD.xor p q in
          let pqr = BDD.xor pq r in
          let pr = BDD.exists (BDD.support q) pqr in
          let (c0,c1) = BDD.cofactor 1 pqr in
          let pr2 = BDD.dor c0 c1 in
          assert_equal ~cmp:BDD.equal pr pr2
        );
      "cofactor4" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let r = BDD.ithvar man 2 in
          let pq = BDD.xor p q in
          let pqr = BDD.xor pq r in
          let pr = BDD.forall (BDD.support q) pqr in
          let (c0,c1) = BDD.cofactor 1 pqr in
          let pr2 = BDD.dand c0 c1 in
          assert_equal ~cmp:BDD.equal pr pr2
        );
      "cofactor4" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let r = BDD.ithvar man 2 in
          let pq = BDD.xor p q in
          let pqr = BDD.xor pq r in
          let (c0,c1) = BDD.cofactor 4 pqr in
          assert_equal ~cmp:BDD.equal c0 c1
        );
      "cofactor5" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let r = BDD.ithvar man 2 in
          let pq = BDD.xor p q in
          let pqr = BDD.xor pq r in
          let (c0,c1) = BDD.cofactor (-1) pqr in
          assert_equal ~cmp:BDD.equal c0 c1
        );
      "cofactor5" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let npq = BDD.dnot (BDD.dor p q) in
          let (c0,c1) = BDD.cofactor 4 npq in
          assert_equal ~cmp:BDD.equal c0 c1
        );
      "cofactor6" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let npq = BDD.dnot (BDD.dor p q) in
          (* ~(p \/ q) = ~p /\ ~q *)
          let (c0,c1) = BDD.cofactor 1 npq in
          assert_equal (BDD.is_false c1) true;
          assert_equal ~cmp:BDD.equal c0 (BDD.dnot p)
        );
      "fold_unit" >:: (fun () ->
          let man = BDD.init ~cache:100 () in
          let p = BDD.ithvar man 0 in
          let q = BDD.ithvar man 1 in
          let npq = BDD.dnot (BDD.dor p q) in
          let imp p q = BDD.dor (BDD.dnot p) q in
          let ite l v r =
            let v = BDD.ithvar man v in
            BDD.dand (imp (BDD.dnot v) l) (imp v r)
          in
          let npq2 = BDD.fold (function
              | BDD.False ->
                BDD.dfalse man
              | BDD.True ->
                BDD.dtrue man
              | BDD.Not r ->
                BDD.dnot r
              | BDD.If (e0,v,e1) ->
                ite e0 v e1
            ) npq in
          assert_equal ~cmp:BDD.equal npq npq2
        );
    ] end
