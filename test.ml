open OUnit2

(*let print_sat l =
  let open MLBDD in
  let first = ref true in
  List.iter (fun (b,v) ->
      if !first then first := false
      else print_string ", ";
      if not b then print_string "~";
      print_int v;
    ) l;
  print_endline ""*)

let bdd_tests = "MLBDD tests" >::: [
      "true_canonical" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let t1 = MLBDD.dtrue man in
          let t2 = MLBDD.dtrue man in
          assert_equal ~cmp:MLBDD.equal t1 t2
        );
      "false_canonical" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let t1 = MLBDD.dfalse man in
          let t2 = MLBDD.dfalse man in
          assert_equal ~cmp:MLBDD.equal t1 t2
        );
      "true != false" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let t1 = MLBDD.dfalse man in
          let t2 = MLBDD.dtrue man in
          let r = MLBDD.equal t1 t2 in
          assert_equal r false
        );
      "and false" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let v0 = MLBDD.ithvar man 0 in
          let t1 = MLBDD.dfalse man in
          let r = MLBDD.is_false (MLBDD.dand v0 t1) in
          assert_equal r true
        );
      "double_negate" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let t1 = MLBDD.dfalse man in
          let t2 = MLBDD.dnot (MLBDD.dnot t1) in
          assert_equal ~cmp:MLBDD.equal t1 t2
        );
      "and_canonical" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let v0 = MLBDD.ithvar man 0 in
          let v1 = MLBDD.ithvar man 1 in
          let t1 = MLBDD.dand v0 v1 in
          let t2 = MLBDD.dand v1 v0 in
          assert_equal ~cmp:MLBDD.equal t1 t2
        );
      "eq_exists" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let v0 = MLBDD.ithvar man 0 in
          let v1 = MLBDD.ithvar man 1 in
          let v2 = MLBDD.ithvar man 2 in
          let t1 = MLBDD.dand 
              (MLBDD.eq v0 v1)
              (MLBDD.eq v1 v2) in
          let t1 = MLBDD.exists (MLBDD.support v1) t1 in
          let t2 = MLBDD.eq v0 v2 in
          assert_equal ~cmp:MLBDD.equal t1 t2
        );
      "and_project" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let v0 = MLBDD.ithvar man 0 in
          let v1 = MLBDD.ithvar man 1 in
          let t1 = MLBDD.dand v0 v1 in
          let t2 = MLBDD.exists (MLBDD.support v1) t1 in
          assert_equal ~cmp:MLBDD.equal v0 t2
        );
      "tautology" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let imply a b = MLBDD.dor (MLBDD.dnot a) b in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = imply p q in
          let hypoth = MLBDD.dand pq p in
          let modus_ponens = imply hypoth q in
          let taut = MLBDD.forall (MLBDD.support modus_ponens) modus_ponens in
          assert_equal (MLBDD.is_true taut) true
        );
      "support string" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.xor p q in
          let supp = MLBDD.support pq in
          let s = MLBDD.string_of_support supp in
          assert_equal s "0,1"
        );
      "bddstring1" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let s = MLBDD.to_string p in
          assert_equal s "(0, F, T,2)"
        );
      "bddstring2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let s = MLBDD.to_string (MLBDD.dnot p) in
          assert_equal s "~(0, F, T,2)"
        );
      "bddstringb1" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let s = MLBDD.to_stringb p in
          assert_equal s "(0, F, T,2)"
        );
      "sat2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dand p q in
          match MLBDD.sat pq with
          | None -> assert_failure "unreachable"
          | Some s ->
            assert_equal s [(true, 0); (true, 1)]
        );
      "allsat2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dand p q in
          let r = MLBDD.allsat pq in
          assert_equal r [[(true, 0); (true, 1)]]
        );
      "itersat2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dand p q in
          MLBDD.itersat (fun s ->
              assert_equal s [(true, 0); (true, 1)]
            ) pq
        );
      "prime2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dor p q in
          match MLBDD.prime pq with
          | None -> assert_failure "unreachable"
          | Some s ->
            let r = s = [(true, 0)] || s = [(true, 1)] in
            assert_equal r true
        );
      "allprime2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dor p q in
          let p = MLBDD.allprime pq in
          let r1 = [[(true, 1)];[(true, 0)]] in
          let r2 = [[(true, 0)];[(true, 1)]] in
          let r = p = r1 || p = r2 in
          assert_equal r true
        );
      "iterprime2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dor p q in
          MLBDD.iterprime (fun s ->
              let r1 = [(true, 0)] in
              let r2 = [(true, 1)] in
              let r = s = r1 || s = r2 in
              assert_equal r true
            ) pq
        );
      "cofactor1" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dand p q in
          let (c0,c1) = MLBDD.cofactor 0 pq in
          assert_equal (MLBDD.is_false c0) true;
          assert_equal ~cmp:MLBDD.equal c1 q
        );
      "cofactor2" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let pq = MLBDD.dand p q in
          let (c0,c1) = MLBDD.cofactor 1 pq in
          assert_equal (MLBDD.is_false c0) true;
          assert_equal ~cmp:MLBDD.equal c1 p
        );
      "cofactor3" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let r = MLBDD.ithvar man 2 in
          let pq = MLBDD.xor p q in
          let pqr = MLBDD.xor pq r in
          let pr = MLBDD.exists (MLBDD.support q) pqr in
          let (c0,c1) = MLBDD.cofactor 1 pqr in
          let pr2 = MLBDD.dor c0 c1 in
          assert_equal ~cmp:MLBDD.equal pr pr2
        );
      "cofactor4" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let r = MLBDD.ithvar man 2 in
          let pq = MLBDD.xor p q in
          let pqr = MLBDD.xor pq r in
          let pr = MLBDD.forall (MLBDD.support q) pqr in
          let (c0,c1) = MLBDD.cofactor 1 pqr in
          let pr2 = MLBDD.dand c0 c1 in
          assert_equal ~cmp:MLBDD.equal pr pr2
        );
      "cofactor4" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let r = MLBDD.ithvar man 2 in
          let pq = MLBDD.xor p q in
          let pqr = MLBDD.xor pq r in
          let (c0,c1) = MLBDD.cofactor 4 pqr in
          assert_equal ~cmp:MLBDD.equal c0 c1
        );
      "cofactor5" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let r = MLBDD.ithvar man 2 in
          let pq = MLBDD.xor p q in
          let pqr = MLBDD.xor pq r in
          let (c0,c1) = MLBDD.cofactor (-1) pqr in
          assert_equal ~cmp:MLBDD.equal c0 c1
        );
      "cofactor5" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let npq = MLBDD.dnot (MLBDD.dor p q) in
          let (c0,c1) = MLBDD.cofactor 4 npq in
          assert_equal ~cmp:MLBDD.equal c0 c1
        );
      "cofactor6" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let npq = MLBDD.dnot (MLBDD.dor p q) in
          (* ~(p \/ q) = ~p /\ ~q *)
          let (c0,c1) = MLBDD.cofactor 1 npq in
          assert_equal (MLBDD.is_false c1) true;
          assert_equal ~cmp:MLBDD.equal c0 (MLBDD.dnot p)
        );
      "fold_unit" >:: (fun ctx ->
          let man = MLBDD.init ~cache:100 () in
          let p = MLBDD.ithvar man 0 in
          let q = MLBDD.ithvar man 1 in
          let npq = MLBDD.dnot (MLBDD.dor p q) in
          let imp p q = MLBDD.dor (MLBDD.dnot p) q in
          let ite l v r =
            let v = MLBDD.ithvar man v in
            MLBDD.dand (imp (MLBDD.dnot v) l) (imp v r)
          in
          let npq2 = MLBDD.fold (function
              | MLBDD.False ->
                MLBDD.dfalse man
              | MLBDD.True ->
                MLBDD.dtrue man
              | MLBDD.Not r ->
                MLBDD.dnot r
              | MLBDD.If (e0,v,e1) ->
                ite e0 v e1
            ) npq in
          assert_equal ~cmp:MLBDD.equal npq npq2
        );
      "mkite" >:: (fun ctx ->
          let man = MLBDD.init () in
          let b1 = MLBDD.ithvar man 0 in
          let f = MLBDD.dfalse man in
          let t = MLBDD.dtrue man in
          let b2 = MLBDD.ite f 0 t in
          assert_equal ~cmp:MLBDD.equal b1 b2
        );
      "permute" >:: (fun ctx ->
          let man = MLBDD.init () in
          let a = MLBDD.ithvar man 0 in
          let na = MLBDD.dnot a in
          let b = MLBDD.ithvar man 1 in
          let nb = MLBDD.dnot b in
          let b1 = MLBDD.dor na b in
          let b2 = MLBDD.dor a nb in
          assert_equal ~cmp:(fun a b -> not (MLBDD.equal a b)) b1 b2;
          let b3 = MLBDD.permute [| 1; 0 |] b2 in
          assert_equal ~cmp:MLBDD.equal b1 b3
        );
    ]

let weakhash_tests = "weak hash tests" >::: [
      "hash_add" >:: (fun ctx ->
          let module H = WeakHash.Make(struct
              type t = int
              let equal = (=)
              let hash = Hashtbl.hash
            end) in
          let w = H.create 17 in
          let h = Hashtbl.create 17 in
          let add k v =
            Hashtbl.replace h k v;
            H.replace w k v
          in

          for i = 0 to 100 do
            add i (string_of_int i)
          done;

          Hashtbl.iter (fun k v ->
                assert_equal v (H.find w k)
              ) h
        );
      (*"hash_big" >:: (fun ctx ->
          let module H = WeakHash.Make(struct
              type t = int
              let equal = (=)
              let hash = Hashtbl.hash
            end) in
          let w = H.create 17 in
          let h = Hashtbl.create 17 in
          let element = String.make 5 'a' in

          let hstart = Unix.gettimeofday () in
          for i = 0 to 1_000_000 do
            Hashtbl.replace h i element
          done;
          let hend = Unix.gettimeofday () in

          let wstart = Unix.gettimeofday () in
          for i = 0 to 1_000_000 do
            H.add w i element
          done;
          let wend = Unix.gettimeofday () in

          (*OUnit2.logf ctx `Info "Hashtbl: %f, WeakHash: %f\n" (hend -. hstart) (wend -. wstart);*)

          let hstart = Unix.gettimeofday () in
          for i = 0 to 1_000_000 do
            ignore(Hashtbl.find h i)
          done;
          let hend = Unix.gettimeofday () in

          let wstart = Unix.gettimeofday () in
          for i = 0 to 1_000_000 do
            ignore(H.find w i)
          done;
          let wend = Unix.gettimeofday () in

          (*OUnit2.logf ctx `Info "Hashtbl: %f, WeakHash: %f\n" (hend -. hstart) (wend -. wstart);*)
          ()

          (*Hashtbl.iter (fun k v ->
                assert_equal v (H.find w k)
              ) h*)
        );*)
    ]

let _ =
  run_test_tt_main begin "all" >::: [
      bdd_tests;
      weakhash_tests;
    ] end
