(* Binary decision diagrams *)

type var = int

type node =
  | NFalse
  | NIf of node (* 0 edge *)
        * var  (* variable *)
        * cnode    (* 1 edge *)
        * int  (* code always even *)

and cnode = node   (* node *)
            * bool (* inverted *)

module HashedIf = struct
  (* hash structure for code * var * code *)
  type t = int * var * int

  let equal (l1,v1,r1) (l2,v2,r2) =
    l1 == l2 && r1 == r2 && v1 == v2

  let hash (l,v,r) = Hashtbl.hash (l,v,r)
end

(*module IfHashCons = Hashtbl.Make(HashedIf)*)
module IfHashCons = WeakHash.Make(HashedIf)

type man = {
  bdd_hc : node IfHashCons.t;
  bdd_hc_stamp : int ref;
  and_cache : (int * int,cnode) Hashtbl.t;
  xor_cache : (int * int,cnode) Hashtbl.t;
}

let clear ctx = 
  IfHashCons.clear ctx.bdd_hc;
  Hashtbl.clear ctx.and_cache;
  Hashtbl.clear ctx.xor_cache

let init ?cache:(init=1002403) () = {
  bdd_hc = IfHashCons.create init;
  bdd_hc_stamp = ref 2;
  and_cache = Hashtbl.create init;
  xor_cache = Hashtbl.create init;
}

type t = {
  man: man;
  node: cnode;
}

let manager t = t.man

let equal t1 t2 =
  t1.man == t2.man &&
  match t1.node, t2.node with
  | (NFalse, a),(NFalse, b) when a == b -> true
  | (NIf(_,_,_,ida), a),(NIf(_,_,_,idb), b) when ida == idb && a == b -> true
  | _ -> false

let dtrue man =
  { man; node = (NFalse, true) }

let dfalse man =
  { man; node = (NFalse, false) }

let is_true t =
  t.node = (NFalse, true)

let is_false t =
  t.node = (NFalse, false)

let ident_node = function
  | NFalse -> 0
  | NIf(_,_,_,id) -> id

let ident_cnode = function
  | (node, true) -> (ident_node node) + 1
  | (node, false) -> (ident_node node)

let not_cnode cn =
  let (node, inv) = cn in
  (node, not inv)

(* raw, internal mkif function *)
let mkif_int man e0 v e1 =
  (* normalize to ensure that complement is only on 1 edge *)
  let (inverted,e0,e1) = match e0 with
    | (node, true) ->
      (true, node, (not_cnode e1))
    | (node, false) ->
      (false, node, e1)
  in
  (* compute the ids for the 0 and 1 sides *)
  let id0 = ident_node e0 in
  let id1 = ident_cnode e1 in
  
  if id0 == id1 then
    (* ids are the same, don't produce a node *)
    (e0,inverted)
  else
    (* ids are different, produce a node *)
    let res_node = begin
      let key = (id0, v, id1) in
      try
        (* if such a node already exists, use that *)
        IfHashCons.find man.bdd_hc key
      with Not_found ->
        (* if, not create a new id (always even!) *)
        let id = !(man.bdd_hc_stamp) in
        man.bdd_hc_stamp := id + 2;
        let f = NIf(e0, v, e1, id) in
        IfHashCons.replace man.bdd_hc key f;
        f
    end in
    (res_node, inverted)

let rec to_string_int (e,inv) =
  let s = match e with
    | NFalse -> "F"
    | NIf(e0, v, e1, id) ->
      let s0 = to_string_int (e0,false) in
      let s1 = to_string_int e1 in
      "("^(string_of_int v)^", "^s0^", "^s1^","^(string_of_int id)^")"
  in
  if inv then
    if s = "F" then "T" else "~"^s
  else
    s

let rec to_stringb_int inv1 (e,inv2) =
  let inv = inv1 <> inv2 in
  match e, inv with
  | NFalse, true -> "T"
  | NFalse, false -> "F"
  | NIf(e0, v, e1, id), _ ->
    let s0 = to_stringb_int inv (e0,false) in
    let s1 = to_stringb_int inv e1 in
    "("^(string_of_int v)^", "^s0^", "^s1^","^(string_of_int id)^")"


let ithvar man var =
  let node = mkif_int man (NFalse,false) var (NFalse,true) in
  { man; node }

let dnot t =
  {t with node = not_cnode t.node}

let rec dand_int man t1 t2 =
  match t1, t2 with
  | (NFalse, false), _ -> (NFalse, false)
  | (NFalse, true), _ -> t2
  | _, (NFalse, false) -> (NFalse, false)
  | _, (NFalse, true) -> t1
  | (NIf(e0a, va, e1a, _),inva), (NIf(e0b, vb, e1b, _),invb) ->
    let ida = ident_cnode t1 in
    let idb = ident_cnode t2 in
    if ida == idb then t1 else begin
      let key = if ida < idb then (ida, idb) else (idb, ida) in
      try
        Hashtbl.find man.and_cache key
      with Not_found ->
        let e0a = (e0a,inva) in
        let e1a = if inva then not_cnode e1a else e1a in
        let e0b = (e0b,invb) in
        let e1b = if invb then not_cnode e1b else e1b in
        let c = vb - va in
        let f =
          if c = 0 then
            mkif_int man (dand_int  man e0a e0b) va (dand_int  man e1a e1b)
          else if c < 0 then
            mkif_int man (dand_int  man e0b t1) vb (dand_int  man e1b t1)
          else
            mkif_int man (dand_int  man e0a t2) va (dand_int  man e1a t2)
        in
        Hashtbl.replace man.and_cache key f;
        f
    end

let dand t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = dand_int man t1 t2 }

let to_string t =
  to_string_int t.node

let to_stringb t =
  to_stringb_int false t.node
      
let dor t1 t2 =
  dnot (dand (dnot t1) (dnot t2))

let nand t1 t2 =
  dnot (dand t1 t2)

let nxor t1 t2 =
  let v1 = nand t1 t2 in
  let v2 = nand t1 v1 in
  let v3 = nand t2 v1 in
  dand v2 v3

let xor t1 t2 =
  dnot (nxor t1 t2)

let eq = nxor

let imply a b =
  dor (dnot a) b

let rec ite f var t =
  let v = ithvar f.man var in
  dand (imply v t) (imply (dnot v) f)


let cofactor v t =
  let visited = Hashtbl.create ((IfHashCons.length t.man.bdd_hc)*3/2) in
  let rec cofactor = function
    | (NIf(e0, vc, e1, id), inv) as node ->
      begin try
          Hashtbl.find visited id
        with Not_found ->
          let res = if v = vc then
              let e0 = (e0,inv) in
              let e1 = if inv then not_cnode e1 else e1 in
              (e0,e1)
            else if v < vc then
              (node,node)
            else (* v > vc *)
              let e0 = (e0,inv) in
              let e1 = if inv then not_cnode e1 else e1 in
              let (e00,e01) = cofactor e0 in
              let (e10,e11) = cofactor e1 in
              let r0 = mkif_int t.man e00 vc e10 in
              let r1 = mkif_int t.man e01 vc e11 in
              (r0, r1)
          in
          Hashtbl.replace visited id res;
          res
      end
    | res -> res,res
  in
  let (r0,r1) = cofactor t.node in
  ({t with node=r0}, {t with node=r1})



module Int = struct
  type t = int
  let compare a b = a - b
end

module ISet = Set.Make(Int)



let support t =
  let visited = Hashtbl.create ((IfHashCons.length t.man.bdd_hc)*3/2) in
  let support = ref ISet.empty in
  let rec itersupport (node,inv) =
    match node with
    | NIf(e0, v, e1, id) ->
      if Hashtbl.mem visited id then
        ()
      else begin
        Hashtbl.replace visited id ();
        support := ISet.add v !support;
        itersupport (e0,false);
        itersupport e1;
      end
    | _ ->
      ()
  in
  itersupport t.node;
  !support

let rec string_of_support s =
  let b = Buffer.create 80 in
  let first = ref true in
  ISet.iter (fun i ->
      if !first then
        first := false
      else
        Buffer.add_string b ",";
      Buffer.add_string b (string_of_int i)
    ) s;
  Buffer.contents b

let list_of_support s =
  ISet.elements s

let exists support t =
  let man = t.man in
  let supp = ISet.fold (fun i r -> i::r) support [] in
  let rec exists supp (t: cnode) =
    match t,supp with
    | (NFalse, _), _ -> t
    | _, [] -> t
    | (NIf(e0, v, e1, _id), inv), h::rest ->
      let c = h - v in
      if c = 0 then begin
        let e0 = (e0,inv) in
        let e0 = exists rest e0 in
        let e1 = if inv then not_cnode e1 else e1 in
        let e1 = exists rest e1 in
        let e0 = not_cnode e0 in
        let e1 = not_cnode e1 in
        let not_res = dand_int man e0 e1 in
        not_cnode not_res
      end else if c < 0 then begin
        (* v > h: already passed h, so skip h *)
        exists rest t
      end else begin
        (* h > v: not there yet, do recursive building *)
        let e0 = (e0,inv) in
        let e0 = exists supp e0 in
        let e1 = if inv then not_cnode e1 else e1 in
        let e1 = exists supp e1 in
        mkif_int man e0 v e1
      end
  in
  { man; node = exists supp t.node }


let forall support t =
  dnot (exists support (dnot t))

type support = ISet.t


let sat t =
  let visited = Hashtbl.create 2048 in

  let rec sat inv1 (node,inv2) =
    let inv = inv1 <> inv2 in
    match node, inv with
    | NFalse, false -> None
    | NFalse, true -> Some []
    | NIf(e0, v, e1, id), _ ->
      try
        Hashtbl.find visited (inv,id)
      with Not_found ->
        let res = match sat inv (e0,false) with
          | Some res ->
            Some ((false,v)::res)
          | None ->
            match sat inv e1 with
            | Some res ->
              Some ((true,v)::res)
            | None -> None
        in
        Hashtbl.replace visited (inv,id) res;
        res
  in
  sat false t.node

let bdd_of_cube man cube =
  List.fold_left (fun cube (pos,v) ->
      let v = ithvar man v in
      let v = if pos then v else dnot v in
      dand cube v
    ) (dtrue man) cube

let rec prime_cube incube cube t =
  match cube with
  | [] -> incube
  | (pos,v)::cube' ->
    let p1 = bdd_of_cube t.man incube in
    let p2 = bdd_of_cube t.man cube' in
    let cube = dand p1 p2 in
    (* test if cube without v is still an implicant *)
    let f = dand cube (dnot t) in
    if is_false f then
      (* it was not needed *)
      prime_cube incube cube' t
    else
      (* it was needed *)
      prime_cube ((pos,v)::incube) cube' t

let prime_cube cube t =
  prime_cube [] cube t

let prime torig t =
  match sat t with
  | None -> None
  | Some sat -> Some (prime_cube sat torig)

let rec iter_satisfiability satf f t =
  match satf t with
  | None -> ()
  | Some sat ->
    (* call on satisfying assignment *)
    f sat;
    (* block this satisfying assignment *)
    let cube = bdd_of_cube t.man sat in
    let blocking_clause = dnot cube in
    let t = dand blocking_clause t in
    iter_satisfiability satf f t

let all sat t =
  let res = ref [] in
  iter_satisfiability sat (fun sat -> res := sat :: !res) t;
  !res

let itersat f t = iter_satisfiability sat f t
let allsat t = all sat t
let iterprime f t = iter_satisfiability (prime t) f t
let allprime t = all (prime t) t


let prime t = prime t t

type 'a e =
  | False
  | True
  | Not of 'a
  | If of 'a * var * 'a


let fold f t =
  let visited = Hashtbl.create ((IfHashCons.length t.man.bdd_hc)*3/2) in
  let rec fold node =
    let id = ident_cnode node in
    begin try
        Hashtbl.find visited id
      with Not_found ->
        let res = match node with
          | (NFalse, false) ->
            f False
          | (NFalse, true) ->
            f True
          | (NIf(el, v, er, _), pol) ->
            let rl = fold (el,false) in
            let rr = fold er in
            let r = f (If (rl, v, rr)) in
            if pol then
              f (Not r)
            else
              r
        in
        Hashtbl.replace visited id res;
        res
    end
  in
  fold t.node

let permute perm t =
  let man = t.man in
  fold (function
      | False -> dfalse man
      | True -> dtrue man
      | Not e -> dnot e
      | If (l, v, r) ->
        ite l (if v < Array.length perm then perm.(v) else v) r
    ) t


