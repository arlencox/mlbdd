(* Binary decision diagrams *)


module type WHS = sig
  type key
  type 'a t

  val create : int -> 'a t

  val replace : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val clear : 'a t -> unit
  val length : 'a t -> int
end

module WeakHash(H: Hashtbl.HashedType) : WHS with type key = H.t = struct
  type key = H.t

  type 'a t = {
    mutable weak: 'a Weak.t;
    mutable key: (H.t * int) option array;
    mutable length: int;
  }

  let create init_size =
    let init_size =
      if init_size > Sys.max_array_length - 1 then
        Sys.max_array_length - 1
      else
        init_size
    in
    {
      weak = Weak.create init_size;
      key = Array.make init_size None;
      length = 0;
    }

  let insert_raw t h k v =
    let len = Array.length t.key in
    let i = h mod len in
    let rec insert_index i =
      match t.key.(i) with
      | None ->
        t.key.(i) <- Some (k, h);
        Weak.set t.weak i (Some v);
        t.length <- t.length + 1
      | Some _ ->
        insert_index ((i + 1) mod len)
    in
    insert_index i

  let resize t =
    let new_alloc = Array.length t.key * 2 in
    let t' = create new_alloc in
    Array.iteri (fun i -> function
        | None -> ()
        | Some (k, h) ->
          match Weak.get t.weak i with
          | None -> ()
          | Some ptr ->
            insert_raw t' h k ptr
      ) t.key;
    t.weak <- t'.weak;
    t.key <- t'.key;
    t.length <- t'.length

  let replace t k v =
    if t.length + 1 > Array.length t.key lsr 1 then
      resize t;
    insert_raw t (H.hash k) k v

  let find_raw t k =
    let len = Array.length t.key in
    let h = H.hash k in
    let rec find_index i =
      let i = i mod len in
      match t.key.(i) with
      | None ->
        raise Not_found
      | Some (k',h') ->
        if h == h' && H.equal k k' then
          match Weak.get t.weak i with
          | None ->
            find_index (i+1)
          | Some v -> (v,i)
        else
          find_index (i+1)
    in
    find_index h

  let remove t k =
    try
      let i = snd (find_raw t k) in
      let len = Array.length t.key in
      let rec accum acc i =
        let i = i mod len in
        match t.key.(i) with
        | None -> acc
        | Some (k,h) ->
          t.key.(i) <- None;
          t.length <- t.length - 1;
          match Weak.get t.weak i with
          | None -> accum acc (i+1)
          | Some v -> accum ((k,h,v)::acc) (i+1)
      in
      t.key.(i) <- None;
      t.length <- t.length - 1;
      let acc = accum [] (i+1) in
      List.iter (fun (k,h,v) -> insert_raw t h k v) acc
    with Not_found ->
      ()

  let find t k =
    fst (find_raw t k)

  let mem t k =
    try
      ignore(find_raw t k);
      true
    with Not_found ->
      false

  let iter f t =
    Array.iteri (fun i -> function
        | None -> ()
        | Some (k,_h) ->
          match Weak.get t.weak i with
          | None -> ()
          | Some v ->
            f k v
      ) t.key

  let clear t =
    Weak.fill t.weak 0 (Weak.length t.weak) None;
    Array.fill t.key 0 (Array.length t.key) None;
    t.length <- 0

  let length t =
    t.length
end

module Raw = struct
  (* Low-level implementation of BDDs *)

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

  module IfHashCons = WeakHash(HashedIf)

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

  let flush ctx =
    Hashtbl.clear ctx.and_cache;
    Hashtbl.clear ctx.xor_cache


  let init ?cache:(init=1002403) () = {
    bdd_hc = IfHashCons.create init;
    bdd_hc_stamp = ref 2;
    and_cache = Hashtbl.create init;
    xor_cache = Hashtbl.create init;
  }

  type t = cnode

  let id = function
    | (NIf (e0,v,e1,id), true) -> id + 1
    | (NIf (e0,v,e1,id), false) -> id
    | (NFalse, true) -> -1
    | (NFalse, false) -> -2

  let hash = id

  let equal t1 t2 =
    match t1, t2 with
    | (NFalse, a),(NFalse, b) when a == b -> true
    | (NIf(_,_,_,ida), a),(NIf(_,_,_,idb), b) when ida == idb && a == b -> true
    | _ -> false

  let dtrue = (NFalse, true)

  let dfalse = (NFalse, false)

  let is_true t =
    t = (NFalse, true)

  let is_false t =
    t = (NFalse, false)

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
  let mkif_int man (e0: cnode) v (e1: cnode) : cnode =
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

  let rec to_string (e,inv) =
    let s = match e with
      | NFalse -> "F"
      | NIf(e0, v, e1, id) ->
        let s0 = to_string (e0,false) in
        let s1 = to_string e1 in
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
    mkif_int man (NFalse,false) var (NFalse,true)

  let dnot t = not_cnode t

  let rec dand man t1 t2 =
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
              mkif_int man (dand man e0a e0b) va (dand man e1a e1b)
            else if c < 0 then
              mkif_int man (dand man e0b t1) vb (dand man e1b t1)
            else
              mkif_int man (dand man e0a t2) va (dand man e1a t2)
          in
          Hashtbl.replace man.and_cache key f;
          f
      end

  let rec xor man t1 t2 =
    let ida = ident_cnode t1 in
    let idb = ident_cnode t2 in
    if ida == idb then
      (NFalse, false)
    else if ida == ident_cnode (dnot t2) then
      (NFalse, true)
    else match t1, t2 with
      | (NFalse, false), other
      | other, (NFalse, false) ->
        other
      | (NFalse, true), other
      | other, (NFalse, true) ->
        dnot other
      | (NIf(e0a, va, e1a, _),inva), (NIf(e0b, vb, e1b, _),invb) ->
        let key = if ida < idb then (ida, idb) else (idb, ida) in
        try
          Hashtbl.find man.xor_cache key
        with Not_found ->
          let e0a = (e0a,inva) in
          let e1a = if inva then not_cnode e1a else e1a in
          let e0b = (e0b,invb) in
          let e1b = if invb then not_cnode e1b else e1b in
          let c = vb - va in
          let f =
            if c = 0 then
              mkif_int man (xor man e0a e0b) va (xor man e1a e1b)
            else if c < 0 then
              mkif_int man (xor man e0b t1) vb (xor man e1b t1)
            else
              mkif_int man (xor man e0a t2) va (xor man e1a t2)
          in
          Hashtbl.replace man.xor_cache key f;
          f

     

  let to_stringb t =
    to_stringb_int false t

  let dor man t1 t2 =
    dnot (dand man (dnot t1) (dnot t2))

  let nand man t1 t2 =
    dnot (dand man t1 t2)

  let imply man a b =
    dor man (dnot a) b

  let eq man a b =
    dnot (xor man a b)

  let nxor = eq
  (*let eq man a b =*)
    (*dand man (imply man a b) (imply man b a)*)

  (*let nxor = eq*)

  (*let xor man a b = dnot (nxor man a b)*)


  let rec ite man f var t =
    let v = ithvar man var in
    dand man (imply man v t) (imply man (dnot v) f)

  (*let rec ite man f g h =
    let idf = ident_cnode f in
    let idg = ident_cnode g in
    let idng = ident_cnode (dnot g) in
    let idh = ident_cnode h in
    let idnh = ident_cnode (dnot h) in
    match idf, idg, idh with
    | 1, _, _ -> g
    | 0, _, _ -> h
    | _, 1, _
    | idf, idg, _ when idf == idg ->
      if idh == 0 then
        f
      else
        dand man (dnot f) (dnot h)
    | _, 0, _
    | idf, idg, _ when idf == (idg lxor 1) ->


    

    if idf == 1 then
      g
    else if idf == 0 then
      h
    else if g == 1 || idf == idg then
      if idh == 0 then
        f
      else
        dand man (dnot f) (dnot h)
    else if idg == 0 || fid == idng then
      if idh == 1 then
        dnot f
      else
        dand man (dnot f) h
    else if idh == 0 || idf == idh then
      dand man f g
    else if idh == 1 || idf == idnh then
      dand man f (dnot g)
    else if idg == idh then
      g
    else if idg == idnh then
      xor man f h
    else


    if f = (NFalse, true) then
      g
    else if f = (NFalse, false) then
      h
    else if g = (NFalse, true) || 

  let rec ite man h f g =
    if f = (NFalse, true) then


    match f, t with
    | (NFalse, _), (NFalse, _) ->
      mkif_int man f var t
    | (NFalse, _), (NIf(e0b, vb, e1b, _), invb) ->
      if vb <
      
    let ida = ident_cnode t1 in
    let idb = ident_cnode t2 in
    if ida == idb then
      (NFalse, false)
    else if ida == ident_cnode (dnot t2) then
      (NFalse, true)
    else match t1, t2 with
      | (NFalse, false), other
      | other, (NFalse, false) ->
        other
      | (NFalse, true), other
      | other, (NFalse, true) ->
        dnot other
      | (NIf(e0a, va, e1a, _),inva), (NIf(e0b, vb, e1b, _),invb) ->
        let key = if ida < idb then (ida, idb) else (idb, ida) in
        try
          Hashtbl.find man.xor_cache key
        with Not_found ->
          let e0a = (e0a,inva) in
          let e1a = if inva then not_cnode e1a else e1a in
          let e0b = (e0b,invb) in
          let e1b = if invb then not_cnode e1b else e1b in
          let c = vb - va in
          let f =
            if c = 0 then
              mkif_int man (xor man e0a e0b) va (xor man e1a e1b)
            else if c < 0 then
              mkif_int man (xor man e0b t1) vb (xor man e1b t1)
            else
              mkif_int man (xor man e0a t2) va (xor man e1a t2)
          in
          Hashtbl.replace man.xor_cache key f;
          f*)


  let cofactor man v t =
    let visited = Hashtbl.create ((IfHashCons.length man.bdd_hc)*3/2) in
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
                let r0 = mkif_int man e00 vc e10 in
                let r1 = mkif_int man e01 vc e11 in
                (r0, r1)
            in
            Hashtbl.replace visited id res;
            res
        end
      | res -> res,res
    in
    cofactor t



  module Int = struct
    type t = int
    let compare a b = b - a
  end

  module ISet = Set.Make(Int)



  let support man t =
    let visited = Hashtbl.create ((IfHashCons.length man.bdd_hc)*3/2) in
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
    itersupport t;
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

  let support_of_list l =
    List.fold_left (fun s e -> ISet.add e s) ISet.empty l


  let exists_sorted man supp t =
    let visited = Hashtbl.create 1023 in
    let rec exists_inner man supp (t: cnode) =
      match t,supp with
      | (NFalse, _), _ -> t
      | _, [] -> t
      | (NIf(e0, v, e1, id), inv), h::rest ->
        try
          Hashtbl.find visited (id,inv)
        with Not_found ->
          let c = h - v in
          let result = if c = 0 then begin
              let e0 = (e0,inv) in
              let e0 = exists_inner man rest e0 in
              let e1 = if inv then not_cnode e1 else e1 in
              let e1 = exists_inner man rest e1 in
              let e0 = not_cnode e0 in
              let e1 = not_cnode e1 in
              let not_res = dand man e0 e1 in
              not_cnode not_res
            end else if c < 0 then begin
              (* v > h: already passed h, so skip h *)
              exists_inner man rest t
            end else begin
              (* h > v: not there yet, do recursive building *)
              let e0 = (e0,inv) in
              let e0 = exists_inner man supp e0 in
              let e1 = if inv then not_cnode e1 else e1 in
              let e1 = exists_inner man supp e1 in
              mkif_int man e0 v e1
            end
          in
          Hashtbl.replace visited (id,inv) result;
          result
    in
    exists_inner man supp t

  let exists man support t =
    let supp = List.sort (fun a b -> a - b) support in
    exists_sorted man supp t

  let forall_sorted man supp t =
    dnot (exists_sorted man supp (dnot t))

  let forall man support t =
    dnot (exists man support (dnot t))


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
    sat false t

  let bdd_of_cube man cube =
    List.fold_left (fun cube (pos,v) ->
        let v = ithvar man v in
        let v = if pos then v else dnot v in
        dand man cube v
      ) dtrue cube

  let rec prime_cube man incube cube t =
    match cube with
    | [] -> incube
    | (pos,v)::cube' ->
      let p1 = bdd_of_cube man incube in
      let p2 = bdd_of_cube man cube' in
      let cube = dand man p1 p2 in
      (* test if cube without v is still an implicant *)
      let f = dand man cube (dnot t) in
      if is_false f then
        (* it was not needed *)
        prime_cube man incube cube' t
      else
        (* it was needed *)
        prime_cube man ((pos,v)::incube) cube' t

  let prime_cube man cube t =
    prime_cube man [] cube t

  let prime man torig t =
    match sat t with
    | None -> None
    | Some sat -> Some (prime_cube man sat torig)

  let rec iter_satisfiability man satf f t =
    match satf t with
    | None -> ()
    | Some sat ->
      (* call on satisfying assignment *)
      f sat;
      (* block this satisfying assignment *)
      let cube = bdd_of_cube man sat in
      let blocking_clause = dnot cube in
      let t = dand man blocking_clause t in
      iter_satisfiability man satf f t

  let all man sat t =
    let res = ref [] in
    iter_satisfiability man sat (fun sat -> res := sat :: !res) t;
    !res

  let itersat man f t = iter_satisfiability man sat f t
  let allsat man t = all man sat t
  let iterprime man f t = iter_satisfiability man (prime man t) f t
  let allprime man t = all man (prime man t) t


  let prime man t = prime man t t

  type 'a e =
    | False
    | True
    | Not of 'a
    | If of 'a * var * 'a

  let inspect = function
    | (NFalse, false) -> False
    | (NFalse, true) -> True
    | (_, true) as t -> Not (dnot t)
    | (NIf (e0, v, e1, _), false) ->
      If ((e0, false), v, e1)

  type 'a b =
    | BFalse
    | BTrue
    | BIf of 'a * var * 'a

  let inspectb = function
    | (NFalse, false) -> BFalse
    | (NFalse, true) -> BTrue
    | (NIf (e0, v, e1, _), true) ->
      BIf ((e0, true), v, (dnot e1))
    | (NIf (e0, v, e1, _), false) ->
      BIf ((e0, false), v, e1)

  type 'a fold = (int, 'a) Hashtbl.t

  let fold_init man : 'a fold =
      Hashtbl.create ((IfHashCons.length man.bdd_hc)*3/2)


  let rec fold_cont visited man f node =
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
            let rl = fold_cont visited man f (el,false) in
            let rr = fold_cont visited man f er in
            let r = f (If (rl, v, rr)) in
            if pol then
              f (Not r)
            else
              r
        in
        Hashtbl.replace visited id res;
        res
    end

  let fold man f t =
    let visited = fold_init man in
    fold_cont visited man f t

  let rec foldb_cont visited man f node =
    let id = ident_cnode node in
    begin try
        Hashtbl.find visited id
      with Not_found ->
        let res = match node with
          | (NFalse, false) ->
            f BFalse
          | (NFalse, true) ->
            f BTrue
          | (NIf(el, v, er, _), pol) ->
            let rl = foldb_cont visited man f (el, pol) in
            let rr = foldb_cont visited man f (if pol then dnot er else er) in
            f (BIf (rl, v, rr))
        in
        Hashtbl.replace visited id res;
        res
    end

  let foldb man f t =
    let visited = fold_init man in
    foldb_cont visited man f t

  let permute man perm t =
    fold man (function
        | False -> dfalse
        | True -> dtrue
        | Not e -> dnot e
        | If (l, v, r) ->
          ite man l (if v < Array.length perm then perm.(v) else v) r
      ) t

  let permutef man f t =
    fold man (function
        | False -> dfalse
        | True -> dtrue
        | Not e -> dnot e
        | If (l, v, r) ->
          ite man l (f v) r
      ) t
end


type var = int

type man = Raw.man 

let clear ctx = 
  Raw.clear ctx

let flush ctx =
  Raw.flush ctx

let init ?cache:(init=1002403) () =
  Raw.init ~cache:init ()

type t = {
  man: man;
  node: Raw.t;
}

let manager t = t.man

let equal t1 t2 =
  t1.man == t2.man &&
  Raw.equal t1.node t2.node

let dtrue man = {
  man = man;
  node = Raw.dtrue;
}

let dfalse man = {
  man = man;
  node = Raw.dfalse;
}

let is_true t =
  Raw.is_true t.node

let is_false t =
  Raw.is_false t.node

let ithvar man var =
  { man; node = Raw.ithvar man var }

let dnot t =
  {t with node = Raw.dnot t.node}

let dand t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.dand man t1 t2 }

let to_string t =
  Raw.to_string t.node

let to_stringb t =
  Raw.to_stringb t.node

let dor t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.dor man t1 t2 }

let nand t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.nand man t1 t2 }

let nxor t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.nxor man t1 t2 }

let eq = nxor

let xor t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.xor man t1 t2 }


let imply t1 t2 =
  let man = t1.man in
  let t1 = t1.node in
  let t2 = t2.node in
  { man; node = Raw.imply man t1 t2 }

let ite f var t =
  let man = f.man in
  let f = f.node in
  let t = t.node in
  { man; node = Raw.ite man f var t }

let cofactor v t =
  let (r0, r1) = Raw.cofactor t.man v t.node in
  ({t with node = r0},{t with node = r1})


let support t =
  Raw.support t.man t.node

let string_of_support s =
  Raw.string_of_support s

let list_of_support s =
  Raw.list_of_support s

let support_of_list l =
  Raw.support_of_list l

let exists support t =
  let supp = Raw.ISet.fold (fun a b -> a::b) support [] in
  (*let supp = list_of_support support in*)
  { t with node = Raw.exists_sorted t.man supp t.node }


let forall support t =
  let supp = Raw.ISet.fold (fun a b -> a::b) support [] in
  { t with node = Raw.forall_sorted t.man supp t.node }

type support = Raw.support

let sat t =
  Raw.sat t.node

let prime t =
  Raw.prime t.man t.node

let itersat f t = Raw.itersat t.man f t.node
let allsat t = Raw.allsat t.man t.node
let iterprime f t = Raw.iterprime t.man f t.node
let allprime t = Raw.allprime t.man t.node

type 'a e = 'a Raw.e =
  | False
  | True
  | Not of 'a
  | If of 'a * var * 'a

type 'a b = 'a Raw.b =
  | BFalse
  | BTrue
  | BIf of 'a * var * 'a

let id t =
  Raw.id t.node

let hash = id

let inspect t =
  match Raw.inspect t.node with
  | False -> False
  | True -> True
  | Not a -> Not {t with node = a}
  | If (e0, v, e1) -> If ({t with node = e0}, v, {t with node = e1}) 

let inspectb t =
  match Raw.inspectb t.node with
  | BFalse -> BFalse
  | BTrue -> BTrue
  | BIf (e0, v, e1) -> BIf ({t with node = e0}, v, {t with node = e1}) 

type 'a fold = 'a Raw.fold

let fold_init man =
  Raw.fold_init man

let fold_cont visited f t =
  Raw.fold_cont visited t.man f t.node

let foldb_cont visited f t =
  Raw.foldb_cont visited t.man f t.node

let fold f t =
  Raw.fold t.man f t.node

let foldb f t =
  Raw.foldb t.man f t.node

let permute perm t =
  { t with node = Raw.permute t.man perm t.node }

let permutef f t =
  { t with node = Raw.permutef t.man f t.node }
