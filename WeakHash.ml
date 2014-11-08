module type S = sig
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

module Make(H: Hashtbl.HashedType) : S with type key = H.t = struct
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
