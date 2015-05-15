(**

  {1 OCaml BDD }

  A library for reduced ordered binary decision diagrams (ROBDDs) written in OCaml

  {2 Introduction}

  This is a simple library that implements reasonably efficient binary decision
  diagrams in OCaml.  A binary decision diagram (BDD) is a canonical
  representation of a boolean formula.  It represents the standard elements
  including and, or, not, variables, and the constants true and false.

  This is intended to serve as a nice middle ground between very primitive BDD
  implementations written in OCaml, and OCaml wrappers around high-performance
  C/C++ libraries.  What differentiates this implementation from other simple
  implementations is the tracking of complement edges.  This means that the not
  operation ([dnot]) has O(1) complexity, as opposed to O(n) complexity, where
   n is the number of nodes in the representation of the BDD (potentially
   exponential with respect to the number of variables).

  The API of this library is styled after mlcuddidl, which is a wrapper around
  the high-performance CUDD library.  The mlcuddidl library is available
  {{:https://gforge.inria.fr/projects/mlxxxidl/}here}.

  This implementation of BDDs is developed and maintained by Arlen Cox and is
  made available under the MIT license.

  {2 Example}

  All of the operations on BDDs are performed with respect to a manager.  A
  manager is responsible for keeping track of tables of all of the allocated
  BDD nodes across all of the BDDs created from that manager.  BDDs from two
  different managers must never be mixed.
  
  To create a manager, the [init] function below can be used.  The following
  code constructs a manager [man] and constructs a BDD representing the 0th
  variable.

{[
let man = init ~cache:1024 () in
let v0 = ithvar man 0 in
]}

  Variables, are represented by integers.  Lower numbers are closer to the root
  of the decision tree and higher numbers are closer to the leaves of the
  decision tree.  We have already seen the variable 0 constructed above, but
  constructing the variable 0 again (even if we bind it to a different OCaml name)
  is equal to the original variable 0:

{[
let v1 = ithvar man 0 in
assert(equal v0 v1)
]}

  This is because BDDs are canonical.  Two formulas that are logically equivalent
  have a single representation in the BDD, no matter how they are constructed.

  Formulas can be constructed by calling constructor functions such [dtrue],
  which constructs the formula representing {i true}.  Similarly, the formula
  that represents the conjunction of two variables [a] and [b] can be
  constructed in the following way:

{[
let a = ithvar man 0 in
let b = ithvar man 1 in
let ab = dand a b in
]}

  Furthermore, it is possible to constrain that three variables are equal to
  each other:
  
{[
let a_eq_b = eq a b in
let c = ithvar man 2 in
let b_eq_c = eq b c in
let eq_abc = dand a_eq_b b_eq_c in
]}

  Because this is a canonical representation, it wouldn't matter if [b_eq_c]
  were constructed with [eq c b], [eq a c], or [eq c a].  The result would
  always be the same.

{[
let eq_abc2 = dand a_eq_b (eq c b) in
assert(equal eq_abc eq_abc2);
let eq_abc3 = dand a_eq_b (eq a c) in
assert(equal eq_abc eq_abc3);
let eq_abc4 = dand a_eq_b (eq c a) in
assert(equal eq_abc eq_abc4);
]}

  Additionally, BDDs support quantification, so we can existentially quantify
  out [b] from [eq_abc] to get that [a] = [c]:

{[
let eq_ac = exists (support b) eq_abc in
assert(equal eq_ac (eq a c))
]}

  Finally, there are some satisfiability and optimization options.  For example,
  we can determine that [eq_ac] is satisfiable.

{[
assert(sat eq_ac <> None);
]}

  Additionally, it is possible to iterate over a number of satisfying
  assignments:

{[
itersat (fun sat ->
      print_endline "-----------"
        List.iter (fun (is_pos,v) ->
            if not is_pos then
              print_string "~";
            print_endline (string_of_int v)
          ) sat
    ) eq_ac
]}

  Similarly, it is possible to iterate over and print out all of the prime
  implicants of the formula.

{[
iterprime (fun sat ->
      print_endline "-----------"
        List.iter (fun (is_pos,v) ->
            if not is_pos then
              print_string "~";
            print_endline (string_of_int v)
          ) sat
    ) eq_ac
]}
  

*)

(** {2 API Documentation } *)

(** Variables have type [var], which is an integer *)
type var = int

(** [man] is the type of a BDD manager that is passed to primitive constructors
    [ithvar], [dtrue], and [dfalse] *)
type man

(** [t] is the type of a BDD.  It is the root of a directed acyclic graph that
    represents the formula *)
type t

(** [suppor] is the type of support for a BDD.  A support is the set of
    variables that are needed to represent a formula *)
type support

(** {3 Manager Manipulation and Meta-Functionality} *)

(**
  [init ()] creates a new BDD manager with a default cache size.

  [init ~cache:n ()] creates a new BDD manager with a cache of size [n].

  Larger caches require fewer growth steps, where the cache must be resized and
  memory must be realllocated. *)
val init : ?cache:int -> unit -> man

(** [clear man] clears all of the caches in the BDD.  BDDs are still accessible
    after clearing the cache, but canonicity may be lost.  It is recommended
    that this command not be used unless necessary.  It is likely much safer to
    allocate a new manager than to clear the cache of an existing one. *)
val clear : man -> unit

(** [manager t] retrieves the manager for the BDD expression [t]. *)
val manager : t -> man

(** [support t] computes the supporting variable set for the bdd [t]. *)
val support : t -> support

(** [list_of_support support] converts a support structure [support] into
    a list of variable indices that constitute the support *)
val list_of_support : support -> int list

(** [string_of_support support] creates a string representation of a supporting
    set. *)
val string_of_support : support -> string

(** {3 Queries} *)

(** [is_true t] returns true if the BDD is the formula {i true}. *)
val is_true : t -> bool

(** [is_false t] returns true if the BDD is the formula {i false}. *)
val is_false : t -> bool

(** [equal t1 t2] returns true if the two BDDs represent the same function. *)
val equal : t -> t -> bool

(** {3 Constructors} *)


(** [dtrue man] returns the BDD representing the formula {i true}. *)
val dtrue : man -> t

(** [dtrue man] returns the BDD representing the formula {i false}. *)
val dfalse : man -> t

(** [ithvar man v] returns th BDD representing the variable [v].  If [v] is {i
    true}, the formula is {i true}, otherwise if [v] is {i false}, the formula
    is {i false} *)
val ithvar : man -> var -> t

(** [dnot t] returns the complement or negation of the BDD [t]. *)
val dnot : t -> t

(** [dand t1 t2] returns the BDD representing the conjunction of [t1] and [t2]. *)
val dand : t -> t -> t

(** [dor t1 t2] returns the BDD representing the disjunction of [t1] and [t2]. *)
val dor : t -> t -> t


(** [dor t1 t2] returns the BDD representing the negation of the conjunction of
    [t1] and [t2]. *)
val nand : t -> t -> t

(** [dxor t1 t2] returns the BDD representing the exclusive-or of [t1] and
    [t2]. *)
val xor : t -> t -> t

(** [nxor t1 t2] returns the BDD representing the negation of the exclusive-or
    of [t1] and [t2]. *)
val nxor : t -> t -> t

(** [eq t1 t2] returns the BDD that is {i true} whenever [t1] is equal to [t2]
    (equivalent to [nxor]) *)
val eq : t -> t -> t

(** [ite f v t] returns the bdd that is [f] whenever the variable [v] is false
    and [t] whenever the variable [v] is true *)
val ite : t -> var -> t -> t

(** [imply t1 t2] returns the BDD that represents [t1] implies [t2] *)
val imply : t -> t -> t

(** [exists support t] returns the BDD where each variable in [support] has
    been existentially quantified in [t] *)
val exists : support -> t -> t

(** [forall support t] returns the BDD where each variable in [support] has
    been universally quantified in [t] *)
val forall : support -> t -> t

(** [cofactor v t] returns the negative and positive cofactor of [t] with
    respect to variable [v] *)
val cofactor : var -> t -> t * t

(** [permute p t] permutes the variables in the BDD [t] using the permutation
    array [p].  At each variable's index in [p] is the new index for that
    variable. *)
val permute : var array -> t -> t

(** [permute f t] permutes teh variables in the BDD [t] using the permutation
    function [f].  The function returns a new index for each index. *)
val permutef : (var -> var) -> t -> t

(** {3 Iteration} *)

type 'a e =
  | False               (** the false BDD *)
  | True                (** the true BDD *)
  | Not of 'a           (** not result *)
  | If of 'a * var * 'a (** result from false branch, variable, result from
                            true branch *)

(** [fold f t] folds over the BDD's structure calling [f] on each node in the
    BDD. *)
val fold : ('r e -> 'r) -> t -> 'r

(** {3 Satisfiability and Primality Queries} *)

(** [sat t] returns [None] if unsatisfiable, otherwise it returns a list of
    assignments and the corresponding variable that would make the formula {i
    true}. Note that this may not be a complete satisfying assignment.  Not
    every variable int the support is necessarily included. *)
val sat : t -> (bool * var) list option


(** [allsat t] returns the complete list of satisfying assignments that could
    possibly be returned by [sat]. *)
val allsat : t -> (bool * var) list list

(** [itersat f t] iterates over the complete list of satisfying assignments that
    could possibly be returned by [sat], calling [f] on each one *)
val itersat : ((bool * var) list -> unit) -> t -> unit

(** [prime  t] returns [None] if unsatisfiable, otherwise it returns a prime
    implicant of the formula [t].  A prime implicant is is represented as a list
    of polarities and variables.  If the polarity is [true], then the variable
    appears unnegated in the implicant.  If the polarity is [false], then the
    variable appears negated in the implicant. *)
val prime : t -> (bool * var) list option

(** [allprime t] returns the list of all prime implicants of the formula [t].
  A prime implicant is is represented as a list of polarities and variables.
  If the polarity is [true], then the variable appears unnegated in the
  implicant.  If the polarity is [false], then the variable appears negated in
  the implicant. *)
val allprime : t -> (bool * var) list list

(** [iterprime f t] iterates over the complete list of prime implicants could
    possibly be returned by [allprime], calling [f] on each one *)
val iterprime : ((bool * var) list -> unit) -> t -> unit

(** {3 String Conversion} *)

(** [to_string t] returns a string representation of the BDD.  It represents
  the internal structure the following way: [~] represents negation of a
  variable for a term. [F] represents the {i false} terminal. [T] represents
  the {i true} terminal.  An If-Then-Else structure is represented as a 4-tuple
  (var, if_false, if_true, id), where if_false is the BDD if the var is false
  and if_true is the BDD if the var is true.  Id is the unique identifier for
  this BDD in the cache.  *)
val to_string : t -> string

(** [to_stringb t] returns a similar string representation, but it removes
    complement edges, so that the BDD is shows as the formula would be
    represented in traditional If-Then-Else normal form *)
val to_stringb : t -> string



