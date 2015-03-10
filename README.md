OCaml Binary Decision Diagrams
==============================

The mlbdd library provides a simple, easy-to-use, easy-to-extend implementation
of binary decision diagrams (BDDs) in OCaml.  It is well tested and well
documented.  The library itself has no depdendencies and is thus easy to include
in applications that might, for example, be compiled with js_of_ocaml or other
tools that rely on pure OCaml.  It is also easier to integrate with existing
projects due to its lack of dependencies.

To build this library, ensure that OCaml 4.01 or greater and findlib is
installed and run

    make

To test this library, ensure that ocamlfind and ounit is installed and run

    make test

Examples of usage are included in the documentation.

Rationale
---------

There are currently several implementations of BDDs for OCaml.  These include
the following:

  - John Harrison: http://www.cl.cam.ac.uk/~jrh13/atp/OCaml/bdd.ml
  - Jean-Christophe Filliâtre: https://www.lri.fr/~filliatr/ftp/ocaml/bdd
  - Xavier Leroy: https://gforge.inria.fr/scm/viewvc.php/attic/xlsat/?root=sodiac

While there are many nice design aspects of these packages, there are also
significant probelms with all of them including lack of performance and API
limitations.  To solve these problems this library implements BDDs in OCaml
combining ideas from commercial-grade C BDD implementations with good ideas
from the above libraries.

There are two performance problems associated with the above BDD
implementations.  The first is indirection inefficiency and the second is
algorithmic inefficiency.  Indirection inefficiency arises when extra
indirections are introduced when they are not required.  The first BDD
implementaiton suffer from this problem due to the implementation of hash
consing that it uses.

Efficient Hash Consing
----------------------

Hash consing is required for the implementation of BDDs.  Hash consing ensures
that the same node in the diagram is only created once and that traversal
algorithms need not handle the same logical expression more than once.
Typically hash consing is implemented with a hash table, where the table maps
unique identifiers (usually integers) to nodes of the BDD.  For example:

    type node =
      | True
      | False
      | If of int * var * int

where the two integers are the unique identifiers for the left and the right
branches of the `If` node.  Then there is a hash table that maps these integers
into nodes:

    type hash = (int,node) Hashtbl.t

Unfortunately, when traversing this BDD, every dereference requires a hash table
lookup.  For example, to find the left child of an `If` node requires the following
sequence of operations:

  1. Fetch integer id from `If` data structure
  2. Find integer id in hash table
  3. Dereference the pointer found in the hash table

A much better solution is to have the type directly represented:

    type node =
      | True
      | False
      | If of node * var * node

This means that this extra hash table lookup, which is a significant operation can be
avoided:

  1. Fetch pointer from `If` data structure
  2. Dereference the pointer

The problem is getting the sharing that the hash table provided above without
losing direct pointers.  The solution to this is to add an extra field to each
(or at least recursive) nodes to give a unique id for that node and then to do
the appropriate dereferencing when constructing a node rather than when
accessing it.

    type node =
      | True
      | False
      | If of node * var * node * int

    type hash = (int * var * int, node) Hashtbl.t
    
Using this structure, the pointers are direct, but hash consing is still possible.
When constructing a new node, the ids of the two children are looked up and then
the hash of those ids and the var are used to determine if the node has already been
created.  If so, existing pointer is returned, otherwise a new node is constructed
and returned.

This solution, which is employed by the latter two BDDs improves the performance
of traversing BDDs without complicating the structure.  The exact structure employed
by mlbdd is borrowed from Xavier Leroy's BDD implementation in xlsat.


BDD Algorithm Improvements
--------------------------

The biggest performance improvement for the BDD structures comes from the
addition of complement edges. Complement edges enable each nested BDD to be
either a BDD or its complement.  This means that there is a potential size
improvement for BDDs.  Since the each BDD represents itself and its complement,
the total size of a BDD can require half the nodes.  More importantly,
complement edges mean that computing the complement of a BDD is an O(1)
operation instead of an O(n) operation where n is the number of nodes in the
BDD.  This reduction in complexity for negation makes many operations
significantly faster.

The trick with complement edges is ensuring canonicity.  To do this the form of
BDDs is changed to the following, where a BDD is a cnode.  

    type node =
      | False
      | If of node * var * cnode * int

    and cnode = bool * node

There are several simplifications that come out of this form.  First, there is
no `True` anymore.  The `False` node complemented gives a representation of
true, so, `True` was redundant.  Second, complement edges are not allowed
everwhere.  Note that a BDD is a `cnode`, but `If` take a `node` and a `cnode`.
The restriction here is that the negative edge out of a node cannot be
complemented.  This can be guaranteed by the following conversion:


        |               |
        |               o
        |               |
        V      ==>      V
       / \             / \
      o   \           /   o
     /     \         /     \
    V0     V1       V0     V1


Where `o` is used on an edge to represent complement.

The resulting BDD implementation is significantly faster, and because
complement is essentially free, most operations can be implemented in a simpler
fashion using combinations of other operations without the typical overhead of
all of the negation operations that are required.  For example, using just the
`and` operation it is efficient and easy to provide `or`, `nand`, `nor`, and
`implies`.

