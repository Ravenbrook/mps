
.. index::
   single: hash array; introduction

Hash arrays
===========

The :term:`location dependency` feature of the MPS allows the
:term:`client program` to implement address-based hash tables in the
presence of a :term:`moving memory manager`, re-hashing the tables
when the addresses they contain might have moved.

However, when a frequently-used hash table grows large enough, the following sequence of events may take place:

1. The hash table discovers that its location dependency is stale.

2. A new array is allocated to contain the re-hashed keys.

3. The new array is large enough to push the *new size* of the
   :term:`nursery space` (that is, the amount of newly allocated
   memory since the last collection in the first :term:`generation` in
   the :term:`generation chain` for the pool containing the array)
   close to its capacity.

4. A small amount of additional allocation causes the new size of the
   nursery generation to exceed its capacity, which causes the MPS to
   start a new collection of that generation. This in turn causes the
   hash table to become stale again.

When the hash table reaches this critical size, the client program may find that a large fraction of its time is being spent re-hashing the table.

In order to avoid this happening, the MPS provides a mechanism allowing you specify that the newly allocated array does not contibute to the new size of the nursery space: this cuts off the vicious cycle at step 3.

See :ref:`topic-collection-schedule` for an explanation of the *new
size* of a generation, and how the MPS uses this to determine when to
start a collection of that generation.


.. index::
   single: hash array; interface

Hash array interface
--------------------

::

    #include "mpscvm.h"

.. c:macro:: MPS_KEY_AP_HASH_ARRAYS

    A :term:`keyword argument` (of type :c:type:`mps_bool_t`,
    defaulting to false) to :c:func:`mps_ap_create_k`. If true, it
    specifies that blocks allocated from the allocation point do not
    contribute to the *new size* of the :term:`nursery space` for the
    purposes of deciding whether to start a collection of that
    generation.

    .. note::

        This keyword argument is only supported by :ref:`pool-amc`
        pools.
