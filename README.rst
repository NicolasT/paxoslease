PaxosLease
==========
The `paxoslease` package provides a Haskell_ implementation of the PaxosLease_
protocol. It is a model-only implementation, which means at its core it doesn't
perform any networking, persistence,... It's up to the user to provide an
interpreter providing these services.

A demo application is included for reference.

.. _Haskell: http://haskell.org
.. _PaxosLease: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.370.8775
