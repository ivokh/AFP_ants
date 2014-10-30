Koen Wermer         - 3705951
Sebastiaan Jong     - 5546303
Ivo Klein Hofmeijer - 3683311


CodeGenerator.hs
This module contains the library that we built to create ant code.

Algorithm.hs
This implements our ants algorithm using our library.
Because we overloaded combine and use it on a large number of arguments, we get a context reduction overflow error.
Compiling using the ghc flag -fcontext-stack40 solves this problem.

DescriptionCode.hs
This module implements the strategy explained in the task description, using our library.