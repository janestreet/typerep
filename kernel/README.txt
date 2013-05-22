The kernel is a reduced part of the library that is standalone.
It does not depend on sexp, bin_io unlike the typerep library.
The separation is done in the hope to allow more flexibility in
using that library without necessarly pulling too much dependencies
from janestreet libraries.
