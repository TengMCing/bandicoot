
# bandicoot 0.1.0

* Added a `NEWS.md` file to track changes to the package.

# bandicoot 0.2.0

* Introduce `super()` to the package.

# bandicoot 1.0.0

* First release

# bandicoot 1.0.1

* Introduce `make_instantiator()` to the package. This is a convenient function to make wrapper for instantiation method.
* Add a message in `print.bandicoot_oop()` pointing users to the documentation.
* Add a new argument `doc_pkg` to `new_class()` for specifying the Name of the package providing the class documentation, referenced as `?{doc_pkg}::{class}`, where `{class}` is the class name. A new attribute `..doc_pkg..` is added to store this information.
* `BASE` is now constructed with `doc_pkg = "bandicoot"`.
