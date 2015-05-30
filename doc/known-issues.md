# Known issues

## C++ code inliner

* Defining a function without return type (typically used with `main`) is not
  supported.
* Defining a class and a variable of the same class simultaneously (`struct A
  {} a;`) is not supported.
* Unused global variables get removed. This is fine, unless you use such a
  variable for side effects. In that case, mark the variable with a comment
`/// caide keep` or `/** caide keep **/` (note the triple slash and the double
asterisk). For example,   
``` 
struct SideEffect {...};   
/// caide keep   
SideEffect instance; 
```

  In general, if you find that caide removes a declaration by mistake, mark
this declaration with `caide keep` (and file an issue :)).

* On Linux, if you use system headers, then caide will not be able to parse
  one of GNU-specific headers from `pb_ds` namespace. Workaround: either apply
[this small
change](https://github.com/slycelote/caide/commit/699f47ee267ea657b8b719cf3fb2d87337596ec3)
directly in your system (file path is
`/usr/include/c++/4.8/ext/pb_ds/detail/bin_search_tree_/traits.hpp` or
similar), or use MinGW headers coming with caide (initialize caide with `caide
init`, without `--cpp-use-system-headers` flag).


