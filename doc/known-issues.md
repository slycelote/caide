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

* Integral constants get removed even if they are used as a template argument: 
``` 
const int N = 42; 
template<int n> func() {} 
func<N>(); 
```

