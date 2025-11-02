# caide
Automates certain common tasks that you do during programming competitions:
parsing, running and debugging problem tests, inlining library code.

* Parses problem statement and extracts test cases
* Generates solution scaffold
* Inlines library code and prepares a single source file for submission
* Runs the tests on your solution
* Assists in debugging the tests

caide is designed to support multiple programming languages and IDEs/editors.

This project is available at the following mirrors:

* [Codeberg](https://codeberg.org/slycelote/caide)
* [GitHub mirror](https://github.com/slycelote/caide)

## Screenshots
Codelite and command line caide in Linux: ([full size](http://i.imgur.com/cZsP6Z0.png))

![ ](http://i.imgur.com/cZsP6Z0l.png)

Codelite in Windows: ([full size](http://i.imgur.com/mLmQDgD.png))

![ ](http://i.imgur.com/mLmQDgDl.png)

Visual Studio:

![ ](http://i.imgur.com/dSy9CQO.png)

## Download

caide is split into the following components (click the links to learn more):

* [caide](https://codeberg.org/slycelote/caide/src/branch/master/libcaide/README.md)
  is core command line application implementing all functionality. Windows and
Linux are supported. Theoretically, it should also build on OS X.
* [VsCaide](https://codeberg.org/slycelote/caide/src/branch/master/vscaide/README.md)
  is caide frontend (extension) for Visual Studio; currently supports C++ and
C# programming languages.
* [tccaide](https://codeberg.org/slycelote/caide/src/branch/master/tccaide/README.md)
  is a plugin for Topcoder arena required if you want to work with Topcoder
problems.

Download caide from [releases
page](https://github.com/slycelote/caide/releases). You will need *either* the
command line executable for your platform, *or* the Visual Studio extension.
In addition, if you want Topcoder support, you will need the Arena plugin.

## Documentation

* [Features](https://codeberg.org/slycelote/caide/src/branch/master/doc/features.md)

* [Known
  issues](https://codeberg.org/slycelote/caide/src/branch/master/doc/known-issues.md)

* [Command line
  application](https://codeberg.org/slycelote/caide/src/branch/master/libcaide/README.md)

* [Visual Studio extension
  ](https://codeberg.org/slycelote/caide/src/branch/master/vscaide/README.md)

* [Plugin for Topcoder
  Arena](https://codeberg.org/slycelote/caide/src/branch/master/tccaide/README.md)

