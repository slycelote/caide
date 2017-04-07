# caide
Automates certain common tasks that you do during programming competitions:
parsing, running and debugging problem tests, inlining library code.

caide is inspired by such projects as
[chelper](https://code.google.com/p/idea-chelper) or
[jhelper](https://github.com/AlexeyDmitriev/JHelper). Its purpose is to
automate the following tasks:

* Parsing problem statement and extracting test cases
* Generating solution scaffold
* Inlining library code and preparing a single source file for submission
* Running the tests on your solution
* Debugging the tests

Unlike other projects, caide is designed to support multiple programming
languages and IDEs/editors.

## Screenshots
Codelite and command line caide in Linux: ([full size](http://i.imgur.com/cZsP6Z0.png))

![ ](http://i.imgur.com/cZsP6Z0l.png)

Codelite in Windows: ([full size](http://i.imgur.com/mLmQDgD.png))

![ ](http://i.imgur.com/mLmQDgDl.png)

Visual Studio:

![ ](http://i.imgur.com/xjpp3tU.png)

## Download

caide is split into the following components (click the links to learn more):

* [caide](https://github.com/slycelote/caide/blob/release/libcaide/README.md)
  is core command line application implementing all functionality. Windows and
Linux are supported. Theoretically, it should also build on OS X.
* [VsCaide](https://github.com/slycelote/caide/blob/release/vscaide/README.md)
  is caide frontend (extension) for Visual Studio; currently supports C++ and
C# programming languages.
* [tccaide](https://github.com/slycelote/caide/blob/release/tccaide/README.md)
  is a plugin for Topcoder arena required if you want to work with Topcoder
problems.

Download caide from [releases
page](https://github.com/slycelote/caide/releases). You will need *either* the
command line executable for your platform, *or* the Visual Studio extension.
In addition, if you want Topcoder support, you will need the Arena plugin.

## Documentation

* [Features](https://github.com/slycelote/caide/blob/release/doc/features.md)

* [Known
  issues](https://github.com/slycelote/caide/blob/release/doc/known-issues.md)

* [Command line
  application](https://github.com/slycelote/caide/blob/release/libcaide/README.md)

* [Visual Studio extension
  ](https://github.com/slycelote/caide/blob/release/vscaide/README.md)

* [Plugin for Topcoder
  Arena](https://github.com/slycelote/caide/blob/release/tccaide/README.md)

* [Developer
  documentation](https://github.com/slycelote/caide/tree/release/libcaide/doc/dev)

