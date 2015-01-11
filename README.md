# caide
Automates certain common tasks that you do during programming competitions:
parsing, running and debugging problem tests, inlining library code.

caide is inspired by such projects as [chelper](https://code.google.com/p/idea-chelper)
or [jhelper](https://github.com/AlexeyDmitriev/JHelper). Its purpose is to automate the following tasks:

* Parsing problem statement and extracting test cases
* Generating solution scaffold
* Inlining library code and preparing a single source file for submission
* Running the tests on your solution
* Debugging the tests

Unlike other projects, caide is designed to support multiple programming languages and IDEs/editors.

caide is split into the following projects:

* [libcaide](https://github.com/slycelote/caide/blob/master/libcaide/README.md) is core command line application implementing all functionality. Windows and Linux are supported. Theoretically, it should also build on OS X.
* [VsCaide](https://github.com/slycelote/caide/blob/master/vscaide/README.md) is caide frontend (extension) for Visual Studio; currently supports only C++ programming language.

Download caide from [releases page](https://github.com/slycelote/caide/releases).

See developer documentation in [libcaide/doc](https://github.com/slycelote/caide/tree/master/libcaide/doc) directory.

