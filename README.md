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
* libcaide is core command line application implementing all functionality. Windows and Linux are supported.
* vscaide is caide frontend (extension) for Visual Studio; supports only C++ programming language.

