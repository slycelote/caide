caide is a command line application which is the core of caide software suite.

# Installation
 Simply unpack caide executable to your hard drive. Optionally, put it into a
directory in your PATH for easier access.


# Quick start
1. Run `caide init` in an empty directory to initialize caide project.
2. Edit `caide.ini` file if you want to modify some settings.
3. Run `caide problem <URL>` or `caide contest <URL>` to parse a problem or a
   contest.
4. Checkout a problem with `caide checkout <problem ID>`. Problem ID coincides
   with the name of a subdirectory in your project's root.
5. Implement a solution to the problem in `problemID.cpp` file.
6. Compile `problemID.cpp` and `problemID_test.cpp` files into an executable
   file, e.g. `g++ problemID*.cpp -o problemID`.
7. Run this executable from `problemID` directory. This will run your solution
   on all tests. (Note: [IDEs](#ide) or [builders](#builder) can simplify
compilation and running the tests.)
8. If some tests failed, you can debug your solution, e.g. with `gdb
   problemID`. 'Smart testing' will guarantee that failing tests are run
before the ones that passed.
9. Run `caide make` to generate a single file (`submission.cpp`) for
   submission to an online judge.


# Supported online judges

Caide can parse problem definitions (problem name and sample tests)
automatically. You need to provide a URL of problem or contest (`caide
problem` and `caide contest` commands).

<a name="http-server"></a>
## CHelper and Competitive Companion browser extensions

Caide is compatible with [chelper chrome
extension](https://chrome.google.com/webstore/detail/chelper-extension/eicjndbmlajfjdhephbcjdeegmmoadip)
and [Competitive Companion browser extension](https://github.com/jmerle/competitive-companion/).
(To parse a problem, click the plus sign that appears in tab bar for supported
sites.) Because of how the extensions work, you have to be running a local
server that will accept requests from the browser. This is done in command line
with `caide httpServer`. Hit Return to stop the server.


<a name="configuration"></a>
# Configuration
Most settings are stored in `caide.ini` file in the project root.

## [core] section
[core] section represents general settings:

* `language` is default programming language for newly created/parsed
  problems. Possible values: `cpp`, `csharp`. Settings for each language are
  kept in the corresponding section of the file.
* `features` is a comma separated list of 'features' (optional pieces of
  functionality). Settings for each feature are kept in the corresponding
section of the file. Currently the only implemented feature is 'codelite'
which enables a limited [support for Codelite IDE](#codelite).
* `chelper_port` (default 4243) and `companion_port` (default 8080) control 
  ports that [`caide httpServer` command](#http-server) uses. Set a port to
  -1 to disable the corresponding server.


<a name="cpp"></a>
## [cpp] section
[cpp] section contains settings for C++ language.

* `clang_options` is a comma separated list of command line options for clang
  parser. (Defaults should always work.) Most important of them are:
  - `-target <target>` sets up compiler whose headers you want to use. This
    option, if set correctly, should eliminate the need to list system headers
manually. `<target>` should be:
     * `<arch>-linux` on Linux, where `<arch>` is either `i386` or `x86_64`,
       depending on your system's architecture.
     * `i386-mingw32` on Windows with MinGW compiler.
     * `i386-pc-windows-msvc` on Windows with Visual Studio compiler.

  - `-isystem <path>` adds a location of system headers such as `<algorithm>`
    or `<time.h>`. On Linux, you can find out which directories are required
with `g++ -v -x c++ -E /dev/null`.
  - `-I <path>` adds a location for user headers. `cpplib` is most likely the
    only directory that should be specified here.
  - `-DONLINE_JUDGE` (or similar) - a preprocessor symbol defined on judge
    servers. This must be *on* because submitted file will be executed on a
    judge server.
  - `-std=c++14` - set the version of C++ standard.

  The full list of command line options can be found in [clang user
manual](http://clang.llvm.org/docs/UsersManual.html) or by running `clang++
--help`. However, since caide runs only clang parser, most of the options
won't have any effect.

* `keep_macros` is a comma separated list of macro definitions which will be
  kept in submission file even if they are not used. For example, if you use
the following code:

        #ifdef _WIN32
        #define LLD "%I64d"
        #else
        #define LLD "%lld"
        #endif

  and compile locally on Linux, then it will be replaced with:

        #define LLD "%lld"

  which is probably not what you intended. To fix this, add `_WIN32` to
  `keep_macros` setting. Then both `#if` branches will be kept in submission
  file.

  By default `keep_macros` contains a list of common compiler-specific and
  OS-specific definitions, so the example above actually works correctly.



## Problem settings

 Each problem folder contains a `problem.ini` file which manages settings
specific for that problem. Currently the only setting is `double_precision`
which controls the maximum admissible difference between a floating point
number in the output and the corresponding floating point number in the
etalon.

<a name="templates"></a>
## Templates

`templates` folder contains skeleton code files and project files from which
corresponding files for new problems are generated. For example, if you're
unhappy with generated solution code, you can edit its template.

<a name="builder"></a>
## Builder

A builder is a script that compiles and runs your solution. If a builder is
configured, you can invoke it with `caide test`.

To configure a builder for a programming language, add a setting
`build_and_run_tests`, in the section for that language (e.g., `[cpp]`). The
value of the setting should be the actual command that runs your builder.

[Samples](https://github.com/slycelote/caide/tree/release/libcaide/doc/samples)
directory contains examples of builders for C++ and C# programming languages.

<a name="ide"></a>
# IDE support
## Visual Studio

See
[readme](https://github.com/slycelote/caide/tree/release/vscaide/README.md)
for VsCaide extension in that project's directory.

<a name="codelite"></a>
## Codelite

caide core contains limited support for [Codelite IDE](http://codelite.org).
(Make sure to use the latest version of the IDE from the website.) To enable
Codelite support, set `features` setting in `[core]` section to `codelite`.
Once this feature is enabled, caide will generate and update Codelite
workspace every time it makes any change in caide project.  Generated Codelite
projects already have correct settings, such as working directory.  You can
simply run and debug your solutions from Codelite.  However, all other actions
like switching between problems and parsing contests must still be done via
the console application: changes to Codelite projects will not be preserved.
(This is similar to how cmake works.)

You can edit template Codelite project in [templates](#templates) directory.
In particular, you may want to set compiler to something that exists in your
setup. You can edit the template in a text editor, or you can modify a
generated project in Codelite GUI and replace the
`codelite_project_template.project` file with the modified project.

<a name="inliner"></a>
# C++ code inliner

Online judges typically require that you send a single code file. It makes
using prewritten code more difficult: you have to copy and paste it into your
solution. You could also put all your prewritten code in one giant template,
but some online judges have a rule forbidding unused code. caide lets you use
your library of prewritten code in a normal way: put \*.h and \*.cpp files
into `cpplib` directory and #include necessary headers. `caide make` command
then will then generate a single submission file from your solution and
library - this is referred to as 'inlining' the code.

If you have troubles with C++ inliner, change the language to `simplecpp` - it
will disable the inliner but you'll still be able to use other features, such
as 'smart testing'. Here is what you need to be aware of when writing library
code:

1. caide comes with a specific set of system headers (a MinGW implementation).
   You have to write standards-complying code to make sure both caide inliner
and your compiler can parse it. In particular, avoid using compiler specific
functions.

   You can [setup](#cpp) caide to use your system headers instead.

2. You can think of inliner as roughly the following algorithm:

  * Concatenate all \*.cpp files into one.
  * Replace all `#include "user_header.h"` entries with the contents of the
    header.
  * Remove unused code.

   This should give you an idea of what kind of code you should avoid:
   identically named classes/global variables, `#define`s with different
   meaning in different files etc.

3. Sometimes the inliner will erroneously remove a used declaration (a
   function or a variable). You should [report it](#bugs), but for an
   immediate fix, mark the declaration with `// caide keep` or `/* caide keep
   */` comment.


# Command reference

Run `caide -h` for the list of commands. Commands marked with (Internal) are
'plumbing layer' not meant for the end user but for use in scripts (such as
[builders](#builder)), IDEs etc.


<a name="bugs"></a>
# Troubleshooting

You can report issues on [github](https://github.com/slycelote/caide/issues)
or in [Codeforces thread](http://codeforces.com/blog/entry/18838). Try to
include as much information as possible, such as:

* Your operating system (in Linux, output of `uname -a`).
* Your caide.ini file.
* If the problem is related to C++ code inliner, add a `-v` option to
  `clang_options` list in caide.ini file. This will produce more diagnostics
output.


# Description of files in caide folder

* `caide.ini` is a text file containing general caide settings.
* Source code files named `submission` contain code of the latest problem you
  built (ran `cade make` on) ready for submission to an online judge.
* `caide_archive` directory contains archived problems, with solutions. To
  restore a problem, move the corresponding directory from this folder to the
top caide folder.
* `cpplib` is the directory where you put your prewritten C++ code.
* `include` directory contains an implementation of C++ standard library as a
  fallback in case you can't [use your system's headers](#cpp).
* `templates` directory generates solution scaffold and testing code. You can
  tweak most of the code generated when a problem is created by modifying
these source files.
* `.caide` directory contains internal caide files. Don't modify them unless
  you know what you're doing.

Each problem has its own directory named after the problem ID. For instance,
the [CodeChef TEST](http://www.codechef.com/problems/TEST) problem will have a
directory named `chefTEST` containing the following files:

* `*.in` and `*.out` files contain sample input and output. You can add your
  own sample cases.
  - Rename `*.out` file to `*.skip` to skip the test.
  - Delete it or rename to something else to indicate that the etalon output
    is unknown.

  (You can also do this in VsCaide GUI.)

* `problem.ini` contains problem settings.
* Source code file named `chefTEST` where you implement solution to the
  problem. Typically, this is the only file you need to edit.
* Source code file named `chefTEST_test`. This file contains code running all
  sample test cases. If the problem needs a custom checker, you can implement
it here.
* Source code file named `submission` contains the code ready for submission
  to an online judge.
* Source code file named `main`, together with `chefTEST` file, will be used
  to generate submission file.
* `.caideproblem` directory contains internal caide files. Don't modify them
  unless you know what you're doing.

