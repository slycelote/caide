(**Note**: this is documentation for development version of caide. README for
the latest release is located
[here](https://github.com/slycelote/caide/blob/v1.1.0/libcaide/README.md).)

caide is a command line application which is the core of caide software suite.

# Installation
 Simply unpack caide executable to your hard drive. Optionally, put it into a
directory in your PATH for easier access.

 On Linux you will need to ensure certain libraries are installed: libstdc++6,
zlib1g, libgmp10. On a 64-bit system append `:i386` to package names.

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


<a name="configuration" />
# Configuration
Most settings are stored in `caide.ini` file in the project root.

## [core] section
[core] section represents general settings:

* `language` is default programming language for newly created/parsed
  problems. Possible values: `cpp`, `simplecpp`, `csharp`. `simplecpp` is a
fallback option that disables C++ code inliner in case you have problems with
it.  Settings for each language are kept in the corresponding section of the
file.
* `features` is a comma separated list of 'features' (optional pieces of
  functionality). Settings for each feature are kept in the corresponding
section of the file. Currently the only implemented feature is 'codelite'
which enables a limited [support for Codelite IDE](#codelite).

<a name="cpp"/>
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
  - `-std=c++11` - you can remove this if you feel nostalgic.

  The full list of command line options can be found in [clang user
manual](http://clang.llvm.org/docs/UsersManual.html) or by running `clang++
--help`. However, since caide runs only clang parser, most of the options
won't have any effect.

* `keep_macros` is a comma separated list of macro definitions which will be
  kept in submission file even if they are not used. For example, if you use
the following code:

        #ifdef LOCAL_RUN
        #   define Endl std::endl
        #else
        #   define Endl "\n"
        #endif

  and compile locally with `-DLOCAL_RUN`, then it will be replaced with:

        #   define Endl std::endl

  which is probably not what you intended. To fix this, add `LOCAL_RUN` to
`keep_macros` setting. Then both `#if` branches will be kept in submission
file.

  By default `keep_macros` contains a single definition `ONLINE_JUDGE`. So the
example above may be rewritten as

        #ifdef ONLINE_JUDGE
        #   define Endl "\n"
        #else
        #   define Endl std::endl
        #endif


## Problem settings

 Each problem folder contains a `problem.ini` file which manages settings
specific for that problem. Currently the only setting is `double_precision`
which controls the maximum admissible difference between a floating point
number in the output and the corresponding floating point number in the
etalon.

<a name="templates"/>
## Templates

`templates` folder contains skeleton code files and project files from which
corresponding files for new problems are generated. For example, if you're
unhappy with generated solution code, you can edit its template.

<a name="builder"/>
## Builder

A builder is a script that compiles and runs your solution. If a builder is
configured, you can invoke it with `caide test`.

To configure a builder for a programming language, add a setting
`build_and_run_tests`, in the section for that language (e.g., `[cpp]`). The
value of the setting should be the actual command that runs your builder.

`templates/builder-samples` directory contains examples of builders for C++
and C# programming languages.

<a name="ide" />
# IDE support
## Visual Studio

See [readme](https://github.com/slycelote/caide/tree/master/vscaide/README.md)
for VsCaide extension in that project's directory.

<a name="codelite"/>
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

<a name="inliner"/>
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

   You can [setup](#cpp) caide to use your system headers instead. VsCaide
will do this automatically.

2. You can think of inliner as roughly the following algorithm:

  * Concatenate all \*.cpp files into one.
  * Replace all `#include "user_header.h"` entries with the contents of the
    header.
  * Remove unused code.

   This should give you an idea of what kind of code you should avoid:
identically named classes/variables, `#define`s with different meaning in
different files etc.

3. Some features (like partially specialized template classes) have been less
   tested than others. Some unused code (like local variables) doesn't get
removed yet.  Expect some rough edges and feel free to open issues.


# Command reference
 Run `caide -h` for the list of commands. Commands marked with (Internal) are
'plumbing layer' not meant for the end user but for use in scripts (such as
[builders](#builder)), IDEs etc.

