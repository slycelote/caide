VsCaide is a Visual Studio extension which serves as caide frontend. Rather
than using caide as a command line application, you can do everything from
Visual Studio GUI.

# Installation

[Visual Studio 2013 Community
Edition](http://www.visualstudio.com/en-us/news/vs2013-community-vs.aspx)
(free) or higher is required. To install, locate VsCaide in extensions
manager. Alternatively, double click downloaded \*.vsix file.

For Visual Studio 2017, you'll also need Windows Universal CRT SDK (can be
found in Visual Studio installer under Individual Components tab, section
"Compilers, build tools and runtimes").


# Quick start

Select View -> Other Windows -> Caide. Recommended (and default) location of
Caide window is under Solution Explorer.

In Caide window, click 'Create caide solution' button and select an empty
folder where your solution will be created. Use buttons in Caide window to
create/parse problems. Implement the solution in generated `solve` function.
(You can modify solution/test templates in `templates` directory.)

After building the corresponding project, `submission.cpp` file is created in
the problem's directory (and also copied to the solution's directory). This is
the file to submit to an online judge.

You can run/debug tests by clicking corresponding buttons in Caide window or
by pressing `Ctrl+F5`/`F5`.

Your library of prewritten code goes into `cpplib` project.

For Topcoder support you will also need the [Arena
plugin](https://github.com/slycelote/caide/tree/release/tccaide/README.md).

# Supported online judges

Caide can parse problem definitions (problem name and sample tests)
automatically. You need to provide a URL of problem or contest in 'New
problem' or 'Parse contest' dialogs.

## CHelper Chrome extension

Caide is compatible with [chelper chrome
extension](https://chrome.google.com/webstore/detail/chelper-extension/eicjndbmlajfjdhephbcjdeegmmoadip).
(To parse a problem, click the plus sign that appears in tab bar for supported
sites.) Because of how the extension works, VsCaide has to run a local HTTP
server that will accept requests from Chrome. (You may need to click Allow in
Windows firewall prompt.) You can disable this feature by adding a setting
`enable_http_server` equal to `false` to `[vscaide]` section in caide.ini
file.

# Configuration

VsCaide should work out of the box. However, you can tweak most configuration
options described in [caide
README](https://github.com/slycelote/caide/tree/release/libcaide/README.md#configuration).
Make sure to read the section on [C++
inliner](https://github.com/slycelote/caide/tree/release/libcaide/README.md#inliner)
too. In addition, the [property
file](http://msdn.microsoft.com/en-us/library/669zx6zc.aspx) `vs_common.props`
in solution root directory is included into all generated projects. You can
modify this file in Property Manager window.

# Troubleshooting

You can report issues on [github](https://github.com/slycelote/caide/issues)
or in [Codeforces thread](http://codeforces.com/blog/entry/18838). Try to
include as much information as possible, such as:

* All installed versions of Visual Studio
* Error message. You can get detailed output in Output window (View menu ->
  Output).
* Your caide.ini file.
* If the problem is related to C++ code inliner, add a `-v` option to
  `clang_options` list in caide.ini file. This will produce more diagnostics
output.

