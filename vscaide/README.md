VsCaide is a Visual Studio extension which serves as caide frontend. Rather than using caide as a command line application, you can do everything from Visual Studio GUI.

# Installation

[Visual Studio 2013 Community Edition](http://www.visualstudio.com/en-us/news/vs2013-community-vs.aspx) (free) or higher is required. To install, locate VsCaide in extensions manager. Alternatively, double click downloaded \*.vsix file.

# Quick start

Select View -> Other Windows -> Caide. Recommended (and default) location of Caide window is under Solution Explorer.

In Caide window, click 'Create caide solution' button and select an empty folder where your solution will be created. The rest should be self-explanatory.

You can run/debug tests by clicking corresponding buttons in Caide window or by pressing `Ctrl+F5`/`F5`.

# Configuration

You can use most configuration options described in [libcaide README](https://github.com/slycelote/caide/tree/master/libcaide/README.md#configuration). Make sure to read the section on [C++ inliner](https://github.com/slycelote/caide/tree/master/libcaide/README.md#inliner) too. In addition, the [property file](http://msdn.microsoft.com/en-us/library/669zx6zc.aspx) `vs_common.props` in solution root directory is included into all generated projects. You can modify this file in Property Manager window.

