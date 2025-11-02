tccaide is a plugin for Topcoder arena required to support Topcoder problems
in caide tools.

# Installation

Note: JRE 1.7+ is required.

1. Create a caide project in one of caide tools. See
   [caide](https://codeberg.org/slycelote/caide/src/branch/master/libcaide/README.md)
   for a command line tool or
   [VsCaide](https://codeberg.org/slycelote/caide/src/branch/master/vscaide/README.md)
   for a Visual Studio extension.

2. Download tccaide.jar file from
   [releases page](https://github.com/slycelote/caide/releases/).

3. [Log into
   Arena](http://www.topcoder.com/contest/arena/ContestAppletProd7.2.jnlp) and
   select Options -> Editor from main menu.

4. Click Add button. Use the following settings:
  * Name - caide
  * EntryPoint - net.slycelote.caide.EntryPoint
  * ClassPath - path to the jar file you downloaded.

5. Click OK, and then Configure.

6. Input path to caide project that you created in the first step. Click
   'Verify & Save'.

# Quick start

Simply open a problem in Topcoder Arena. Solution template and testing code
will be created automatically. Don't forget to hit 'Compile' button in Arena
before submitting!


# Configuration

The topcoder plugin doesn't have any options. Refer to [caide
documentation](https://codeberg.org/slycelote/caide/src/branch/master/libcaide/README.md)
for general caide configuration or to
[VsCaide documentation](https://codeberg.org/slycelote/caide/src/branch/master/vscaide/README.md)
for the Visual Studio extension configuration.

