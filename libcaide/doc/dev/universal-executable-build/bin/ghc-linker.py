#!/bin/env python
import os, shutil, subprocess, sys, tempfile

# This script, based on the idea from [1], should be used as a linker with GHC
# to produce an 'as static as possible' executable.
# For instance, with cabal: cabal build --ghc-option '-pgml ghc-linker.py'
# [1]: https://tesser.org/doc/posts/2015-08-21-statically-linking-libgmp-in-haskell-programs.html


LIBS_FOR_STATIC_LINK = ['gmp', 'z', 'ssl', 'crypto']

def all_args(command):
    """Quoting 'man ld' (or 'man g++'):

       @file

           Read command-line options from file.  The options read are inserted
           in place of the original @file option.  If file does not exist, or
           cannot be read, then the option will be treated literally, and not
           removed.

           Options in file are separated by whitespace.  A whitespace character
           may be included in an option by surrounding the entire option in
           either single or double quotes.  Any character (including a
           backslash) may be included by prefixing the character to be included
           with a backslash.  The file may itself contain additional @file
           options; any such options will be processed recursively.

    Note that some quoted arguments may be processed incorrectly (e.g. if a quoted argument contains the substring ' @')
    """
    for arg in command:
        s = None
        if arg[0] == '@':
            try:
                with open(arg[1:], 'r') as f:
                    s = f.read()
            except IOError:
                s = None
        if s:
            for arg1 in all_args(s.split()):
                yield arg1
        else:
            yield arg

def find_library(lib):
    if ('CFLAGS' in os.environ and '-m32' in os.environ['CFLAGS']) or ('GHC_LINKER_32' in os.environ):
        lib_dirs = ['/lib', '/usr/lib', '/usr/local/lib'] # for 32-bit
    else:
        lib_dirs = ['/lib64', '/usr/lib64', '/usr/local/lib64', '/usr/lib64/openssl11'] # for 64-bit
    for dir_path in lib_dirs:
        path = dir_path + '/lib' + lib + '.a'
        if os.path.isfile(path):
            return path
    raise IOError("Could not find static library " + lib)


def main():
    args = list(all_args(sys.argv[1:]))
    # cabal-install insists on compiling shared libraries even when we ask it not to.
    # Don't try to statically link in that case.
    if '-shared' not in args and '"-shared"' not in args:
        static_link_args = sum([['-l'+lib, '"-l'+lib+'"', "'-l"+lib+"'"] for lib in LIBS_FOR_STATIC_LINK], [])
        args = [arg for arg in args if arg not in static_link_args]
        args += ['-static-libstdc++', '-static-libgcc']
        args += [find_library(lib) for lib in LIBS_FOR_STATIC_LINK]

    args_file_name = None
    try:
        with tempfile.NamedTemporaryFile(prefix='linker.args.', delete=False) as f:
            args_file_name = f.name
            for arg in args:
                f.write(arg)
                f.write(' ')
        subprocess.call(['g++', '@' + args_file_name])
    finally:
        if args_file_name:
            try:
                if False:
                    # For debugging
                    with open(args_file_name) as f:
                        args = f.read()
                    with open('linker-log.txt', 'a') as f:
                        f.write(args)
                        f.write('\n')
                os.unlink(args_file_name)
            except:
                pass


if __name__ == '__main__':
    main()

