#!/bin/env python3
import os, shutil, subprocess, sys, tempfile

# This script, based on the idea from [1], should be used as a linker with GHC
# to produce an 'as static as possible' executable.
# For instance, with cabal: cabal build --ghc-option '-pgml ghc-linker.py'
# [1]: https://tesser.org/doc/posts/2015-08-21-statically-linking-libgmp-in-haskell-programs.html


# Static versions of these libraries need to be installed in the system.
LIBS_FOR_STATIC_LINK = ['gmp', 'z', 'ssl', 'crypto', 'stdc++']

# ghc uses gcc as linker
REAL_LINKER = 'gcc'

# When setting a custom linker via -pgml, ghc doesn't pass these arguments.
# So we restore them manually.
# NOTE: -Wl,--no-as-needed must be the first argument
DEFAULT_LINKER_ARGS = ["-Wl,--no-as-needed", "-no-pie"]

DEBUG = False

def all_args(command):
    """Quoting 'man gcc':

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
    """
    # XXX: some quoted arguments may be processed incorrectly (e.g. if a quoted argument contains the substring ' @')
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

def main():
    args = list(all_args(sys.argv[1:]))
    if DEBUG:
        with open('/tmp/linker-log.txt', 'a') as f:
            print(sys.argv, file=f)
            print(args, file=f)
            print('', file=f)

    if not (set(args) & {'-o', '"-o"', "'-o'"}) or (set(args) & {'-shared', '"-shared"', "'-shared'"}):
        # ghc calls linker many times, not just for the final executable.
        # In particular, some autogen'ed files are linked into shared libraries.
        # Don't try to statically link in that case, simply pass through the arguments.
        subprocess.check_call([REAL_LINKER] + DEFAULT_LINKER_ARGS + sys.argv[1:])
        return

    new_args = DEFAULT_LINKER_ARGS + ['-static-libgcc', '-static-libstdc++']
    for arg in args:
        for lib in LIBS_FOR_STATIC_LINK:
            if arg in ['-l'+lib, '"-l'+lib+'"', "'-l"+lib+"'"]:
                new_args.append("-l:lib" + lib + ".a") # NOTE: GNU ld extension
                break
        else:
            new_args.append(arg)
    args = new_args

    args_file_name = None
    try:
        with tempfile.NamedTemporaryFile(mode='w', prefix='linker.args.', delete=False) as f:
            args_file_name = f.name
            for arg in args:
                f.write(arg)
                f.write(' ')
        subprocess.check_call([REAL_LINKER, '@' + args_file_name])
    finally:
        if args_file_name:
            try:
                if DEBUG:
                    with open(args_file_name) as f:
                        args = f.read()
                    with open('/tmp/linker-log.txt', 'a') as f:
                        f.write(args)
                        f.write('\n')
                os.unlink(args_file_name)
            except:
                pass


if __name__ == '__main__':
    main()

