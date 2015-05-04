#!/bin/bash
cur_dir=$( cd $(dirname "${BASH_SOURCE[0]}") ; pwd )
[ -d "$cur_dir" ] || exit 420

build_dir="$cur_dir/clangbuilddebug"
mkdir -p "$build_dir"
cd "$build_dir"
"$cur_dir/llvm/configure"   "--with-clang-srcdir=$cur_dir/clang" \
                            "--disable-polly" \
                            "--disable-shared" \
                            "--disable-pic" \
                            "--enable-bindings=none" \
                            "--disable-clang-arcmt" \
                            "--disable-clang-static-analyzer" \
                            "--disable-assertions" \
                            "--disable-keep-symbols" \
                            "--disable-jit" \
                            "--disable-docs" \
                            "--disable-doxygen" \
                            "--disable-threads" \
                            "--disable-zlib" \
                            "--enable-targets=x86" \
                            "--disable-terminfo" \
                            "--enable-bindings=none" \
                            "--enable-libcpp=no" \
                            "--prefix=$build_dir/out" \
                            "--enable-debug-symbols" "--enable-debug-runtime"

