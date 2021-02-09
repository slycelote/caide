#!/usr/bin/env bash
function compare_with {
    local etalon_dir=$etalon_dir/$1
    shift 1
    local file
    for file in "$@"
    do
        diff -bB --strip-trailing-cr $etalon_dir/$file ./$file || return 1
    done
    return 0
}

function run_csharp_executable {
    if [ "x$MONO" = "x" ]
    then "$1"
    else "$MONO" "$1"
    fi
}

function cxx {
    if [ "x$CXXFLAGS" = "x" ]
    then "$CXX" "$@"
    else "$CXX" $CXXFLAGS "$@"
    fi
}

function render_js {
    "$PHANTOMJS" --load-images=false --ignore-ssl-errors=true --local-url-access=false "$cur_dir/render.js" "$1" .page.html
}

function parse_with_js {
    render_js "$1"
    "$CAIDE" problem --from-file .page.html "$1"
}

function basic_init {
    cp -r "$cur_dir"/_basic_init/* "$cur_dir"/_basic_init/.caide .
}

# MSYS shell doesn't necessarily kill background jobs on exit
function cleanup_on_exit {
    local ec=$?
    kill -9 %1 %2 %3 %4 %5 2>/dev/null || true
    exit $ec
}

trap cleanup_on_exit EXIT

