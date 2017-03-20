function compare_with {
    local etalon_dir=$etalon_dir/$1
    shift 1
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

function render_js {
    "$PHANTOMJS" --load-images=false "$cur_dir/render.js" "$1" .page.html
}

