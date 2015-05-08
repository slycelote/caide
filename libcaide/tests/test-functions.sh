function compare_with {
    local etalon_dir=$etalon_dir/$1
    shift 1
    for file in "$@"
    do
        diff -bB --strip-trailing-cr $etalon_dir/$file ./$file || return 1
    done
    return 0
}

