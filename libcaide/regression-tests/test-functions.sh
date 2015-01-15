function compare_with {
    local etalon_dir=$etalon_dir/$1
    shift 1
    for file in "$@"
    do
        diff -wBq ./$file $etalon_dir/$file || return 1
    done
    return 0
}

