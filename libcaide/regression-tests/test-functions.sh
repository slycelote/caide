function compare_with {
    local etalon_dir=$etalon_dir/$1
    diff -qr . $etalon_dir && return 0 || return 1
}

