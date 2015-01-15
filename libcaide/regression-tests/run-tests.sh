#!/bin/bash
set -e
set -u

cur_dir=$( cd $(dirname "${BASH_SOURCE[0]}") ; pwd )
[ -d $cur_dir ] || exit 420

tmp_dir=$cur_dir/tmp
export caide=$cur_dir/../dist/build/caide/caide
functions_file=$cur_dir/test-functions.sh

mkdir -p $tmp_dir

failed=0
passed=0
shopt -s nullglob
for f in $( cd $cur_dir ; ls *.test )
do
    echo " == Running $f... =="
    full_test_path=$cur_dir/$f
    export etalon_dir=$cur_dir/${f%.test}
    work_dir=$tmp_dir/${f%.test}
    rm -rf $work_dir

    [ -d $etalon_dir/init ] && cp -R $etalon_dir/init $work_dir || mkdir -p $work_dir

    cd $work_dir

    if bash -e -c "source $cur_dir/test-functions.sh; source $full_test_path" ; then
        echo " == Passed =="
        passed=$((passed+1))
    else
        echo " == Failed =="
        failed=$((failed+1))
    fi

done

echo "$passed tests passed, $failed tests failed"

exit $failed

