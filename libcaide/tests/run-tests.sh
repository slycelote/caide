#!/bin/bash
set -e
set -u
shopt -s nullglob

cur_dir=$( cd $(dirname "${BASH_SOURCE[0]}") ; pwd )
[ -d $cur_dir ] || exit 255

if [ ! -v CAIDE ]; then
    for f in $cur_dir/../dist-newstyle/build/*/{,*{,/libcaide-*}}/build/caide/caide{,.exe} $cur_dir/../dist/build/caide/caide{,.exe} $cur_dir/../.stack-work/install/*/{,*{,/*}}/bin/caide{,.exe}
    do
        if [[ -f "$f" && -x "$f" ]] ; then
            CAIDE="$f"
            break
        fi
    done
fi

if [ -v CAIDE ]; then
    echo "Executable under test: $CAIDE"
else
    echo "Failed to find caide executable"
    exit 255
fi

tmp_dir=$cur_dir/tmp

# Depending on the OS distribution, this may be required for phantomjs
# export QT_QPA_PLATFORM=offscreen

export cur_dir
export CAIDE
# On Windows use something like CSC=/c/Windows/Microsoft.NET/Framework/v4.0.30319/csc.exe ./run-tests.sh
export CSC=${CSC:-mcs}
export CXX=${CXX:-c++}
export CXXFLAGS
export MONO=${MONO:-}
export PHANTOMJS=${PHANTOMJS:-phantomjs}

mkdir -p $tmp_dir

failed=0
passed=0
failed_tests=""

if [ "$#" -eq 0 ]; then
    tests=($( cd $cur_dir ; ls *.test ))
else
    tests=( "$@" )
fi

for f in "${tests[@]}"
do
    echo " == Running $f... =="
    full_test_path=$cur_dir/$f
    export etalon_dir=$cur_dir/${f%.test}
    work_dir=$tmp_dir/${f%.test}
    rm -rf $work_dir

    if [ -d $etalon_dir/init ] ; then
        cp -R $etalon_dir/init $work_dir
    else
        mkdir -p $work_dir
    fi

    cd $work_dir

    if bash -e -c "source $cur_dir/test-functions.sh; source $full_test_path" ; then
        echo " == Passed =="
        passed=$((passed+1))
    else
        echo " == Failed =="
        failed=$((failed+1))
        failed_tests="$failed_tests $f"
    fi

done

echo "$passed test(s) passed, $failed test(s) failed"
echo "$failed_tests"

exit $failed

