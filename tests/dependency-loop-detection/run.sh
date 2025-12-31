#!/bin/sh

dir=$(dirname "$(realpath "$0")")
pass_count=0
fail_count=0
for sample in "$dir"/module-trees/sample-*/; do
    expected=$(cat "$sample/expected")
    printf 'Sample: %s\n' "$(basename "$sample")"
    env GUILE_LOAD_PATH="$sample:$GUILE_LOAD_PATH" \
        guild dependency-loops "$sample"
    if [ "$?" = "$expected" ]; then
        echo '### Test passed'
        pass_count=$(( pass_count + 1 ))
    else
        echo '### Test failed'
        fail_count=$(( fail_count + 1 ))
    fi
done

printf '%s tests passed\n%s tests failed\n' \
       "$pass_count" "$fail_count"
