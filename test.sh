#!/bin/bash

TEST_CASE=$1
BASE_CMDS=base.f
QEMU_PIDS_BEFORE=$(pgrep qemu)

# This should have -daemonize in order for this to work.
output=$(./run-qemu.sh 2>/dev/null)
pts_number=$(echo $output|sed "s@.*/dev/pts/\\([0123456789]\\).*@\1@")

declare -a test_cases
test_index=0
if [[ $TEST_CASE == "all" ]]; then
    pushd tests
    for i in $(ls /*.f); do
        test_cases[$test_index]=$(basename $i .f)
        $test_index += 1
    done
    popd
else
    test_cases[0]=$TEST_CASE
fi

for test_case in "${test_cases[@]}"; do
    SCREEN_LOGFILE=${test_case}.log
    CLEANED_LOGFILE=${SCREEN_LOGFILE}.cleaned

    rm ${SCREEN_LOGFILE} 2>/dev/null
    screen -d -m -L -Logfile ${SCREEN_LOGFILE} /dev/pts/${pts_number}
    sleep 1
    screen -X readbuf ./tests/${test_case}.f
    screen -X paste .
    screen -X quit

    # Fixup screen logfile because who fucking uses carriage returns these days
    tr '\r' '\n' < ${SCREEN_LOGFILE} > ${CLEANED_LOGFILE}

    # Output available in SCREEN_LOGFILE
    if ! diff $CLEANED_LOGFILE ./tests/${TEST_CASE}_expected.out; then
        echo "Test \"${test_case}\" failed. Got output"
        cat ${CLEANED_LOGFILE}
        echo "But expected"
        cat ./tests/${TEST_CASE}_expected.out
    else
        echo "Test \"${test_case}\" passed."
    fi

done


QEMU_PIDS_AFTER=$(pgrep qemu)
NEW_QEMU_PIDS=$(comm -13 <(echo "${QEMU_PIDS_BEFORE}") <(echo "${QEMU_PIDS_AFTER}") )

for pid in ${NEW_QEMU_PIDS}; do
    echo "Cleaning up QEMU PID ${pid}"
    kill ${pid}
done
