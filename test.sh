#!/bin/bash

TEST_CASE=$1
SCREEN_LOGFILE=log.0
CLEANED_LOGFILE=log_cleaned.0
BASE_CMDS=base.f

QEMU_PIDS_BEFORE=$(pgrep qemu)

# This should have -daemonize in order for this to work.
output=$(./run-qemu.sh 2>/dev/null)
pts_number=$(echo $output|sed "s@.*/dev/pts/\\([0123456789]\\).*@\1@")

rm ${SCREEN_LOGFILE}
screen -d -m -L -Logfile ${SCREEN_LOGFILE} /dev/pts/${pts_number}
sleep 1

# To be enabled when I have base commands to use
#screen -X readbuf ${BASE_CMDS}
#screen -X paste .

# Here is where the "test cases" should go
screen -X readbuf ./tests/${TEST_CASE}.f
screen -X paste .

screen -X quit

# Fixup screen logfile because who fucking uses carriage returns these days
tr '\r' '\n' < ${SCREEN_LOGFILE} > ${CLEANED_LOGFILE}

# Output available in SCREEN_LOGFILE
if ! diff $CLEANED_LOGFILE ./tests/${TEST_CASE}_expected.out; then
    echo "Test \"${TEST_CASE}\" failed. Got output"
    cat ${CLEANED_LOGFILE}
    echo "But expected"
    cat ./tests/${TEST_CASE}_expected.out
fi

QEMU_PIDS_AFTER=$(pgrep qemu)
NEW_QEMU_PIDS=$(comm -13 <(echo "${QEMU_PIDS_BEFORE}") <(echo "${QEMU_PIDS_AFTER}") )

for pid in ${NEW_QEMU_PIDS}; do
    echo "Killing PID ${pid}"
    kill ${pid}
done
