#!/bin/bash

source env.sh

qemu-system-riscv64 \
   -daemonize \
   -serial pty  \
   -M virt \
   -bios $HOME/buildroot-2022.02.5/output/images/fw_jump.elf \
   -kernel $PWD/nickforth.bin \
   -d int \
   -drive file=./disk.raw,if=none,id=d1 \
   -device virtio-blk-device,drive=d1 "$@"
