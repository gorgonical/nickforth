#!/bin/bash

source env.sh

qemu-system-riscv64 \
   -serial pty  \
   -M virt \
   -bios $HOME/buildroot-2022.02.5/output/images/fw_jump.elf \
   -kernel $PWD/nickforth.bin \
   -d int --trace "virtio*" \
   -drive file=./disk.raw,if=none,id=d1 \
   -global virtio-mmio.force-legacy=false \
   -device virtio-blk-device,drive=d1 "$@"
