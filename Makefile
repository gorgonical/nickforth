SHELL := /bin/bash
CROSS_COMPILE := riscv64-buildroot-linux-uclibc-
OBJCOPY		     = $(CROSS_COMPILE)objcopy

all: nickforth.bin

nickforth: nickforth.S empty.o
	source env.sh && $(CROSS_COMPILE)gcc -g -O0 -nostdlib -static \
		-Tlinker.ld \
		-Wl,-Ttext,0x80200000 \
		-Wl,-static \
		-Wa,--defsym,_HAVEDEFS=0 \
		$(BUILD_ID_NONE) -o $@ $^

nickforth-defs: nickforth.S data.o
	source env.sh && $(CROSS_COMPILE)gcc -g -O0 -nostdlib -static \
		-Tlinker.ld \
		-Wl,-Ttext,0x80200000 \
		-Wl,-static \
		-Wa,--defsym,_HAVEDEFS=1 \
		$(BUILD_ID_NONE) -o $@ $^

nickforth.bin: nickforth
	source env.sh && $(OBJCOPY) -O binary $< $@

nickforth-defs.bin: nickforth-defs
	source env.sh && $(OBJCOPY) -O binary $< $@

data.o: data.bin
	source env.sh && $(OBJCOPY) -I binary -O elf64-littleriscv data.bin data.o --rename-section .data=.end.data
	source env.sh && $(OBJCOPY) --redefine-sym _binary_data_bin_start=data_segment_start data.o

clean:
	rm nickforth.bin nickforth

run:
	./run-qemu.sh
	rm log.0;
	screen -d -m  -L -Logfile log.0 /dev/pts/3; sleep 1
	screen -X readbuf screen.cmd
	screen -X paste .
	screen -X quit
	less log.0
	pkill qemu

test:
	./test.sh all
