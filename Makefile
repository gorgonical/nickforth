CROSS_COMPILE := riscv64-buildroot-linux-uclibc-
OBJCOPY		     = $(CROSS_COMPILE)objcopy

all: nickforth.bin

nickforth: nickforth.S
	$(CROSS_COMPILE)gcc -g -O0 -nostdlib -static \
		-Tlinker.ld \
		-Wl,-Ttext,0x80200000 \
		-Wl,-static \
		$(BUILD_ID_NONE) -o $@ $<

nickforth.bin: nickforth
	$(OBJCOPY) -O binary $< $@

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
