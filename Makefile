CROSS_COMPILE := riscv64-buildroot-linux-uclibc-

all: nickforth

nickforth: nickforth.S
	$(CROSS_COMPILE)gcc -O0 -Wl,-Ttext,0 -nostdlib -static $(BUILD_ID_NONE) -o $@ $<
