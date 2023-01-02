.SUFFIXES:
.PRECIOUS: %.s %.o

%.out: %.o
	x86_64-unknown-linux-musl-ld -o $@ -static $$LINUX_LIBC/lib/{crt1.o,libc.a} $^

%.o: %.s
	x86_64-unknown-linux-musl-as -o $@ $<

%.s: %.c target/debug/tocc
	target/debug/tocc $< -o $@

target/debug/tocc: src/**
	cargo build
