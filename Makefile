export RUST_BACKTRACE=1

.PHONY: build run

build:
	cargo build --release

run:
	#./target/release/encore examples/simple/index.js
	./target/release/encore examples/tricky/index.js

clean:
	cargo clean
	rm -rf dist
