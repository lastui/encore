.PHONY: build run

build:
	cargo build --release

run:
	./target/release/encore run examples/index.js


clean:
	cargo clean
	rm -rf dist
