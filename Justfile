tip := "./target/debug/tip"
file_check := "./target/debug/file_check"
quiet := "-q"

test: tip-unit-test lit-test

tip-unit-test:
	RUSTFLAGS=-Awarnings cargo test -p tip {{quiet}}

build-tip:
	RUSTFLAGS=-Awarnings cargo build -p tip {{quiet}}

build-file-check:
	RUSTFLAGS=-Awarnings cargo build -p file_check {{quiet}}

build-lit:
	RUSTFLAGS=-Awarnings cargo build -p lit {{quiet}}

lit-test: build-tip build-file-check
	cargo run -p lit {{quiet}} -- ./test_programs {{tip}} {{file_check}}
