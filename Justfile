tip := "./target/debug/tip"
file_check := "./target/debug/file_check"

test: tip-unit-test lit-test

tip-unit-test:
	cargo test -p tip

build-tip:
	cargo build -p tip

build-file-check:
	cargo build -p file_check

build-lit:
	cargo build -p lit

lit-test: build-tip build-file-check
	cargo run -p lit -- ./test_programs {{tip}} {{file_check}}
