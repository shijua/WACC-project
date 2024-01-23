# Specify Directory Locations

SRC_DIR := src
OUT_DIR := target


all: whack

whack:
	cargo build

check:
	cargo check

test:
	cargo test

clean:
	cargo clean


.PHONY: all whack check test clean
