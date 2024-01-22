# Specify Directory Locations

SRC_DIR := src
OUT_DIR := target


all: whack

whack:
	cargo build --release

test:
	cargo test

clean:
	cargo clean


.PHONY: all whack test clean
