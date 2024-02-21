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


create-docker-image:
	docker stop assembler || true
	docker rm assembler || true
	docker rmi assembler || true
	docker build --platform linux/x86_64 -t assembler .

integration-test:
	docker stop assembler || true
	docker rm assembler || true
	# docker run -it --name assembler -v ${PWD}:/project assembler  bash -c 'cd project && make whack && python3 integration_test.py wacc-examples-36'
	docker run -it --platform=linux/x86_64 --name assembler -v ${PWD}:/project assembler bash





.PHONY: all whack check test clean
