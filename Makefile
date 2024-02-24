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
	rm -f draft draft.s


create-docker-image:
	docker stop assembler || true
	docker rm assembler || true
	docker rmi assembler || true
	docker build --platform linux/x86_64 -t assembler .

assemble-local:
	docker stop assembler || true
	docker rm assembler || true
	# docker run -it --name assembler -v ${PWD}:/project assembler  bash -c 'cd project && make whack && python3 integration_test.py wacc-examples-36'
	docker run -it --platform=linux/x86_64 --name assembler -v ${PWD}:/project assembler bash

integration-test-local:
	docker stop assembler || true
	docker rm assembler || true
	docker run -it --name assembler -v ${PWD}:/project assembler  bash -c 'cd project && make whack && python3 integration_test.py wacc-examples-36/valid/basic/exit/exitBasic.wacc'






.PHONY: all whack check test clean
