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
	rm -rf out/


create-docker-image:
	docker stop assembler || true
	docker rm assembler || true
	docker rmi assembler || true
	docker build --platform linux/x86_64 -t assembler .

assemble-local:
	docker stop assembler || true
	docker rm assembler || true
	docker run -it --platform=linux/x86_64 --name assembler -v ${PWD}:/project assembler bash

#TEST_SOURCE = wacc-examples-36/valid/IO/print/println.wacc wacc-examples-36/valid/basic wacc-examples-36/valid/variables wacc-examples-36/valid/if
TEST_SOURCE =  wacc-examples-36/valid/basic \
			wacc-examples-36/valid/IO/print \
			wacc-examples-36/valid/IO \
			wacc-examples-36/valid/if wacc-examples-36/valid/while \
			wacc-examples-36/valid/expressions wacc-examples-36/valid/scope \
			wacc-examples-36/valid/sequence wacc-examples-36/valid/variables \
			wacc-examples-36/valid/function wacc-examples-36/valid/pairs \
			wacc-examples-36/valid/array wacc-examples-36/valid/runtimeErr \
			wacc-examples-36/invalid/syntaxErr wacc-examples-36/invalid/semanticErr \

integration-test-local:
	docker stop assembler || true
	docker rm assembler || true
	docker run -it --name assembler -v ${PWD}:/project assembler  bash -c 'cd project && make whack && python3 integration_test.py $(TEST_SOURCE)'

.PHONY: all whack check test clean
