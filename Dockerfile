# Dockerfile

# Use a base image with Java and Maven pre-installed
FROM rust:latest

# Install environment dependencies
RUN apt-get update && \
    apt-get install -y ruby && \
    apt-get install -y gcc && \
    gem install rest-client

## Set the working directory in the container
#WORKDIR ~
#
## Copy the necessary files into the container
#COPY . .

#RUN cd project
## build the program
#RUN make whack
#
## clone the examples repo
#RUN git submodule update --init --recursive
#
## run integration tests
#RUN python3 integration_tests.py wacc_examples-36/valid/IO/print/print.wacc
