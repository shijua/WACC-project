# Dockerfile

# Use a base image with Java and Maven pre-installed
FROM rust:latest

# Install environment dependencies
RUN apt-get update && \
    apt-get install -y ruby && \
    gem install rest-client

# Set the working directory in the container
WORKDIR ~

# Copy the necessary files into the container
COPY . .

# build the program
RUN make whack

# clone the examples repo
RUN git submodule update --init --recursive
