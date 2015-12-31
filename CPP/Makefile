CC=clang++
EXECUTABLE=letter
INSTALL_DIR=/usr/local/bin
BUILD_DIR=build
CFLAGS=-g -o $(BUILD_DIR)/$(EXECUTABLE)
LLFLAGS= $(shell llvm-config-3.7 --cxxflags --ldflags --system-libs --libs core mcjit orcjit native runtimedyld)
FILES=src/*.cpp

all:
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $(LLFLAGS) $(FILES)

clean:
	rm -rf $(BUILD_DIR)

purge:
	rm $(EXECUTABLE)

install: all
	mv $(EXECUTABLE) $(INSTALL_DIR)
