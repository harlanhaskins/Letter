CC=clang++
EXECUTABLE=letter
INSTALL_DIR=/usr/local/bin/
BUILD_DIR=build
EXECUTABLE_LOC=$(BUILD_DIR)/$(EXECUTABLE)
CFLAGS=-g -o $(EXECUTABLE_LOC)
LLFLAGS= $(shell llvm-config --cxxflags --ldflags --system-libs --libs core mcjit orcjit native runtimedyld)
FILES=src/*.cpp

$(EXECUTABLE_LOC):
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $(LLFLAGS) $(FILES)

all: $(EXECUTABLE_LOC)

clean:
	rm -rf $(BUILD_DIR)

purge:
	rm $(EXECUTABLE)

install: all
	mv $(EXECUTABLE_LOC) $(INSTALL_DIR)
