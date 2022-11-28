V=0
INCLUDE_DIR=./include
SRC_DIR=./src
OBJ_DIR=./obj
CC=gcc
CFLAGS=-I$(INCLUDE_DIR)
LINKER=-lglut -lGL

CFLAGS+=-DGBEMU_DBG=1

$(shell mkdir -p $(OBJ_DIR))

_HEADERS=gbcpu.h isa.h memorymap.h utils.h
HEADERS=$(patsubst %,$(INCLUDE_DIR)/%,$(_HEADERS))

_OBJ=main.o isa.o utils.o memorymap.o utils.o
OBJ=$(patsubst %,$(OBJ_DIR)/%,$(_OBJ))

$(OBJ_DIR)/%.o: src/%.c $(HEADERS)
	$(CC) -c -o $@ $< $(CFLAGS)

gameboyemu: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

cleanobj:
	rm -rf $(OBJ_DIR)/*.o
	
cleanexec:
	rm -rf gameboyemu

clean: cleanobj cleanexec