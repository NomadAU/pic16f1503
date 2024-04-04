# build blinkLib
#

all: $(LIB_DIR)/blinkLib.X.a

$(LIB_DIR)/blinkLib.X.a: blink.o
	$(LINKER) -r $(LIB_DIR)/blinkLib.X.a blink.o --target PIC16F1503

blink.o: blink.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o blink.o blink.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_BLINK_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp
	
clean:
	rm *.d *.i *.o
