# build DACLib
#

all: $(LIB_DIR)/DACLib.X.a

$(LIB_DIR)/DACLib.X.a: DAC.o
	$(LINKER) -r $(LIB_DIR)/DACLib.X.a DAC.o --target PIC16F1503

DAC.o: DAC.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o DAC.o DAC.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_DAC_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp
	
clean:
	rm *.d *.i *.o
