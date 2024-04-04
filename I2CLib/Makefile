# build I2CLib
#

all: $(LIB_DIR)/I2CLib.X.a

$(LIB_DIR)/I2CLib.X.a: I2C.o
	$(LINKER) -r $(LIB_DIR)/I2CLib.X.a I2C.o --target PIC16F1503

I2C.o: I2C.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o I2C.o I2C.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_I2C_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp
	
clean:
	rm *.d *.i *.o

