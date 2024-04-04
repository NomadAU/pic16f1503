# build I2C examples

all: masterUsingLib.hex slaveUsingLib.hex $(LIB_DIR)\I2CLib.X.a

masterUsingLib.hex: masterUsingLib.o
	$(ASSEMBLER) -mcpu=PIC16F1503 masterUsingLib.o \
	-o masterUsingLib.hex \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull  -l"TimerLib.X.a" -l"BlinkLib.X.a" -l"I2CLib.X.a"  -L$(LIB_DIR) \
	-mcallgraph=std -Wl,-Map=masterUsingLib.map -mno-download-hex -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4  	
	
slaveUsingLib.hex: slaveUsingLib.o
	$(ASSEMBLER) -mcpu=PIC16F1503 slaveUsingLib.o \
	-o slaveUsingLib.hex \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull  -l"TimerLib.X.a" -l"BlinkLib.X.a" -l"I2CLib.X.a"  -L$(LIB_DIR) \
	-mcallgraph=std -Wl,-Map=slaveUsingLib.map -mno-download-hex -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4  	
	
masterUsingLib.o: masterUsingLib.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o masterUsingLib.o masterUsingLib.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DMASTER_USING_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp

slaveUsingLib.o: slaveUsingLib.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o slaveUsingLib.o slaveUsingLib.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DSLAVE_USING_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp

clean:
	rm *.d *.i *.lst *.o *.sym *.rlf *.map
