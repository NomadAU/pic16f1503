# build DAC example

all: DACTest.hex $(LIB_DIR)\timerLib.X.a $(LIB_DIR)\DACLib.X.a

DACTest.hex: DACTest.o
	$(ASSEMBLER) -mcpu=PIC16F1503 DACTest.o \
	-o DACTest.hex \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull  -l"TimerLib.X.a" -l"DACLib.X.a" -L$(LIB_DIR) \
	-mcallgraph=std -Wl,-Map=DACTest.map -mno-download-hex -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4 	
	
DACTest.o: DACTest.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o DACTest.o DACTest.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 \
	-I$(INCLUDE_DIR) -xassembler-with-cpp

clean:
	rm *.d *.i *.lst *.o *.sym *.rlf *.map 
