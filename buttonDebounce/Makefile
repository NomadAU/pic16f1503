# build button debounce example

all: ButtonBounce.hex $(LIB_DIR)\timerLib.X.a 

ButtonBounce.hex: ButtonBounce.o
	$(ASSEMBLER) -mcpu=PIC16F1503 ButtonBounce.o \
	-o ButtonBounce.hex \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull  -l"TimerLib.X.a" -L$(LIB_DIR) \
	-mcallgraph=std -Wl,-Map=ButtonBounce.map -mno-download-hex -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4 	
	
ButtonBounce.o: ButtonBounce.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o ButtonBounce.o ButtonBounce.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 \
	-I$(INCLUDE_DIR) -xassembler-with-cpp

clean:
	rm *.d *.i *.lst *.o *.sym *.rlf *.map 
