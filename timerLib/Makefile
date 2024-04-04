# build TimerLib
#

all: $(LIB_DIR)/TimerLib.X.a

$(LIB_DIR)/TimerLib.X.a: timer.o
	$(LINKER) -r $(LIB_DIR)/TimerLib.X.a timer.o --target PIC16F1503

timer.o: timer.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o timer.o timer.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_TIMER_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp
	
clean:
	rm *.d *.i *.o
