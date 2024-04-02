# build blinkLib
#
ASSEMBLER="C:\Program Files\Microchip\xc8\v2.46\pic-as\bin\pic-as.exe"
LINKER="C:\Program Files\Microchip\xc8\v2.46\bin\xc8-ar.exe"
INCLUDE_DIR="..\1603includes"
DEVICE_FAMILY_PACK="C:/Program Files/Microchip/MPLABX/v6.20/packs/Microchip/PIC12-16F1xxx_DFP/1.7.242/xc8"

all: blinkLib.X.a

blinkLib.X.a: blink.o
	$(LINKER) -r blinkLib.X.a blink.o --target PIC16F1503

blinkLib.o: blinkLib.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o blink.o blink.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_I2C_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp
	
clean:
	rm *.d *.i *.o *.a

