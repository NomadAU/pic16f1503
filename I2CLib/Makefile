# build I2CLib
#
ASSEMBLER="C:\Program Files\Microchip\xc8\v2.46\pic-as\bin\pic-as.exe"
LINKER="C:\Program Files\Microchip\xc8\v2.46\bin\xc8-ar.exe"
INCLUDE_DIR="C:\MJB\Dev\PICProjects\1603includes"
DEVICE_FAMILY_PACK="C:/Program Files/Microchip/MPLABX/v6.20/packs/Microchip/PIC12-16F1xxx_DFP/1.7.242/xc8"

all: I2CLib.X.a

I2CLib.X.a: I2C.o
	$(LINKER) -r I2CLib.X.a I2C.o --target PIC16F1503

I2C.o: I2C.asm
	$(ASSEMBLER) -mcpu=PIC16F1503 -c -o I2C.o I2C.asm \
	-mdfp=$(DEVICE_FAMILY_PACK) \
	-msummary=+mem,-psect,-class,-hex,-file,-sha1,-sha256,-xml,-xmlfull -fmax-errors=20 -mwarn=0 -DBUILD_I2C_LIB \
	-I$(INCLUDE_DIR) -xassembler-with-cpp

