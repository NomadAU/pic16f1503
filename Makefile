export ASSEMBLER="C:\Program Files\Microchip\xc8\v2.46\pic-as\bin\pic-as.exe"
export LINKER="C:\Program Files\Microchip\xc8\v2.46\bin\xc8-ar.exe"
export INCLUDE_DIR=../1503includes
export LIB_DIR=../1503libs
export DEVICE_FAMILY_PACK="C:/Program Files/Microchip/MPLABX/v6.20/packs/Microchip/PIC12-16F1xxx_DFP/1.7.242/xc8"

SUBDIRS = I2CLib timerLib blinkLib DACLib i2c-asm-example dac-asm-example

#SUBDIRS = src doc whatever

.PHONY: all clean

all clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir -f Makefile $@; \
	done



#clean: $(SUBDIRS)
#$(SUBDIRS):
#	$(MAKE) -C $@

#all: $(SUBDIRS)
#$(SUBDIRS):
#	$(MAKE) -C $@
#
#.PHONY: all $(SUBDIRS)