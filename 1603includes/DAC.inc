; Pins used for internal DAC module
;				      PIC16F1503
;			       +----------:_:----------+
;		   +3.3V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> DAC output
;			 <>  3 : RA4               RA1 : 12 <> 
;			 <>  4 : RA3/MCLR          RA2 : 11 <> DAC output     
;	                 <>  5 : RC5               RC0 : 10 <> 
;                        <>  6 : RC4               RC1 : 9  <> 
;                        <>  7 : RC3               RC2 : 8  <> Red LED
;			       +-----------------------:
;                                       DIP-14
    
; Pins used for external MCP4901 DAC module
;                                     PIC16F1503
;                              +----------:_:----------+
;                    +5V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> 
;	         SDO o/p <>  3 : RA4               RA1 : 12 <> 
;                        <>  4 : RA3/MCLR          RA2 : 11 <>      
;                        <>  5 : RC5               RC0 : 10 <> SCK (Clock) o/p
;                        <>  6 : RC4               RC1 : 9  <> SDI not used
; SS (Slave Select) o/p  <>  7 : RC3               RC2 : 8  <> RED LED o/p
;                              +-----------------------+
;                                       DIP-14
    
#ifdef BUILD_DAC_LIB

; if NOT using flash memory reads, then alternative registers can be used
#define SHIFTBUFFER PMDATL
#define WREG_COPY PMDATH
    
extrn		d1,d2,d3,WRegCopy,shiftBuffer
global		MCP4901_init,MCP4901_write_default,MCP4901_write_alt,MCP4901_shutdown
global		MCP4901_pin_setup,DAC_init,DAC_pin_setup,DAC_write,DAC_disable
global		DAC_enable
 
#else
 
global 		d1,d2,d3
 
 ; if NOT using flash memory reads, then alternative registers can be used
 ;#define USE_ALT_DAC_REGISTERS
 #ifdef USE_ALT_DAC_REGISTERS    
  #define MCP4901_write MCP4901_write_alt
  global 	WRegCopy,shiftBuffer
  ; add labels to satisfy the linker which has extrn's for these
  WRegCopy: 
  shiftBuffer:
 #else
  #define MCP4901_write MCP4901_write_default
  global 	WRegCopy,shiftBuffer
 #endif
extrn		MCP4901_init,MCP4901_write_default,MCP4901_write_alt,MCP4901_shutdown
extrn		MCP4901_pin_setup,DAC_init,DAC_pin_setup,DAC_write,DAC_disable
extrn		DAC_enable
#endif