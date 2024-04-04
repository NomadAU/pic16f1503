#define USE_MCP4901
;#define USE_ALT_DAC_REGISTERS
; DACTest: Sample code to demonstrate how to drive the DAC module.  Output was
;	    wired to the gate of a MOSFET.  One DAC output pin was insufficient
;	    to provide the required threshold voltage (due to V drop - FET pulls
;	    19-26mA) so both DAC output pins had to be used.
;
;	    If USE_MCP4901 is defined, the code uses the MCP4901 functions in
;	    the DAC library instead
;
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
;   Pin summary
;	1	VDD	+3.3V
;	8	RC2	Red LED
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 28 Jan 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h
;
;				      PIC16F1503
;			       +----------:_:----------+
;		   +3.3V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> DAC output
;			 <>  3 : RA4               RA1 : 12 <> 
;			 <>  4 : RA3/MCLR          RA2 : 11 <> DAC output     
;	                 <>  5 : RC5               RC0 : 10 <> StringA
;                        <>  6 : RC4               RC1 : 9  <> 
;                        <>  7 : RC3               RC2 : 8  <> Red LED
;			       +-----------------------:
;                                       DIP-14
;
; when using MCP4901 DAC module
;                                     PIC16F1503
;                              +----------:_:----------+
;                    +5V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> 
;	         SDO o/p <>  3 : RA4               RA1 : 12 <> 
;                        <>  4 : RA3/MCLR          RA2 : 11 <>      
;                        <>  5 : RC5               RC0 : 10 <> SCK (Clock) o/p
;                StringA <>  6 : RC4               RC1 : 9  <> SDI not used
; SS (Slave Select) o/p  <>  7 : RC3               RC2 : 8  <> RED LED o/p
;                              +-----------------------+
;                                       DIP-14

PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "macros.inc"
#include "Timer.inc"
#include "DAC.inc"
    
; See respective data sheet for additional information on configuration word.
config FOSC = INTOSC    ; Oscillator Selection bits (HS oscillator)
config WDTE = OFF       ; Watchdog Timer (WDT disabled)
config PWRTE = OFF      ; Power-up Timer Enable bit (Power-up Timer is disabled)
config CP = OFF         ; Code Protection bit (Code protection disabled)
config MCLRE = ON
config BOREN = ON
config CLKOUTEN = OFF
config WRT = OFF
config STVREN = OFF
config LVP = OFF
config LPBOR = OFF
config BORV = LO
    
#ifdef USE_MCP4901
 #define	LOW_DAC_SETTING	    123 ;100 ;115
 #define	HIGH_DAC_SETTING    180 ;254 ;240
#else
 #define	LOW_DAC_SETTING	    25
 #define	HIGH_DAC_SETTING    31
#endif
    
TOGGLE_RED_LED MACRO	
    banksel	LATC
    movlw	LATC_LATC2_MASK   ; set bit for RC2
    xorwf	LATC, f
ENDM
    
DO_DELAY MACRO	
    call	delay100ms
ENDM
    
;**********************************************************************
; Power-On-Reset entry point
;**********************************************************************
PSECT POR_Vec,global,class=CODE,delta=2
    global  resetVec
resetVec:
    goto    main

;objects in Common RAM - address 70h
psect udata_shr,global,class=COMMON,space=1,delta=1,noexec
    d1:			DS	1
    d2:			DS	1
    d3:			DS	1
    currentVoltage:	DS	1
    WRegCopy:		DS	1
    shiftBuffer:	DS	1
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    retfie
    
END_ISR_Vec:

psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
#ifdef USE_MCP4901
    call	MCP4901_init
#else
    call	DAC_init
#endif
    return
   
setupOscillator:
    ; initialise internal oscillator to 16MHz
    banksel	OSCCON
    movlw	01111000B	    ; Int. osc. 16 MHz
    movwf	OSCCON 
    btfss	HFIOFR	    ; Int. osc. running?
    goto	$-1		    ; No, loop back
    btfss	HFIOFS	    ; Osc. stable?
    goto	$-1		    ; No, loop back.
    return

setupPrescaler:
    ; The following calculation is used to determine the period of each TMR0 tick
    ; when Ficyc = Fosc(16 MHz)/4 = 4 MHz, and a 1:256 prescaler is used
    ; 1/(4MHz/256/256) = 16.384 ms
    banksel	OPTION_REG
    movlw       00000111B	    ; prescaler 1/128; Ficyc = Fosc(16 MHz)/4 = 4 MHz
	       ;-----000	1:2
	       ;-----001	1:4
	       ;-----010	1:8
	       ;-----011	1:16
	       ;-----100	1:32
	       ;-----101	1:64
	       ;-----110	1:128
	       ;-----111	1:256
    movwf       OPTION_REG	    ; giving 4 MHz/128 = 31250 Hz (period = 32 us)
    return

setupIOPins:
    banksel	ANSELA
    clrf	ANSELA		    ; set all PORTA pins to digital I/O
    clrf	ANSELC		    ; set all PORTC pins to digital I/O
    
    ; set all pins as input apart from RC0 (StringA).  DAC output pins will be
    ; set by call to DAC library
    banksel	TRISA		    ; set PORTA pins output
    movlw	11111111B
    movwf	TRISA
    movwf	TRISC
    bcf		TRISC, TRISC_TRISC2_POSN    ; set LED RC2 output  
    
#ifdef USE_MCP4901
    bcf		TRISC, TRISC_TRISC4_POSN   ; set RC4 StringA as output
#else
    bcf		TRISC, TRISC_TRISC0_POSN   ; set RC0 StringA as output
#endif
    
#ifdef USE_MCP4901
    call	MCP4901_pin_setup
#else
    call	DAC_pin_setup
#endif
    
    return

;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    call	turnOnStringA
loop:
    ; set the output voltage value, ranging from 
    ; 00011111B to 00000000B  20 (LEDs low) to 31 (LEDs high)
    movlw	LOW_DAC_SETTING
    movwf	currentVoltage
   
brightenLED:
    call	flashLED
    movf	currentVoltage, w
;    movlw	00000100B
#ifdef USE_MCP4901
    call	MCP4901_write
#else
    call	DAC_write
#endif
    
    DO_DELAY
;    goto	brightenLED
    
    incf	currentVoltage, f
    ; check if overflowed into bit 5
    movlw	HIGH_DAC_SETTING + 1
    xorwf	currentVoltage, w
;    andwf	currentVoltage, w
    skipz
    goto	brightenLED
    
    ; now start to dim the LED
    movlw	HIGH_DAC_SETTING - 1
    movwf	currentVoltage
dimLED:
    movf	currentVoltage, w
#ifdef USE_MCP4901
    call	MCP4901_write
#else
    call	DAC_write
#endif
    
    DO_DELAY
    decf	currentVoltage, f
    ; check if we've gone below 20
    movlw	LOW_DAC_SETTING - 1
    xorwf	currentVoltage, w
    skipz
    goto	dimLED
    ; now start to brighten the LED
    movlw	LOW_DAC_SETTING
    movwf	currentVoltage
    goto	brightenLED

turnOnStringA:
    banksel	LATC
    movlw	00000001
    movwf	LATC
    return
    
flashLED:
    banksel	LATC
    bsf		LATC, LATC_LATC2_POSN
    call	delay100ms
    bcf		LATC, LATC_LATC2_POSN
    return

END resetVec