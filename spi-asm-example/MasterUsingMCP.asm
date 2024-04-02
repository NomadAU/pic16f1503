
#ifdef MASTER_USING_MCP
;#define USE_ALT_DAC_REGISTERS
; Master: Sample code to demonstrate use of the MSSP module as master driving 
;        MCP4901 DAC via SPI
;
;   Pin summary
;	1	VDD	+5V
;	3	RA4	Alternative SDO
;	7	RC3	Slave Select
;	8	RC2	Red LED
;	9	RC1	SDI
;      10	RC0	SCK
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 17 Feb 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h
;
;
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
;    
    
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "defines.inc"
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

#define MAX_COUNT_VALUE 5    
  
;**********************************************************************
; Power-On-Reset entry point
;**********************************************************************
PSECT POR_Vec,global,class=CODE,delta=2
    global  resetVec
resetVec:
    goto    main

;objects in Common RAM - address 70h
psect udata_shr,global,class=COMMON,space=1,delta=1,noexec
    flagWord:		DS	1
    bufferValue:	DS	1
    flashCounter:	DS	1
    voltageLevel:	DS	1
    d1:			DS	1
    d2:			DS	1
    d3:			DS	1
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

;PSECT MainCode,global,class=CODE,delta=2
psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupIOPins

    movlw	5
    call	flashLED
    
;    call	SPI_init
    call	MCP4901_init
    return
   
setupOscillator:
    ; initialise internal oscillator to 16MHz
    banksel	OSCCON
    movlw	01111000B	    ; Int. osc. 16 MHz
    movwf	OSCCON 
    return
    btfss	HFIOFR	    ; Int. osc. running?
    goto	$-1		    ; No, loop back
    btfss	HFIOFS	    ; Osc. stable?
    goto	$-1		    ; No, loop back.
    return

setupIOPins:
    banksel	ANSELA
    clrf	ANSELA		; all PORTA pins digital
    clrf	ANSELC		; all PORTC pins digital

    banksel	TRISA	
    movlw	11111111B	; set all pins input
    movwf	TRISA
    movlw	11111011B	; set all pins input except RC2 (LED)
    movwf	TRISC
    
    call	MCP4901_pin_setup

    return
  
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    call	delay5s
    movlw	100
    movwf	voltageLevel
loop:
    movlw	1
    call	flashLED
    
    movf	voltageLevel,w
    call	MCP4901_write
    
    call	delay5s
    
    call	MCP4901_shutdown
    
    call	delay5s
    
    ; increment the voltage level
    incf	voltageLevel, f
    skipz
    goto	loop
    movlw	1
    movwf	voltageLevel
    goto	loop
    
flashLED:
    movwf	flashCounter
flashAgain:
    banksel	LATC
    bsf		LATC, LATC_LATC2_POSN
    call	delay100ms
    bcf		LATC, LATC_LATC2_POSN
    call	delay100ms
    decfsz	flashCounter,f
    goto	flashAgain
    return

stabilisationWait:
    call	delay1s
    return

#endif


    

