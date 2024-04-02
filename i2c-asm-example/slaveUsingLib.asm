#ifdef SLAVE_USING_LIB
; Slave: Sample code to demonstrate use of the MSSP module to drive 
;	 slave device via I2C
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
;   Pin summary
;	1	VDD	+3.3V
;	8	RC2	Red LED
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 4 Mar 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h
;
;
;                                     PIC16F1503
;                              +----------:_:----------+
;                    +5V <>  1 : VDD               VSS : 14 <> GND
;            RED LED o/p <>  2 : RA5               RA0 : 13 <> 
;	                 <>  3 : RA4               RA1 : 12 <> 
;                        <>  4 : RA3/MCLR          RA2 : 11 <>      
;                        <>  5 : RC5               RC0 : 10 <> SCL (input)
;                        <>  6 : RC4               RC1 : 9  <> SDA (input) 
;                        <>  7 : RC3               RC2 : 8  <> 
;                              +-----------------------+
;                                       DIP-14
;    
    
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "Timer.inc"
;#include "blink.inc"
#include "I2C.inc"
    

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
    
#define ISRCalled	0
    
//I2C Test Properties
#define SLAVE_ADDRESS 0x60	; unique address for this slave
    
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
    bufferValue:	DS	1
    bufferCopy:		DS	1
    I2C_control:	DS	1
    blinkByte:		DS	1
    blinkCount:		DS	1
    bitCount:		DS	1
    colourWord:		DS	1
    slaveAddress:	DS	1
    counter:		DS	1
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    banksel	PIR1
    btfsc	PIR1, PIR1_SSP1IF_POSN
    goto	SSP_or_BCL_set
    btfss	PIR2, PIR2_BCL1IF_POSN
    retfie
    
SSP_or_BCL_set:
    call	I2C_processMSSPInterrupt
    retfie
    
END_ISR_Vec:

;PSECT MainCode,global,class=CODE,delta=2
psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupIOPins
    call	I2C_InitI2CPins
    movlw	SLAVE_ADDRESS
    call	I2C_SlaveInit
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

setupIOPins:  ; RA0 - SCL, RA1 = SDA
    banksel	ANSELA
    clrf	ANSELA		; all PORTA pins digital
    clrf	ANSELC		; all PORTC pins digital

    ; set all PORTA pins as output
    banksel	TRISA	
    clrf	TRISA		; set all PORTA as output
    ; set all as output
    clrf	TRISC
    
    ; turn off LED
    banksel	LATA
    bcf		LATA, LATA_LATA5_POSN
    return
  
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    clrf	I2C_control
    
loop:
    btfss	I2C_control, dataReceived
    goto	$-1
    bcf		I2C_control, dataReceived
    movf	bufferValue,w
    call	flashLEDMultipleFast
    ; increment the buffer and release the clock
    incf	bufferValue,f
    call	I2C_ClockRelease
    goto	loop
    
turnOnRedLED:
    banksel	LATA
    bsf		LATA, LATA_LATA5_POSN
    return
    
turnOffRedLED:
    banksel	LATA
    bcf		LATA, LATA_LATA5_POSN
    return
   
turnOnOrangeLED:
turnOnGreenLED:    
turnOffOrangeLED:
turnOffGreenLED:    
    
flashLEDMultipleFast:
    movwf	counter
flashAgainFast:
    call	flashLEDFast
    call	delay50ms
    decfsz	counter, f
    goto	flashAgainFast
    return

flashLEDFast:
    banksel	LATA
    bsf		LATA, LATA_LATA5_POSN
    call	delay50ms
    bcf		LATA, LATA_LATA5_POSN
    return
 
#endif
