#ifdef MASTER_USING_LIB
; Master: Sample code demonstrating use of the I2CLib.
;	2 PIC MCUs are connected, one as master, one as slave via I2C.
;	A variety of transmission patterns are implemented, including use of
;	STOP and RESTART, multiple consecutive writes and reads.
;	
;	Whenever a value is written to the slave, it is implemented and stored.
;	The implemented value is returned to the master whenever the master
;	issues a read from the slave.
;
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
;   Pin summary
;	1	VDD	+3.3V
;	8	RC2	Red LED
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 6 Mar 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h
;
;
;                                     PIC16F1503
;                              +----------:_:----------+
;                    +5V <>  1 : VDD               VSS : 14 <> GND
;            RED LED o/p <>  2 : RA5               RA0 : 13 <> o/p
;	                 <>  3 : RA4               RA1 : 12 <> o/p
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
#include "defines.inc"
#include "macros.inc"
#include "Timer.inc"
#include "blink.inc"
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
    
//I2C constants
#define BAUD	    100		; Desired Baud rate (100K)
#define FOSC	    16000	; Oscillator Clock in kHz    
#define SLAVE_ADDRESS 0x60	; address assigned to PIC slave
    
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
    counter:		DS	1
    bufferValue:	DS	1
    blinkByte:		DS	1
    blinkCount:		DS	1
    colourWord:		DS	1
    bitCount:		DS	1
    bufferCopy:		DS	1
    I2C_control:	DS	1
    slaveAddress:	DS	1
    
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
    call	setupPrescaler
    call	setupIOPins
    call	I2C_InitI2CPins
    call	I2C_MasterInit
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

setupIOPins:  ; RA0 - SCL, RA1 = SDA
    banksel	ANSELA
    clrf	ANSELA		; all PORTA pins digital
    clrf	ANSELC		; all PORTC pins digital

    ; set all PORTA pins as output
    banksel	TRISA	
    clrf	TRISA		; set all PORTA as output
    
    ; pull all PORTA pins low
    banksel	LATA
    clrf	LATA
    
    ; set RC0 and RC1 as input, remainder as output (including RC2=LED)
    banksel	TRISC	
    movlw	00000011B	 
	       ;xx------	unimplemented read/set as 1
	       ;--0-----	TRISC5 output
	       ;---0----	TRISC4 output
	       ;----0---	TRISC3 output
	       ;-----0--	TRISC2 output RC2 is red LED
	       ;------1-	TRISC1 input SDA
	       ;-------1	TRISC0 input SCL 
    movwf	TRISC

    ; pull all PORTC output pins low
    banksel	LATC
    clrf	LATC
    
    return
  
I2C_init:
    ;Configure MSSP module for Master Mode
    banksel	SSP1CON1
    movlw	00101000B   ; Enables MSSP and uses appropriate
			    ; PORTC pins for I2C mode (SSPEN set) AND
			    ; Enables I2C Master Mode (SSPMx bits)
    movwf	SSP1CON1	    ; This is loaded into SSPCON
    ; Configure Input Levels and slew rate as I2C Standard Levels
    banksel	SSP1STAT
    movlw	10000000B   ; Slew Rate control (SMP) set for 100kHz
    movwf	SSP1STAT	    ; mode and input levels are I2C spec,
    ; loaded in SSP1STAT

    ; Configure Baud Rate
    banksel	SSP1ADD
    movlw	FOSC / (4 * BAUD) - 1	; Calculates SSPADD Setting for
    movwf	SSP1ADD			; desired Baud rate and sets up SSPADD
    
    call	stabilisationWait
    
    return

;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    movlw	1
    movwf	bufferValue
    movlw	SLAVE_ADDRESS
    movwf	slaveAddress
    ; add a delay to allow pulse app to capture flows
    movlw	2
    call	flashLEDMultipleFast
    call	delay1s
    call	delay1s

loop:
    
writeFirst:        
    ; write (first) value x to slave 
    movf	bufferValue,w
    call	flashLEDMultipleFast
    setFirstWrite			; indicate multiple writes
    movf	bufferValue,w
    call	I2C_WriteToSlave
    skipIfOK
    goto	failed
    call	delay500ms
    
writeNext:    
    ; write (next) value x to slave 
    movf	bufferValue,w
    call	flashLEDMultipleFast
    setNextWrite			; indicate multiple writes
    movf	bufferValue,w
    call	I2C_WriteToSlave
    skipIfOK
    goto	failed
    call	delay500ms

writeLast:
    ; write (last) value x to slave, with restart
    movf	bufferValue,w
    call	flashLEDMultipleFast
    setLastWrite			; indicate last write
    setRestart				; finish with a restart
    movf	bufferValue,w
    call	I2C_WriteToSlave
    skipIfOK
    goto	failed
    call	delay500ms
    
    ; =============== read from slave =====================
    ; read a few times from slave
readFirst:    
    call	turnOnRedLED
    call	delay1s
    call	turnOffRedLED
    call	delay500ms
    setFirstRead			; indicate start of multiple reads
    call	I2C_ReadFromSlave
    skipIfOK
    goto	failed
    
    movf	bufferValue,w
    call	flashLEDMultipleFast
    call	delay500ms

readNext:
    call	turnOnRedLED
    call	delay1s
    call	turnOffRedLED
    call	delay500ms
    setNextRead			; indicate start of multiple reads
    call	I2C_ReadFromSlave
    skipIfOK
    goto	failed
    
    movf	bufferValue,w
    call	flashLEDMultipleFast
    call	delay500ms
    
readLast:
    ; final read from slave
    call	turnOnRedLED
    call	delay1s
    call	turnOffRedLED
    call	delay500ms
    setLastRead
    setRestart
    call	I2C_ReadFromSlave
    skipIfOK
    goto	failed

    movf	bufferValue,w
    call	flashLEDMultipleFast
    call	delay500ms
    
    goto	checkForReset

failed:
    movlw	10
    call	flashLEDMultipleFast
    goto	doDelay

checkForReset:    
    ; reset counter if value is 6 or greater
    movlw	6
    subwf	bufferValue,w   ; subtract W from the File Register    
    btfsc	STATUS,STATUS_C_POSN	; Test for Carry Flag Set 
    goto	resetValue		; SomeVar >= CompTo
    goto	doDelay
resetValue:
    movlw	1
    movwf	bufferValue
    
doDelay:    
    call	delay1s
    call	delay1s
    goto	loop

stabilisationWait:
    call	delay1s
    return
    
flashLEDMultiple:
    movwf	counter
flashAgain:
    call	flashLED
    call	delay100ms
    decfsz	counter, f
    goto	flashAgain
    return
    
flashLEDMultipleFast:
    movwf	counter
flashAgainFast:
    call	flashLEDFast
    call	delay50ms
    decfsz	counter, f
    goto	flashAgainFast
    return
    
flashLEDMultipleSlow:
    movwf	counter
flashAgainSlow:
    call	flashLED
    call	delay500ms
    decfsz	counter, f
    goto	flashAgainSlow
    return
    
flashLED:
    banksel	LATA
    bsf		LATA, LATA_LATA5_POSN
    call	delay100ms
    bcf		LATA, LATA_LATA5_POSN
    return
 
flashLEDFast:
    banksel	LATA
    bsf		LATA, LATA_LATA5_POSN
    call	delay50ms
    bcf		LATA, LATA_LATA5_POSN
    return
 
turnOnRedLED:
    banksel	LATA
    bsf		LATA, LATA_LATA5_POSN
    return
    
turnOffRedLED:
    banksel	LATA
    bcf		LATA, LATA_LATA5_POSN
    return

blinkLEDx:
    call	turnOnRedLED
    call	delay200ms
    call	turnOffRedLED
    return
    
turnOnOrangeLED:
turnOnGreenLED:    
turnOffOrangeLED:
turnOffGreenLED:    
        
#endif


