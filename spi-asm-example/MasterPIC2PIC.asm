#ifdef MASTER_PIC_2_PIC
; Master: Sample code to demonstrate use of the MSSP module as master driving a
;        slave device via SPI
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

#if 0
skipz  MACRO
    btfss	STATUS, STATUS_Z_POSITION
ENDM
#endif
    
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
    SPICounter:		DS	1
    d1:			DS	1
    d2:			DS	1
    d3:			DS	1
    ssp1_rx_data:	DS	1
    SSP1STAT_copy:	DS	1
    
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
    
    call	SPI_init
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

    ; SDO   - RA4 - output
    
    banksel	TRISA	
    movlw	11101111B	; set RA4 as output (alternative SDO pin)
	       ;xx------	unimplemented read/set as 1
	       ;--1-----	TRISA5 input push button
	       ;---0----	TRISA4 output SDO pin
	       ;----x---	unimplemented read/set as 1
	       ;-----1--	TRISA2 input
	       ;------1-	TRISA1 input
	       ;-------1	TRISA0 input
    movwf	TRISA
    
    ; SCK   - RC0 - output
    ; SDI   - RC1 - input
    ; LED   - RC2 - output
    ; SS    - RC3 - output
    
    movlw	00110010B	; set RC0 (SCK) and RC2 (LED) as output, RC1 (SDI) as input not used 
	       ;xx------	unimplemented read/set as 1
	       ;--1-----	TRISC5 input
	       ;---1----	TRISC4 input
	       ;----0---	TRISC3 output SS
	       ;-----0--	TRISC2 output RC2 is red LED
	       ;------1-	TRISC1 input RC1 SDI pin (not used)
	       ;-------0	TRISC0 output RC0 SCK pin 
    movwf	TRISC
    
    return
  
SPI_init:
    clrf	INTCON		; Disable all interrupts
    banksel	TRISC
    
    ; set alternative SDO pin
    banksel	APFCON
    bsf		APFCON, APFCON_SDOSEL_POSN  ; SDO function is on RA4
    
    ; complete the rest of SPI setup but don't enable just yet
    banksel	SSP1STAT
    movlw       01000000B
	       ;0-------	SMP input data sampled at end of data o/p time
	       ;-1------	CKE xmit occurs on transition from active to idle clock
	       ;--xxxxxx	I2C only
    movwf       SSP1STAT
    
    ; set SDO function to alternative RA4 pin
    banksel	APFCON
    bsf		APFCON, APFCON_SDOSEL_POSN  ; SDO function is on RA4
    
    ; set the SPI mode and clock speed
    banksel     SSP1CON1
    movlw       00110010B
	       ;0-------	WCOL write collision bit - no collision
	       ;-0------	SSPOV receive overflow indicator bit - no OF
	       ;--1-----	SPEN enable synchronous serial port
	       ;---1----	CKP clock polarity, idle state for clock is high
	       ;----0010	SPI Master mode, Fosc/64
    movwf       SSP1CON1
    
    ; set SS high
;    banksel	LATC
;    bsf		LATC, LATC_LATC3_POSN
    
    call	stabilisationWait
    
    return

;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    call	delay5s
    movlw	1
    movwf	SPICounter
loop:
    movf	SPICounter,w
    call	flashLED
    call	SPI_write
    call	delay1s
    
    ; check if counter has gone past max
    
    goto	loop
    
    ; 8 bit value is in WREG 
SPI_write:
    
    ; now write current count to the buffer
    movf	SPICounter,w
    
    ; pull SS low
    banksel	LATC
    bcf	LATC, LATC_LATC3_POSN
    
;    call	delay5s
    
    ; write the value to SPI transmit buffer
    banksel	SSP1BUF
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
readBuf:
    ; clear the BF flag by reading SSP1BUF
    banksel	SSP1BUF
    movf	SSP1BUF, w
   
    ; set SS high
    banksel	LATC
    bsf		LATC, LATC_LATC3_POSN
    
    ; increment the SPICounter
    incf	SPICounter,f
    ; reset if counter has gone past MAX_COUNT_VALUE
    movlw	MAX_COUNT_VALUE + 1
    xorwf	SPICounter, w
    skipz	
    return
    
    movlw	1
    movwf	SPICounter
    return

spi1_write:
    ; wait till transmit buffer to be available if needed
    banksel SSP1STAT ; Make sure you are in the SSP registers bank
    btfss   SSP1STAT, SSP1STAT_BF_POSN ; Make sure there is nothing received in the buffer
    goto    spi1_send ; Buffer is available for transmission
    banksel SSP1BUF
    movf    SSP1BUF, W ; There is a received data waiting for handling
    movwf   ssp1_rx_data ; <- should declare this variable. store the data if needed
spi1_send:
    call    delay500ms
    banksel SSP1CON1
    bcf     SSP1CON1, SSP1CON1_WCOL_POSN ; Clear the write collision flag before writing to the buffer
    movlw   10101010B
    
    banksel SSP1BUF
    movwf   SSP1BUF ; Write the data first
    banksel SSP1CON1
    btfsc   SSP1CON1, SSP1CON1_WCOL_POSN ; Then check whether a write collision has occured
    goto    spi1_write ; The shift register is still busy, keep checking until the data has been sent
    ; Now that the data has sent successfully, wait till BF flag goes high
    
    movlw   5
    call    flashLED
    
spi1_wait_response:
    banksel SSP1STAT ; Make sure you are in the SSP registers bank
    btfss   SSP1STAT, SSP1STAT_BF_POSN
    goto    spi1_wait_response

    ; clear the BF flag by reading SSP1BUF
    banksel SSP1BUF
    movf    SSP1BUF, w
    return
    
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
