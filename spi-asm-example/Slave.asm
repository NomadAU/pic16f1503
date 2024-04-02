#ifdef SLAVE
#define USE_SPI_INTERRUPT
; Slave: Sample code demonstrating SPI slave using
;	 - simple loop testing for BF flag, and
;	 - interrupt driven
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
;	    SDO not used <>  3 : RA4               RA1 : 12 <> 
;                        <>  4 : RA3/MCLR          RA2 : 11 <>      
;                        <>  5 : RC5               RC0 : 10 <> SCK (Clock) i/p
;                        <>  6 : RC4               RC1 : 9  <> SDI (Data In)
; SS (Slave Select) i/p  <>  7 : RC3               RC2 : 8  <> RED LED o/p
;                              +-----------------------+
;                                       DIP-14
;    
    
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>

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
    
#define	    DATA_RECEIVED_BIT 0
#define	    IN_SEND_MODE 1
    
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
    d1:			DS	1
    d2:			DS	1
    d3:			DS	1
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
#ifdef USE_SPI_INTERRUPT
ISR_Vec:
    banksel	PIR1
    btfsc	PIR1, PIR1_SSP1IF_POSN
    call	processSPIReceive
    retfie
    
processSPIReceive:
    ; clear the interrupt flag
    bcf		PIR1, PIR1_SSP1IF_POSN
    bsf		flagWord, DATA_RECEIVED_BIT

    ; copy the value from the receive buffer
    banksel	SSP1BUF
    movf	SSP1BUF, w
    movwf	bufferValue
    
    ; write in new value
    movlw	11001100B
    movwf	SSP1BUF

    return

#else
    
ISR_Vec:
    retfie

#endif
END_ISR_Vec:

;PSECT MainCode,global,class=CODE,delta=2
psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupIOPins
    call	SPI_init
    return
   
setupOscillator:
    ; initialise internal oscillator to 16MHz
    banksel	OSCCON
    movlw	01111000B	    ; Int. osc. 16 MHz
    movwf	OSCCON 
;    return
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
    movlw	11101111B	; set RA4 as output (alternative SDO pin) not used
	       ;xx------	unimplemented read/set as 1
	       ;--1-----	TRISA5 input (push button)
	       ;---0----	TRISA4 output SDO pin
	       ;----x---	unimplemented read/set as 1
	       ;-----1--	TRISA2 input
	       ;------1-	TRISA1 input
	       ;-------1	TRISA0 input
    movwf	TRISA
    
    ; SS    - RC3 - input
    ; SCK   - RC0 - input
    ; SDI   - RC1 - input
    ; LED   - RC2 - output
    
    movlw	00111011B	; set RC0 (SCK), RC1 (SDI) and RC3 (SS) as input, RC2 (LED) as output 
	       ;xx------	unimplemented read/set as 1
	       ;--1-----	TRISC5 input
	       ;---1----	TRISC4 input
	       ;----1---	TRISC3 input RC3 is SS (Slave Select) pin
	       ;-----0--	TRISC2 output RC2 is red LED
	       ;------1-	TRISC1 input RC1 SDI pin 
	       ;-------1	TRISC0 input RC0 SCK pin 
    movwf	TRISC

    return
  
SPI_init:
    clrf	INTCON		; Disable all interrupts
    banksel	TRISC
    
    ; set alternative SDO pin, even though we won't be sending any data
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
    
    movlw       00110100B
	       ;0-------	WCOL write collision bit - no collision
	       ;-0------	SSPOV receive overflow indicator bit - no OF
	       ;--1-----	SPEN disable synchronous serial port
	       ;---1----	CKP clock polarity, idle state for clock is high
	       ;----0100	0100 = SPI Slave mode, clock = SCKx pin, SS pin control enabled
    banksel	SSP1CON1
    movwf       SSP1CON1

#ifdef USE_SPI_INTERRUPT
    ; enable up the receive interrupts
    banksel	PIE1
    bsf		PIE1, PIE1_SSP1IE_POSN
    bsf		INTCON, INTCON_PEIE_POSN
    bsf		INTCON, INTCON_GIE_POSN
#endif
    
    ; now wait for the SS line to go high, meaning SPI master is ready
    banksel	PORTC
    btfss	PORTC, PORTC_RC3_POSN
    goto	$-1

    ; now enable SPI
;    banksel	SSP1CON1
;    bsf	SSP1CON1, SSP1CON1_SSPEN_POSN	; enable SPI
    
    call	stabilisationWait
    return

;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    movlw	5
    call	flashLED
loop:
#ifdef USE_SPI_INTERRUPT

    btfss	flagWord, DATA_RECEIVED_BIT
    goto	$-1
    
    bcf		flagWord, DATA_RECEIVED_BIT
    movf	bufferValue,w
    call	flashLED
    
    goto	loop
    
    ; read the data from the buffer
    banksel	SSP1BUF
    movf	SSP1BUF, w
    movwf	bufferValue

    ; write different value to buffer
    movlw	11001100B
    movwf	SSP1BUF

    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
    banksel	PIR1
    ; clear the interrupt flag
    bcf		PIR1, PIR1_SSP1IF_POSN
    
    goto	loop
#else
    ; wait for buffer full flag
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
    movlw	1
    call	flashLED
    
    ; read the value from the receive buffer which clears the BF flag
    banksel	SSP1BUF
    movf	SSP1BUF, w
    
    ; write different value to buffer
    movlw	11001100B
    movwf	SSP1BUF

    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
    goto	loop
#endif
    
    ; write value back to buffer
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
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
    
delay100ms:
    movlw	0x36
    movwf	d1
    movlw	0xE0
    movwf	d2
    movlw	0x01
    movwf	d3
delay100ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay100ms_0
    nop    
    return
	
delay1s:
    movlw	0x23
    movwf	d1
    movlw	0xB9
    movwf	d2
    movlw	0x09
    movwf	d3
delay1s_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay1s_0
    goto	$+1
    return        
#endif
    


