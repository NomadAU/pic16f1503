#ifdef TEST_MIXED_TIMERS_AND_PWM
//#define SIMULATOR 1
; testMixedTimersAndPWM: Test interaction between Timer0 and Timer1 with PWM
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
; 
;
;   Pin summary
;	1	VDD	+3.3V
;	8	RC2	Red LED
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 28 Jan 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h,-pData_Vec=70h
;
;				      PIC16F1503
;			       +----------:_:----------+
;		   +3.3V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> 
;			 <>  3 : RA4               RA1 : 12 <> 
;			 <>  4 : RA3/MCLR          RA2 : 11 <>      
;	                 <>  5 : RC5               RC0 : 10 <> 
;                        <>  6 : RC4               RC1 : 9  <> 
;                        <>  7 : RC3               RC2 : 8  <> Red LED
;			       +-----------------------:
;                                       DIP-14

PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "defines.inc"
#include "macros.inc"
#include "Timer.inc"
#include "LightPrograms.inc"

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

#define	RED_PIN		RC2
#define TMR0_VALUE 200  ; count-up from d'100' to 255 = 155 counts
#define TMR0_ROLLOVER_COUNT 63  ; this gives close to 1 second interval
#define TMR0_SECONDS_VALUE 3  
#define	fastBit 0   ; bit in flagword denoting TMR1 is in fast mode
			
TOGGLE_RED_LED MACRO	
    banksel	LATC
    movlw	LATC_LATC2_MASK   ; set bit for RC2
    xorwf	LATC, f
ENDM
    
TOGGLE_RC5 MACRO	
    banksel	LATC
    movlw	LATC_LATC5_MASK   ; set bit for RC5
    xorwf	LATC, f
ENDM

SET_TIMER1_COUNTER MACRO	; sets .5 second overflow
    banksel	T1CON	
    movf	TMR1H_val,w
    movwf	TMR1H		    
ENDM
    
SET_MAX_DUTY macro
    banksel	PWM1DCH
    movlw	11111111B
    movwf	PWM1DCH
    movlw	11000000B
    movwf	PWM1DCL
endm
    
SET_MID_DUTY macro
    banksel	PWM1DCH
    movlw	00100000B
    movwf	PWM1DCH
    clrf	PWM1DCL
endm
    
; low duty value testing
; PWM1DCH    PWM1DCL	    Gate voltage    Result
; 1000 0000  0000 0000	    3.57	    Lights off
; 1000 1000  0000 0000	    4.51	    Lights on
; 1000 0100  0000 0000	    4.53	    Lights on
; 1000 0010  0000 0000	    4.53	    Lights on
; 1000 0001  0000 0000	    4.54	    Lights on
; 1000 0000  1000 0000	    4.52	    Lights on
; 1000 0000  0100 0000	    4.52	    Lights on
; 0100 1111  0000 0000	    3.48	    Lights off
; 0101 1111  0000 0000	    3.27	    Lights off
; 0111 1111  0000 0000	    3.30	    Lights off
; 0111 1111  1100 0000	    3.29	    Lights off
    
; 1010 0000  1100 0000	    4.54	    Lights on
; 1010 0000  0100 0000	    4.51	    Lights on
; 1010 0000  0000 0000	    3.38	    Lights off
SET_LOW_DUTY macro
    banksel	PWM1DCH
    movlw	10001000B
    movwf	PWM1DCH
    movlw	00000000B
    movwf	PWM1DCL
;    clrf	PWM1DCL
endm
    
;**********************************************************************
; Power-On-Reset entry point
;**********************************************************************
PSECT POR_Vec,global,class=CODE,delta=2
    global  resetVec
resetVec:
    goto    main

;objects in Common RAM - address 70h
;PSECT Data_Vec,global,class=Data_Vec,space=1,delta=1,noexec
;PSECT Data_Vec,global,class=RAM,space=1,delta=1,noexec    
;PSECT Data_Vec,global,class=RAM,space=1,delta=1,noexec    
psect udata_shr,global,class=COMMON,space=1,delta=1,noexec
    d1:			DS      1	
    d2:			DS      1	
    d3:			DS      1	
    flashCounter:	DS      1	
    tmr0RolloverCount:	DS      1	
    tmr0CountIndex:	DS      1	
    tmr0CountValue:	DS      1	
    tmr0SecondsCount:	DS      1
    tmr1Status:		DS	1
    TMR1H_val:		DS	1
    TMR1H_index:	DS	1
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    btfsc	TMR0IF
    call	processTimer0Expired
    banksel	PIR1
    btfsc	TMR1IF             
    call	processTimer1Expired
    retfie
    
processTimer0Expired:
    bcf         TMR0IF	    ; clear Timer0 interrupt flag
    decfsz      tmr0RolloverCount, f	    ; more ticks?
    return

    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
	
    decfsz	tmr0SecondsCount, f
    return
    
    movlw	TMR0_SECONDS_VALUE
    movwf	tmr0SecondsCount
    
;    bcf         T0IE	    ; disable Timer0 interrupt
    ; rollover count has gone to zero so we toggle the LED
;    TOGGLE_RC5
    call	getNextTMR1HValue
;    SET_TIMER1_COUNTER
;    bsf         T0IE	    ; enable Timer0 interrupt
    return
    
processTimer1Expired:
    bcf		TMR1IF	    ; clear interrupt flag
    SET_TIMER1_COUNTER
;    TOGGLE_RED_LED
    call	toggleStrings
    retfie
    
END_ISR_Vec:

;PSECT MainCode,global,class=CODE,delta=2
psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	setupPWM
    call	initialiseTMR2
    call	turnOnStringA
    clrf	TMR1H_index
    call	initialiseTMR0
    call	initialiseTMR1
    call	initDAC    
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

    banksel	TRISA		    ; set PORTA pins output apart from the 2 input sensor pins
    clrf	TRISA
    
    banksel	TRISC	
    clrf	TRISC		    ; set all PORTC pins as output

    banksel	LATA		    
    clrf	LATA		    ; set all PORTA pins output low
    clrf	LATC		    ; set all PORTC pins output low
    
    return
  
initialiseTMR1:
    banksel	PIE1
    ; just return if TMR1 is already enabled
    btfsc	TMR1IE
    return
    bsf		TMR1IE
    call	getNextTMR1HValue
    movwf	TMR1H_val
    banksel	T1CON
    movwf	TMR1H
    movlw	11000101B
	       ;11------    TMR1CS Timer1 clock source is LFINTOSC
	       ;--00----    T1CKPS 1:1 prescale value
	       ;--01----    T1CKPS 1:2 prescale value
	       ;--10----    T1CKPS 1:4 prescale value
	       ;--11----    T1CKPS 1:8 prescale value
	       ;----x---    Unimplemented
	       ;-----1--    T1SYNC do not sync async clock
	       ;------x-    Unimplemented
	       ;-------1    TMR1ON turn timer on
    movwf      T1CON
    bsf		PEIE
    bsf		GIE
;    RED_LED_ON
    return
    
initialiseTMR0:
    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
    movlw	TMR0_SECONDS_VALUE
    movwf	tmr0SecondsCount
    bsf         T0IE	    ; enable Timer0 interrupt
    bsf         GIE	    ; enable global interrupt
    return
    
setupPWM:
    banksel	PR2
    movlw	128	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movwf	PR2
    banksel	PWM1DCH
    clrf	PWM1DCH
    clrf	PWM1DCL
    
    banksel	PWM1CON
    movlw	11100000B   
	       ;1-------    PWM1EN - PWM1 enabled
	       ;-1------    PWM1OE - output pin not enabled (RC5)
	       ;--1-----    PWM1OUT - output value bit
	       ;---0----    PWM1POL - output is active-high
	       ;----xxxx    Unimplemented
    movwf	PWM1CON

    banksel	TRISC
    bcf		TRISC, TRISC_TRISC5_POSITION
    return
    
initialiseTMR2:
    banksel	T2CON
    ;;;SET TIMER 2 PRESCALE VALUE;;;
    ;PRESCALE = 16 SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
;    movlw	00000000B   
	       ;x-------    Unimplemented
	       ;-0000---    T2OUTPS - postscaler 1:1 (not used for PWM)
	       ;-----0--    TMR2ON: - Timer2 is off
	       ;------00    T2CKPS - prescaler 1:1
;    movwf	T2CON
    clrf	T2CON 
    banksel	TMR2
    ;;;CLEAR TIMER 2 MODULE;;;
    CLRF TMR2
    ;;;ENABLE TIMER 2 MODULE;;;
    banksel	T2CON
    bsf		T2CON, T2CON_TMR2ON_POSITION
    return
    
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    SET_LOW_DUTY
loop:
;    SLEEP      ; TMR0 does not work during sleep mode
    goto	loop

getNextTMR1HValue:
    movf	TMR1H_index, w
    call	periodVals
    movwf	TMR1H_val	; store returned value
    ; test if 0, i.e. end of table
    iorlw	0x00
    btfsc	STATUS, STATUS_ZERO_POSN
    goto	resetPeriodTableIndex
    incf	TMR1H_index, f
    return
resetPeriodTableIndex:
    clrf	TMR1H_index
    goto	getNextTMR1HValue
    
periodVals:
    addwf	PCL         ;Offset program counter by adding W
    retlw	0xf2	    
    retlw	0xf2	    
    retlw	0xf2	    
    retlw	0xf2	    

    
    
    retlw	0x10	           
    retlw	0x20	           
    retlw	0x30            
    retlw	0x40            
    retlw	0x50          
    retlw	0x60	    
    retlw	0x70	    
    retlw	0x80	    
    retlw	0x90	    
    retlw	0xa0	    
    retlw	0xb0	    
    retlw	0xc0	    
    retlw	0xd0	    
    retlw	0xd8	    
    retlw	0xe0	    
    retlw	0xe8	    
    retlw	0xf0	    
    retlw	0xf2	    
    retlw	0xf4	    
    retlw	0xf6	    
    retlw	0xf8	    
    retlw	0xfa	    
    retlw	0xfc	    
    retlw	0  
end resetVec 
    
#endif