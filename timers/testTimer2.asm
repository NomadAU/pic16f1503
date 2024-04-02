#ifdef TEST_TIMER_2
; Timer0Test:	Test setup of TMR2
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
			
TOGGLE_RED_LED MACRO	
    banksel	LATC
    movlw	LATC_LATC2_MASK   ; set bit for RC2
    xorwf	LATC, f
ENDM
    
SET_TIMER2_COUNTER MACRO	; sets 1 second overflow
    banksel	PR2
    movlw	0xff
    movwf	PR2
ENDM
    
;**********************************************************************
; Power-On-Reset entry point
;**********************************************************************
PSECT POR_Vec,global,class=CODE,delta=2
    global  resetVec
resetVec:
    PAGESEL main                ;jump to the main routine
    goto    main

;objects in Common RAM - address 70h
PSECT Data_Vec,global,class=Data_Vec,space=1,delta=1,noexec
    d1:	    DS      1	
    d2:	    DS      1	
    d3:	    DS      1	
    timer2RolloverCount:    DS	1
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    banksel	PIR1
    btfsc	TMR2IF   
;    banksel	LATC
;    bsf		LATC2
    call	processTimer2Expired
    retfie
    
processTimer2Expired:
    
    bcf		TMR2IF	    ; clear interrupt flag
    banksel	PIE1
    ; just return if TMR2 is already enabled
    bcf		TMR2IE
    
;    SET_TIMER2_COUNTER
    
    decfsz	timer2RolloverCount, f
    goto	enableAndReturn
    
    movlw	244
    movwf	timer2RolloverCount
    
    TOGGLE_RED_LED
    banksel	PIE1
enableAndReturn:    
    bsf		TMR2IE
    return
    
END_ISR_Vec:

    PSECT   MainCode,global,class=CODE,delta=2

initialisation:    ; setup peripherals, start timer, enter endless loop at the end.
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	initialiseTMR2
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
  
initialiseTMR2:
    banksel	PIE1
    ; just return if TMR2 is already enabled
    btfsc	TMR2IE
    return
    bsf		TMR2IE
    movlw	11000101B
	       ;x-------    Unimplemented
	       ;-1111---    T2OUTPS postscaler bits 1:16
	       ;-----1--    TMR2ON Timer2 is on
	       ;------11    T2CKPS prescale bits = 64
    banksel	T2CON
    movwf	T2CON
    movlw	244
    movwf	timer2RolloverCount
    bsf		PEIE
    bsf		GIE
;    RED_LED_ON
    return
    
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    
loop:
    goto	loop
    movlw	244
    movwf	timer2RolloverCount
    clrf	T2CON ; Stop Timer2, Prescaler = 1:1,
    ; Postscaler = 1:1
    clrf	TMR2 ; Clear Timer2 register
    clrf	INTCON ; Disable interrupts
    banksel	PIE1
    clrf	PIE1 ; Disable peripheral interrupts
    banksel	PIR1
    clrf	PIR1 ; Disable peripheral interrupts
    movlw	0x72 ; Postscaler = 1:15, Prescaler = 1:16
    movlw	11000101B
    movwf	T2CON ; Timer2 is off
    bsf		TMR2ON ; Timer2 starts to increment
;
; The Timer2 interrupt is disabled, do polling on the overflow bit
;
T2_OVFL_WAIT:
    btfss	PIR1, PIR1_TMR2IF_POSITION ; Has TMR2 interrupt occurred?
    goto	T2_OVFL_WAIT ; NO, continue loop
;
; Timer has overflowed
;
    bcf		TMR2IF ; YES, clear flag and continue.    
    
    decfsz	timer2RolloverCount, f
    goto	T2_OVFL_WAIT
    
    movlw	244
    movwf	timer2RolloverCount
    
    TOGGLE_RED_LED
    banksel	PIR1
    
;    call	delay1s
    
    goto	T2_OVFL_WAIT
    
    
    
;    SLEEP      ; TMR0 does not work during sleep mode
    goto	loop

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

end resetVec 
    
#endif
    