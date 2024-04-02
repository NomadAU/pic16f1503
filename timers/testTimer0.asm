#ifdef TEST_TIMER_0
; Timer0Test:	Test setup of TMR0
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
; Assembled with pic-as (v2.32) under MPLAB X IDE (v5.50) 22 Jun 2023
;
; Add this line in the project properties box 
;	"pic-as Global Options -> pic-as Linker -> Additional options": 
;
;	-Wa,-a -Wl,-pmyPOR=0h,-pmyISR=4h,-pmyData=70h
    
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
; 50 = 31.37 60 = 37.62 61 = 38.15 63 = 39.34
#define TMR0_ROLLOVER_COUNT 63  ; this gives close to 1 second interval
    
TOGGLE_RED_LED MACRO	
    banksel	LATC
    movlw	LATC_LATC2_MASK   ; set bit for RC2
    xorwf	LATC, f
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
    tmr0RolloverCount:	DS      1	
    tmr0CountIndex:	DS      1	
    tmr0CountValue:	DS      1	
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    btfsc	TMR0IF
    call	processTimer0Expired
    retfie
    
processTimer0Expired:
    bcf         TMR0IF	    ; clear Timer0 interrupt flag
;    movlw	TMR0_VALUE
;    banksel	TMR0
;    movwf	TMR0
    
    decfsz      tmr0RolloverCount, f	    ; more ticks?
    return

    bcf         T0IE	    ; disable Timer0 interrupt
    ; rollover count has gone to zero so we toggle the LED
    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
    TOGGLE_RED_LED
    bsf         T0IE	    ; enable Timer0 interrupt
    return
    
END_ISR_Vec:

    PSECT   MainCode,global,class=CODE,delta=2

initialisation:    ; setup peripherals, start timer, enter endless loop at the end.
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	initialiseTMR0
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
  
initialiseTMR0:
    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
    bsf         T0IE	    ; enable Timer0 interrupt
    bsf         GIE	    ; enable global interrupt
    return
    
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    
Loop:
;    SLEEP      ; TMR0 does not work during sleep mode
    goto	Loop

end resetVec 
    
#endif