; ButtonBounce: Sample code to demonstrate how to debounce a button press
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
;   Pin summary
;	1	VDD	+3.3V
;	8	RC2	Red LED
;	2	RA5	button
;      14	VSS	Ground
;    
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 15 Feb 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h
;
;
;                                     PIC16F1503
;                              +----------:_:----------+
;                    +5V <>  1 : VDD               VSS : 14 <> GND
;                 button <>  2 : RA5               RA0 : 13 <> 
;                        <>  3 : RA4               RA1 : 12 <> 
;                        <>  4 : RA3/MCLR          RA2 : 11 <>      
;                        <>  5 : RC5               RC0 : 10 <> 
;                        <>  6 : RC4               RC1 : 9  <> 
;                        <>  7 : RC3               RC2 : 8  <> RED LED
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
    
TOGGLE_RED_LED MACRO	
    banksel	LATC
    movlw	LATC_LATC2_MASK   ; set bit for RC2
    xorwf	LATC, f
ENDM
    
; vars used by TimerLib library
global 		d1,d2,d3
extrn		delay5us

#define INIT_COUNT	90	; count up from 90, (166 ticks before overflow)
#define OVERFLOW_COUNT	2	; approx 20ms time for debounce
#define LED_ON_BIT		    0
#define ISSUE_I2C_REQUEST	    1
#define WAITING_FOR_BUTTON_PRESS    2
#define TEMPERATURE_READ	    3
#define BUTTON_PRESSED		    4
    
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
    d1:			DS	1
    d2:			DS	1
    d3:			DS	1
    tickCounter:	DS	1
    overflowCounter:	DS	1
    flashCounter:	DS	1
    flagWord:		DS	1
    
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT ISR_Vec,global,class=CODE,delta=2
    global  ISR_Vec
	
ISR_Vec:
    btfsc	INTCON, INTCON_T0IF_POSN    ; timer interrupt
    call	processTimerExpired
    btfsc	INTCON, INTCON_IOCIF_POSN   ; has pin change occurred?
    call	processPinChange
    retfie
    
processPinChange:
    bcf		INTCON, INTCON_IOCIE_POSN   ; disable change interrupt
    banksel	IOCAF
    bcf		IOCAF, IOCAF_IOCAF5_POSN    ; clear RA5 interrupt flag

    ; wait for about 20ms to allow button bounce to stabilise
    banksel	TMR0
    movlw	INIT_COUNT
    movwf	TMR0		; load tick count to timer
    movlw       OVERFLOW_COUNT	; load number of overflows we want
    movwf       overflowCounter

    ; enable TMR0
    bcf		INTCON, INTCON_T0IF_POSN    ; clear the timer0 interrupt flag
    bsf		INTCON, INTCON_T0IE_POSN    ; enable timer0 interrupt
    return
    
processTimerExpired:
    bcf         INTCON, INTCON_T0IF_POSN    ; clear Timer0 interrupt flag
    movlw       INIT_COUNT	; re-initialise count
    movwf       TMR0
    decfsz      overflowCounter, f  ; more TMR0 overflows required?
    return			; yes, return

    ; end of debounce period, is button still pressed?
    banksel	PORTA
    btfsc	PORTA, PORTA_RA5_POSN
    bsf		flagWord, BUTTON_PRESSED
    bsf		INTCON, INTCON_IOCIE_POSN   ; enable change interrupt
    return
    
END_ISR_Vec:

;PSECT MainCode,global,class=CODE,delta=2
psect code,global,class=CODE,delta=2
	
initialisation:    ; setup peripherals, start timer
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	setupIOC
    call	flashLED
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

    banksel	TRISA		    ; set PORTA pins output apart from the button input pins
    clrf	TRISA
    bsf		TRISA, TRISA_TRISA5_POSN    ; button pin is input
    
    banksel	TRISC	
    clrf	TRISC		    ; set all PORTC pins as output

    banksel	LATA		    
    clrf	LATA		    ; set all PORTA pins output low
    clrf	LATC		    ; set all PORTC pins output low
    
    return
    
setupIOC:   ; RA5 held high using WPU.  Pulled low when button pressed.
    banksel	IOCAN
    clrf	IOCAF
    bsf		IOCAP, IOCAN_IOCAN5_POSN    ; set IOC (-ve going) for RA5
    banksel	OPTION_REG
    bcf		OPTION_REG, OPTION_REG_nWPUEN_POSN
    banksel	WPUA
    bsf		WPUA, WPUA_WPUA5_POSN	    ; turn on weak pull up for RA5
    bsf		INTCON, INTCON_IOCIE_POSN   ; enable interrupt for IOC
    bsf		INTCON, INTCON_GIE_POSN
    return
  
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
    clrf	flagWord
loop:
    btfss	flagWord, BUTTON_PRESSED
    goto	loop
    
    bcf		flagWord, BUTTON_PRESSED
    banksel	LATC
    btfss	LATC, LATC_LATC2_POSN
    goto	turnOn
    bcf		LATC, LATC_LATC2_POSN
    goto	loop
turnOn:    
    movlw	1 << LATC_LATC2_POSN
    bsf		LATC, LATC_LATC2_POSN
    goto	loop

flashLED:
    banksel	LATC
    bsf		LATC, LATC_LATC2_POSN
    call	delay1s
    bcf		LATC, LATC_LATC2_POSN
    call	delay200ms
    bsf		LATC, LATC_LATC2_POSN
    call	delay1s
    bcf		LATC, LATC_LATC2_POSN
    return
    
END resetVec    