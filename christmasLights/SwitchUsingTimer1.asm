#if SWITCH_USING_TIMER1
;#define SIMULATOR
;
; Program using Timer1 to switch the strings and Timer0 to modify the Timer1
; TMR1H and TMR1L values.  This means that the switch frequency is controlled
; by Timer1 but the changing of the frequency is controlled by Timer0
;
;  Example project for the PIC16F1503 controller using the pic-as(v2.45) tool chain.
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 28 Jan 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h,-pData_Vec=70h
;
;                            PIC16F1503
;                     +----------:_:----------+
;         +3.3V <>  1 : VDD               VSS : 14 <> GND
;               <>  2 : RA5               RA0 : 13 <> 
;               <>  3 : RA4               RA1 : 12 <> 
;               <>  4 : RA3/MCLR          RA2 : 11 <>      
;               <>  5 : RC5               RC0 : 10 <> 
;               <>  6 : RC4               RC1 : 9  <> 
;               <>  7 : RC3               RC2 : 8  <> 
;                     +-----------------------:
;                                DIP-14
	

PAGEWIDTH   132
RADIX       DEC


; PIC16F1503 Configuration Bit Settings
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

;    #define MAX_DUTY_VALUE 45
    #define MAX_DUTY_VALUE 42
    #define INIT_COUNT 100  ; count-up from d'100' to 255 = 155 counts
;    #define TICK_COUNT 10   ; d'10' * 5 ms = 50 ms update delay
;    #define TICK_COUNT 25   ; d'25' * 5 ms = 125 ms update delay
;    #define TICK_COUNT 50   ; d'50' * 5 ms = 250 ms update delay
;    #define TICK_COUNT 100   ; d'100' * 5 ms = 500 ms update delay
;    #define TICK_COUNT 200   ; d'200' * 5 ms = 1 second update delay
;    #define TICK_COUNT 400   ; d'200' * 5 ms = 1 second update delay
    #define TICK_COUNT 800   ; d'200' * 5 ms = 1 second update delay
    
    #define INCREMENT_BIT	    0
    #define STRING_A_BIT	    1
    #define TIMER1_ACTIVE	    2
    
    #define TIMER1_HIGH    0x80
    #define TIMER1_LOW	    0xe7
#define TMR0_ROLLOVER_COUNT 63  ; this gives close to 1 second interval
#define TMR0_SECONDS	    1
    
    
; ============== macros ===============
TURN_OFF_TIMER1 macro
    bcf		flagWord, TIMER1_ACTIVE
endm
 
SET_MIN_DUTY macro
    banksel	PWM1DCH
    movlw	00000000B
    movwf	PWM1DCH
    movlw	00000000B
    movwf	PWM1DCL
endm
    
SET_MAX_DUTY macro
    banksel	PWM1DCH
    movlw	11111111B
    movwf	PWM1DCH
    movlw	11000000B
    movwf	PWM1DCL
endm

SET_TIMER1_COUNT_REGISTERS MACRO	; 1 second interval: TMR1H=0x80 TMR1L=0xe7
    banksel	T1CON	
    movf	TMR1H_val
    movwf	TMR1H		    
    clrf	TMR1L
ENDM
    
DO_DELAY MACRO
;    call	delay1s
;    call	delay100ms
    call	delay50ms
ENDM
    
FLASH_IT MACRO
    banksel	LATC
    bsf		RED_LATCH
    DO_DELAY
    bcf		RED_LATCH
    DO_DELAY
ENDM

TURN_ON_STRING_A MACRO
    banksel	LATC
    movlw	00000110B
	       ;xxxxx---    Unimplemented
	       ;-----1--    RC2 on
	       ;------1-    RC1 on
    movwf      LATC
ENDM
    
TOGGLE_STRING MACRO
    banksel	LATC
    movlw	00011110B
    xorwf	LATC, f
ENDM

TOGGLE_RC5_LED MACRO
    banksel	LATC
    movlw	1<<PORTC_RC5_POSN
    xorwf	LATC, f
ENDM

SET_TMR0_SECONDS MACRO
    movlw	TMR0_SECONDS
    movwf	tmr0Seconds
ENDM
    
DISABLE_TIMER1 MACRO
    banksel	PIE1
    bcf		TMR1IE
ENDM
    
ENABLE_TIMER1 MACRO
    banksel	PIE1
    bsf		TMR1IE
ENDM
    
#include <xc.inc>
#include "defines.inc"
#include "defs.inc"
#include "blink.inc"
    
; vars used by BlinkLib library
global		bitCount,blinkByte,blinkCount,colourWord

; vars used by TimerLib library
global 		d1,d2,d3

; functions used by BlinkLib library
global		turnOnRedLED, turnOffRedLED, turnOnGreenLED, turnOffGreenLED, turnOnOrangeLED, turnOffOrangeLED

extrn		delay1s, delay5s, delay50ms, delay100ms, delay200ms, delay500ms,blinkLEDnTimes,quickBlinkLEDnTimes, blinkWReg
    
; ============== File registers for variables (max 16 words) =================
PSECT Data_Vec,global,class=Data_Vec,space=1,delta=1,noexec
    ;objects in Common RAM - address 70h
    flagWord:	    DS      1	;reserve 1 byte for flagWord
    blinkCount:	    DS      1	;how many flashes LED will do
    blinkByte:	    DS      1   ;reserve 1 byte for blinkByte
    bitCount:	    DS      1   ;reserve 1 byte for bitCount
    colourWord:	    DS      1   ;value determines colour of LED to flash
    d1:		    DS      1   ;reserve 1 byte used by timer.asm
    d2:		    DS      1   ;reserve 1 byte used by timer.asm
    d3:		    DS      1   ;reserve 1 byte used by timer.asm
    d4:		    DS      1   ;reserve 1 byte used by timer.asm
    tmr0RolloverCount:    DS	    1	; value 
    timer1CounterValue:    DS	    1	; counter value for timer1
    ticksPerTimer1Change: DS	    1   ; number of counts before changing timer1
    flashCount:	    DS      1   ; counter for the timer1 functions
    TMR1H_index:    DS	    1	; index into timer1 counter table
    tmr0Seconds:    DS	    1	; TMR0 counter
    TMR1H_val:	    DS	    1   ; holds the value for Timer1 TMR1H register
    
END_Data_Vec:   
    
; ============== Power-on entry point =================
PSECT POR_Vec,class=CODE,delta=2,reloc=2
POR_Vec:
    nop
    goto	main

; ============== Interrupt vector and handler =================
PSECT ISR_Vec,global,class=CODE,delta=2
    GLOBAL  ISR_Vec

ISR_Vec:
    btfsc	TMR0IF
    call	processTimer0Expired
    banksel	PIR1
    btfsc	TMR1IF             
    call	processTimer1Expired
    retfie;    
    
processTimer1Expired:	    ; toggles the strings
    bcf		TMR1IF	    ; clear interrupt flag
    banksel	T1CON	
    movf	TMR1H_val
;    movlw	0xf8
    movwf	TMR1H
    
;    movlw	0xf8
;    movwf	TMR1H
    clrf	TMR1L
;    SET_TIMER1_COUNT_REGISTERS	
    call	toggleTheStrings
    return

processTimer0Expired:	    ; sets the Timer1 register values
    bcf         TMR0IF	    ; clear Timer0 interrupt flag
    decfsz      tmr0RolloverCount, f	    ; more ticks?
    return

    bcf         TMR0IE	    ; disable Timer0 interrupt
    ; rollover count has gone to zero so we toggle the LED
    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
    decfsz	tmr0Seconds, f
    goto	justReturn
    
;    TOGGLE_RC5_LED
    call	getNextTMR1HValue
    movlw	TMR0_SECONDS
    movwf	tmr0Seconds
;    SET_TMR0_SECONDS
    bsf         TMR0IE	    ; enable Timer0 interrupt
    return
justReturn:
    bsf         TMR0IE	    ; enable Timer0 interrupt
    return
    
END_ISR_Vec:

initialisation:    ; start timers
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
;    call	setupPWM
;    call	initialiseTMR2
    call	initialiseTMR0
    call	initialiseTMR1
    clrf	TMR1H_index
    clrf	timer1CounterValue
    movlw	TICK_COUNT
    movwf	ticksPerTimer1Change
    return
   
setupOscillator:
    ; initialise internal oscillator to 16MHz
    banksel	OSCCON
    movlw	01111000B	    ; Int. osc. 16 MHz
    movwf	OSCCON ;
#ifndef SIMULATOR
    btfss	HFIOFR	    ; Int. osc. running?
    goto	$-1		    ; No, loop back
    btfss	HFIOFS	    ; Osc. stable?
    goto	$-1		    ; No, loop back.
#endif
    return

setupPrescaler:
    ; The following calculation is used to determine the period of each TMR0 tick
    ; when Ficyc = Fosc(16 MHz)/4 = 4 MHz, and a 1:256 prescaler is used
    ; 1/(4MHz/256/256) = 16.384 ms
    banksel	OPTION_REG
    movlw       00000110B	    ; prescaler 1/128; Ficyc = Fosc(16 MHz)/4 = 4 MHz
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
    TURN_ON_STRING_A    
    return
  
setupPWM:
    banksel	PR2
    movlw	128	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movlw	64	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movlw	16	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movlw	128	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movlw	32	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
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
    
initialiseTMR0:
    movlw	TMR0_SECONDS
    movwf	tmr0Seconds
;    SET_TMR0_SECONDS
    bsf         T0IE	    ; enable Timer0 interrupt
    bsf         GIE	    ; enable global interrupt
    return
    
initialiseTMR1:
    banksel	PIE1
    ; just return if TMR1 is already enabled
    btfsc	TMR1IE
    return
    bsf		TMR1IE
    SET_TIMER1_COUNT_REGISTERS
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
    banksel	T1CON
    movwf       T1CON
    bsf		PEIE
    bsf		GIE
    return
    
initialiseTMR2:		; used by PWM
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
    clrf	TMR2
    banksel	T2CON
    bsf		T2CON, T2CON_TMR2ON_POSITION	; enable Timer2
    return
    
main:
    call	initialisation
;    TURN_OFF_TIMER1
    SET_MAX_DUTY
   
;    call	getCounterVal
;    movf	timer1CounterValue, w
    
;    call	blinkWReg

doNada:
    banksel	PIE1
    bcf		TMR1IE
    
;    DISABLE_TIMER1
    
    movlw	0xf8
    movwf	TMR1H_val
    banksel	T1CON	
    movwf	TMR1H
    
    banksel	PIE1
    bsf		TMR1IE
    
;    ENABLE_TIMER1
    
    call	delay5s
    goto	doNada
    
    movlw	0xe0
    movwf	TMR1H
    call	delay5s
    movlw	0xf0
    movwf	TMR1H
    call	delay5s
    movlw	0xf8
    movwf	TMR1H
    call	delay5s
    goto	doNada
    movlw	0xf0
    movwf	TMR1H
    call	delay5s
    goto	doNada
     
    banksel	PWM1CON
    bsf		PWM1CON, PWM1CON_PWM1OE_POSN
    bsf		flagWord, INCREMENT_BIT
    
loop:  
;    call	doEnableTest
    goto	loop
    
    call	toggleTheStrings
;    TOGGLE_STRING
;    call	delay5s
    call	delay100ms
    goto	loop
;    goto	$	    ; just let the interrupt change the duty cycle
    
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
    retlw	0xe0	    
    retlw	0xf0	    
    retlw	0             
    
toggleTheStrings:
    banksel	LATC
    movlw	00011110B
    xorwf	LATC, f
    return
    
turnOnRedLED:
    banksel	LATC
    bsf		LATC5
    return
    
turnOffRedLED:
    banksel	LATC
    bcf		LATC5
    return
    
xturnOnRedLED:
    banksel	LATC
    bcf		RED_LATCH
    return
    
xturnOffRedLED:
turnOnGreenLED:
turnOffGreenLED:
turnOnOrangeLED:
turnOffOrangeLED:
    return
    
doSOS:    
    ; do 3 short flashes, followed by 3 long, followed by 3 short
    ; turn on lights fully
    TURN_OFF_TIMER1
    SET_MAX_DUTY
    TURN_ON_STRING_A
    movlw 3
    call	doShortFlash
    
    call	delay100ms
    call	delay100ms
    call	delay100ms
    
    movlw 3
    call	doLongFlash
    
    call	delay100ms
    call	delay100ms
    call	delay100ms
    
    movlw 3
    call	doShortFlash
    return

doShortFlash:
    movwf	blinkCount
shortAgain:
;    TOGGLE_STRING
    call	turnOnPWMOutput
    call	delay1s
    call	delay100ms
    call	delay50ms
    call	turnOffPWMOutput
    call	delay1s
    call	delay50ms
    call	delay100ms
    decfsz	blinkCount, f
    goto	shortAgain
;    goto	shortAgain
    return
    
doLongFlash:
    movwf	blinkCount
longAgain:
;    TOGGLE_STRING
    call	turnOnPWMOutput
    call	delay1s
;    call	delay500ms
    call	delay100ms
    call	delay100ms
    call	delay100ms
    call	delay50ms
;    call	delay50ms
    call	turnOffPWMOutput
    call	delay1s
    call	delay100ms
    call	delay50ms
;    call	delay50ms
    decfsz	blinkCount, f
    goto	longAgain
    return

turnOnPWMOutput:
    banksel	PWM1CON
    bsf		PWM1CON, PWM1CON_PWM1OE_POSN
    return
    
turnOffPWMOutput:
    banksel	PWM1CON
    bcf		PWM1CON, PWM1CON_PWM1OE_POSN
    return
    
turnOnPWM:
    banksel	PWM1DCH
    movlw	0xff
    movwf	PWM1DCH
    movlw	11000000B
    movwf	PWM1DCL
    banksel	PWM1CON
    bcf		PWM1CON, PWM1CON_PWM1OE_POSN
    return

turnOffPWM:
    banksel	PWM1DCH
    clrf	PWM1DCH
    clrf	PWM1DCL
    banksel	PWM1CON
    bcf		PWM1CON, PWM1CON_PWM1OE_POSN
    return

debugFlash:    
;    FLASH_IT
;    FLASH_IT
;    FLASH_IT
;    FLASH_IT
;    FLASH_IT
    banksel	LATC
    bsf		RED_LATCH
    DO_DELAY
    bcf		RED_LATCH
    DO_DELAY
    return
    
delay:
;    call	delay50ms
;delay1s, delay5s, delay50ms, delay100ms, delay200ms,    
    call	delay100ms
    return
	
END POR_Vec   
#endif