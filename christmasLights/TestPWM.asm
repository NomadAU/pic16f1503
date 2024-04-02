#ifdef TEST_PWM
;    
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
;    PWM output <>  5 : RC5               RC0 : 10 <> 
;               <>  6 : RC4               RC1 : 9  <> 
;               <>  7 : RC3               RC2 : 8  <> 
;                     +-----------------------:
;                                DIP-14
    
PROCESSOR   16F1503
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

    #define INIT_COUNT 100  ; count-up from d'100' to 255 = 155 counts
;    #define TICK_COUNT 10   ; d'10' * 5 ms = 50 ms update delay
;    #define TICK_COUNT 25   ; d'25' * 5 ms = 125 ms update delay
;    #define TICK_COUNT 50   ; d'50' * 5 ms = 250 ms update delay
;    #define TICK_COUNT 100   ; d'100' * 5 ms = 500 ms update delay
    #define TICK_COUNT 200   ; d'200' * 5 ms = 1 second update delay
    #define LIGHT_ON_SECONDS	    120
    #define EAST_LIGHT_ON_SECONDS   LIGHT_ON_SECONDS
    #define WEST_LIGHT_ON_SECONDS   LIGHT_ON_SECONDS
    #define KEEPALIVE_FLASH_INTERVAL   5

    ; Testing has shown that setting a pin high can influence the pins 
    ; nearby.  Most probably due to the proximity of the cables being
    ; used on the test rig.  
    ; Originally, RA0 and RA1 were being used, but setting RA0 high would also
    ; set RA1 high, and vice versa.  Same issue with using RA5 and RA4.
    ;
    ; To avoid this RA2 and RA5 have been chosen.
    
    #define EASTGATE_INPUT_PIN	    PORTA_RA2_POSN
    #define EASTGATE_INPUT_BITMASK  PORTA_RA2_MASK
    #define EASTGATE_POSITIVE_EDGE  IOCAP2
    #define EASTGATE_INTERRUPT_FLAG IOCAF2
    #define WESTGATE_INPUT_PIN	    PORTA_RA5_POSN
    #define WESTGATE_INPUT_BITMASK  PORTA_RA5_MASK
    #define WESTGATE_POSITIVE_EDGE  IOCAP5
    #define WESTGATE_INTERRUPT_FLAG IOCAF5

    #define EASTGATE_CONTROL_LATCH  LATC3
    #define WESTGATE_CONTROL_LATCH  LATC4
    #define REAREASTGATE_CONTROL_LATCH	LATC0
    #define REARWESTGATE_CONTROL_LATCH	LATC1
    
xSET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON	
    movlw	0x80		    
    movwf	TMR1H		    
    movlw	0xe7
    movwf	TMR1L
ENDM
SET_TIMER1_COUNTER MACRO	; sets < 1 second overflow
    banksel	T1CON	
    movlw	0xC0		    
    movwf	TMR1H		    
    movlw	0xe7
    movwf	TMR1L
ENDM
DO_DELAY MACRO
;    call	delay1s
;    call	delay100ms
    call	delay50ms
ENDM
    
    #include "defines.inc"
    #include "defs.inc"
    #include "blink.inc"
    
FLASH_IT MACRO
    banksel	LATC
    bsf		RED_LATCH
    DO_DELAY
    bcf		RED_LATCH
    DO_DELAY
ENDM
    
TURN_ON_STRING_A MACRO
banksel	LATC
    movlw	00011000B
	       ;xx------    Unimplemented
	       ;--0-----    RC5 off
	       ;---11---    RC3/RC4 on
	       ;-----00-    RC1/RC2 off
	       ;-------0    RC0 off
    movwf      LATC
ENDM
    
TURN_ON_STRING_B MACRO
banksel	LATC
    movlw	00000110B
	       ;xx------    Unimplemented
	       ;--0-----    RC5 off
	       ;---00---    RC3/RC4 on
	       ;-----11-    RC1/RC2 off
	       ;-------0    RC0 off
    movwf      LATC
ENDM

    ; vars used by BlinkLib library
    global		bitCount,blinkByte,blinkCount,colourWord

    ; vars used by TimerLib library
    global 		d1,d2,d3

    ; functions used by BlinkLib library
    global		turnOnRedLED, turnOffRedLED, turnOnGreenLED, turnOffGreenLED, turnOnOrangeLED, turnOffOrangeLED
 
    extrn		delay1s, delay5s, delay50ms, delay100ms, delay200ms, delay500ms,blinkLEDnTimes,quickBlinkLEDnTimes

    #include "defines.inc"
    
;**********************************************************************
; Power-On-Reset entry point
;**********************************************************************
	global	POR_Vec,main
	psect	POR_Vec,class=CODE,delta=2,reloc=2
POR_Vec:
	nop	; Suggested Microchip errata workaround
	goto	main

    ;objects in Common RAM - address 70h
PSECT Data_Vec,global,class=Data_Vec,space=1,delta=1,noexec
    ;objects in Common RAM - address 70h
	fadeState:	    DS      1	;reserve 1 byte for flagWord
	blinkCount:	    DS      1	;how many flashes LED will do
	blinkByte:	    DS      1   ;reserve 1 byte for blinkByte
	bitCount:	    DS      1   ;reserve 1 byte for bitCount
	colourWord:	    DS      1   ;value determines colour of LED to flash
	d1:		    DS      1   ;reserve 1 byte used by blinkBits.asm
	d2:		    DS      1   ;reserve 1 byte used by blinkBits.asm
	d3:		    DS      1   ;reserve 1 byte used by blinkBits.asm
	d4:		    DS      1   ;reserve 1 byte used by blinkBits.asm
	eastSeconds:	    DS	    1	; time for East light on
	westSeconds:	    DS	    1	; time for West light on
	keepAliveFlashSecs: DS	    1   ; number of seconds between KA flash
	timerCount:	    DS	    1	; TMR0 counter
        flashCount:	    DS      1   ; counter for the timer1 functions
    
;**********************************************************************
; Interrupt vector and handler
;**********************************************************************
PSECT   ISR_Vec,global,class=CODE,delta=2
	global  ISR_Vec
	
ISR_Vec:	
    retfie;
endISR_Vec:

initialisation:    ; setup peripherals, start timer, enter endless loop at the end.
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	setupPWM
    call	initialiseTMR2
;    call	initialiseTMR1
    return
   
setupOscillator:
    ; initialise internal oscillator to 16MHz
    banksel	OSCCON
    movlw	01111000B	    ; Int. osc. 16 MHz
    movwf	OSCCON ;
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
    movlw       00000110B	    ; prescaler 1/128; Ficyc = Fosc(16 MHz)/4 = 4 MHz
    movwf       OPTION_REG	    ; giving 4 MHz/128 = 31250 Hz (period = 32 us)
    return

setupIOPins:
    banksel	ANSELA
    clrf	ANSELA		    ; set all PORTA pins to digital I/O
    clrf	ANSELC		    ; set all PORTC pins to digital I/O

    banksel	TRISA		    ; set PORTA pins output apart from the 2 input sensor pins
    movlw	(1<<EASTGATE_INPUT_PIN) | (1<<WESTGATE_INPUT_PIN)
    movwf	TRISA
    
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
    movlw	32	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
    movlw	16	;SET PR2 TO 128 DECIMAL SO THE PWM PERIOD = 2064uS => PWM FREQUENCY = 484Hz
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
    
initialiseTMR1:
    banksel	PIE1
    ; just return if TMR1 is already enabled
    btfsc	TMR1IE
    return
    bsf		TMR1IE
    SET_TIMER1_COUNTER    
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
    
loop:  
    clrf	PWM1DCH
    clrf	PWM1DCL
    banksel	PWM1CON
    bsf		PWM1CON, PWM1CON_PWM1OE_POSN
    banksel	PWM1DCH
doInc:
    call	delay1s
    movlw	00001000B
    addwf	PWM1DCH,f	; add 8 to higher 8 bits
    btfss	STATUS,STATUS_CARRY_POSN ; begin countdown if rollover
    goto	doInc
    movlw	0xff
    movwf	PWM1DCH
    call	debugFlash
    banksel	PWM1DCH
    
doDec:
    call	delay1s
;    bsf		STATUS, STATUS_CARRY_POSN
;    bsf		STATUS, STATUS_ZERO_POSN
    movlw	00001000B
;    movlw	0x78	; 2s complement of 8
    subwf	PWM1DCH,f;
;    addwf	PWM1DCH,f;
    btfsc	STATUS,STATUS_CARRY_POSN ; begin countup if rollover
;    btfss	STATUS,STATUS_ZERO_POSN	; begin count up if zero
;    btfsc	STATUS,STATUS_CARRY_POSN ; begin count up if zero
;    btfsc	STATUS,STATUS_DC_POSN ; begin count up if zero
;    btfss	STATUS,STATUS_ZERO_POSN ; begin count up if zero
    goto	doDec
    ; check if C and Z flags are 0 - this means we've gone negative
;    movlw	1<<STATUS_CARRY_POSN | 1<<STATUS_ZERO_POSN
;    andwf	STATUS
;    btfss	STATUS, STATUS_ZERO_POSN
;    goto	doDec
    clrf	PWM1DCH
    call	debugFlash
    banksel	PWM1DCH
    goto	doInc
    
    
turnOnRedLED:
turnOffRedLED:
turnOnGreenLED:
turnOffGreenLED:
turnOnOrangeLED:
turnOffOrangeLED:
    return
    
debugFlash:    
    FLASH_IT
    FLASH_IT
    FLASH_IT
    FLASH_IT
    FLASH_IT
    return
    
delay:
;    call	delay50ms
;delay1s, delay5s, delay50ms, delay100ms, delay200ms,    
    call	delay100ms
    return
    
end POR_Vec    
#endif