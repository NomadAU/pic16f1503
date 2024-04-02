#ifdef TIMER0_TEST
//#define TEST_LIGHTS 1
    
; Control:	Detects pin change on receiver XBee and turns on security
;		light corresponding to XBee pin
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
; This is a simpler design that utilises XBee IO line passing to pass the state
; of the East and West gate sensor beams.
; The control XBee sets RA2 high when its East Gate counterpart is activated and
; RA5 for the West Gate.
;
; The East gate light is turned on by setting RC3 high, West gate by setting
; RC4 high.
;
; A signal high is set on XBee pins D3 and D6 when East and West gate sensors 
; are activated.  This is transmitted via IO line passing to the XBee on the
; rear deck. As a result, both front and back lights are activated whenever the
; gate sensor is triggered.
;
; The requirement for the processor is simply to flash an LED whenever it detects
; that the beam has been broken.  Once broken, Timer0 is used to count a number
; of Red LED flashes
;
; After bench testing it was found that parasitic voltage spikes were causing 
; invalid IOC switches whenever a gate sensor was received and the light was
; turned on. Therefore, a 'debounce' solution has been implemented which 
; ensures that the XBee signal persists high for a period of time.  
; When either RA2 or RA5 IOC is triggered, the code periodically checks the 
; state of the pin for 1 second to ensure it is still held high.  If any of the
; checks fail this test, the IOC is deemed to be spurious.  The gate sensor is
; designed to hold its 'sending' pin high for 2 seconds.
;
; The IR sensor is connected to XBee as follows
;   DIO1 for East gate
;   DI02 for West gate
; The IR sensor pin is pulled high when the beam is broken.
;
; The IR sensor is connected to the MCU as follows
;   DIO1 to RA2 for East gate
;   DIO2 to RA5 for West gate
;
;   Pin summary
;	1	VDD	+3.3V
;       13	RA2	XBee DI01 (East)
;       12	RA5	XBee DI02 (West)
;	10	RC0	Rear East light signal
;	9	RC1	Rear West light signal
;	8	RC2	Red LED
;	7	RC3	East gate switch
;	6	RC4	West gate switch
;      14	VSS	Ground
;    
; Assembled with pic-as (v2.32) under MPLAB X IDE (v5.50) 22 Jun 2023
;
; Add this line in the project properties box 
;	"pic-as Global Options -> pic-as Linker -> Additional options": 
;
;	-Wa,-a -Wl,-pmyPOR=0h,-pISR_Vec=4h,-pmyData=70h
    
;				      PIC16F1503
;			       +----------:_:----------+
;		   +3.3V <>  1 : VDD               VSS : 14 <> GND
; Beam sensor West Gate  <>  2 : RA5               RA0 : 13 <> 
;			 <>  3 : RA4               RA1 : 12 <> 
;			 <>  4 : RA3/MCLR          RA2 : 11 <>      
;	      PWM Output <>  5 : RC5               RC0 : 10 <> 
;		      Q4 <>  6 : RC4               RC1 : 9  <> Q1
;                     Q3 <>  7 : RC3               RC2 : 8  <> Q2
;			       +-----------------------:
;                                       DIP-14
    
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
    #define DUTY_VALUE 45
    #define INIT_COUNT 100  ; count-up from d'100' to 255 = 155 counts
;    #define TICK_COUNT 10   ; d'10' * 5 ms = 50 ms update delay
;    #define TICK_COUNT 25   ; d'25' * 5 ms = 125 ms update delay
;    #define TICK_COUNT 50   ; d'50' * 5 ms = 250 ms update delay
;    #define TICK_COUNT 100   ; d'100' * 5 ms = 500 ms update delay
    #define TICK_COUNT 200   ; d'200' * 5 ms = 1 second update delay
#ifdef TEST_LIGHTS
    #define LIGHT_ON_SECONDS	    10
#else
    #define LIGHT_ON_SECONDS	    120
#endif
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
    
    #define INCREMENT_BIT	    0
    
SET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON		; 8,134
    movlw	0xe0		    
    movwf	TMR1H		    
    movlw	0x39
    movwf	TMR1L
ENDM
zSET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON		; 16,268
    movlw	0x80		    
    movwf	TMR1H		    
    movlw	0xe7
    movwf	TMR1L
ENDM
ySET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON		; 32,536
    movlw	0xc0		    
    movwf	TMR1H		    
    movlw	0x73
    movwf	TMR1L
ENDM
xSET_TIMER1_COUNTER MACRO	; sets < 1 second overflow
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
    flagWord:	    DS      1	;reserve 1 byte for flagWord
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
    banksel	PIR1
    btfsc	TMR1IF             
    call	processTimer1Expired
    retfie;
endISR_Vec:
    
processTimer1Expired:
    bcf		TMR1IF	    ; clear interrupt flag
    return
    
initialisation:    ; setup peripherals, start timer, enter endless loop at the end.
    call	setupOscillator
    call	setupPrescaler
    call	setupIOPins
    call	setupPWM
    call	initialiseTMR2
    call	initialiseTMR1
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
;    call	turnOnPWM
;    call	delay1s
;    call	turnOffPWM
;    call	delay1s
;    goto	loop
    call	turnOnRedLED
    banksel	PWM1DCH
    movlw	DUTY_VALUE
    movwf	PWM1DCH
    banksel	PWM1CON
    bsf		PWM1CON, PWM1CON_PWM1OE_POSN
    
    goto	$	; just let the interrupt change the duty cycle
    
turnOnRedLED:
    banksel	LATC
    bcf		RED_LATCH
    return
    
turnOffRedLED:
turnOnGreenLED:
turnOffGreenLED:
turnOnOrangeLED:
turnOffOrangeLED:
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