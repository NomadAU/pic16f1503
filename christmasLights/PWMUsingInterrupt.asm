#if PWM_USING_INTERRUPT
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
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h,-pData_Vec=70h
    
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
    #define MAX_DUTY_VALUE 42
    #define INIT_COUNT 100  ; count-up from d'100' to 255 = 155 counts
    #define TICK_COUNT 200   ; d'200' * 5 ms = 1 second update delay
    #define LIGHT_ON_SECONDS	    120
    #define EAST_LIGHT_ON_SECONDS   LIGHT_ON_SECONDS
    #define WEST_LIGHT_ON_SECONDS   LIGHT_ON_SECONDS
    #define KEEPALIVE_FLASH_INTERVAL   5
    #define INCREMENT_BIT	    0
    #define STRING_A_BIT	    1
    #define TIMER1_ACTIVE	    2


    
    #include <xc.inc>
    #include "defines.inc"
    #include "defs.inc"
    #include "blink.inc"
    
TURN_OFF_TIMER1 macro
    bcf		flagWord, TIMER1_ACTIVE
endm
 
SET_MAX_DUTY macro
    banksel	PWM1DCH
    movlw	11111111B
    movwf	PWM1DCH
    movlw	11000000B
    movwf	PWM1DCL
endm
    
SET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON		; 2,033
    movlw	0xf8		    
    movwf	TMR1H		    
    movlw	0x0e
    movwf	TMR1L
ENDM
bSET_TIMER1_COUNTER MACRO	; sets 1 second overflow
    banksel	T1CON		; 4,067
    movlw	0xf0		    
    movwf	TMR1H		    
    movlw	0x1c
    movwf	TMR1L
ENDM
aSET_TIMER1_COUNTER MACRO	; sets 1 second overflow
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
FLASH_IT macro
    banksel	LATC
    bsf		RED_LATCH
    DO_DELAY
    bcf		RED_LATCH
    DO_DELAY
endm
DO_DELAY MACRO
;    call	delay1s
;    call	delay100ms
    call	delay50ms
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
    d1:		    DS      1   ;reserve 1 byte used by timer.asm
    d2:		    DS      1   ;reserve 1 byte used by timer.asm
    d3:		    DS      1   ;reserve 1 byte used by timer.asm
    d4:		    DS      1   ;reserve 1 byte used by timer.asm
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
    retfie
endmyISR:
    
initialisation:    ; setup peripherals, start timer, enter endless loop at the end.
;    call	setupOscillator
;    call	setupPrescaler
;    call	setupIOPins
;    call	setupPWM
;    call	initialiseTMR2
;    call	initialiseTMR1
    return
   
;**********************************************************************
; main program
;**********************************************************************
main:
    call	initialisation
;FLASH_IT macro
    banksel	LATC
    bsf		RED_LATCH
    DO_DELAY
    bcf		RED_LATCH
    DO_DELAY
;endm
    
    
loop:  
    goto	loop
;    call	turnOnPWM
;    call	delay1s
;    call	turnOffPWM
;    call	delay1s
;    goto	loop
    call	turnOnRedLED
    banksel	PWM1DCH
    movlw	MAX_DUTY_VALUE
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