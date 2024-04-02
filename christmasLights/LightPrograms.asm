#ifdef BUILD_LIGHT_PROGRAMS_LIB
; LightPrograms: Collection of functions for controlling the light patterns
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "Timer.inc"
#include "LightPrograms.inc"
#include "macros.inc"
    
; dits and dahs - a dah is typically 3 times the length of a dit
; although longer is fine (we use 4).
; Space between dits/dahs within an encoded character is 3 x dit
DOT_PERIOD MACRO
    call	delay100ms
ENDM
DASH_PERIOD MACRO
    call	delay400ms
ENDM
INTER_DOT_DASH_PERIOD MACRO
    call	delay100ms
ENDM    
INTER_LETTER_PERIOD MACRO
    call	delay300ms
ENDM    
INTER_WORD_PERIOD MACRO
    call	delay700ms
ENDM    
INTER_SENTENCE_PERIOD MACRO
    call	rapidFlash
ENDM    
    
;extrn		lightsOn,lightsOff
PSECT LightFunctions,global,class=CODE,delta=2
    
; flash lights in Merry Christmas pattern	
merryXmas:
    movwf	programCounter1
    
    call	initDAC

    movlw	HIGH_DAC_SETTING
    banksel	DACCON1
    movwf	DACCON1
    
;    call	setLightsHigh
    
    bsf		DACEN
;    call	lightsOn
    banksel	LATC
    movlw	00000001
    movwf	LATC

;    call	turnOnStringA
    call	disableStringSwitch
    
    bsf		RC2
    goto	$
    
merryXmas_again:
    ; merry
    ; m = --
    movlw	2	    
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    call	toggleStrings
    
    ; e = .
    movlw	1	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings
    
    ; r = .-.
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings
    
    ; r = .-.
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; y = -.--
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    INTER_WORD_PERIOD
    
    ; Christmas
    ; c = -.-.
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; h = ....
    movlw	4	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; r = .-.
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    movlw	1	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; i = ..
    movlw	2	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; s = ...
    movlw	3	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; t = -
    movlw	1	    
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; m = --
    movlw	2	    
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; a = .-
    movlw	1	    
    movwf	flashCounter
    call	dot
    movlw	1	    
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    ; s = ...
    movlw	3	    
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    INTER_LETTER_PERIOD
    INTER_SENTENCE_PERIOD    
    
    decfsz	programCounter1,f
    goto	merryXmas_again
    call	lightsOff
    return
    
; flash lights in SOS pattern	
SOS:
    movwf	programCounter1
    call	setLightsHigh
    call	turnOnStringA
    call	disableStringSwitch
    
SOS_again:

    movlw	3
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings
    
    movlw	3
    movwf	flashCounter
    call	dash
    
    INTER_LETTER_PERIOD
    call	toggleStrings

    movlw	3
    movwf	flashCounter
    call	dot

    call	toggleStrings
    INTER_LETTER_PERIOD
    INTER_SENTENCE_PERIOD    
    
    decfsz	programCounter1,f
    goto	SOS_again
    call	lightsOff
    return
    
; flash lights in HO HO HO pattern	
HOHOHO:
    ; H is 4 dots, O is 3 dashes
    movwf	programCounter1
    call	setLightsHigh
    call	turnOnStringA
    call	disableStringSwitch
HO_HO_HO_again:    
    movlw	3
    movwf	programCounter2	; track the number of HO messages
    
HO_again:
    movlw	4		; 4 dots is H
    movwf	flashCounter
    call	dot
    
    INTER_LETTER_PERIOD
    call	toggleStrings
 
    movlw	3		; 3 dashes is O
    movwf	flashCounter
    call	dash
    
    INTER_WORD_PERIOD
    call	toggleStrings
    
    decfsz	programCounter2,f
    goto	HO_again
    
    INTER_SENTENCE_PERIOD
    
    decfsz	programCounter1,f
    goto	HO_HO_HO_again
    call	lightsOff
    return
    
dot:
    call	lightsOn
    DOT_PERIOD
    call	lightsOff
    INTER_DOT_DASH_PERIOD
    decfsz	flashCounter,f
    goto	dot
    return
    
dash:
    call	lightsOn
    DASH_PERIOD
    call	lightsOff
    INTER_DOT_DASH_PERIOD
    decfsz	flashCounter,f
    goto	dash
    return

doBreak:
    call	delay200ms
    call	lightsOn
    call	delay500ms
    call	lightsOff
    call	delay200ms
    return
    
turnOnStringA:
    banksel	LATC
    movlw	00000001
    movwf	LATC
    return
    
turnOnStringB:
    banksel	LATC
    movlw	00000010
    movwf	LATC
    return

toggleStrings:
;    return
    banksel	LATC
    movlw	00000011B
    xorwf	LATC, f
    return
  
lightsOn:
    bsf		DACEN
    return
    
lightsOff:
    bcf		DACEN
    return
    
initDAC:
    banksel	DACCON0
    movlw       10110000B	   
	       ;1-------	DACEN - enable bit
	       ;-x------	unimplemented
	       ;--1-----	DACOE1 - output on RA0 - use both to provide sufficient current
	       ;---1----	DACOE2 - output on RA2
	       ;----x---	unimplemented
	       ;-----0--	DACPSS - use VDD
	       ;------xx	unimplemented
    movwf       DACCON0
    return
    
; light state can be controlled a number of ways:
;   - Power to the FET is controlled by the gate voltage, driven by DAC.
;     Setting the DAC to low value turns the lights off
;   - There are 2 light strings controlled through the H bridge circuit.
;     Control pins are normally either 1,0 or 0,1.  Setting both to 0 will
;     turn off the lights.
;   - Use the DAC enable flag to turn on/off the DAC output voltage
;
;   We use the DAC enable flag to control the on/off state of the lights
setLightsHigh:
    movlw	HIGH_DAC_SETTING
    banksel	DACCON1
    movwf	DACCON1
    return

DO_DELAY MACRO	
    call	delay50ms
ENDM
    
; modify the DAC output from low to high
increasingIntensity:
    call	lightsOn
    movlw	HIGH_DAC_SETTING
    movwf	currentVoltage
    banksel	DACCON1
    
brightenLED:
    movf	currentVoltage, w
    movwf	DACCON1
    goto	$
    DO_DELAY
    incf	currentVoltage, f
    ; check if overflowed into bit 5
    movlw	HIGH_DAC_SETTING + 1
    andwf	currentVoltage, w
    skipnz
    goto	brightenLED
    
    ; now start to dim the LED
    movlw	HIGH_DAC_SETTING - 1
    movwf	currentVoltage
dimLED:
    movf	currentVoltage, w
    movwf	DACCON1
    DO_DELAY
    decf	currentVoltage, f
    ; check if we've gone below 20
    movlw	LOW_DAC_SETTING - 1
    xorwf	currentVoltage, w
    skipz
    goto	dimLED
    ; now start to brighten the LED
    movlw	LOW_DAC_SETTING
    movwf	currentVoltage
    goto	brightenLED
    
increasingFlash:
    movwf	programCounter1
    call	lightsOn	    ; enable DAC output
    call	setLightsHigh	    ; set high DAC output
    call	turnOnStringA
    call	enableStringSwitch   
    ; check when programCounter has reached zero and exit
    btfss	programStatus,PROGRAM_COMPLETE_BIT
    goto	$-1
    bcf		programStatus,PROGRAM_COMPLETE_BIT
    call	disableStringSwitch
    call	lightsOff
    return
    
getNextTMR1HValue:
    movf	TMR1H_index, w
    call	periodVals
    movwf	TMR1H_val	; store returned value
    ; test if 0, i.e. end of table
    iorlw	0x00
    skipnz
;    btfsc	STATUS, STATUS_ZERO_POSN
    goto	resetPeriodTableIndex
    incf	TMR1H_index, f
    return
resetPeriodTableIndex:
    clrf	TMR1H_index
    decfsz	programCounter1,f    ; decrement the program counter
    goto	getNextTMR1HValue
    bsf		programStatus,PROGRAM_COMPLETE_BIT ; set the complete flag
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
    
    call	getNextTMR1HValue
    return
    
processTimer1Expired:
    bcf		TMR1IF	    ; clear interrupt flag
    SET_TIMER1_COUNTER
;    btfsc	programStatus,SWITCH_STRINGS_BIT
    btfsc	programStatus,ENABLE_SWITCH_BIT
    call	toggleStrings
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
  
enableStringSwitch:
    bsf		TMR1IE	    ; enable Timer1 interrupt
    clrf	TMR1H_index
    call	getNextTMR1HValue
    movwf	TMR1H_val
    movlw	TMR0_ROLLOVER_COUNT
    movwf	tmr0RolloverCount
    movlw	TMR0_SECONDS_VALUE
    movwf	tmr0SecondsCount
    bsf         T0IE	    ; enable Timer0 interrupt
    bsf		programStatus,ENABLE_SWITCH_BIT
    return
    
disableStringSwitch:
    bcf		programStatus,ENABLE_SWITCH_BIT
    return
    
rapidFlash:
    call	setLightsHigh	    ; set high DAC output
    call	turnOnStringA
    movlw	0xfc
    call	lightsOn	    ; enable DAC output
    movwf	TMR1H_val		    
    ; Timer1 overflow causes the strings to toggle
    ; enable Timer1 and the status flag that allows string switching
    bsf		programStatus,ENABLE_SWITCH_BIT
    call	delay500ms
    call	lightsOff    ; disable DAC output
    bcf		programStatus,ENABLE_SWITCH_BIT
    return
    
END 
    
#endif